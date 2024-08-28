//! Build

// Modules
mod lock;

// Exports
pub use lock::BuildResult;

// Imports
use {
	self::lock::{BuildLock, BuildLockDepGuard},
	crate::{
		error::ResultMultiple,
		expand,
		rules::{Command, CommandArg, DepItem, Expr, ExprTree, OutItem, Rule, Target},
		util::{self, ArcStr},
		AppError,
		Expander,
		Rules,
	},
	anyhow::Context,
	dashmap::DashMap,
	futures::{stream::FuturesUnordered, StreamExt, TryFutureExt},
	indexmap::IndexMap,
	itertools::Itertools,
	std::{borrow::Cow, collections::BTreeMap, ffi::OsStr, future::Future, ops::Try, sync::Arc, time::SystemTime},
	tokio::{fs, process, sync::Semaphore, task},
};

/// Event
#[derive(Clone, Debug)]
pub enum Event {
	/// Target dependency built
	TargetDepBuilt {
		/// Target that was being built
		target: Target<ArcStr>,

		/// Dependency that was built
		dep: Target<ArcStr>,
	},
}

/// Target rule
#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub struct TargetRule {
	/// Name
	name: ArcStr,

	/// Patterns
	pats: Arc<BTreeMap<ArcStr, ArcStr>>,
}

/// Builder
#[derive(Debug)]
pub struct Builder {
	/// Event sender
	event_tx: async_broadcast::Sender<Event>,

	/// Event receiver
	// Note: We don't care about the receiver, since we can
	//       just create them from the sender, but if dropped
	//       it closes the channel, so we need to keep it around.
	_event_rx: async_broadcast::InactiveReceiver<Event>,

	/// Expander
	expander: Expander,

	/// All rules' build lock
	rules_lock: DashMap<TargetRule, BuildLock>,

	/// Rule output tree
	///
	/// Used to quickly find rules for a file
	rule_output_tree: ExprTree<ArcStr>,

	/// Execution semaphore
	exec_semaphore: Semaphore,

	/// If the execution semaphore should be closed on the first error
	stop_builds_on_first_err: bool,
}

impl Builder {
	/// Creates a new builder
	pub fn new(jobs: usize, rules: &Rules, stop_builds_on_first_err: bool) -> Result<Self, AppError> {
		let (event_tx, event_rx) = async_broadcast::broadcast(jobs);
		let event_rx = event_rx.deactivate();

		let expander = Expander::new();

		// Create the rule output tree
		let mut rule_output_tree = ExprTree::new();
		for (rule_name, rule) in &rules.rules {
			for output in &rule.output {
				// Expand the file first
				// Expand all expressions in the output file
				// Note: This doesn't expand patterns, so we can match those later
				let output_file = match output {
					OutItem::File { file: output_file, .. } => output_file,
				};
				let expand_visitor = expand::Visitor::from_aliases([&rule.aliases, &rules.aliases])
					.with_default_pat(expand::FlowControl::Keep);
				let output_file = expander.expand_expr(output_file, &expand_visitor)?;


				// Then try to insert it
				if let Some(prev_rule_name) = rule_output_tree
					.insert(&output_file, rule_name.clone())
					.context("Unable to add rule output to tree")
					.map_err(AppError::Other)?
				{
					return Err(AppError::Other(anyhow::anyhow!(
						"Multiple rules match the same output file: {output_file}\n  first rule: {prev_rule_name}\n  \
						 second rule: {rule_name}"
					)));
				};
			}
		}

		Ok(Self {
			event_tx,
			_event_rx: event_rx,
			expander,
			rules_lock: DashMap::new(),
			rule_output_tree,
			exec_semaphore: Semaphore::new(jobs),
			stop_builds_on_first_err,
		})
	}

	/// Returns all build results
	pub async fn build_results(&self) -> IndexMap<TargetRule, Option<Result<BuildResult, ()>>> {
		self.rules_lock
			.iter()
			.map(async move |rule_lock| (rule_lock.key().clone(), rule_lock.value().res().await))
			.collect::<FuturesUnordered<_>>()
			.collect()
			.await
	}

	/// Sends an event, if any are subscribers
	async fn send_event(&self, make_event: impl FnOnce() -> Event + Send) {
		// Note: We only send them if there are any receivers (excluding ours, which is inactive),
		//       to ensure we don't deadlock waiting for someone to read the events
		if self.event_tx.receiver_count() > 0 {
			// Note: We don't care about the event
			let _: Option<Event> = self
				.event_tx
				.broadcast(make_event())
				.await
				.expect("Event channel was closed");
		}
	}

	/// Subscribes to builder events
	pub fn subscribe_events(&self) -> async_broadcast::Receiver<Event> {
		self.event_tx.new_receiver()
	}

	/// Finds a target's rule
	fn target_rule(
		&self,
		target: &Target<ArcStr>,
		rules: &Rules,
	) -> Result<Option<(Rule<ArcStr>, TargetRule)>, AppError> {
		let target_rule = match *target {
			// If we got a file, check which rule can make it
			Target::File { ref file, .. } => match self.rule_output_tree.find(file) {
				Some((name, pats)) => {
					tracing::trace!(%target, %name, "Found target rule");
					TargetRule {
						name,
						pats: Arc::new(pats),
					}
				},

				None => return Ok(None),
			},

			// If we got a rule name with patterns, find it and replace all patterns
			Target::Rule { ref rule, ref pats } => TargetRule {
				name: rule.clone(),
				pats: Arc::clone(pats),
			},
		};

		// Find the rule and expand it
		let rule = rules
			.rules
			.get(&*target_rule.name)
			.ok_or_else(|| AppError::UnknownRule {
				rule_name: (*target_rule.name).to_owned(),
			})?;
		let expand_visitor = expand::Visitor::new([&rule.aliases, &rules.aliases], [&target_rule.pats]);
		let rule = self
			.expander
			.expand_rule(rule, &expand_visitor)
			.map_err(AppError::expand_rule(&*rule.name))?;

		Ok(Some((rule, target_rule)))
	}

	/// Resets a build
	pub async fn reset_build(&self, target: &Target<ArcStr>, rules: &Rules) -> Result<(), AppError> {
		// Get the rule for the target
		let Some((_, target_rule)) = self.target_rule(target, rules)? else {
			return Ok(());
		};

		// Get the built lock, or create it
		// Note: Important to clone since we'll be `await`ing with it.
		let build_lock = self
			.rules_lock
			.entry(target_rule)
			.or_insert_with(BuildLock::new)
			.clone();

		// Then remove it's state
		build_lock.lock_build().await.reset();

		Ok(())
	}

	/// Builds an expression-encoded target
	pub async fn build_expr(
		self: &Arc<Self>,
		target: &Target<Expr>,
		rules: &Arc<Rules>,
		ignore_missing: bool,
		reason: BuildReason,
	) -> Result<(BuildResult, Option<BuildLockDepGuard>), AppError> {
		// Expand the target
		let expand_visitor = expand::Visitor::from_aliases([&rules.aliases]);
		let target = self
			.expander
			.expand_target(target, &expand_visitor)
			.map_err(AppError::expand_target(target))?;

		// Then build
		self.build(&target, rules, ignore_missing, reason).await
	}

	/// Builds a target
	// TODO: Remove this wrapper function once the compiler behaves.
	#[expect(
		clippy::manual_async_fn,
		reason = "For some reason, without this wrapper, the compiler
			can't see that `build_inner`'s future is `Send`"
	)]
	pub fn build<'a>(
		self: &'a Arc<Self>,
		target: &'a Target<ArcStr>,
		rules: &'a Arc<Rules>,
		ignore_missing: bool,
		reason: BuildReason,
	) -> impl Future<Output = Result<(BuildResult, Option<BuildLockDepGuard>), AppError>> + Send + 'a {
		async move { self.build_inner(target, rules, ignore_missing, reason).await }
	}

	/// Inner function for [`Builder::build`]
	#[expect(clippy::too_many_lines, reason = "TODO: Split it up more")]
	pub async fn build_inner<'a>(
		self: &'a Arc<Self>,
		target: &'a Target<ArcStr>,
		rules: &'a Arc<Rules>,
		ignore_missing: bool,
		reason: BuildReason,
	) -> Result<(BuildResult, Option<BuildLockDepGuard>), AppError> {
		tracing::trace!(%target, reason=?reason.0.as_ref().map(|reason| &reason.target), "Building target");

		// Normalize file paths
		let target = match *target {
			Target::File { ref file, is_static } => Target::File {
				file: util::normalize_path(file).into(),
				is_static,
			},
			ref target @ Target::Rule { .. } => target.clone(),
		};
		let target = Arc::new(target);

		// Check if we're being built recursively, and if so, return error
		reason.for_each(|parent_target| match &*target == parent_target {
			true => Err(AppError::FoundRecursiveRule {
				target:         target.to_string(),
				parent_targets: reason.collect_all().iter().map(Target::to_string).collect(),
			}),
			false => Ok(()),
		})?;

		// Get the rule for the target
		let Some((rule, target_rule)) = self.target_rule(&target, rules)? else {
			match *target {
				Target::File { ref file, .. } => match fs::symlink_metadata(&**file).await {
					Ok(metadata) => {
						let build_time = metadata.modified().map_err(AppError::get_file_modified_time(&**file))?;
						tracing::trace!(%target, ?build_time, "Found target file");
						return Ok((
							BuildResult {
								build_time,
								built: false,
							},
							None,
						));
					},
					Err(_) if ignore_missing => {
						tracing::debug!(?file, "Ignoring missing target file");
						return Ok((
							BuildResult {
								// Note: We simply pretend the file was built right now
								// TODO: Check if we should instead use a really old time?
								build_time: SystemTime::now(),
								built:      false,
							},
							None,
						));
					},
					Err(err) =>
						return Err(AppError::MissingFile {
							file_path: (**file).into(),
							source:    err,
						}),
				},
				// Note: If `target_rule` returns `Err` if this was a rule, so we can never reach here
				Target::Rule { .. } => unreachable!(),
			}
		};

		// Get the built lock, or create it
		let build_lock = self
			.rules_lock
			.entry(target_rule)
			.or_insert_with(BuildLock::new)
			.clone();

		// Then check if we're built, and if not lock for building
		let mut build_guard = loop {
			// First check if we're done with a dependency lock
			let build_guard = build_lock.lock_dep().await;
			if let Some(res) = build_guard.res() {
				return res
					.map(|res| (res, Some(build_guard)))
					.map_err(|()| AppError::BuildTarget {
						source: None,
						target: target.to_string(),
					});
			}

			// Otherwise, try to upgrade to a build lock
			// Note: If we were unable to, that means someone else locked, so retry
			//       and lock for dependency for when they finish
			// Note: We drop the lock in the `Err` case, and then immediately try to
			//       get it again at the start of the loop, but since the lock is *fair*,
			//       the writer should have priority, so this shouldn't result in much
			//       waiting for them.
			let res = build_guard.try_upgrade_into_build().await;
			match res {
				Ok(build_guard) => break build_guard,
				Err(_) => continue,
			}
		};

		let res = task::Builder::new()
			.name(&format!("Build {target}"))
			.spawn({
				let this = Arc::clone(self);
				let target = Arc::clone(&target);
				let rules = Arc::clone(rules);
				async move {
					this.build_unchecked(&target, &rule, &rules, ignore_missing, reason)
						.await
				}
			})
			.context("Unable to execute task")
			.map_err(AppError::Other)?
			.await
			.context("Unable to join task")
			.map_err(AppError::Other)?;

		match res {
			Ok(res) => {
				build_guard.finish(res);

				Ok((res, Some(build_guard.into_dep())))
			},
			Err(err) => {
				// If we should, close the exec semaphore to ensure we exit as early as possible
				// Note: This check is racy, but it's fine to print this warning multiple times. We just don't want
				//       to spam the user, since all further errors will likely caused by `AppError::ExecSemaphoreClosed`,
				//       while the first few are the useful ones with the reason why the execution semaphore is being closed.
				if self.stop_builds_on_first_err && !self.exec_semaphore.is_closed() {
					tracing::debug!(err=%err.pretty(), "Stopping all future builds due to failure of target {target}");
					self.exec_semaphore.close();
				}

				build_guard.finish_failed();
				Err(err)
			},
		}
	}

	/// Builds a target without checking if the target is already being built.
	#[expect(clippy::too_many_lines, reason = "TODO: Split this function onto smaller ones")]
	async fn build_unchecked(
		self: &Arc<Self>,
		target: &Target<ArcStr>,
		rule: &Rule<ArcStr>,
		rules: &Arc<Rules>,
		ignore_missing: bool,
		reason: BuildReason,
	) -> Result<BuildResult, AppError> {
		/// Dependency
		#[derive(Clone, Debug)]
		enum Dep {
			/// File
			File {
				file:         ArcStr,
				is_static:    bool,
				is_deps_file: bool,
				is_output:    bool,
				is_optional:  bool,
				exists:       bool,
			},

			/// Rule
			Rule {
				name: ArcStr,
				pats: Arc<BTreeMap<ArcStr, ArcStr>>,
			},
		}

		// Gather all normal dependencies
		let normal_deps = rule
			.deps
			.iter()
			.map(move |dep| async move {
				match *dep {
					DepItem::File {
						ref file,
						is_optional,
						is_static,
						is_deps_file,
					} => Ok(Dep::File {
						file: file.clone(),
						is_static,
						is_deps_file,
						is_output: false,
						is_optional,
						exists: util::fs_try_exists_symlink(&**file)
							.await
							.map_err(AppError::check_file_exists(&**file))?,
					}),
					DepItem::Rule { ref name, ref pats } => Ok(Dep::Rule {
						name: name.clone(),
						pats: Arc::clone(pats),
					}),
				}
			})
			.collect::<FuturesUnordered<_>>()
			.collect::<Vec<_>>()
			.await
			.into_iter()
			.collect::<ResultMultiple<Vec<_>>>()?;

		// And all output dependencies
		#[expect(
			clippy::match_wildcard_for_single_variants,
			reason = "We only care about dependency file variants"
		)]
		let out_deps = rule
			.output
			.iter()
			.map(move |out| async move {
				match out {
					OutItem::File {
						file,
						is_deps_file: true,
					} => Ok(Some(Dep::File {
						file:         file.clone(),
						is_static:    false,
						is_deps_file: true,
						is_output:    true,
						is_optional:  false,
						exists:       util::fs_try_exists_symlink(&**file)
							.await
							.map_err(AppError::check_file_exists(&**file))?,
					})),
					_ => Ok(None),
				}
			})
			.collect::<FuturesUnordered<_>>()
			.filter_map(move |res| async move { res.transpose() })
			.collect::<Vec<_>>()
			.await
			.into_iter()
			.collect::<ResultMultiple<Vec<_>>>()?;

		// Then build all dependencies, as well as any dependency files
		// TODO: Don't collect like 3 times during this
		let deps = util::chain!(normal_deps, out_deps)
			.map(|dep| {
				tracing::trace!(%target, ?rule.name, ?dep, "Found target rule dependency");
				let rule = &rule;
				let reason = &reason;
				async move {
					tracing::trace!(%target, ?rule.name, ?dep, "Building target rule dependency");

					// Get the target to build
					let dep_target = match dep {
						// If output, don't do anything
						Dep::File { is_output: true, .. } => None,

						// Else build it
						Dep::File {
							ref file, is_static, ..
						} => Some(Target::File {
							file: file.clone(),
							is_static,
						}),

						// If a rule, always build
						Dep::Rule { ref name, ref pats } => Some(Target::Rule {
							rule: name.clone(),
							pats: Arc::clone(pats),
						}),
					};

					// Then build it, if we should
					let dep_res = match dep_target {
						Some(dep_target) => {
							let (res, dep_guard) = self
								.build(
									&dep_target,
									rules,
									ignore_missing | matches!(dep, Dep::File { is_optional: true, .. }),
									reason.with_target(target.clone()),
								)
								.await
								.map_err(AppError::build_target(&dep_target))?;
							tracing::trace!(%target, ?rule.name, ?dep, ?res, "Built target rule dependency");

							self.send_event(|| Event::TargetDepBuilt {
								target: target.clone(),
								dep:    dep_target.clone(),
							})
							.await;

							Some((dep_target, res, dep_guard))
						},

						None => None,
					};

					// If the dependency if a dependency deps file or an output deps file (and exists), build it's dependencies too
					#[expect(clippy::wildcard_enum_match_arm, reason = "We only care about some variants")]
					let dep_deps = match &dep {
						// Non-optional we don't check if they exist, so that an error
						// pops up if they don't.
						Dep::File {
							file,
							is_deps_file: true,
							is_output: false,
							is_optional: false,
							..
						} |
						// For optional or output, we only include them if they exist
						Dep::File {
							file,
							is_deps_file: true,
							is_optional: true,
							exists: true,
							..
						} |
						Dep::File {
							file,
							is_deps_file: true,
							is_output: true,
							exists: true,
							..
						} => self
							.build_deps_file(target, file, rule, rules, ignore_missing, reason)
							.await
							.map_err(AppError::build_deps_file(&**file))?,
						_ => vec![],
					};

					let deps = util::chain!(dep_res, dep_deps.into_iter()).collect::<Vec<_>>();
					tracing::trace!(%target, ?rule.name, ?dep, ?deps, "Built target rule dependency dependencies");

					Ok(deps)
				}
			})
			.collect::<FuturesUnordered<_>>()
			.collect::<Vec<_>>()
			.await
			.into_iter()
			.collect::<ResultMultiple<Vec<_>>>()?
			.into_iter()
			.flatten()
			.collect::<Vec<_>>();

		let deps_last_build_time = deps
			.iter()
			.filter(|(dep_target, ..)| !dep_target.is_static())
			.map(|(_, dep_res, _)| dep_res.build_time)
			.max();
		tracing::trace!(%target, ?rule.name, ?deps_last_build_time, ?deps, "Built target rule dependencies");

		// Afterwards check the last time we've built the rule and compare it with
		// the dependency build times.
		let rule_last_build_time = self::rule_last_build_time(rule).await;
		let needs_rebuilt = match (deps_last_build_time, &rule_last_build_time) {
			// If any files were missing, or we had no outputs, build
			(_, Err(_) | Ok(None)) => true,

			// If no dependencies and all outputs exist, don't rebuild
			(None, Ok(_)) => false,

			// If we have dependencies and outputs, rebuild if the dependencies are
			// newer than the outputs
			(Some(deps_last_build_time), Ok(Some(rule_last_build_time))) =>
				deps_last_build_time > *rule_last_build_time,
		};

		// Then rebuild, if needed
		if needs_rebuilt {
			tracing::trace!(%target, ?rule.name, ?deps_last_build_time, ?rule_last_build_time, "Rebuilding target rule");
			self.rebuild_rule(rule)
				.await
				.map_err(AppError::build_rule(&*rule.name))?;
		}

		// Then get the build time
		// Note: If we don't have any outputs, just use the current time as the build time
		let cur_build_time = self::rule_last_build_time(rule).await?.unwrap_or_else(SystemTime::now);
		let res = BuildResult {
			build_time: cur_build_time,
			built:      needs_rebuilt,
		};

		Ok(res)
	}

	/// Builds all dependencies of a `deps` file.
	///
	/// Returns the latest modification date of the dependencies
	async fn build_deps_file(
		self: &Arc<Self>,
		parent_target: &Target<ArcStr>,
		deps_file: &str,
		rule: &Rule<ArcStr>,
		rules: &Arc<Rules>,
		ignore_missing: bool,
		reason: &BuildReason,
	) -> Result<Vec<(Target<ArcStr>, BuildResult, Option<BuildLockDepGuard>)>, AppError> {
		tracing::trace!(target=?parent_target, ?rule.name, ?deps_file, "Building dependencies of target rule dependency-file");
		let (output, deps) = self::parse_deps_file(deps_file).await?;

		match rule.output.is_empty() {
			// If there were no outputs, make sure it matches the rule name
			// TODO: Seems kinda weird for it to match the rule name, but not sure how else to check this here
			true =>
				if output != *rule.name {
					return Err(AppError::DepFileMissingRuleName {
						deps_file_path: deps_file.into(),
						rule_name:      rule.name.to_string(),
						dep_output:     output,
					});
				},

			// If there were any output, make sure the dependency file applies to one of them
			false => {
				let any_matches = rule.output.iter().any(|out| match out {
					OutItem::File { file, .. } => **file == output,
				});
				if !any_matches {
					return Err(AppError::DepFileMissingOutputs {
						deps_file_path: deps_file.into(),
						rule_outputs:   rule.output.iter().map(OutItem::to_string).collect(),
						dep_output:     output,
					});
				}
			},
		}

		// Build all dependencies
		// Note: We don't want to fail-early, see the note on `deps` in `build_unchecked`
		let deps_res = deps
			.into_iter()
			.map(|dep| {
				let dep = ArcStr::from(util::normalize_path(&dep));
				tracing::trace!(?rule.name, ?dep, "Found rule dependency");
				let dep_target = Target::File {
					file:      dep,
					is_static: false,
				};
				async move {
					let (res, dep_guard) = self
						.build(
							&dep_target,
							rules,
							ignore_missing,
							reason.with_target(parent_target.clone()),
						)
						.await
						.map_err(AppError::build_target(&dep_target))?;

					self.send_event(|| Event::TargetDepBuilt {
						target: parent_target.clone(),
						dep:    dep_target.clone(),
					})
					.await;

					Ok((dep_target, res, dep_guard))
				}
			})
			.collect::<FuturesUnordered<_>>()
			.collect::<Vec<_>>()
			.await
			.into_iter()
			.collect::<ResultMultiple<_>>()?;

		Ok(deps_res)
	}

	/// Rebuilds a rule
	pub async fn rebuild_rule(&self, rule: &Rule<ArcStr>) -> Result<(), AppError> {
		// Lock the semaphore
		// Note: If we locked it per-command, we could exit earlier
		//       when closed, but that would break some executions.
		//       For example, some executions emit an output file and
		//       then need to treat it. If we stopped in the middle,
		//       when trying again the file would already exist, but
		//       be in a "bad state", and since all the modification dates
		//       match it wouldn't be rebuilt.
		let Ok(_permit) = self.exec_semaphore.acquire().await else {
			do yeet AppError::ExecSemaphoreClosed {};
		};

		for cmd in &rule.exec.cmds {
			self.exec_cmd(&rule.name, cmd).await?;
		}

		Ok(())
	}

	/// Executes a command
	#[expect(unused_results, reason = "Due to the builder pattern of `Command`")]
	async fn exec_cmd(&self, rule_name: &str, cmd: &Command<ArcStr>) -> Result<(), AppError> {
		// Process all arguments
		let args = cmd
			.args
			.iter()
			.map(move |arg| async move {
				match arg {
					CommandArg::Expr(arg) => Ok(Some(Cow::Borrowed(OsStr::new(&**arg)))),
				}
			})
			.enumerate()
			.map(|(idx, fut)| fut.map_ok(move |arg| (idx, arg)))
			.collect::<FuturesUnordered<_>>()
			.collect::<Vec<_>>()
			.await
			.into_iter()
			.collect::<ResultMultiple<Vec<_>>>()?
			.into_iter()
			.sorted_by_key(|&(idx, _)| idx)
			.filter_map(|(_, arg)| arg)
			.collect::<Vec<_>>();

		let (program, args) = args.split_first().ok_or_else(|| AppError::RuleExecEmpty {
			rule_name: rule_name.to_owned(),
		})?;

		// Create the command
		let mut os_cmd = process::Command::new(&**program);
		os_cmd.args(args.iter().map(|arg| &**arg));

		// Set the working directory, if we have any
		if let Some(cwd) = &cmd.cwd {
			os_cmd.current_dir(&**cwd);
		}

		// Then spawn it and measure
		tracing::debug!(target: "zbuild_exec", "{} {}",
			program.to_string_lossy(),
			args.iter().map(|arg| arg.to_string_lossy()).join(" ")
		);
		let (duration, ()) = util::try_measure_async(async {
			os_cmd
				.status()
				.await
				.map_err(AppError::spawn_command(cmd))?
				.exit_ok()
				.map_err(AppError::command_failed(cmd))
		})
		.await?;
		tracing::trace!(target: "zbuild_exec", ?rule_name, ?program, ?args, ?duration, "Execution duration");

		Ok(())
	}
}

/// Parses a dependencies file
// TODO: Support multiple dependencies in each file
async fn parse_deps_file(file: &str) -> Result<(String, Vec<String>), AppError> {
	// Read it
	let contents = fs::read_to_string(file).await.map_err(AppError::read_file(file))?;

	// Parse it
	let (output, deps) = contents.split_once(':').ok_or_else(|| AppError::DepFileMissingColon {
		deps_file_path: file.into(),
	})?;
	let output = output.trim().to_owned();
	let deps = deps.split_whitespace().map(str::to_owned).collect();

	Ok((output, deps))
}

/// Returns the last build time of a rule.
///
/// Returns `Err` if any files didn't exist,
/// Returns `Ok(None)` if rule has no outputs
async fn rule_last_build_time(rule: &Rule<ArcStr>) -> Result<Option<SystemTime>, AppError> {
	// Note: We get the time of the oldest file in order to ensure all
	//       files are at-least that old
	let built_time = rule
		.output
		.iter()
		.map(move |item| async move {
			let file = match item {
				OutItem::File { file, .. } => file,
			};
			let metadata = fs::symlink_metadata(&**file)
				.await
				.map_err(AppError::read_file_metadata(&**file))?;
			let modified_time = metadata.modified().map_err(AppError::get_file_modified_time(&**file))?;

			Ok(modified_time)
		})
		.collect::<FuturesUnordered<_>>()
		.collect::<Vec<_>>()
		.await
		.into_iter()
		.collect::<ResultMultiple<Vec<_>>>()?
		.into_iter()
		.min();
	Ok(built_time)
}

/// Build reason inner
#[derive(Clone, Debug)]
pub struct BuildReasonInner {
	/// Target
	target: Target<ArcStr>,

	/// Previous reason
	prev: BuildReason,
}

/// Build reason
#[derive(Clone, Debug)]
pub struct BuildReason(Option<Arc<BuildReasonInner>>);

impl BuildReason {
	/// Creates an empty build reason
	pub const fn empty() -> Self {
		Self(None)
	}

	/// Adds a target to this build reason
	pub fn with_target(&self, target: Target<ArcStr>) -> Self {
		Self(Some(Arc::new(BuildReasonInner {
			target,
			prev: self.clone(),
		})))
	}

	/// Iterates over all reasons
	pub fn for_each<F, R>(&self, mut f: F) -> R
	where
		F: FnMut(&Target<ArcStr>) -> R,
		R: Try<Output = ()>,
	{
		let mut reason = &self.0;
		while let Some(inner) = reason {
			f(&inner.target)?;
			reason = &inner.prev.0;
		}

		R::from_output(())
	}

	/// Collects all reasons
	pub fn collect_all(&self) -> Vec<Target<ArcStr>> {
		let mut targets = vec![];

		let mut reason = &self.0;
		while let Some(inner) = reason {
			targets.push(inner.target.clone());
			reason = &inner.prev.0;
		}

		targets
	}
}
