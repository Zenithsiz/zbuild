//! Build

// Modules
mod expand_visitor;
mod lock;
mod match_expr;

// Exports
pub use lock::BuildResult;

// Imports
use {
	self::{
		expand_visitor::{GlobalVisitor, RuleOutputVisitor, RuleVisitor},
		lock::{BuildLock, BuildLockDepGuard},
		match_expr::match_expr,
	},
	crate::{
		rules::{DepItem, Expr, OutItem, Rule, Target},
		util::{self, CowStr},
		AppError,
		Expander,
		Rules,
	},
	dashmap::DashMap,
	filetime::FileTime,
	futures::{stream::FuturesUnordered, StreamExt, TryStreamExt},
	itertools::Itertools,
	std::{
		collections::HashMap,
		mem,
		time::{Duration, SystemTime},
	},
	tokio::{fs, process::Command, sync::Semaphore},
};

// TODO: If the user zbuild file is not generated properly, it can
//       make us deadlock, check for cycles somehow

/// Event
#[derive(Clone, Debug)]
pub enum Event<'s> {
	/// Target dependency built
	TargetDepBuilt {
		/// Target that was being built
		target: Target<'s, CowStr<'s>>,

		/// Dependency that was built
		dep: Target<'s, CowStr<'s>>,
	},
}

/// Target rule
#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub struct TargetRule<'s> {
	/// Name
	name: &'s str,

	/// Patterns (sorted)
	pats: Vec<(CowStr<'s>, CowStr<'s>)>,
}

/// Builder
#[derive(Debug)]
pub struct Builder<'s> {
	/// Event sender
	event_tx: async_broadcast::Sender<Event<'s>>,

	/// Event receiver
	// Note: We don't care about the receiver, since we can
	//       just create them from the sender, but if dropped
	//       it closes the channel, so we need to keep it around.
	_event_rx: async_broadcast::InactiveReceiver<Event<'s>>,

	/// Expander
	expander: Expander<'s>,

	/// All rules' build lock
	rules_lock: DashMap<TargetRule<'s>, BuildLock<'s>>,

	/// Execution semaphore
	exec_semaphore: Semaphore,
}

impl<'s> Builder<'s> {
	/// Creates a new builder
	pub fn new(jobs: usize) -> Self {
		let (event_tx, event_rx) = async_broadcast::broadcast(jobs);
		let event_rx = event_rx.deactivate();

		Self {
			event_tx,
			_event_rx: event_rx,
			expander: Expander::new(),
			rules_lock: DashMap::new(),
			exec_semaphore: Semaphore::new(jobs),
		}
	}

	/// Returns all build results
	pub fn into_build_results(self) -> HashMap<Target<'s, CowStr<'s>>, Result<BuildResult, AppError>> {
		self.rules_lock
			.into_iter()
			.flat_map(|(_, lock)| lock.into_res())
			.collect()
	}

	/// Sends an event, if any are subscribers
	async fn send_event(&self, make_event: impl FnOnce() -> Event<'s> + Send) {
		// Note: We only send them if there are any receivers (excluding ours, which is inactive),
		//       to ensure we don't deadlock waiting for someone to read the events
		if self.event_tx.receiver_count() > 0 {
			// Note: We don't care about the event
			let _: Option<Event<'_>> = self
				.event_tx
				.broadcast(make_event())
				.await
				.expect("Event channel was closed");
		}
	}

	/// Subscribes to builder events
	pub fn subscribe_events(&self) -> async_broadcast::Receiver<Event<'s>> {
		self.event_tx.new_receiver()
	}

	/// Finds a target's rule
	fn target_rule(
		&self,
		target: &Target<'s, CowStr<'s>>,
		rules: &Rules<'s>,
	) -> Result<Option<(Rule<'s, CowStr<'s>>, TargetRule<'s>)>, AppError> {
		let target_rule = match *target {
			// If we got a file, check which rule can make it
			Target::File { ref file, .. } => match self.find_rule_for_file(file, rules)? {
				Some((rule, pats)) => {
					tracing::trace!(?target, ?rule.name, "Found target rule");
					let target_rule = TargetRule {
						name: rule.name,
						pats: pats.into_iter().sorted().collect(),
					};
					(rule, target_rule)
				},

				None => return Ok(None),
			},

			// If we got a rule name with patterns, find it and replace all patterns
			Target::Rule { ref rule, ref pats } => {
				// Find the rule and expand it
				let rule = rules.rules.get(&**rule).ok_or_else(|| AppError::UnknownRule {
					rule_name: (**rule).to_owned(),
				})?;
				let rule = self
					.expander
					.expand_rule(rule, &mut RuleVisitor::new(&rules.aliases, &rule.aliases, pats))
					.map_err(AppError::expand_rule(rule.name))?;
				let target_rule = TargetRule {
					name: rule.name,
					pats: pats
						.iter()
						.map(|(pat, value)| (pat.clone(), value.clone()))
						.sorted()
						.collect(),
				};
				(rule, target_rule)
			},
		};

		Ok(Some(target_rule))
	}

	/// Resets a build
	pub async fn reset_build(&self, target: &Target<'s, CowStr<'s>>, rules: &Rules<'s>) -> Result<(), AppError> {
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
		build_lock.lock_build().await.reset(target);

		Ok(())
	}

	/// Builds an expression-encoded target
	pub async fn build_expr(
		&self,
		target: &Target<'s, Expr<'s>>,
		rules: &Rules<'s>,
		ignore_missing: bool,
	) -> Result<(BuildResult, Option<BuildLockDepGuard<'s>>), AppError> {
		// Expand the target
		let target = self
			.expander
			.expand_target(target, &mut GlobalVisitor::new(&rules.aliases))
			.map_err(AppError::expand_target(target))?;

		// Then build
		self.build(&target, rules, ignore_missing).await
	}

	/// Builds a target
	pub async fn build(
		&self,
		target: &Target<'s, CowStr<'s>>,
		rules: &Rules<'s>,
		ignore_missing: bool,
	) -> Result<(BuildResult, Option<BuildLockDepGuard<'s>>), AppError> {
		tracing::trace!(?target, "Building target");

		// Normalize file paths
		let target = match *target {
			Target::File { ref file, is_static } => Target::File {
				file: CowStr::Owned(util::normalize_path(file)),
				is_static,
			},
			ref target @ Target::Rule { .. } => target.clone(),
		};

		// Get the rule for the target
		let Some((rule, target_rule)) = self.target_rule(&target, rules)? else {
			match target {
				Target::File { ref file, .. } => match fs::metadata(&**file).await {
					Ok(metadata) => {
						let build_time = self::file_modified_time(metadata);
						tracing::trace!(?target, ?build_time, "Found target file");
						return Ok((
							BuildResult {
								build_time,
								built_here: false,
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
								built_here: false,
								built:      false,
							},
							None,
						));
					},
					Err(err) =>
						do yeet AppError::MissingFile {
							file_path: (**file).into(),
							err,
						},
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

		// Then check if built
		let build_guard = build_lock.lock_dep().await;
		match build_guard.res(&target) {
			// If we got it, we were built, so just return the guard
			Some(res) => res.map(|res| (res, Some(build_guard))),

			// Else build first
			// Note: Tokio read lock don't support upgrading, so we do a double-checked
			//       lock here.
			None => {
				mem::drop(build_guard);
				#[expect(clippy::shadow_unrelated)] // They are the same even if redeclared
				let mut build_guard = build_lock.lock_build().await;

				match build_guard.res(&target) {
					// If we got it in the meantime, return it
					Some(res) => res.map(|res| (res, Some(build_guard.into_dep()))),

					// Else build
					None => {
						let res = self.build_unchecked(&target, &rule, rules, ignore_missing).await;
						let res = build_guard.finish(&target, res);
						res.map(|res| (res, Some(build_guard.into_dep())))
					},
				}
			},
		}
	}

	/// Builds a target without checking if the target is already being built.
	#[expect(clippy::too_many_lines)] // TODO: Split this function onto smaller ones
	#[async_recursion::async_recursion]
	async fn build_unchecked(
		&self,
		target: &Target<'s, CowStr<'s>>,
		rule: &Rule<'s, CowStr<'s>>,
		rules: &Rules<'s>,
		ignore_missing: bool,
	) -> Result<BuildResult, AppError> {
		/// Dependency
		#[derive(Clone, Debug)]
		enum Dep<'s, 'a> {
			/// File
			File {
				file:        CowStr<'s>,
				is_static:   bool,
				is_dep_file: bool,
				is_output:   bool,
				is_optional: bool,
				exists:      bool,
			},

			/// Rule
			Rule {
				name: CowStr<'s>,
				pats: &'a HashMap<CowStr<'s>, CowStr<'s>>,
			},
		}

		// Gather all normal dependencies
		// Note: It's fine to fail early here, we don't need to check all files
		//       if one fails
		let normal_deps = rule
			.deps
			.iter()
			.map(async move |dep| match *dep {
				DepItem::File {
					ref file,
					is_optional,
					is_static,
				} => Ok(Dep::File {
					file: file.clone(),
					is_static,
					is_dep_file: false,
					is_output: false,
					is_optional,
					exists: util::fs_try_exists(&**file)
						.await
						.map_err(AppError::check_file_exists(&**file))?,
				}),
				DepItem::DepsFile {
					ref file,
					is_optional,
					is_static,
				} => Ok(Dep::File {
					file: file.clone(),
					is_static,
					is_dep_file: true,
					is_output: false,
					is_optional,
					exists: util::fs_try_exists(&**file)
						.await
						.map_err(AppError::check_file_exists(&**file))?,
				}),
				DepItem::Rule { ref name, ref pats } => Ok(Dep::Rule {
					name: name.clone(),
					pats,
				}),
			})
			.collect::<FuturesUnordered<_>>()
			.try_collect::<Vec<_>>()
			.await?;

		// And all output dependencies
		// Note: Fine to fail early here, see note above for `normal_deps`
		let out_deps = rule
			.output
			.iter()
			.map(async move |out| match out {
				OutItem::File { .. } => Ok(None),
				OutItem::DepsFile { file } => Ok(Some(Dep::File {
					file:        file.clone(),
					is_static:   false,
					is_dep_file: true,
					is_output:   true,
					is_optional: false,
					exists:      util::fs_try_exists(&**file)
						.await
						.map_err(AppError::check_file_exists(&**file))?,
				})),
			})
			.collect::<FuturesUnordered<_>>()
			.filter_map(async move |res| res.transpose())
			.try_collect::<Vec<_>>()
			.await?;

		// Then build all dependencies, as well as any dependency files
		// Note: We don't want to fail early here, since commands might still be
		//       running, so we first collect all results, and then fail
		// TODO: Cancel dependencies that haven't even started building yet before failing?
		// TODO: Don't collect like 3 times during this
		let deps = util::chain!(normal_deps, out_deps)
			.map(|dep| {
				tracing::trace!(?target, ?rule.name, ?dep, "Found target rule dependency");
				let rule = &rule;
				async move {
					tracing::trace!(?target, ?rule.name, ?dep, "Building target rule dependency");

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
						Dep::Rule { ref name, pats } => Some(Target::Rule {
							rule: name.clone(),
							pats: pats.clone(),
						}),
					};

					// Then build it, if we should
					let (dep_res, dep_guard) = match &dep_target {
						Some(dep_target) => {
							let (res, dep_guard) = self
								.build(
									dep_target,
									rules,
									ignore_missing | matches!(dep, Dep::File { is_optional: true, .. }),
								)
								.await
								.map_err(AppError::build_target(dep_target))?;
							tracing::trace!(?target, ?rule.name, ?dep, ?res, "Built target rule dependency");

							self.send_event(|| Event::TargetDepBuilt {
								target: target.clone(),
								dep:    dep_target.clone(),
							})
							.await;

							(Some(res), Some(dep_guard))
						},

						None => (None, None),
					};

					// If the dependency if a dependency deps file or an output deps file (and exists), build it's dependencies too
					#[allow(clippy::wildcard_enum_match_arm, reason = "We only care about some variants")]
					let dep_deps = match &dep {
						Dep::File {
							file,
							is_dep_file: true,
							is_output: false,
							..
						} |
						Dep::File {
							file,
							is_dep_file: true,
							is_output: true,
							exists: true,
							..
						} => self
							.build_deps_file(target, file, rule, rules, ignore_missing)
							.await
							.map_err(AppError::build_dep_file(&**file))?,
						_ => vec![],
					};

					let deps = util::chain!(
						match (dep_target, dep_res, dep_guard) {
							(Some(dep_target), Some(dep_res), Some(dep_guard)) =>
								Some((dep_target, dep_res, dep_guard)),
							(None, None, None) => None,
							_ => unreachable!(),
						},
						dep_deps.into_iter()
					)
					.collect::<Vec<_>>();
					tracing::trace!(?target, ?rule.name, ?dep, ?deps, "Built target rule dependency dependencies");

					Ok(deps)
				}
			})
			.collect::<FuturesUnordered<_>>()
			.collect::<Vec<_>>()
			.await
			.into_iter()
			.collect::<Result<Vec<_>, _>>()?
			.into_iter()
			.flatten()
			.collect::<Vec<_>>();

		let deps_last_build_time = deps
			.iter()
			.filter(|(dep_target, ..)| !dep_target.is_static())
			.map(|(_, dep_res, _)| dep_res.build_time)
			.max();
		tracing::trace!(?target, ?rule.name, ?deps_last_build_time, ?deps, "Built target rule dependencies");

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
			tracing::trace!(?target, ?rule.name, "Rebuilding target rule");
			self.rebuild_rule(rule).await.map_err(AppError::build_rule(rule.name))?;
		}

		// Then get the build time
		// Note: If we don't have any outputs, just use the current time as the build time
		let cur_build_time = self::rule_last_build_time(rule).await?.unwrap_or_else(SystemTime::now);
		let res = BuildResult {
			build_time: cur_build_time,
			built_here: needs_rebuilt,
			built:      needs_rebuilt,
		};

		Ok(res)
	}

	/// Builds all dependencies of a `deps` file.
	///
	/// Returns the latest modification date of the dependencies
	async fn build_deps_file(
		&self,
		parent_target: &Target<'s, CowStr<'s>>,
		dep_file: &str,
		rule: &Rule<'s, CowStr<'s>>,
		rules: &Rules<'s>,
		ignore_missing: bool,
	) -> Result<Vec<(Target<'s, CowStr<'s>>, BuildResult, Option<BuildLockDepGuard<'s>>)>, AppError> {
		tracing::trace!(target=?parent_target, ?rule.name, ?dep_file, "Building dependencies of target rule dependency-file");
		let (output, deps) = self::parse_deps_file(dep_file).await?;

		match rule.output.is_empty() {
			// If there were no outputs, make sure it matches the rule name
			// TODO: Seems kinda weird for it to match the rule name, but not sure how else to check this here
			true =>
				if output != rule.name {
					return Err(AppError::DepFileMissingRuleName {
						dep_file_path: dep_file.into(),
						rule_name:     rule.name.to_owned(),
						dep_output:    output,
					});
				},

			// If there were any output, make sure the dependency file applies to one of them
			false => {
				let any_matches = rule.output.iter().any(|out| match out {
					OutItem::File { file } | OutItem::DepsFile { file } => file == &output,
				});
				if !any_matches {
					return Err(AppError::DepFileMissingOutputs {
						dep_file_path: dep_file.into(),
						rule_outputs:  rule.output.iter().map(OutItem::to_string).collect(),
						dep_output:    output,
					});
				}
			},
		}

		// Build all dependencies
		// Note: We don't want to fail-early, see the note on `deps` in `build_unchecked`
		let deps_res = deps
			.into_iter()
			.map(|dep| {
				let dep = CowStr::Owned(util::normalize_path(&dep));
				tracing::trace!(?rule.name, ?dep, "Found rule dependency");
				let dep_target = Target::File {
					file:      dep,
					is_static: false,
				};
				async move {
					let (res, dep_guard) = self
						.build(&dep_target, rules, ignore_missing)
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
			.collect::<Result<Vec<_>, _>>()?;

		Ok(deps_res)
	}

	/// Rebuilds a rule
	#[expect(unused_results)] // Due to the builder pattern of `Command`
	pub async fn rebuild_rule(&self, rule: &Rule<'s, CowStr<'s>>) -> Result<(), AppError> {
		// Lock the semaphore
		let _permit = self
			.exec_semaphore
			.acquire()
			.await
			.context("Executable semaphore was closed")
			.map_err(AppError::Other)?;

		for cmd in &rule.exec.cmds {
			let (program, args) = cmd.args.split_first().ok_or_else(|| AppError::RuleExecEmpty {
				rule_name: rule.name.to_owned(),
			})?;

			// Create the command
			let mut os_cmd = Command::new(&**program);
			os_cmd.args(args.iter().map(|arg| &**arg));

			// Set the working directory, if we have any
			if let Some(cwd) = &rule.exec.cwd {
				os_cmd.current_dir(&**cwd);
			}

			// Then spawn it and measure
			tracing::debug!(target: "zbuild_exec", "{} {}", program, args.join(" "));
			let (duration, ()) = util::try_measure_async(async {
				os_cmd
					.spawn()
					.map_err(AppError::spawn_command(cmd))?
					.wait()
					.await
					.map_err(AppError::wait_command(cmd))?
					.exit_ok()
					.map_err(AppError::command_failed(cmd))
			})
			.await?;
			tracing::trace!(target: "zbuild_exec", rule_name=?rule.name, ?program, ?args, ?duration, "Execution duration");
		}

		Ok(())
	}

	/// Finds a rule for `file`
	// TODO: Not make this `O(N)` for the number of rules.
	#[expect(clippy::type_complexity)] // TODO: Add some type aliases / struct
	pub fn find_rule_for_file(
		&self,
		file: &str,
		rules: &Rules<'s>,
	) -> Result<Option<(Rule<'s, CowStr<'s>>, HashMap<CowStr<'s>, CowStr<'s>>)>, AppError> {
		for rule in rules.rules.values() {
			for output in &rule.output {
				// Expand all expressions in the output file
				// Note: This doesn't expand patterns, so we can match those later
				let output_file = match output {
					OutItem::File { file: output_file } | OutItem::DepsFile { file: output_file } => output_file,
				};
				let output_file = self
					.expander
					.expand_expr(output_file, &mut RuleOutputVisitor::new(&rules.aliases, &rule.aliases))?;

				// Then try to match the output file to the file we need to create
				if let Some(rule_pats) = self::match_expr(&output_file, &output_file.cmpts, file)? {
					let rule = self
						.expander
						.expand_rule(rule, &mut RuleVisitor::new(&rules.aliases, &rule.aliases, &rule_pats))
						.map_err(AppError::expand_rule(rule.name.to_owned()))?;
					return Ok(Some((rule, rule_pats)));
				}
			}
		}

		// If we got here, there was no matching rule
		Ok(None)
	}
}

/// Parses a dependencies file
// TODO: Support multiple dependencies in each file
async fn parse_deps_file(file: &str) -> Result<(String, Vec<String>), AppError> {
	// Read it
	let contents = fs::read_to_string(file).await.map_err(AppError::read_file(file))?;

	// Parse it
	let (output, deps) = contents.split_once(':').ok_or_else(|| AppError::DepFileMissingColon {
		dep_file_path: file.into(),
	})?;
	let output = output.trim().to_owned();
	let deps = deps.split_whitespace().map(str::to_owned).collect();

	Ok((output, deps))
}

/// Returns the last build time of a rule.
///
/// Returns `Err` if any files didn't exist,
/// Returns `Ok(None)` if rule has no outputs
async fn rule_last_build_time<'s>(rule: &Rule<'s, CowStr<'s>>) -> Result<Option<SystemTime>, AppError> {
	// Note: We get the time of the oldest file in order to ensure all
	//       files are at-least that old
	// Note: It's fine to fail early here, see the note on `normal_deps` in `build_unchecked`.
	let built_time = rule
		.output
		.iter()
		.map(async move |item| {
			let file = match item {
				OutItem::File { file } | OutItem::DepsFile { file } => file,
			};
			fs::metadata(&**file)
				.await
				.map(self::file_modified_time)
				.map_err(AppError::read_file_metadata(&**file))
		})
		.collect::<FuturesUnordered<_>>()
		.try_collect::<Vec<_>>()
		.await?
		.into_iter()
		.min();
	Ok(built_time)
}

/// Returns the file modified time
#[expect(clippy::needless_pass_by_value)] // We use it in `.map`, which makes it convenient to receive by value
fn file_modified_time(metadata: std::fs::Metadata) -> SystemTime {
	let file_time = FileTime::from_last_modification_time(&metadata);
	let unix_offset = Duration::new(
		file_time
			.unix_seconds()
			.try_into()
			.expect("File time was before unix epoch"),
		file_time.nanoseconds(),
	);

	SystemTime::UNIX_EPOCH + unix_offset
}
