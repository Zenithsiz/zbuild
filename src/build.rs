//! Build

// Modules
mod expand_expr;
mod expand_rule;
mod expand_target;
mod match_expr;

// Imports
use {
	self::{
		expand_expr::{expand_expr, expand_expr_string},
		expand_rule::expand_rule,
		expand_target::expand_target,
		match_expr::match_expr,
	},
	crate::{
		rules::{DepItem, Expr, OutItem, Rule, Target},
		util,
		AppError,
		Rules,
	},
	dashmap::{mapref::entry::Entry as DashMapEntry, DashMap},
	filetime::FileTime,
	futures::{pin_mut, stream::FuturesUnordered, StreamExt, TryStreamExt},
	std::{
		collections::HashMap,
		fs,
		future::Future,
		mem,
		sync::Arc,
		task::{Poll, Waker},
		time::{Duration, SystemTime},
	},
	tokio::{
		process::Command,
		sync::{broadcast, Mutex, Semaphore},
	},
};

// TODO: If the user zbuild file is not generated properly, it can
//       make us deadlock, check for cycles somehow

/// Event
#[derive(Clone, Debug)]
pub enum Event {
	/// Target dependency
	TargetDep {
		target: Target<String>,
		dep:    Target<String>,
	},
}

/// Builder
#[derive(Debug)]
pub struct Builder {
	/// Event sender
	event_tx: broadcast::Sender<Event>,

	/// All targets' build status
	targets: DashMap<Target<String>, BuildStatus>,

	/// Command execution semaphore
	exec_semaphore: Semaphore,
}

impl Builder {
	/// Creates a new builder
	pub fn new(jobs: usize) -> Self {
		// Note: we don't care about the receiver, since we can
		//       just create them from the sender
		let (event_tx, _) = broadcast::channel(1);

		Self {
			event_tx,
			targets: DashMap::<Target<String>, BuildStatus>::new(),
			exec_semaphore: Semaphore::new(jobs),
		}
	}

	/// Sends ann event, if any are subscribed
	fn send_event(&self, make_event: impl FnOnce() -> Event) {
		// Note: We're fine with the possible desync here when
		//       someone subscribes after the check
		if self.event_tx.receiver_count() != 0 {
			let _ = self.event_tx.send(make_event());
		}
	}

	/// Subscribes to builder events
	pub fn subscribe_events(&self) -> broadcast::Receiver<Event> {
		self.event_tx.subscribe()
	}

	/// Returns all targets built.
	///
	/// If any target is still being build, ignores it.
	pub async fn targets(&self) -> HashMap<Target<String>, Result<BuildResult, AppError>> {
		self.targets
			.iter()
			.map(async move |entry| Some((entry.key().clone(), entry.value().build_result().await?)))
			.collect::<FuturesUnordered<_>>()
			.filter_map(async move |opt| opt)
			.collect()
			.await
	}

	/// Builds an expression-encoded target
	pub async fn build_expr(&self, target: &Target<Expr>, rules: &Rules) -> Result<BuildResult, AppError> {
		// Expand the target
		let global_expr_visitor = expand_expr::GlobalVisitor::new(&rules.aliases);
		let target = self::expand_target(target, global_expr_visitor).map_err(AppError::expand_target(target))?;

		// Then build
		self.build(&target, rules).await
	}

	/// Builds a target
	pub async fn build(&self, target: &Target<String>, rules: &Rules) -> Result<BuildResult, AppError> {
		// Check if we need to build and create a new entry, if so
		let (build_status, do_build) = match self.targets.entry(target.clone()) {
			// If there's already a build status, check it
			DashMapEntry::Occupied(entry) => (entry.get().clone(), false),

			// Else if it's vacant, take responsibility for building
			DashMapEntry::Vacant(entry) => {
				let build_status = BuildStatus::new();
				entry.insert(build_status.clone());
				(build_status, true)
			},
		};

		// Then check if we need to build
		match do_build {
			true => {
				let res = self.build_unchecked(target, rules).await;
				build_status.finish_build(res).await
			},

			false => build_status.await_built().await,
		}
	}

	/// Builds a target without checking if the target is already being built.
	// TODO: Split this function onto smaller ones
	#[async_recursion::async_recursion]
	async fn build_unchecked(&self, target: &Target<String>, rules: &Rules) -> Result<BuildResult, AppError> {
		// Find and expand the rule to use for this target
		let rule = match &target {
			// If we got a file, check which rule can make it
			Target::File { file } => match self::find_rule_for_file(file, rules)? {
				Some(rule) => rule,

				// Else, if no rule can make it, we'll assume it's an existing file
				// and return it's modification date as the build time.
				None => {
					let metadata = fs::metadata(file).map_err(AppError::missing_file(file))?;
					let res = BuildResult {
						build_time: self::file_modified_time(metadata),
						built:      false,
					};
					return Ok(res);
				},
			},

			// If we got a rule name with patterns, find it and replace all patterns
			Target::Rule { rule, pats } => {
				// Find the rule and expand it
				let rule = rules.rules.get(rule).ok_or_else(|| AppError::UnknownRule {
					rule_name: rule.clone(),
				})?;
				self::expand_rule(rule, &rules.aliases, &rule.aliases, pats)
					.map_err(AppError::expand_rule(&rule.name))?
			},
		};
		tracing::trace!(?target, ?rule, "Found rule");

		/// Dependency
		#[derive(Clone, Copy, Debug)]
		enum Dep<'a> {
			/// File
			File {
				file:        &'a str,
				is_static:   bool,
				is_dep_file: bool,
				is_output:   bool,
				exists:      bool,
			},

			/// Rule
			Rule {
				name: &'a str,
				pats: &'a HashMap<String, String>,
			},
		}

		// Gather all normal dependencies
		let deps = rule
			.deps
			.iter()
			.map(|dep| match *dep {
				DepItem::File { ref file, is_static } => Ok(Dep::File {
					file,
					is_static,
					is_dep_file: false,
					is_output: false,
					exists: fs::try_exists(file).map_err(AppError::check_file_exists(file))?,
				}),
				DepItem::DepsFile { ref file, is_static } => Ok(Dep::File {
					file,
					is_static,
					is_dep_file: true,
					is_output: false,
					exists: fs::try_exists(file).map_err(AppError::check_file_exists(file))?,
				}),
				DepItem::Rule { ref name, ref pats } => Ok(Dep::Rule { name, pats }),
			})
			.collect::<Result<Vec<_>, _>>()?;

		// And all output dependencies
		let out_deps = rule
			.output
			.iter()
			.map(|out| match out {
				OutItem::File { .. } => Ok(None),
				OutItem::DepsFile { file } => Ok(Some(Dep::File {
					file,
					is_static: false,
					is_dep_file: true,
					is_output: true,
					exists: fs::try_exists(file).map_err(AppError::check_file_exists(file))?,
				})),
			})
			.filter_map(|res| res.transpose())
			.collect::<Result<Vec<_>, _>>()?;

		// Then build all dependencies, as well as any dependency files
		let deps_res = util::chain!(deps, out_deps,)
			.map(|dep| {
				tracing::trace!(?dep, ?rule.name, "Building dependency of rule");
				let rule = &rule;
				async move {
					// Get the target to build
					let dep_target = match dep {
						// If static, but exists, or output, don't do anything
						Dep::File {
							is_static: true,
							exists: true,
							..
						} |
						Dep::File { is_output: true, .. } => None,

						// Else if non-static or static and doesn't exist, build it
						Dep::File {
							file, is_static: false, ..
						} |
						Dep::File {
							file,
							is_static: true,
							exists: false,
							..
						} => Some(Target::File { file: file.to_owned() }),

						// If a rule, execute it, regardless if it's out of date.
						Dep::Rule { name, pats } => Some(Target::Rule {
							rule: name.to_owned(),
							pats: pats.clone(),
						}),
					};

					// Then build it, if we should
					let dep_res = match dep_target {
						Some(dep_target) => self
							.build(&dep_target, rules)
							.await
							.map(|res| {
								self.send_event(|| Event::TargetDep { target: target.clone(), dep: dep_target.clone() });
								Some(res)
							})
							.map_err(AppError::build_target(&dep_target))?,
						None => None,
					};

					// If the dependency if a dependency deps file or an output deps file (and exists), build it's dependencies too
					let dep_deps_res = match dep {
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
							.build_deps_file(target, file, rule, rules)
							.await
							.map_err(AppError::build_dep_file(file))?,

						_ => None,
					};

					// Finally select the latest time between the dependency and
					// the dependency's dependencies.
					// If none, we don't have a build time
					let res = match (dep_res, dep_deps_res) {
						(Some(dep_res), Some(dep_deps_res)) => Some(dep_res.latest(dep_deps_res)),
						(Some(res), None) | (None, Some(res)) => Some(res),
						(None, None) => None,
					};
					Ok::<_, AppError>(res)
				}
			})
			.collect::<FuturesUnordered<_>>()
			.try_collect::<Vec<_>>()
			.await?
			.into_iter()
			// Note: We get `None` when there were no dependencies for the rule
			.flatten()
			.max_by_key(|res| res.build_time);

		// Afterwards check the last time we've built the rule and compare it with
		// the dependency build times.
		let rule_last_build_time = self::rule_last_build_time(&rule);
		let needs_rebuilt = match (deps_res, &rule_last_build_time) {
			// If any files were missing, or we had no outputs, build
			(_, Err(_) | Ok(None)) => true,

			// If no dependencies and all outputs exist, don't rebuild
			(None, Ok(_)) => false,

			// If we have dependencies and outputs, rebuild if the dependencies are
			// newer than the outputs
			(Some(deps_res), Ok(Some(rule_last_build_time))) => deps_res.build_time > *rule_last_build_time,
		};
		tracing::trace!(?needs_rebuilt, ?target, ?deps_res, ?rule_last_build_time, "Rebuild");

		// Then rebuild, if needed
		if needs_rebuilt {
			self.rebuild_rule(&rule)
				.await
				.map_err(AppError::build_rule(&rule.name))?;
		}

		// Then get the build time
		// Note: If we don't have any outputs, just use the current time as the build time
		let cur_build_time = self::rule_last_build_time(&rule)?.unwrap_or_else(SystemTime::now);
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
		&self,
		parent_target: &Target<String>,
		dep_file: &str,
		rule: &Rule<String>,
		rules: &Rules,
	) -> Result<Option<BuildResult>, AppError> {
		let (output, deps) = self::parse_deps_file(dep_file)?;

		match rule.output.is_empty() {
			// If there were no outputs, make sure it matches the rule name
			// TODO: Seems kinda weird for it to match the rule name, but not sure how else to check this here
			true =>
				if output != rule.name {
					return Err(AppError::DepFileMissingRuleName {
						dep_file_path: dep_file.into(),
						rule_name:     rule.name.clone(),
						dep_output:    output,
					});
				},

			// If there were any output, make sure the dependency file applies to one of them
			false => {
				let any_matches = rule.output.iter().any(|out| match out {
					OutItem::File { file } => file == &output,
					OutItem::DepsFile { file } => file == &output,
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
		let deps_res = deps
			.into_iter()
			.map(|dep| async move {
				let dep_target = Target::File { file: dep.clone() };
				self.send_event(|| Event::TargetDep {
					target: parent_target.clone(),
					dep:    dep_target.clone(),
				});
				self.build(&dep_target, rules)
					.await
					.map_err(AppError::build_target(&dep_target))
			})
			.collect::<FuturesUnordered<_>>()
			.try_collect::<Vec<_>>()
			.await?
			.into_iter()
			.max_by_key(|res| res.build_time);

		Ok(deps_res)
	}

	/// Rebuilds a rule
	pub async fn rebuild_rule(&self, rule: &Rule<String>) -> Result<(), AppError> {
		let _exec_guard = self.exec_semaphore.acquire().await.expect("Exec semaphore was closed");

		for cmds in &rule.exec.cmds {
			let (program, args) = cmds.args.split_first().ok_or_else(|| AppError::RuleExecEmpty {
				rule_name: rule.name.clone(),
			})?;

			// Create the command
			let mut cmd = Command::new(program);
			cmd.args(args);

			// Set the working directory, if we have any
			if let Some(cwd) = &rule.exec.cwd {
				cmd.current_dir(cwd);
			}

			// Then spawn it
			tracing::info!(target: "zbuild_exec", "{} {}", program, args.join(" "));
			cmd.spawn()
				.map_err(AppError::spawn_command(cmds))?
				.wait()
				.await
				.map_err(AppError::wait_command(cmds))?
				.exit_ok()
				.map_err(AppError::command_failed(cmds))?;
		}


		Ok(())
	}
}

/// Build status inner
#[derive(Clone, Debug)]
pub enum BuildStatusInner {
	/// Building
	Building {
		/// Wakers
		wakers: Vec<Waker>,
	},

	/// Built
	Built {
		/// Built result
		res: Result<BuildResult, Arc<AppError>>,
	},
}

/// Build status
#[derive(Clone, Debug)]
pub struct BuildStatus {
	/// Inner
	inner: Arc<Mutex<BuildStatusInner>>,
}

impl BuildStatus {
	/// Creates a new build subscriber
	pub fn new() -> Self {
		Self {
			inner: Arc::new(Mutex::new(BuildStatusInner::Building { wakers: vec![] })),
		}
	}

	/// Finishes building
	pub async fn finish_build(&self, res: Result<BuildResult, AppError>) -> Result<BuildResult, AppError> {
		let mut inner = self.inner.lock().await;
		match &mut *inner {
			BuildStatusInner::Building { wakers } => {
				let wakers = mem::take(wakers);
				let res = res.map_err(Arc::new);
				*inner = BuildStatusInner::Built { res: res.clone() };
				mem::drop(inner);

				for waker in wakers {
					waker.wake();
				}

				res.map_err(AppError::Shared)
			},
			BuildStatusInner::Built { .. } => unreachable!("Already finished building"),
		}
	}

	/// Returns the build result, if any
	pub async fn build_result(&self) -> Option<Result<BuildResult, AppError>> {
		let mut inner = self.inner.lock().await;
		match &mut *inner {
			BuildStatusInner::Building { .. } => None,
			BuildStatusInner::Built { res } => Some(res.clone().map_err(AppError::Shared)),
		}
	}

	/// Awaits until the build is done
	pub async fn await_built(&self) -> Result<BuildResult, AppError> {
		std::future::poll_fn(|ctx| {
			// Lock
			let inner_fut = self.inner.lock();
			pin_mut!(inner_fut);
			let mut inner = inner_fut.poll(ctx).ready()?;

			// Check if built
			match &mut *inner {
				// If not, add a waker and return pending
				// Note: Since we're holding onto the lock, we don't need to double check here
				BuildStatusInner::Building { wakers } => {
					wakers.push(ctx.waker().clone());
					Poll::Pending
				},

				// Else it's ready
				BuildStatusInner::Built { res } => Poll::Ready(res.clone().map_err(AppError::Shared)),
			}
		})
		.await
	}
}

/// Build result
#[derive(Clone, Copy, Debug)]
pub struct BuildResult {
	/// Build time
	pub build_time: SystemTime,

	/// If built from a rule
	pub built: bool,
}

impl BuildResult {
	/// Returns the latest of two build results
	pub fn latest(self, other: Self) -> Self {
		std::cmp::max_by_key(self, other, |res| res.build_time)
	}
}

/// Parses a dependencies file
// TODO: Support multiple dependencies in each file
fn parse_deps_file(file: &str) -> Result<(String, Vec<String>), AppError> {
	// Read it
	let contents = fs::read_to_string(file).map_err(AppError::read_file(file))?;

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
fn rule_last_build_time(rule: &Rule<String>) -> Result<Option<SystemTime>, AppError> {
	// Note: We get the time of the oldest file in order to ensure all
	//       files are at-least that old
	rule.output
		.iter()
		.map(|item| {
			let file = match item {
				OutItem::File { file } => file,
				OutItem::DepsFile { file } => file,
			};
			fs::metadata(file)
				.map(self::file_modified_time)
				.map_err(AppError::read_file_metadata(file))
		})
		.reduce(|lhs, rhs| Ok(lhs?.min(rhs?)))
		.transpose()
}

/// Returns the file modified time
fn file_modified_time(metadata: fs::Metadata) -> SystemTime {
	let file_time = FileTime::from_last_modification_time(&metadata);
	let unix_offset = Duration::new(file_time.unix_seconds() as u64, file_time.nanoseconds());

	SystemTime::UNIX_EPOCH + unix_offset
}

/// Finds a rule for `file`
// TODO: Not make this `O(N)` for the number of rules.
pub fn find_rule_for_file(file: &str, rules: &Rules) -> Result<Option<Rule<String>>, AppError> {
	for rule in rules.rules.values() {
		for output in &rule.output {
			// Expand all expressions in the output file
			// Note: This doesn't expand patterns, so we can match those later
			let output = match output {
				OutItem::File { file } => file,
				OutItem::DepsFile { file } => file,
			};
			let file_cmpts = self::expand_expr(
				output,
				&mut expand_expr::RuleOutputVisitor::new(&rules.aliases, &rule.aliases),
			)?;

			// Then try to match the output file to the file we need to create
			if let Some(rule_pats) = self::match_expr(output, &file_cmpts, file)? {
				let rule = self::expand_rule(rule, &rules.aliases, &rule.aliases, &rule_pats)
					.map_err(AppError::expand_rule(&rule.name))?;
				return Ok(Some(rule));
			}
		}
	}

	// If we got here, there was no matching rule
	Ok(None)
}
