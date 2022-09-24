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
	dashmap::DashMap,
	filetime::FileTime,
	futures::{stream::FuturesUnordered, StreamExt, TryStreamExt},
	std::{
		collections::HashMap,
		mem,
		path::Path,
		sync::Arc,
		time::{Duration, SystemTime},
	},
	tokio::{
		fs,
		process::Command,
		sync::{OwnedRwLockReadGuard, OwnedRwLockWriteGuard, RwLock, Semaphore},
	},
};

// TODO: If the user zbuild file is not generated properly, it can
//       make us deadlock, check for cycles somehow

// TODO: By implementing locks at the target-level, we might get races
//       when a single rule has multiple targets that each want to be
//       built simultaneously

/// Event
#[derive(Clone, Debug)]
pub enum Event {
	/// Target dependency built
	TargetDepBuilt {
		target: Target<String>,
		dep:    Target<String>,
	},
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

	/// All targets' build status
	targets: DashMap<Target<String>, BuildLock>,

	/// Command execution semaphore
	exec_semaphore: Semaphore,
}

impl Builder {
	/// Creates a new builder
	#[must_use]
	pub fn new(jobs: usize) -> Self {
		let (event_tx, event_rx) = async_broadcast::broadcast(jobs);
		let event_rx = event_rx.deactivate();

		Self {
			event_tx,
			_event_rx: event_rx,
			targets: DashMap::<Target<String>, BuildLock>::new(),
			exec_semaphore: Semaphore::new(jobs),
		}
	}

	/// Sends an event, if any are subscribers
	async fn send_event(&self, make_event: impl FnOnce() -> Event + Send) {
		// Note: We only send them if there are any receivers (excluding ours, which is inactive),
		//       to ensure we don't deadlock waiting for someone to read the events
		if self.event_tx.receiver_count() > 0 {
			self.event_tx
				.broadcast(make_event())
				.await
				.expect("Event channel was closed");
		}
	}

	/// Subscribes to builder events
	pub fn subscribe_events(&self) -> async_broadcast::Receiver<Event> {
		self.event_tx.new_receiver()
	}

	/// Returns all targets built.
	///
	/// Waits for targets being built
	pub async fn targets(&self) -> HashMap<Target<String>, Result<BuildResult, AppError>> {
		self.targets
			.iter()
			.map(async move |entry| {
				let res = entry.value().res().await?;
				Some((entry.key().clone(), res))
			})
			.collect::<FuturesUnordered<_>>()
			.filter_map(async move |opt| opt)
			.collect()
			.await
	}

	/// Resets a build
	pub async fn reset_build(&self, target: &Target<String>) -> Result<(), AppError> {
		// Get the built lock, or create it
		let build_lock = self
			.targets
			.entry(target.clone())
			.or_insert_with(BuildLock::new)
			.clone();

		// Then remove it's state
		build_lock.lock_build().await.reset();

		Ok(())
	}

	/// Builds an expression-encoded target
	pub async fn build_expr(
		&self,
		target: &Target<Expr>,
		rules: &Rules,
	) -> Result<(Result<BuildResult, AppError>, BuildLockDepGuard), AppError> {
		// Expand the target
		let global_expr_visitor = expand_expr::GlobalVisitor::new(&rules.aliases);
		let target = self::expand_target(target, global_expr_visitor).map_err(AppError::expand_target(target))?;

		// Then build
		Ok(self.build(&target, rules).await)
	}

	/// Builds a target
	pub async fn build(
		&self,
		target: &Target<String>,
		rules: &Rules,
	) -> (Result<BuildResult, AppError>, BuildLockDepGuard) {
		tracing::trace!(?target, "Building target");

		// Get the built lock, or create it
		let build_lock = self
			.targets
			.entry(target.clone())
			.or_insert_with(BuildLock::new)
			.clone();

		// Then check if built
		let build_guard = build_lock.lock_dep().await;
		match build_guard.res() {
			// If we got it, we were built, so just return the guard
			Some(res) => (res, build_guard),

			// Else build first
			// Note: Tokio read lock don't support upgrading, so we do a double-checked
			//       lock here.
			None => {
				mem::drop(build_guard);
				#[allow(clippy::shadow_unrelated)] // They are the same even if redeclared
				let mut build_guard = build_lock.lock_build().await;

				match build_guard.res() {
					// If we got it in the meantime, return it
					Some(res) => (res, build_guard.into_dep()),

					// Else build
					None => {
						let res = self.build_unchecked(target, rules).await;
						let res = build_guard.finish(res);
						(res, build_guard.into_dep())
					},
				}
			},
		}
	}

	/// Builds a target without checking if the target is already being built.
	#[expect(clippy::too_many_lines)] // TODO: Split this function onto smaller ones
	#[async_recursion::async_recursion]
	async fn build_unchecked(&self, target: &Target<String>, rules: &Rules) -> Result<BuildResult, AppError> {
		// Find and expand the rule to use for this target
		let rule = match *target {
			// If we got a file, check which rule can make it
			Target::File { ref file, .. } => match self::find_rule_for_file(file, rules)? {
				Some(rule) => {
					tracing::trace!(?target, ?rule.name, "Found target rule");
					rule
				},

				// Else, if no rule can make it, we'll assume it's an existing file
				// and return it's modification date as the build time.
				None => {
					let metadata = fs::metadata(file).await.map_err(AppError::missing_file(file))?;
					let build_time = self::file_modified_time(metadata);
					tracing::trace!(?target, ?build_time, "Found target file");
					let res = BuildResult {
						build_time,
						built: false,
					};
					return Ok(res);
				},
			},

			// If we got a rule name with patterns, find it and replace all patterns
			Target::Rule { ref rule, ref pats } => {
				// Find the rule and expand it
				let rule = rules.rules.get(rule).ok_or_else(|| AppError::UnknownRule {
					rule_name: rule.clone(),
				})?;
				self::expand_rule(rule, &rules.aliases, &rule.aliases, pats)
					.map_err(AppError::expand_rule(&rule.name))?
			},
		};


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
		let normal_deps = rule
			.deps
			.iter()
			.map(async move |dep| match *dep {
				DepItem::File { ref file, is_static } => Ok(Dep::File {
					file,
					is_static,
					is_dep_file: false,
					is_output: false,
					exists: fs_try_exists(file).await.map_err(AppError::check_file_exists(file))?,
				}),
				DepItem::DepsFile { ref file, is_static } => Ok(Dep::File {
					file,
					is_static,
					is_dep_file: true,
					is_output: false,
					exists: fs_try_exists(file).await.map_err(AppError::check_file_exists(file))?,
				}),
				DepItem::Rule { ref name, ref pats } => Ok(Dep::Rule { name, pats }),
			})
			.collect::<FuturesUnordered<_>>()
			.try_collect::<Vec<_>>()
			.await?;

		// And all output dependencies
		let out_deps = rule
			.output
			.iter()
			.map(async move |out| match out {
				OutItem::File { .. } => Ok(None),
				OutItem::DepsFile { file } => Ok(Some(Dep::File {
					file,
					is_static: false,
					is_dep_file: true,
					is_output: true,
					exists: fs_try_exists(file).await.map_err(AppError::check_file_exists(file))?,
				})),
			})
			.collect::<FuturesUnordered<_>>()
			.filter_map(async move |res| res.transpose())
			.try_collect::<Vec<_>>()
			.await?;

		// Then build all dependencies, as well as any dependency files
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
						Dep::File { file, is_static, .. } => Some(Target::File {
							file: file.to_owned(),
							is_static,
						}),

						// If a rule, always build
						Dep::Rule { name, pats } => Some(Target::Rule {
							rule: name.to_owned(),
							pats: pats.clone(),
						}),
					};

					// Then build it, if we should
					let (dep_res, dep_guard) = match &dep_target {
						Some(dep_target) => {
							let (res, dep_guard) = self.build(dep_target, rules).await;
							let res = res.map_err(AppError::build_target(dep_target))?;
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
					let dep_deps = match dep {
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
						_ => vec![],
					};

					let deps = util::chain!(
						dep_target
							.zip(dep_res.zip(dep_guard))
							.map(|(dep_target, (dep_res, guard))| (dep_target, dep_res, guard)),
						dep_deps.into_iter()
					)
					.collect::<Vec<_>>();
					tracing::trace!(?target, ?rule.name, ?dep, ?deps, "Built target rule dependency dependencies");

					Ok(deps)
				}
			})
			.collect::<FuturesUnordered<_>>()
			.try_collect::<Vec<_>>()
			.await?
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
		let rule_last_build_time = self::rule_last_build_time(&rule).await;
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
			self.rebuild_rule(&rule)
				.await
				.map_err(AppError::build_rule(&rule.name))?;
		}

		// Then get the build time
		// Note: If we don't have any outputs, just use the current time as the build time
		let cur_build_time = self::rule_last_build_time(&rule).await?.unwrap_or_else(SystemTime::now);
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
	) -> Result<Vec<(Target<String>, BuildResult, BuildLockDepGuard)>, AppError> {
		tracing::trace!(target=?parent_target, ?rule.name, ?dep_file, "Building dependencies of target rule dependency-file");
		let (output, deps) = self::parse_deps_file(dep_file).await?;

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
		let deps_res = deps
			.into_iter()
			.map(|dep| {
				let dep_target = Target::File {
					file:      dep.clone(),
					is_static: false,
				};
				tracing::trace!(?rule.name, ?dep, "Found rule dependency");
				async move {
					let (res, dep_guard) = self.build(&dep_target, rules).await;
					let res = res.map_err(AppError::build_target(&dep_target))?;

					self.send_event(|| Event::TargetDepBuilt {
						target: parent_target.clone(),
						dep:    dep_target.clone(),
					})
					.await;

					Ok((dep_target, res, dep_guard))
				}
			})
			.collect::<FuturesUnordered<_>>()
			.try_collect::<Vec<_>>()
			.await?;

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

/// Build state
#[derive(Clone, Debug)]
pub struct BuildState {
	/// Result, if built
	res: Option<Result<BuildResult, Arc<AppError>>>,
}

/// Build lock
#[derive(Clone, Debug)]
pub struct BuildLock {
	/// State
	state: Arc<RwLock<BuildState>>,
}

impl BuildLock {
	/// Creates a new build lock
	pub fn new() -> Self {
		Self {
			state: Arc::new(RwLock::new(BuildState { res: None })),
		}
	}

	/// Locks the build lock for building
	pub async fn lock_build(&self) -> BuildLockBuildGuard {
		BuildLockBuildGuard {
			state: Arc::clone(&self.state).write_owned().await,
		}
	}

	/// Locks the build lock as a dependency
	pub async fn lock_dep(&self) -> BuildLockDepGuard {
		BuildLockDepGuard {
			state: Arc::clone(&self.state).read_owned().await,
		}
	}

	/// Retrieves the result of this state.
	///
	/// Waits for any builders to finish
	pub async fn res(&self) -> Option<Result<BuildResult, AppError>> {
		self.state
			.read()
			.await
			.res
			.clone()
			.map(|res| res.map_err(AppError::Shared))
	}
}

/// Build lock build guard
#[derive(Debug)]
pub struct BuildLockBuildGuard {
	/// State
	state: OwnedRwLockWriteGuard<BuildState>,
}

impl BuildLockBuildGuard {
	/// Returns the result of the build
	pub fn res(&self) -> Option<Result<BuildResult, AppError>> {
		self.state.res.clone().map(|res| res.map_err(AppError::Shared))
	}

	/// Downgrades this build lock into a dependency lock
	pub fn into_dep(self) -> BuildLockDepGuard {
		BuildLockDepGuard {
			state: self.state.downgrade(),
		}
	}

	/// Resets this build.
	pub fn reset(&mut self) {
		self.state.res = None;
	}

	/// Finishes a build
	pub fn finish(&mut self, res: Result<BuildResult, AppError>) -> Result<BuildResult, AppError> {
		let res = res.map_err(Arc::new);
		self.state.res = Some(res.clone());
		res.map_err(AppError::Shared)
	}
}

/// Build lock dependency guard
#[derive(Debug)]
pub struct BuildLockDepGuard {
	/// State
	state: OwnedRwLockReadGuard<BuildState>,
}

impl BuildLockDepGuard {
	/// Returns the result of the build
	pub fn res(&self) -> Option<Result<BuildResult, AppError>> {
		self.state.res.clone().map(|res| res.map_err(AppError::Shared))
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
async fn rule_last_build_time(rule: &Rule<String>) -> Result<Option<SystemTime>, AppError> {
	// Note: We get the time of the oldest file in order to ensure all
	//       files are at-least that old
	let built_time = rule
		.output
		.iter()
		.map(async move |item| {
			let file = match item {
				OutItem::File { file } | OutItem::DepsFile { file } => file,
			};
			fs::metadata(file)
				.await
				.map(self::file_modified_time)
				.map_err(AppError::read_file_metadata(file))
		})
		.collect::<FuturesUnordered<_>>()
		.try_collect::<Vec<_>>()
		.await?
		.into_iter()
		.min();
	Ok(built_time)
}

/// Returns the file modified time
#[allow(clippy::needless_pass_by_value)] // We use it in `.map`, which makes it convenient to receive by value
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

/// Finds a rule for `file`
// TODO: Not make this `O(N)` for the number of rules.
pub fn find_rule_for_file(file: &str, rules: &Rules) -> Result<Option<Rule<String>>, AppError> {
	for rule in rules.rules.values() {
		for output in &rule.output {
			// Expand all expressions in the output file
			// Note: This doesn't expand patterns, so we can match those later
			let output_file = match output {
				OutItem::File { file: output_file } | OutItem::DepsFile { file: output_file } => output_file,
			};
			let file_cmpts = self::expand_expr(
				output_file,
				&mut expand_expr::RuleOutputVisitor::new(&rules.aliases, &rule.aliases),
			)?;

			// Then try to match the output file to the file we need to create
			if let Some(rule_pats) = self::match_expr(output_file, &file_cmpts, file)? {
				let rule = self::expand_rule(rule, &rules.aliases, &rule.aliases, &rule_pats)
					.map_err(AppError::expand_rule(&rule.name))?;
				return Ok(Some(rule));
			}
		}
	}

	// If we got here, there was no matching rule
	Ok(None)
}

/// Async `std::fs_try_exists`
async fn fs_try_exists(path: impl AsRef<Path> + Send) -> Result<bool, std::io::Error> {
	match fs::metadata(path).await {
		Ok(_) => Ok(true),
		Err(err) if err.kind() == std::io::ErrorKind::NotFound => Ok(false),
		Err(err) => Err(err),
	}
}
