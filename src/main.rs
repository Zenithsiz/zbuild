//! `Zbuild` build system

// Features
#![feature(
	exit_status_error,
	decl_macro,
	box_patterns,
	async_closure,
	let_chains,
	yeet_expr,
	must_not_suspend,
	strict_provenance,
	assert_matches,
	try_trait_v2,
	if_let_guard,
	pattern,
	unsigned_signed_diff,
	vec_into_raw_parts,
	ptr_metadata
)]
// Lints
#![allow(
	clippy::print_stdout,
	clippy::print_stderr,
	reason = "We're a binary that should talk to the user"
)]

// Modules
mod args;
mod ast;
mod build;
mod error;
mod expand;
mod logger;
mod rules;
mod util;
mod watcher;

// Imports
use {
	self::{
		ast::Ast,
		build::{BuildReason, Builder},
		error::{AppError, ExitResult},
		expand::Expander,
		rules::Rules,
	},
	args::Args,
	clap::Parser,
	futures::{stream::FuturesUnordered, StreamExt, TryFutureExt},
	std::{
		borrow::Cow,
		collections::BTreeMap,
		env,
		fmt,
		fs,
		path::{Path, PathBuf},
		sync::Arc,
		thread,
		time::{Duration, SystemTime},
	},
	util::ArcStr,
	watcher::Watcher,
};

#[tokio::main(flavor = "current_thread")]
#[expect(clippy::too_many_lines, reason = "TODO: Split it up more")]
async fn main() -> ExitResult {
	// Get all args
	let args = Args::parse();

	// Initialize the logger
	logger::init(args.log_file.as_deref());
	tracing::trace!(?args, "Arguments");

	// Find the zbuild location and change the current directory to it
	// TODO: Not adjust the zbuild path and read it before?
	let zbuild_path = match args.zbuild_path {
		Some(path) => path,
		None => self::find_zbuild().await?,
	};
	tracing::debug!(?zbuild_path, "Found zbuild path");
	let zbuild_dir = zbuild_path.parent().expect("Zbuild path had no parent");
	let zbuild_path = zbuild_path.file_name().expect("Zbuild path had no file name");
	let zbuild_path = Path::new(zbuild_path);
	tracing::debug!(?zbuild_dir, "Moving to zbuild directory");
	env::set_current_dir(zbuild_dir).map_err(AppError::set_current_dir(zbuild_dir))?;

	// Parse the ast
	let zbuild_file = fs::read_to_string(zbuild_path).map_err(AppError::read_file(&zbuild_path))?;
	let zbuild_file = ArcStr::from(zbuild_file);
	tracing::trace!(?zbuild_file, "Read zbuild.yaml");
	let ast = serde_yaml::from_str::<Ast<'_>>(&zbuild_file).map_err(AppError::parse_yaml(&zbuild_path))?;
	tracing::trace!(?ast, "Parsed ast");

	// Build the rules
	let rules = Rules::new(&zbuild_file, ast);
	tracing::trace!(?rules, "Built rules");

	// Get the max number of jobs we can execute at once
	let jobs = match args.jobs {
		Some(0) => {
			tracing::warn!("Cannot use 0 jobs, defaulting to 1");
			1
		},
		Some(jobs) => jobs,
		None => thread::available_parallelism()
			.map_err(AppError::get_default_jobs())?
			.into(),
	};
	tracing::debug!(?jobs, "Concurrent jobs");

	// Then get all targets to build
	let targets_to_build = match args.targets.is_empty() {
		// If none were specified, use the default rules
		true => Cow::Borrowed(rules.default.as_slice()),

		// Else infer them as either rules or files
		// TODO: Maybe be explicit about rule-name inferring?
		//       If a file has the same name as a rule, it may be
		//       unexpected behavior, but we can't just check if the
		//       file exists to disambiguate, because it might not be
		//       created yet
		false => args
			.targets
			.into_iter()
			.map(|target| {
				rules.rules.get(target.as_str()).map_or_else(
					// By default, use a file
					|| rules::Target::File {
						file:      rules::Expr::string(target),
						is_static: false,
					},
					// If there was a rule, use it without any patterns
					// TODO: If it requires patterns maybe error out here?
					|rule| rules::Target::Rule {
						rule: rules::Expr::string(rule.name.clone()),
						pats: Arc::new(BTreeMap::new()),
					},
				)
			})
			.collect(),
	};
	tracing::trace!(
		targets_to_build = ?targets_to_build.iter().map(<_>::to_string).collect::<Vec<_>>(),
		"Found targets to build"
	);

	// Create the builder
	// Note: We should stop builds on the first error if we're *not* watching.
	let builder = Builder::new(jobs, !args.watch);

	// Then create the watcher, if we're watching
	let watcher = args
		.watch
		.then(|| {
			// TODO: Better default?
			let debouncer_timeout_ms = args.watcher_debouncer_timeout_ms.unwrap_or(10.0_f64);
			let debouncer_timeout = { Duration::from_secs_f64(debouncer_timeout_ms / 1000.0) };

			Watcher::new(builder.subscribe_events(), debouncer_timeout)
		})
		.transpose()?;

	// Finally build all targets and start watching
	let (failed_targets, ()) = futures::join!(
		async {
			targets_to_build
				.iter()
				.map(|target| {
					self::build_target(&builder, target, &rules, args.ignore_missing)
						.map_err(|err| (target.clone(), err))
				})
				.collect::<FuturesUnordered<_>>()
				.collect::<Vec<Result<(), _>>>()
				.await
				.into_iter()
				.filter_map(Result::err)
				.collect::<Vec<_>>()
		},
		async {
			if let Some(watcher) = watcher {
				tracing::info!("Starting to watch for all targets");
				watcher.watch_rebuild(&builder, &rules, args.ignore_missing).await;
			}
		}
	);

	// Finally print some statistics
	let targets = builder.into_build_results();
	let total_targets = targets.len();
	let built_targets = targets
		.iter()
		.filter_map(|(_, res)| res.as_ref())
		.filter(|res| res.as_ref().map_or(false, |res| res.built))
		.count();
	tracing::info!("Built {built_targets} targets");
	tracing::info!("Checked {total_targets} targets");

	match failed_targets.is_empty() {
		true => ExitResult::Ok,
		false => {
			tracing::error!("One or more builds failed:");
			for (target, err) in failed_targets {
				tracing::error!(err=%err.pretty(), "Failed to build target {target}");
			}

			ExitResult::Err(AppError::ExitDueToFailedBuilds {})
		},
	}
}

/// Finds the nearest zbuild file
async fn find_zbuild() -> Result<PathBuf, AppError> {
	let cur_path = env::current_dir().map_err(AppError::get_current_dir())?;
	let mut cur_path = cur_path.as_path();

	loop {
		let zbuild_path = cur_path.join("zbuild.yaml");
		match util::fs_try_exists_symlink(&zbuild_path)
			.await
			.map_err(AppError::check_file_exists(&zbuild_path))?
		{
			true => return Ok(zbuild_path),
			false => match cur_path.parent() {
				Some(parent) => cur_path = parent,
				None => return Err(AppError::ZBuildNotFound {}),
			},
		}
	}
}

/// Builds a target.
async fn build_target<T: BuildableTargetInner + fmt::Display + fmt::Debug>(
	builder: &Builder,
	target: &rules::Target<T>,
	rules: &Rules,
	ignore_missing: bool,
) -> Result<(), AppError> {
	tracing::debug!(%target, "Building target");

	// Try to build the target
	let build_start_time = SystemTime::now();
	let res = T::build(target, builder, rules, ignore_missing, BuildReason::empty()).await;

	// Then check the status
	match res {
		Ok(build_res) => {
			// If we actually built the rule, and it didn't just exist, log it
			if build_res.built {
				let build_duration = build_res
					.build_time
					.duration_since(build_start_time)
					.unwrap_or(Duration::ZERO);
				tracing::debug!("Built target {target} in {build_duration:.2?}");
				println!("{target}");
			}

			Ok(())
		},
		Err(err) => {
			tracing::error!(%target, err=%err.pretty(), "Unable to build target");
			Err(err)
		},
	}
}

/// A buildable target inner type
trait BuildableTargetInner: Sized {
	/// Builds this target
	async fn build(
		target: &rules::Target<Self>,
		builder: &Builder,
		rules: &Rules,
		ignore_missing: bool,
		reason: BuildReason<'_>,
	) -> Result<build::BuildResult, AppError>;
}

impl BuildableTargetInner for rules::Expr {
	async fn build(
		target: &rules::Target<Self>,
		builder: &Builder,
		rules: &Rules,
		ignore_missing: bool,
		reason: BuildReason<'_>,
	) -> Result<build::BuildResult, AppError> {
		builder
			.build_expr(target, rules, ignore_missing, reason)
			.await
			.map(|(build_res, _)| build_res)
	}
}

impl BuildableTargetInner for ArcStr {
	async fn build(
		target: &rules::Target<Self>,
		builder: &Builder,
		rules: &Rules,
		ignore_missing: bool,
		reason: BuildReason<'_>,
	) -> Result<build::BuildResult, AppError> {
		builder
			.build(target, rules, ignore_missing, reason)
			.await
			.map(|(build_res, _)| build_res)
	}
}
