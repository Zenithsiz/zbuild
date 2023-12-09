//! `Zbuild` build system

// Features
#![feature(
	exit_status_error,
	decl_macro,
	box_patterns,
	async_closure,
	let_chains,
	lint_reasons,
	yeet_expr,
	must_not_suspend,
	strict_provenance,
	assert_matches
)]
// Lints
#![allow(clippy::print_stdout, reason = "We're a binary that should talk to the user")]

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
	self::{ast::Ast, build::Builder, error::AppError, expand::Expander, rules::Rules},
	args::Args,
	clap::Parser,
	futures::{stream::FuturesUnordered, StreamExt, TryFutureExt},
	std::{
		borrow::Cow,
		collections::HashMap,
		env,
		fmt,
		fs,
		path::{Path, PathBuf},
		thread,
		time::{Duration, SystemTime},
	},
	util::CowStr,
	watcher::Watcher,
};


// Note: We return an `anyhow::Error` because it has good formatting
// TODO: Return our own error once we improve formatting?
#[tokio::main]
async fn main() -> Result<(), anyhow::Error> {
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
	tracing::trace!(?zbuild_file, "Read zbuild.yaml");
	let ast = serde_yaml::from_str::<Ast<'_>>(&zbuild_file).map_err(AppError::parse_yaml(&zbuild_path))?;
	tracing::trace!(?ast, "Parsed ast");

	// Build the rules
	let rules = Rules::new(ast);
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
						rule: rules::Expr::string(rule.name.to_owned()),
						pats: HashMap::new(),
					},
				)
			})
			.collect(),
	};
	tracing::trace!(?targets_to_build, "Found targets to build");

	// Create the builder
	let builder = Builder::new(jobs);

	// Then create the watcher, if we're watching
	let watcher = args
		.watch
		.then(|| {
			// TODO: Better default?
			let debouncer_timeout_ms = args.watcher_debouncer_timeout_ms.unwrap_or(0.0_f64);
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
		.filter(|(_, res)| res.as_ref().map_or(false, |res| res.built))
		.count();
	tracing::info!("Built {built_targets} targets");
	tracing::info!("Checked {total_targets} targets");

	match failed_targets.is_empty() {
		true => Ok(()),
		false => {
			tracing::error!("One or more builds failed:");
			for (target, err) in failed_targets {
				tracing::error!(err=?anyhow::Error::new(err), "Failed to build target {target}");
			}

			anyhow::bail!("Exiting with non-0 due to failed builds");
		},
	}
}

/// Finds the nearest zbuild file
async fn find_zbuild() -> Result<PathBuf, AppError> {
	let cur_path = env::current_dir().map_err(AppError::get_current_dir())?;
	let mut cur_path = cur_path.as_path();

	loop {
		let zbuild_path = cur_path.join("zbuild.yaml");
		match util::fs_try_exists(&zbuild_path)
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
async fn build_target<'s, T: BuildableTargetInner<'s> + fmt::Display + fmt::Debug>(
	builder: &Builder<'s>,
	target: &rules::Target<'s, T>,
	rules: &Rules<'s>,
	ignore_missing: bool,
) -> Result<(), AppError> {
	tracing::debug!(%target, "Building target");

	// Try to build the target
	let build_start_time = SystemTime::now();
	let res = T::build(target, builder, rules, ignore_missing).await;

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
			let err = anyhow::Error::new(err);
			tracing::error!(%target, ?err, "Unable to build target");
			Err(err.downcast().expect("Should be app error"))
		},
	}
}

/// A buildable target inner type
trait BuildableTargetInner<'s>: Sized {
	/// Builds this target
	async fn build(
		target: &rules::Target<'s, Self>,
		builder: &Builder<'s>,
		rules: &Rules<'s>,
		ignore_missing: bool,
	) -> Result<build::BuildResult, AppError>;
}

impl<'s> BuildableTargetInner<'s> for rules::Expr<'s> {
	async fn build(
		target: &rules::Target<'s, Self>,
		builder: &Builder<'s>,
		rules: &Rules<'s>,
		ignore_missing: bool,
	) -> Result<build::BuildResult, AppError> {
		builder
			.build_expr(target, rules, ignore_missing)
			.await
			.map(|(build_res, _)| build_res)
	}
}

impl<'s> BuildableTargetInner<'s> for CowStr<'s> {
	async fn build(
		target: &rules::Target<'s, Self>,
		builder: &Builder<'s>,
		rules: &Rules<'s>,
		ignore_missing: bool,
	) -> Result<build::BuildResult, AppError> {
		builder
			.build(target, rules, ignore_missing)
			.await
			.map(|(build_res, _)| build_res)
	}
}
