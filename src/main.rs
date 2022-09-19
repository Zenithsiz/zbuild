//! `Zbuild` build system

// Features
#![feature(
	seek_stream_len,
	let_else,
	map_try_insert,
	never_type,
	closure_lifetime_binder,
	anonymous_lifetime_in_impl_trait,
	fs_try_exists,
	iterator_try_reduce,
	exit_status_error,
	poll_ready,
	hash_raw_entry,
	decl_macro,
	box_patterns,
	try_blocks
)]

// Modules
mod args;
mod ast;
mod build;
mod error;
mod logger;
mod rules;
mod util;

// Exports
pub use self::{ast::Ast, error::AppError, rules::Rules};

// Imports
use {
	anyhow::Context,
	args::Args,
	clap::StructOpt,
	futures::{stream::FuturesUnordered, TryStreamExt},
	std::{collections::HashMap, env, fs, path::PathBuf},
};


// Note: We return an `anyhow::Error` because it has good formatting
// TODO: Return our own error once we improve formatting?
#[tokio::main]
async fn main() -> Result<(), anyhow::Error> {
	// Initialize the logger
	logger::init();

	// Get all args
	let args = Args::parse();
	tracing::trace!(?args, "Arguments");

	// Find the zbuild location and change the current directory to it
	let zbuild_path = match args.zbuild_path {
		Some(path) => path,
		None => self::find_zbuild()?,
	};
	let zbuild_dir = zbuild_path.parent().expect("Zbuild path had no parent");
	tracing::trace!(?zbuild_path, "Found zbuild path");
	std::env::set_current_dir(zbuild_dir).map_err(AppError::set_current_dir(zbuild_dir))?;

	// Parse the ast
	let ast = {
		let zbuild_file = fs::File::open(&zbuild_path).map_err(AppError::open_file(&zbuild_path))?;
		serde_yaml::from_reader::<_, Ast>(zbuild_file).map_err(AppError::parse_yaml(&zbuild_path))?
	};
	tracing::trace!(target: "zbuild_ast", ?ast, "Parsed ast");

	// Build the rules
	let rules = Rules::new(ast);
	tracing::trace!(target: "zbuild_rules", ?rules, "Rules");

	// Get the max number of jobs we can execute at once
	let jobs = match args.jobs {
		Some(jobs) => jobs,
		None => std::thread::available_parallelism()
			.map_err(AppError::get_default_jobs())?
			.into(),
	};
	tracing::debug!(?jobs, "Found number of jobs to run concurrently");

	// Then get all targets to build
	let targets = match args.targets.is_empty() {
		// If none were specified, use the default rules
		true => rules.default.clone(),

		// Else infer them as either rules or files
		// TODO: Maybe be explicit about rule-name inferring?
		//       If a file has the same name as a rule, it may be
		//       unexpected behavior, but we can't just check if the
		//       file exists to disambiguate, because it might not be
		//       created yet
		false => args
			.targets
			.into_iter()
			.map(|target| match rules.rules.get(&target) {
				// If there was a rule, use it without any patterns
				// TODO: If it requires patterns maybe error out here?
				Some(rule) => rules::Target::Rule {
					rule: rules::Expr::string(rule.name.to_owned()),
					pats: HashMap::new(),
				},

				// Else just create a file
				None => rules::Target::File {
					file: rules::Expr::string(target),
				},
			})
			.collect(),
	};
	tracing::trace!(target: "zbuild_targets", ?targets, "Found targets");

	// Finally create the builder and build all targets
	let builder = build::Builder::new(jobs);
	targets
		.iter()
		.map(|target| {
			let builder = &builder;
			let rules = &rules;
			async move {
				builder
					.build_expr(target, rules)
					.await
					.with_context(|| format!("Unable to build {target:?}"))
			}
		})
		.collect::<FuturesUnordered<_>>()
		.try_collect::<Vec<_>>()
		.await?;
	tracing::info!("Checked {} targets", builder.targets().await);

	Ok(())
}

/// Finds the nearest zbuild file
fn find_zbuild() -> Result<PathBuf, AppError> {
	let cur_path = env::current_dir().map_err(AppError::get_current_dir())?;
	let mut cur_path = cur_path.as_path();

	loop {
		let zbuild_path = cur_path.join("zbuild.yaml");
		match fs::try_exists(&zbuild_path).map_err(AppError::check_file_exists(&zbuild_path))? {
			true => return Ok(zbuild_path),
			false => match cur_path.parent() {
				Some(parent) => cur_path = parent,
				None => return Err(AppError::ZBuildNotFound),
			},
		}
	}
}
