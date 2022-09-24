//! `Zbuild` build system

// Features
#![feature(
	seek_stream_len,
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
	try_blocks,
	async_closure,
	let_chains,
	lint_reasons
)]
// Lints
#![forbid(unsafe_code)]
#![warn(
	clippy::pedantic,
	clippy::nursery,
	clippy::as_conversions,
	clippy::as_underscore,
	clippy::assertions_on_result_states,
	clippy::clone_on_ref_ptr,
	clippy::create_dir,
	clippy::deref_by_slicing,
	clippy::empty_drop,
	clippy::empty_structs_with_brackets,
	clippy::exhaustive_enums,
	clippy::filetype_is_file,
	clippy::format_push_string,
	clippy::get_unwrap,
	clippy::if_then_some_else_none,
	clippy::indexing_slicing,
	clippy::map_err_ignore,
	clippy::mixed_read_write_in_expression,
	clippy::mod_module_files,
	clippy::rc_buffer,
	clippy::rc_mutex,
	clippy::rest_pat_in_fully_bound_structs,
	clippy::same_name_method,
	// Note: Good lint, but has a few too many false positives
	//       so just enable every once in a while to check if
	//       there are any true positive cases
	//clippy::shadow_unrelated,
	clippy::str_to_string,
	clippy::string_slice,
	clippy::string_to_string,
	clippy::try_err,
	clippy::unnecessary_self_imports,
	clippy::unneeded_field_pattern,
	clippy::unwrap_used,
	clippy::verbose_file_reads,
)]
#![allow(clippy::match_bool, clippy::single_match_else)] // Matching boolean-likes looks better than if/else
#![allow(clippy::items_after_statements)] // We'd prefer a lint that would trigger usages of it in previous statements
#![allow(clippy::missing_errors_doc)] // TODO: Create errors on a per-function basic to avoid doing this
#![allow(clippy::module_name_repetitions)] // This is how we organize some modules

// Modules
mod args;
mod ast;
mod build;
mod error;
mod logger;
mod rules;
mod util;
mod watcher;

// Imports
use {
	self::{ast::Ast, build::Builder, error::AppError, rules::Rules},
	args::Args,
	clap::StructOpt,
	futures::{stream::FuturesUnordered, TryStreamExt},
	std::{collections::HashMap, env, fs, path::PathBuf},
	watcher::Watcher,
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
	let targets_to_build = match args.targets.is_empty() {
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
			.map(|target| {
				rules.rules.get(&target).map_or_else(
					// By default, use a file
					|| rules::Target::File {
						file:      rules::Expr::string(target),
						is_static: false,
					},
					// If there was a rule, use it without any patterns
					// TODO: If it requires patterns maybe error out here?
					|rule| rules::Target::Rule {
						rule: rules::Expr::string(rule.name.clone()),
						pats: HashMap::new(),
					},
				)
			})
			.collect(),
	};
	tracing::trace!(target: "zbuild_targets", ?targets_to_build, "Found targets");

	// Create the builder
	let builder = Builder::new(jobs);

	// Then create the watcher, if we're watching
	let watcher = args
		.watch
		.then(|| Watcher::new(builder.subscribe_events()))
		.transpose()?;

	// Finally build all targets
	targets_to_build
		.iter()
		.map(|target| {
			let builder = &builder;
			let rules = &rules;
			async move {
				builder
					.build_expr(target, rules)
					.await
					.map_err(AppError::build_target(target))
			}
		})
		.collect::<FuturesUnordered<_>>()
		.try_collect::<Vec<_>>()
		.await?;

	// Then, if we have a watcher, watch all the dependencies
	if let Some(watcher) = watcher {
		tracing::info!("Starting to watch for all targets");
		watcher.watch_rebuild(&builder, &rules).await?;
	}

	let targets = builder.targets().await;
	let total_targets = targets.len();
	let built_targets = targets
		.iter()
		.filter(|(_, res)| res.as_ref().map_or(false, |res| res.built))
		.count();
	tracing::info!("Built {built_targets} targets");
	tracing::info!("Checked {total_targets} targets");

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
