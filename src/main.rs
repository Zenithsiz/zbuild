//! Build system

// Features
#![feature(
	seek_stream_len,
	let_else,
	map_try_insert,
	label_break_value,
	never_type,
	closure_lifetime_binder,
	anonymous_lifetime_in_impl_trait,
	fs_try_exists,
	iterator_try_reduce,
	exit_status_error,
	poll_ready,
	hash_raw_entry,
	decl_macro
)]

// Modules
mod args;
mod ast;
mod build;
mod logger;
mod rules;
mod util;

// Exports
pub use self::{ast::Ast, rules::Rules};

// Imports
use {
	anyhow::Context,
	args::Args,
	clap::StructOpt,
	futures::{stream::FuturesUnordered, TryStreamExt},
	std::{collections::HashMap, env, fs},
	tracing_subscriber::prelude::*,
};


#[tokio::main]
async fn main() -> Result<(), anyhow::Error> {
	// Initialize the logger
	let log_use_color = env::var("RUST_LOG_COLOR").map_or(true, |value| {
		matches!(value.trim().to_uppercase().as_str(), "1" | "YES" | "TRUE")
	});
	tracing_subscriber::registry()
		.with(
			tracing_subscriber::fmt::layer()
				.with_ansi(log_use_color)
				.with_filter(tracing_subscriber::EnvFilter::from_default_env()),
		)
		.init();

	// Get all args
	let args = Args::parse();
	tracing::trace!(?args, "Arguments");

	// Parse the ast
	let ast = {
		let file = self::find_zbuild()?;
		serde_yaml::from_reader::<_, Ast>(file).context("Unable to parse `zbuild.yaml`")?
	};
	tracing::trace!(target: "zbuild_ast", ?ast, "Parsed ast");

	// Build the rules
	let rules = Rules::new(ast).context("Unable to create rules")?;
	tracing::trace!(target: "zbuild_rules", ?rules, "Rules");

	// Build the default rule
	let jobs = match args.jobs {
		Some(jobs) => jobs,
		None => std::thread::available_parallelism()
			.context("Unable to get available parallelism")?
			.into(),
	};

	let targets = match args.targets.is_empty() {
		// If empty, use the default rules
		true => rules.default.clone(),

		// Else get them
		// TODO: Maybe don't infer rules?
		false => args
			.targets
			.iter()
			.map(|target| match rules.rules.get(target) {
				Some(rule) => rules::Target::Rule {
					rule: rules::Expr::string(rule.name.to_owned()),
					pats: HashMap::new(),
				},
				None => rules::Target::File {
					file: rules::Expr::string(target.to_owned()),
				},
			})
			.collect(),
	};

	let builder = build::Builder::new(jobs);
	targets
		.iter()
		.map(|target| {
			let builder = &builder;
			let rules = &rules;
			async move {
				builder
					.build_unexpanded(target, rules)
					.await
					.with_context(|| format!("Unable to build {target:?}"))
			}
		})
		.collect::<FuturesUnordered<_>>()
		.try_collect::<Vec<_>>()
		.await?;

	tracing::info!("Built {} targets", builder.targets().await);

	Ok(())
}

/// Finds and sets the working directory to the nearest zbuild file
pub fn find_zbuild() -> Result<fs::File, anyhow::Error> {
	match fs::File::open("zbuild.yaml") {
		Ok(file) => Ok(file),
		Err(_) => match env::current_dir().context("Unable to get current directory")?.parent() {
			Some(parent) => {
				env::set_current_dir(parent).context("Unable to set current directory")?;
				self::find_zbuild()
			},
			None => anyhow::bail!("No `zbuild.yaml` file found in current or parent directories"),
		},
	}
}
