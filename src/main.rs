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
mod logger;

// Imports
use {
	anyhow::Context,
	clap::Parser,
	std::{
		env,
		sync::atomic::{self, AtomicUsize},
	},
	tokio::runtime,
	zbuild::{AppError, Args, ExitResult},
};

#[expect(
	unused_results,
	reason = "Runtime builder method provides a lot of unused `&mut Builder`"
)]
fn main() -> ExitResult {
	// Get all args
	let args = Args::parse();

	// Initialize the logger
	logger::init(args.log_file.as_deref());
	tracing::trace!(?args, "Arguments");

	// Build the tokio runtime
	let mut runtime_builder = runtime::Builder::new_multi_thread();
	runtime_builder.enable_all().thread_name_fn(|| {
		static NEXT_THREAD_ID: AtomicUsize = AtomicUsize::new(0);
		let thread_id = NEXT_THREAD_ID.fetch_add(1, atomic::Ordering::AcqRel);
		format!("tokio${thread_id}")
	});

	// Note: Although `worker_threads` can be controlled via `TOKIO_WORKER_THREADS`,
	//       the max number of blocking threads cannot, but it's possible to cut down
	//       time in half for some builds with this option, so we supply it.
	if let Ok(max_blocking_threads) = env::var("TOKIO_MAX_BLOCKING_THREADS") {
		match max_blocking_threads.parse() {
			Ok(max_blocking_threads) => {
				runtime_builder.max_blocking_threads(max_blocking_threads);
			},
			Err(err) => {
				tracing::warn!(
					?max_blocking_threads,
					?err,
					"Unable to parse `TOKIO_MAX_BLOCKING_THREADS`, using default"
				);
			},
		}
	}

	let runtime = runtime_builder
		.build()
		.context("Failed building the Runtime")
		.map_err(AppError::Other)?;

	runtime.block_on(zbuild::run(args))?;
	ExitResult::Ok
}
