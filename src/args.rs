//! Cli manager

// Imports
use std::path::PathBuf;

/// zbuild build system
///
/// Build system based on files and rules.
///
/// You can create rules that may output files, have dependencies and be executed,
/// in order to create the output files.
///
/// Then you may request `zbuild` to create a certain file and it will parse
/// all rules in order to find a way to build the output
#[derive(Debug)]
#[derive(clap::Parser)]
#[clap(author, version, about)]
#[expect(clippy::struct_excessive_bools, reason = "It's normal to have a lot of switches")]
pub struct Args {
	/// All targets to build.
	///
	/// If empty, uses default targets.
	pub targets: Vec<String>,

	/// Zbuild path
	///
	/// Changes process working directory to parent directory of this file
	#[clap(long = "path")]
	pub zbuild_path: Option<PathBuf>,

	/// Number of concurrent jobs.
	///
	/// Defaults to available parallelism
	#[clap(long = "jobs", short = 'j')]
	pub jobs: Option<usize>,

	/// Ignores missing files.
	///
	/// This is useful when you have a `.d` dependency file
	/// that mentions a file, but you changed the code so that
	/// file is no longer requires. The `.d` file only changes
	/// after building the code, but zbuild assumes the file is
	/// needed to build the code.
	#[clap(long = "ignore-missing", short = 'i')]
	pub ignore_missing: bool,

	/// Keeps building files even if an error has occurred.
	///
	/// Normally, whenever an error occurs, further rules are forbidden
	/// to execute, although currently executing rules continue running.
	///
	/// This makes it so that whenever an error occurs,
	/// we continue searching and executing rules until there is nothing
	/// else we can do
	#[clap(long = "keep-going")]
	pub keep_going: bool,

	/// Always build rules, even if their outputs are up to date
	#[clap(long = "always-build")]
	pub always_build: bool,

	/// Watch for file changes and rebuild any necessary targets.
	///
	/// WARNING: If the log file is situated in the same directory as any watched
	///          file, and you're logging at level trace, the log file will grow
	///          immensely fast, as each logged line will result in another after
	///          the watcher debouncer.
	#[clap(long = "watch", short = 'w')]
	pub watch: bool,

	/// Watcher file event debouncer timeout
	#[clap(long = "watcher-debouncer-timeout-ms")]
	pub watcher_debouncer_timeout_ms: Option<f64>,

	/// Logs output to a file.
	///
	/// You can use `RUST_FILE_LOG` to set filtering options
	#[clap(long = "log-file")]
	pub log_file: Option<PathBuf>,
}

#[expect(clippy::derivable_impls, reason = "We want to be explicit with the defaults")]
impl Default for Args {
	fn default() -> Self {
		Self {
			targets: vec![],
			zbuild_path: None,
			jobs: None,
			ignore_missing: false,
			keep_going: false,
			always_build: false,
			watch: false,
			watcher_debouncer_timeout_ms: None,
			log_file: None,
		}
	}
}
