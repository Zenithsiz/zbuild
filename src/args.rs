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
#[derive(PartialEq, Eq, Clone, Debug)]
#[derive(clap::Parser)]
#[clap(author, version, about)]
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

	/// Watch for file changes and rebuild any necessary targets
	#[clap(long = "watch", short = 'w')]
	pub watch: bool,
}
