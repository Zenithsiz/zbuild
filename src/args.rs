//! Cli manager

// Imports
use std::path::PathBuf;

/// Data from the command line
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
}
