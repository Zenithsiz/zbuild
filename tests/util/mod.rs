//! Utilities for all integration tests

// Lints
#![allow(
	dead_code,
	reason = "This module is used from many tests, which might not use everything"
)]

// Imports
use {anyhow::Context, std::fs, tempdir::TempDir, zbuild::Args};

/// Creates a directory with a zbuild manifest, then runs it, and returns the directory
pub async fn with_zbuild<'a, T>(zbuild_manifest: &str, targets: T) -> Result<TempDir, anyhow::Error>
where
	T: AsRef<[&'a str]>,
{
	let temp_dir = TempDir::new("zbuild").context("Unable to create temporary directory")?;
	let zbuild_yaml = temp_dir.path().join("zbuild.yaml");

	fs::write(&zbuild_yaml, zbuild_manifest).context("Unable to write zbuild manifest")?;

	let args = Args {
		targets: targets.as_ref().iter().copied().map(str::to_owned).collect(),
		zbuild_path: Some(zbuild_yaml),
		..Args::default()
	};
	tracing::info!(?args, "Arguments");
	zbuild::run(args).await.context("Unable to run zbuild")?;

	Ok(temp_dir)
}
