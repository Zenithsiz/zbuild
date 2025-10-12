//! Zbuild testing utilities.

// Features
#![feature(must_not_suspend)]

// Imports
use {
	app_error::Context,
	std::fs,
	tempfile::TempDir,
	zbuild::{AppError, Args},
};

/// Creates a directory with a zbuild manifest, then runs it, and returns the directory
pub async fn with_zbuild<'a, T>(zbuild_manifest: &str, targets: T) -> Result<TempDir, AppError>
where
	T: AsRef<[&'a str]>,
{
	self::with_zbuild_multiple(zbuild_manifest, [], targets).await
}

/// Creates a directory with a zbuild manifest and other zbuild files, then runs it, and returns the directory
pub async fn with_zbuild_multiple<'a, T>(
	zbuild_manifest: &str,
	others: impl IntoIterator<Item = (&str, &str)>,
	targets: T,
) -> Result<TempDir, AppError>
where
	T: AsRef<[&'a str]>,
{
	let temp_dir = TempDir::with_prefix("zbuild").context("Unable to create temporary directory")?;

	let zbuild_zb = temp_dir.path().join("zbuild.zb");
	fs::write(&zbuild_zb, zbuild_manifest).context("Unable to write zbuild manifest")?;

	for (name, contents) in others {
		let file = temp_dir.path().join(name);
		fs::write(&file, contents).context("Unable to write zbuild file")?;
	}

	let args = Args {
		targets: targets.as_ref().iter().copied().map(str::to_owned).collect(),
		zbuild_path: Some(zbuild_zb),
		..Args::default()
	};
	tracing::info!(?args, "Arguments");
	zbuild::run(args).await.context("Unable to run zbuild")?;

	Ok(temp_dir)
}
