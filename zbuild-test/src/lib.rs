//! Zbuild testing utilities.

// Features
#![feature(must_not_suspend)]

// Imports
use {
	app_error::Context,
	zbuild::{AppError, Args, Fs, MemFs},
};

/// Creates a directory with a zbuild manifest, then runs it, and returns the directory
pub async fn with_zbuild<'a, T>(zbuild_manifest: &str, targets: T) -> Result<MemFs, AppError>
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
) -> Result<MemFs, AppError>
where
	T: AsRef<[&'a str]>,
{
	let fs = MemFs::new();
	let zbuild_zb = "zbuild.zb";
	fs.write(&zbuild_zb, zbuild_manifest.as_bytes())
		.await
		.context("Unable to write zbuild manifest")?;

	for (name, contents) in others {
		fs.write(name, contents.as_bytes())
			.await
			.context("Unable to write zbuild file")?;
	}

	let args = Args {
		targets: targets.as_ref().iter().copied().map(str::to_owned).collect(),
		zbuild_path: Some(zbuild_zb.into()),
		..Args::default()
	};
	tracing::info!(?args, "Arguments");

	zbuild::run(&fs, args).await.context("Unable to run zbuild")?;

	Ok(fs)
}
