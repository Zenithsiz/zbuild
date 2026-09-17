// Features
#![feature(must_not_suspend)]
// Lints
#![expect(clippy::tests_outside_test_module, reason = "We're an integration test")]

// Imports
use {
	app_error::{Context, app_error},
	zbuild::{ExitResult, Fs},
};

/// Single rule and target
#[tokio::test]
#[tracing_test::traced_test]
async fn basic_none() -> ExitResult {
	let fs = zbuild_test::with_zbuild(
		r#"
rule create_file {
	out "file.out";
	exec builtin.touch "file.out";
}
		"#,
		// Note: We're passing no targets
		[],
	)
	.await?;

	// Note: We're making sure it *doesn't* exist, since we didn't want to build it.
	if fs
		.try_exists_symlink("file.out")
		.context("Unable to check if output file exists")?
	{
		Err(app_error!("Output file was present"))?;
	}

	ExitResult::Ok
}
