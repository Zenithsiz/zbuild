// Features
#![feature(must_not_suspend)]
// Lints
#![expect(clippy::tests_outside_test_module, reason = "We're an integration test")]

// Imports
use {
	app_error::{Context, app_error},
	zbuild::{ExitResult, Fs},
};

/// Single rule with multiple outputs
#[tokio::test]
#[tracing_test::traced_test]
async fn basic_multiple() -> ExitResult {
	let fs = zbuild_test::with_zbuild(
		r#"
rule create_file {
	out "file1.out";
	out "file2.out";

	exec builtin.touch "file1.out";
	exec builtin.touch "file2.out";
}
		"#,
		// Note: Only request `file1.out`
		["file1.out"],
	)
	.await?;

	for file_out in ["file1.out", "file2.out"] {
		if !fs
			.try_exists_symlink(file_out)
			.context("Unable to check if output file exists")?
		{
			Err(app_error!("Output file {file_out:?} was missing"))?;
		}
	}

	ExitResult::Ok
}
