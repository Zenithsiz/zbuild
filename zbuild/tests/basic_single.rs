// Features
#![feature(must_not_suspend)]
// Lints
#![expect(clippy::tests_outside_test_module, reason = "We're an integration test")]

// Modules
mod util;

// Imports
use {
	zbuild::ExitResult,
	app_error::{Context, app_error},
};

/// Single rule and target
#[tokio::test]
#[tracing_test::traced_test]
async fn basic_single() -> ExitResult {
	let temp_dir = util::with_zbuild(
		r#"
rule create_file {
	out "file.out";
	exec "touch" "file.out";
}
		"#,
		["file.out"],
	)
	.await?;

	let file_out = temp_dir.path().join("file.out");
	if !file_out.try_exists().context("Unable to check if output file exists")? {
		Err(app_error!("Output file {file_out:?} was missing"))?;
	}

	ExitResult::Ok
}
