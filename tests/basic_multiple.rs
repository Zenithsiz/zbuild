// Features
#![feature(must_not_suspend, strict_provenance)]
// Lints
#![expect(clippy::tests_outside_test_module, reason = "We're an integration test")]

// Modules
mod util;

// Imports
use {anyhow::Context, zbuild::ExitResult};

/// Single rule with multiple outputs
#[tokio::test]
#[tracing_test::traced_test]
async fn basic_multiple() -> ExitResult {
	let temp_dir = util::with_zbuild(
		"---
rules:
  create_file:
    out: [file1.out, file2.out]
    exec:
      - [touch, file1.out]
      - [touch, file2.out]",
		// Note: Only request `file1.out`
		["file1.out"],
	)
	.await?;

	let file1_out = temp_dir.path().join("file1.out");
	let file2_out = temp_dir.path().join("file2.out");
	for file_out in [file1_out, file2_out] {
		if !file_out.try_exists().context("Unable to check if output file exists")? {
			Err(anyhow::anyhow!("Output file {file_out:?} was missing"))?;
		}
	}

	ExitResult::Ok
}
