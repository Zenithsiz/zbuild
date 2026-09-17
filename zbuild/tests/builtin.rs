// Features
#![feature(must_not_suspend, yeet_expr)]
// Lints
#![expect(clippy::tests_outside_test_module, reason = "We're an integration test")]

// Imports
use {
	app_error::Context,
	zbuild::{ExitResult, Fs},
};

/// Test for `builtin.mkdir_parents`
#[tokio::test]
#[tracing_test::traced_test]
async fn builtin_mkdir_parents() -> ExitResult {
	let fs = zbuild_test::with_zbuild(
		r#"
rule create_dir {
	out "a/b/c";
	exec builtin.mkdir_parents "a/b/c";
}
		"#,
		["a/b/c"],
	)
	.await?;

	if !fs
		.try_exists_symlink("a/b/c")
		.context("Unable to check if output file exists")?
	{
		app_error::bail!("Output file was missing");
	}

	ExitResult::Ok
}

/// Test for `builtin.mkdir_parents` with working directory
#[tokio::test]
#[tracing_test::traced_test]
async fn builtin_mkdir_parents_cwd() -> ExitResult {
	let fs = zbuild_test::with_zbuild(
		r#"
rule create_dir {
	out "a/b/c";
	exec {
		cwd "a/b";
		args builtin.mkdir_parents "c";
	};
}
		"#,
		["a/b/c"],
	)
	.await?;

	if !fs
		.try_exists_symlink("a/b/c")
		.context("Unable to check if output file exists")?
	{
		app_error::bail!("Output file was missing");
	}

	ExitResult::Ok
}

/// Test for `builtin.touch`
#[tokio::test]
#[tracing_test::traced_test]
async fn builtin_touch() -> ExitResult {
	let fs = zbuild_test::with_zbuild_multiple(
		r#"
rule touch_files {
	out "a";
	out "b";
	exec builtin.touch "a" "b";
}
		"#,
		[("a", "abc")],
		["a", "b"],
	)
	.await?;

	for file_out in ["a", "b"] {
		if !fs
			.try_exists_symlink(file_out)
			.context("Unable to check if output file exists")?
		{
			app_error::bail!("Output file {file_out:?} was missing");
		}
	}

	let a_contents = fs.read_to_string("a").context("Unable to read output file")?;
	if a_contents != "abc" {
		app_error::bail!("Output file a was tampered");
	}

	let b_contents = fs.read_to_string("b").context("Unable to read output file")?;
	if !b_contents.is_empty() {
		app_error::bail!("Output file b wasn't empty");
	}

	// TODO: Test that the time was updated.

	ExitResult::Ok
}
