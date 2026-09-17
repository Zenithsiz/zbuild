// Features
#![feature(must_not_suspend, yeet_expr)]
// Lints
#![expect(clippy::tests_outside_test_module, reason = "We're an integration test")]

// Imports
use {
	app_error::Context,
	zbuild::{AppError, Args, ExitResult, Fs, MemFs},
};

/// Test for `--keep-going`
#[tokio::test]
#[tracing_test::traced_test]
async fn keep_going() -> ExitResult {
	self::inner(true).await.context("Unable to test with `--keep-going`")?;
	self::inner(false)
		.await
		.context("Unable to test without `--keep-going`")?;

	ExitResult::Ok
}

/// Inner function to test
///
/// This works by having the following tree:
///
/// ```no_compile
/// A -> B
///  \-> C1 -> C2
/// ```
///
/// Where `B` is always going to fail, after 200ms, to allow all other targets
/// to start running.
///
/// We make `C2` take a long time, to ensure `B` is executed (and fails)
/// before it can return.
///
/// When testing with `keep_going = true`, we ensure that `C1` is still built,
/// despite `C2` only finishing *after* `B` errors out.
///
/// When testing with `keep_going = false`, we ensure that `C1` is not built,
/// since `C2` exits after `B` errors, so nothing else should be built.
async fn inner(keep_going: bool) -> Result<(), AppError> {
	let fs = MemFs::new();

	let zbuild_zb = "zbuild.zb";

	// TODO: Instead of sleeping, use `inotify` to wait for other
	//       actions to happen?
	fs.write(
		zbuild_zb,
		br#"
rule a {
	out "a";
	dep "b";
	dep "c1";
	exec builtin.touch "a";
}

rule b {
	out "b";
	exec "sleep" "0.1";
	exec "false";
	exec builtin.touch "b";
}

rule c1 {
	out "c1";
	dep "c2";
	exec builtin.touch "c1";
}

rule c2 {
	out "c2";
	exec "sleep" "0.2";
	exec builtin.touch "c2";
}
		"#,
	)
	.context("Unable to write zbuild manifest")?;

	let args = Args {
		targets: ["a".to_owned()].into(),
		zbuild_path: Some(zbuild_zb.into()),
		keep_going,
		..Args::default()
	};
	tracing::info!(?args, "Arguments");
	let res = zbuild::run(&fs, args).await;
	app_error::ensure!(res.is_err(), "Expected zbuild error");


	app_error::ensure!(
		!fs.try_exists_symlink("a")
			.context("Unable to check if output file exists")?,
		"Output file \"a\" was created"
	);
	app_error::ensure!(
		!fs.try_exists_symlink("b")
			.context("Unable to check if output file exists")?,
		"Output file \"b\" was created"
	);

	match keep_going {
		true => app_error::ensure!(
			fs.try_exists_symlink("c1")
				.context("Unable to check if output file exists")?,
			"Output file \"c1\" was missing"
		),
		false => app_error::ensure!(
			!fs.try_exists_symlink("c1")
				.context("Unable to check if output file exists")?,
			"Output file \"c1\" was created"
		),
	}

	app_error::ensure!(
		fs.try_exists_symlink("c2")
			.context("Unable to check if output file exists")?,
		"Output file \"c2\" was missing"
	);

	Ok(())
}
