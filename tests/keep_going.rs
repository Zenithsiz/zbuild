// Features
#![feature(must_not_suspend, yeet_expr)]
// Lints
#![expect(clippy::tests_outside_test_module, reason = "We're an integration test")]

// Modules
mod util;

// Imports
use {
	std::fs,
	tempfile::TempDir,
	zbuild::{AppError, Args, ExitResult},
	zutil_app_error::Context,
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
	let temp_dir = TempDir::with_prefix("zbuild").context("Unable to create temporary directory")?;
	let zbuild_zb = temp_dir.path().join("zbuild.zb");

	// TODO: Instead of sleeping, use `inotify` to wait for other
	//       actions to happen?
	fs::write(
		&zbuild_zb,
		r#"
rule a {
	out "a";
	deps ["b", "c1"];
	exec ["touch" "a"];
}

rule b {
	out "b";
	exec [
		"sleep" "0.1",
		"false",
		"touch" "b",
	];
}

rule c1 {
	out "c1";
	deps "c2";
	exec ["touch" "c1"];
}

rule c2 {
	out "c2";
	exec [
		"sleep" "0.2",
		"touch" "c2"
	];
}
		"#,
	)
	.context("Unable to write zbuild manifest")?;

	let args = Args {
		targets: ["a".to_owned()].into(),
		zbuild_path: Some(zbuild_zb),
		keep_going,
		..Args::default()
	};
	tracing::info!(?args, "Arguments");
	let res = zbuild::run(args).await;
	zutil_app_error::ensure!(res.is_err(), "Expected zbuild error");


	let a = temp_dir.path().join("a");
	let b = temp_dir.path().join("b");
	let c1 = temp_dir.path().join("c1");
	let c2 = temp_dir.path().join("c2");
	zutil_app_error::ensure!(
		!a.try_exists().context("Unable to check if output file exists")?,
		"Output file {a:?} was created"
	);
	zutil_app_error::ensure!(
		!b.try_exists().context("Unable to check if output file exists")?,
		"Output file {b:?} was created"
	);

	match keep_going {
		true => zutil_app_error::ensure!(
			c1.try_exists().context("Unable to check if output file exists")?,
			"Output file {c1:?} was missing"
		),
		false => zutil_app_error::ensure!(
			!c1.try_exists().context("Unable to check if output file exists")?,
			"Output file {c1:?} was created"
		),
	}

	zutil_app_error::ensure!(
		c2.try_exists().context("Unable to check if output file exists")?,
		"Output file {c2:?} was missing"
	);

	Ok(())
}
