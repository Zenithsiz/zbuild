// Features
#![feature(must_not_suspend, strict_provenance)]
// Lints
#![expect(clippy::tests_outside_test_module, reason = "We're an integration test")]

// Modules
mod util;

// Imports
use {
	anyhow::Context,
	std::fs,
	tempdir::TempDir,
	zbuild::{Args, ExitResult},
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
async fn inner(keep_going: bool) -> Result<(), anyhow::Error> {
	let temp_dir = TempDir::new("zbuild").context("Unable to create temporary directory")?;
	let zbuild_yaml = temp_dir.path().join("zbuild.yaml");

	// TODO: Instead of sleeping, use `inotify` to wait for other
	//       actions to happen?
	fs::write(
		&zbuild_yaml,
		r#"---
rules:
  a:
    out: [a]
    deps: [b, c1]
    exec:
      - [touch, a]
  b:
    out: [b]
    exec:
      - [sleep, "0.1"]
      - ["false"]
      - [touch, b]
  c1:
    out: [c1]
    deps: [c2]
    exec:
      - [touch, c1]
  c2:
    out: [c2]
    exec:
      - [sleep, "0.2"]
      - [touch, c2]
"#,
	)
	.context("Unable to write zbuild manifest")?;

	let args = Args {
		targets: ["a".to_owned()].into(),
		zbuild_path: Some(zbuild_yaml),
		keep_going,
		..Args::default()
	};
	tracing::info!(?args, "Arguments");
	let res = zbuild::run(args).await;
	anyhow::ensure!(res.is_err(), "Expected zbuild error");


	let a = temp_dir.path().join("a");
	let b = temp_dir.path().join("b");
	let c1 = temp_dir.path().join("c1");
	let c2 = temp_dir.path().join("c2");
	anyhow::ensure!(
		!a.try_exists().context("Unable to check if output file exists")?,
		"Output file {a:?} was created"
	);
	anyhow::ensure!(
		!b.try_exists().context("Unable to check if output file exists")?,
		"Output file {b:?} was created"
	);

	match keep_going {
		true => anyhow::ensure!(
			c1.try_exists().context("Unable to check if output file exists")?,
			"Output file {c1:?} was missing"
		),
		false => anyhow::ensure!(
			!c1.try_exists().context("Unable to check if output file exists")?,
			"Output file {c1:?} was created"
		),
	}

	anyhow::ensure!(
		c2.try_exists().context("Unable to check if output file exists")?,
		"Output file {c2:?} was missing"
	);

	Ok(())
}
