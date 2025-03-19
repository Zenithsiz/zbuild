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
	zbuild::{Args, ExitResult},
	zutil_app_error::Context,
};

#[tokio::test]
#[tracing_test::traced_test]
async fn failure() -> ExitResult {
	let temp_dir = TempDir::with_prefix("zbuild").context("Unable to create temporary directory")?;
	let zbuild_zb = temp_dir.path().join("zbuild.zb");

	// TODO: Instead of sleeping, use `inotify` to wait for other
	//       actions to happen?
	fs::write(
		&zbuild_zb,
		r#"
rule a {
	out "a";
	exec "false";
	exec "touch" "a";
}
		"#,
	)
	.context("Unable to write zbuild manifest")?;

	let args = Args {
		targets: ["a".to_owned()].into(),
		zbuild_path: Some(zbuild_zb),
		..Args::default()
	};
	tracing::info!(?args, "Arguments");
	let res = zbuild::run(args).await;
	zutil_app_error::ensure!(res.is_err(), "Expected zbuild error");


	let a = temp_dir.path().join("a");
	zutil_app_error::ensure!(
		!a.try_exists().context("Unable to check if output file exists")?,
		"Output file {a:?} was created when it shouldn't've"
	);

	ExitResult::Ok
}
