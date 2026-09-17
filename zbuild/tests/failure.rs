// Features
#![feature(must_not_suspend, yeet_expr)]
// Lints
#![expect(clippy::tests_outside_test_module, reason = "We're an integration test")]

// Imports
use {
	app_error::Context,
	zbuild::{Args, ExitResult, Fs, MemFs},
};

#[tokio::test]
#[tracing_test::traced_test]
async fn failure() -> ExitResult {
	let fs = MemFs::new();

	let zbuild_zb = "zbuild.zb";

	// TODO: Instead of sleeping, use `inotify` to wait for other
	//       actions to happen?
	fs.write(
		zbuild_zb,
		br#"
rule a {
	out "a";
	exec "false";
	exec builtin.touch "a";
}
		"#,
	)
	.context("Unable to write zbuild manifest")?;

	let args = Args {
		targets: ["a".to_owned()].into(),
		zbuild_path: Some(zbuild_zb.into()),
		..Args::default()
	};
	tracing::info!(?args, "Arguments");
	let res = zbuild::run(&fs, args).await;
	app_error::ensure!(res.is_err(), "Expected zbuild error");

	app_error::ensure!(
		!fs.try_exists_symlink("a")
			.context("Unable to check if output file exists")?,
		"Output file \"a\" was created when it shouldn't've"
	);

	ExitResult::Ok
}
