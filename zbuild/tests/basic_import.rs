// Features
#![feature(must_not_suspend)]
// Lints
#![expect(clippy::tests_outside_test_module, reason = "We're an integration test")]

// Imports
use zbuild::ExitResult;

/// Import
#[tokio::test]
#[tracing_test::traced_test]
async fn basic_import() -> ExitResult {
	let _temp_dir = zbuild_test::with_zbuild_multiple(r#"include "a.zb";"#, [("a.zb", "")], []).await?;

	ExitResult::Ok
}
