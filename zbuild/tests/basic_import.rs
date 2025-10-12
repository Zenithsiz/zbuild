// Features
#![feature(must_not_suspend)]
// Lints
#![expect(clippy::tests_outside_test_module, reason = "We're an integration test")]

// Modules
mod util;

// Imports
use zbuild::ExitResult;

/// Import
#[tokio::test]
#[tracing_test::traced_test]
async fn basic_import() -> ExitResult {
	let _temp_dir = util::with_zbuild_multiple(r#"include "a.zb";"#, [("a.zb", "")], []).await?;

	ExitResult::Ok
}
