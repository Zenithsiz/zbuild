//! Logger

// Imports
use {
	std::{env, env::VarError},
	tracing::metadata::LevelFilter,
	tracing_subscriber::{prelude::*, EnvFilter},
};

/// Initializes the logger
pub fn init() {
	// Warnings to emit after configuring the logger
	let mut warnings = vec![];

	// Check if we need to use colors
	let log_use_color = match env::var("RUST_LOG_COLOR").map(|var| var.to_lowercase()).as_deref() {
		// By default / `1` / `yes` / `true`, use colors
		Err(VarError::NotPresent) | Ok("1" | "yes" | "true") => true,

		// On `0`, `no`, `false`, don't
		Ok("0" | "no" | "false") => false,

		// Else don't use colors, but warn
		Ok(env) => {
			warnings.push(format!(
				"Ignoring unknown `RUST_LOG_COLOR` value: {env:?}, expected `0`, `1`, `yes`, `no`, `true`, `false`"
			));
			false
		},
		Err(VarError::NotUnicode(err)) => {
			warnings.push(format!("Ignoring non-utf8 `RUST_LOG_COLOR`: {err:?}"));
			false
		},
	};

	// Then create the terminal layer
	let fmt_layer = tracing_subscriber::fmt::layer().with_ansi(log_use_color).with_filter(
		EnvFilter::builder()
			.with_default_directive(LevelFilter::INFO.into())
			.from_env_lossy(),
	);

	// Finally initialize
	tracing_subscriber::registry().with(fmt_layer).init();

	// And emit any warnings
	for warning in warnings {
		tracing::warn!(target: "zbuild_log", "{warning}");
	}
}
