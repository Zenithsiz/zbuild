//! Logger

// Imports
use {
	std::{
		env::{self, VarError},
		fs,
		path::Path,
	},
	tracing::metadata::LevelFilter,
	tracing_subscriber::{prelude::*, EnvFilter},
};

/// Initializes the logger
pub fn init(log_file: Option<&Path>) {
	// Warnings to emit after configuring the logger
	let mut warnings = vec![];

	// Create the terminal layer
	let term_use_colors = self::colors_enabled(&mut warnings);
	let term_layer = tracing_subscriber::fmt::layer().with_ansi(term_use_colors).with_filter(
		EnvFilter::builder()
			.with_default_directive(LevelFilter::INFO.into())
			.from_env_lossy(),
	);

	// Create the file layer, if requested
	let file_layer = log_file.and_then(|log_file| {
		// Try to create the file
		let file = match fs::File::create(log_file) {
			Ok(file) => file,
			Err(err) => {
				warnings.push(format!("Unable to create log file: {err}"));
				return None;
			},
		};

		// Then create the layer
		let layer = tracing_subscriber::fmt::layer()
			.with_writer(file)
			.with_ansi(false)
			.with_filter(
				EnvFilter::builder()
					.with_default_directive(LevelFilter::DEBUG.into())
					.with_env_var("RUST_LOG_FILE")
					.from_env_lossy(),
			);

		Some(layer)
	});

	// Finally initialize
	tracing_subscriber::registry().with(term_layer).with(file_layer).init();
	tracing::debug!(?log_file, ?term_use_colors, "Initialized logging");

	// And emit any warnings
	for warning in warnings {
		tracing::warn!("{warning}");
	}
}

/// Returns whether to colors should be enabled for the terminal layer.
fn colors_enabled(warnings: &mut Vec<String>) -> bool {
	match env::var("RUST_LOG_COLOR").map(|var| var.to_lowercase()).as_deref() {
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
	}
}
