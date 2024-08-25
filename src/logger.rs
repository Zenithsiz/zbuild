//! Logger

// TODO: Parse the environment non-lossy first, then use a default.

// Modules
mod pre_init;

// Imports
use {
	anyhow::Context,
	std::{
		env::{self, VarError},
		fs,
		io::{self, IsTerminal},
		path::Path,
		sync::Mutex,
	},
	tracing::metadata::LevelFilter,
	tracing_subscriber::{prelude::*, EnvFilter, Registry},
};

/// Initializes the logger
///
/// Logs to `stderr`, and to `{log_file}`.
pub fn init(log_path: Option<&Path>) {
	// Create the terminal layer
	let term_layer = self::term_layer();

	// Create the file layer
	let file_layer = log_path.and_then(|log_path| match self::file_layer(log_path) {
		Ok(layer) => Some(layer),
		Err(err) => {
			pre_init::warn(format!("Unable to create file logging layer: {err:?}"));
			None
		},
	});

	// Create the console layer
	#[cfg(feature = "tokio-console")]
	let (console_layer, console_server) = console_subscriber::ConsoleLayer::builder().with_default_env().build();

	// Create a registry with all the layers
	let registry = Registry::default().with(term_layer).with(file_layer);

	#[cfg(feature = "tokio-console")]
	let registry = registry.with(console_layer);

	// Then initialize it
	registry.init();
	tracing::debug!(?log_path, "Initialized logging");

	// And emit all pre-init warnings
	for message in pre_init::take_traces() {
		tracing::trace!("{message}");
	}
	for message in pre_init::take_debugs() {
		tracing::debug!("{message}");
	}
	for message in pre_init::take_warnings() {
		tracing::warn!("{message}");
	}

	// Finally spawn the console server
	#[cfg(feature = "tokio-console")]
	{
		use std::mem;

		let console_serve = tokio::spawn(async move {
			if let Err(err) = console_server.serve().await {
				tracing::warn!(?err, "Unable to spawn tokio console server");
			}
		});
		mem::drop(console_serve);
	}
}

/// Creates the terminal layer
fn term_layer<S>() -> impl tracing_subscriber::Layer<S>
where
	S: tracing::Subscriber + for<'span> tracing_subscriber::registry::LookupSpan<'span> + 'static,
{
	let enable_colors = self::term_enable_colors();
	pre_init::debug(format!("Stderr logging colors: {enable_colors}"));

	let env = env::var("RUST_LOG").unwrap_or_else(|_| "info".to_owned());
	pre_init::debug(format!("Stderr logging filter: {env}"));

	tracing_subscriber::fmt::layer()
		.with_ansi(enable_colors)
		.with_writer(io::stderr)
		.with_filter(
			EnvFilter::builder()
				.with_default_directive(LevelFilter::INFO.into())
				.parse_lossy(env),
		)
}

/// Creates the file layer
fn file_layer<S>(log_path: &Path) -> Result<impl tracing_subscriber::Layer<S>, anyhow::Error>
where
	S: tracing::Subscriber + for<'span> tracing_subscriber::registry::LookupSpan<'span> + 'static,
{
	// Parse the environment
	let env = env::var("RUST_FILE_LOG").unwrap_or_else(|_| "debug".to_owned());
	pre_init::debug(format!("File logging filter: {env}"));

	// Try to create the log file parent, if it doesn't exist.
	let parent_dir = log_path.parent().context("Log path has no parent directory")?;
	fs::create_dir_all(parent_dir).context("Unable to create log path parent directory")?;

	// Then create the file
	let file = fs::File::create(log_path).context("Unable to create log file")?;

	// And finally the layer
	let layer = tracing_subscriber::fmt::layer()
		.with_writer(Mutex::new(file))
		.with_ansi(false)
		.with_filter(EnvFilter::builder().parse_lossy(env));

	Ok(layer)
}

/// Returns whether to colors should be enabled for the terminal layer.
fn term_enable_colors() -> bool {
	match env::var("RUST_LOG_COLOR").map(|var| var.to_lowercase()).as_deref() {
		// If it isn't present, check if we're in a terminal
		Err(VarError::NotPresent) => io::stderr().is_terminal(),

		// Else check user input
		Ok("1" | "yes" | "true") => true,
		Ok("0" | "no" | "false") => false,

		// On invalid input, warn and don't use colors
		Ok(env) => {
			pre_init::warn(format!(
				"Ignoring unknown `RUST_LOG_COLOR` value: {env:?}, expected `0`, `1`, `yes`, `no`, `true`, `false`"
			));
			false
		},
		Err(VarError::NotUnicode(err)) => {
			pre_init::warn(format!("Ignoring non-utf8 `RUST_LOG_COLOR`: {err:?}"));
			false
		},
	}
}
