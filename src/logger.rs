//! Logger

// Imports
use {
	itertools::Itertools,
	std::{
		collections::{HashMap, hash_map},
		env::{self, VarError},
		fs,
		io::{self, IsTerminal},
		path::Path,
		sync::Arc,
	},
	tracing::{Dispatch, Subscriber, dispatcher, metadata::LevelFilter, subscriber::DefaultGuard},
	tracing_subscriber::{EnvFilter, Layer, fmt::format::FmtSpan, prelude::*, registry::LookupSpan},
	zbuild::AppError,
};

/// Temporary subscriber type
type TempSubscriber = impl Subscriber + for<'a> LookupSpan<'a> + Send + Sync + 'static;

/// Logger
pub struct Logger {
	/// Dispatch
	dispatch: Dispatch,

	/// Guard
	_guard: DefaultGuard,
}

impl Logger {
	/// Creates a new stderr-only logger and initializes it as the temporary logger
	#[define_opaque(TempSubscriber)]
	pub fn init_temp() -> Self {
		// Initialize a barebones logger first to catch all logs
		// until our temporary subscriber is up and running.
		let barebones_logger =
			tracing_subscriber::fmt::Subscriber::builder().with_env_filter(EnvFilter::from_default_env());
		let barebones_logger_guard = dispatcher::set_default(&barebones_logger.into());
		tracing::debug!("Initialized barebones logger");

		// Create the initial registry
		let registry = tracing_subscriber::registry();

		// Add the terminal layer
		let term_layer = self::term_layer();
		let registry = registry.with(term_layer);

		// Add the tokio-console layer
		#[cfg(feature = "tokio-console")]
		let registry = registry.with(console_subscriber::spawn());

		// And stuff it into the dispatch
		let registry = Arc::new(registry);
		let dispatch = tracing::Dispatch::new::<Arc<TempSubscriber>>(registry);

		// Then initialize it temporarily
		drop(barebones_logger_guard);
		let guard = dispatcher::set_default(&dispatch);
		tracing::debug!("Initialized temporary stderr logger");

		// Initialize the `log` compatibility too
		tracing_log::LogTracer::builder()
			.init()
			.expect("Unable to initialize `log` compatibility layer");
		tracing::debug!("Initialized `log` compatibility layer");

		Self {
			dispatch,
			_guard: guard,
		}
	}

	/// Fully initializes the logger, potentially with a file to log into
	pub fn init_global(self, log_file: Option<&Path>) {
		// Create the file layer if we can
		let file_layer = log_file.and_then(self::file_layer);

		// Get the original registry we already created and add all layers
		// TODO: Retroactively prepend all logs into these new layers?
		let registry = self.into_temp_subscriber().with(file_layer);

		// And properly initialize
		dispatcher::set_global_default(registry.into()).expect("Unable to set global `tracing` logger");
		tracing::debug!(?log_file, "Initialized global logger");
	}

	/// Retrieves the temporary subscriber
	// TODO a way to get the registry back more easily than this absolute mess
	fn into_temp_subscriber(self) -> TempSubscriber {
		// Get and clone the Arc we initially gave as a subscriber
		let registry = self
			.dispatch
			.downcast_ref::<Arc<TempSubscriber>>()
			.expect("Dispatch had the wrong inner type");
		let registry = Arc::clone(registry);

		// Then drop any references to it (which are stored in `self`) and unwrap it
		drop(self);
		Arc::into_inner(registry).expect("Dispatch had multiple copies of subscriber")
	}
}

/// Creates the terminal layer
fn term_layer<S>() -> impl Layer<S>
where
	S: Subscriber + for<'a> LookupSpan<'a>,
{
	let use_colors = self::colors_enabled();
	let env = self::get_env_filters("RUST_LOG", "info");
	let layer = tracing_subscriber::fmt::layer()
		.with_span_events(FmtSpan::CLOSE)
		.with_ansi(use_colors);
	tracing::debug!("Using colors for terminal logging: {use_colors}");

	#[cfg(debug_assertions)]
	let layer = layer.with_file(true).with_line_number(true).with_thread_names(true);

	layer.with_filter(
		EnvFilter::builder()
			.with_default_directive(LevelFilter::INFO.into())
			.parse_lossy(env),
	)
}

/// Creates the file layer
fn file_layer<S>(log_file: &Path) -> Option<impl Layer<S>>
where
	S: Subscriber + for<'a> LookupSpan<'a>,
{
	// Try to create the file
	let file = match fs::File::create(log_file) {
		Ok(file) => {
			tracing::debug!("Created log file {log_file:?}");
			file
		},
		Err(err) => {
			let err = AppError::new(&err);
			tracing::warn!("Unable to create log file {log_file:?}: {}", err.pretty());
			return None;
		},
	};

	// Then create the layer
	let env = self::get_env_filters("RUST_FILE_LOG", "debug");
	let layer = tracing_subscriber::fmt::layer()
		.with_span_events(FmtSpan::CLOSE)
		.with_writer(file)
		.with_ansi(false)
		.with_filter(EnvFilter::builder().parse_lossy(env));

	Some(layer)
}

/// Returns whether to colors should be enabled for the terminal layer.
fn colors_enabled() -> bool {
	// If `NO_COLOR` is set to non-empty, we shouldn't use colors
	if env::var("NO_COLOR").is_ok_and(|var| !var.is_empty()) {
		return false;
	}

	// Otherwise, enable colors if we're not being piped
	io::stdout().is_terminal()
}

/// Returns the env filters of a variable.
///
/// Adds default filters, if not specified
#[must_use]
fn get_env_filters(env: &str, default: &str) -> String {
	// Default filters
	let default_filters = [(None, default)];

	// Get the current filters
	let env_var;
	let mut cur_filters = match env::var(env) {
		// Split filters by `,`, then src and level by `=`
		Ok(var) => {
			env_var = var;
			env_var
				.split(',')
				.map(|s| match s.split_once('=') {
					Some((src, level)) => (Some(src), level),
					None => (None, s),
				})
				.collect::<HashMap<_, _>>()
		},

		// If there were none, don't use any
		Err(err) => {
			if let VarError::NotUnicode(var) = err {
				tracing::warn!("Ignoring non-utf8 env variable {env:?}: {var:?}");
			}

			HashMap::new()
		},
	};

	// Add all default filters, if not specified
	for (src, level) in default_filters {
		if let hash_map::Entry::Vacant(entry) = cur_filters.entry(src) {
			let _: &mut &str = entry.insert(level);
		}
	}

	// Then re-create it
	let var = cur_filters
		.into_iter()
		.map(|(src, level)| match src {
			Some(src) => format!("{src}={level}"),
			None => level.to_owned(),
		})
		.join(",");
	tracing::trace!("Using {env}={var}");

	var
}
