//! Pre-initialization

// TODO: Use a proper solution that initializes a temporary subscriber and keeps all the records and events.

// Imports
use std::sync::{
	atomic::{self, AtomicBool},
	Mutex,
};

/// If logging as already initialized
static IS_INIT: AtomicBool = AtomicBool::new(false);

/// Creates the pre-init functions for a level
macro pre_init_fns(
	$(
		$NAME:ident, $log:ident, $take:ident;
	)*
) {
	$(
		static $NAME: Mutex<Vec<String>> = Mutex::new(Vec::new());

		/// Emits a log
		#[expect(clippy::allow_attributes, reason = "This is auto-generated, and only applies sometimes")]
		#[allow(dead_code, reason = "All levels are auto-generated, but some may not be logged")]
		pub fn $log(message: impl Into<String>) {
			let message = message.into();
			match IS_INIT.load(atomic::Ordering::Acquire) {
				true => {
					tracing::warn!(?message, "Using pre-initialization logging after initialization");
					tracing::$log!("{message}")
				},
				false => $NAME.lock().expect("Poisoned").push(message),
			}
		}

		/// Retrieves all logs
		pub(super) fn $take() -> Vec<String> {
			IS_INIT.store(true, atomic::Ordering::Release);
			$NAME.lock().expect("Poisoned").drain(..).collect()
		}
	)*
}

pre_init_fns! {
	TRACES  , trace, take_traces  ;
	DEBUGS  , debug, take_debugs  ;
	WARNINGS, warn , take_warnings;
}
