//! Errors

// Imports
use std::{
	convert::Infallible,
	ops::{FromResidual, Yeet},
	process::{self, Termination},
};

/// App error
pub type AppError = zutil_app_error::AppError<AppErrorData>;

/// App error data
#[derive(Clone, Copy, Debug)]
pub struct AppErrorData {
	/// Whether this error should be ignored when printing
	pub should_ignore: bool,
}

#[expect(clippy::derivable_impls, reason = "We want to be explicit")]
impl Default for AppErrorData {
	fn default() -> Self {
		Self { should_ignore: false }
	}
}

/// Exit result
#[derive(Debug)]
pub enum ExitResult {
	Ok,
	Err(AppError),
}

impl Termination for ExitResult {
	fn report(self) -> process::ExitCode {
		match self {
			Self::Ok => process::ExitCode::SUCCESS,
			Self::Err(err) => {
				eprintln!("Error: {}", self::pretty(&err));
				process::ExitCode::FAILURE
			},
		}
	}
}

impl FromResidual<Result<Infallible, AppError>> for ExitResult {
	fn from_residual(residual: Result<Infallible, AppError>) -> Self {
		match residual {
			Ok(never) => match never {},
			Err(err) => Self::Err(err),
		}
	}
}

impl FromResidual<Yeet<AppError>> for ExitResult {
	fn from_residual(Yeet(err): Yeet<AppError>) -> Self {
		Self::Err(err)
	}
}

/// Function to setup pretty printing
pub fn pretty(err: &AppError) -> zutil_app_error::PrettyDisplay<'_, AppErrorData> {
	err.pretty().with_ignore_err(|_, data| data.should_ignore)
}
