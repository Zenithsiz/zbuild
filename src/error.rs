//! Errors

// Imports
use {
	crate::rules::{AliasOp, Target},
	std::{fmt, io, path::PathBuf},
};

/// App error
///
/// Error that will be bubbled up to main when a fatal error occurs
// TODO: Add `Clone`
#[derive(Debug, thiserror::Error)]
pub enum AppError {
	/// Get current directory
	#[error("Unable to get current directory")]
	GetCurrentDir {
		/// Underlying error
		#[source]
		err: io::Error,
	},

	/// Set current directory
	#[error("Unable to set current directory to {dir_path:?}")]
	SetCurrentDir {
		/// Directory that we failed to set as current
		dir_path: PathBuf,

		/// Underlying error
		#[source]
		err: io::Error,
	},

	/// Open file
	#[error("Unable to open file {file_path:?}")]
	OpenFile {
		/// File we failed to open
		file_path: PathBuf,

		/// Underlying error
		#[source]
		err: io::Error,
	},

	/// Check if file exists
	#[error("Unable to check if file exists {file_path:?}")]
	CheckFileExists {
		/// File we failed to check
		file_path: PathBuf,

		/// Underlying error
		#[source]
		err: io::Error,
	},

	/// Parse yaml
	#[error("Unable to parse yaml file {yaml_path:?}")]
	ParseYaml {
		/// Yaml path
		yaml_path: PathBuf,

		/// Underlying error
		#[source]
		err: serde_yaml::Error,
	},

	/// Get default jobs
	#[error("Unable to query system for available parallelism for default number of jobs")]
	GetDefaultJobs {
		/// Underlying error
		#[source]
		err: io::Error,
	},

	/// Zbuild not found
	#[error(
		"No `zbuild.yaml` file found in current or parent directories.\nYou can use `--path {{zbuild-path}}` in order \
		 to specify the manifest's path"
	)]
	ZBuildNotFound,

	/// Path had no parent
	#[error("Path had no parent directory {path:?}")]
	PathParent {
		/// Path that had no parent
		path: PathBuf,
	},

	/// Build target
	#[error("Unable to build target {target_fmt}")]
	BuildTarget {
		/// Formatted target
		target_fmt: String,

		/// Underlying error
		#[source]
		err: Box<Self>,
	},

	/// Unknown alias
	#[error("Unknown alias {alias_name}")]
	UnknownAlias {
		/// Alias name
		alias_name: String,
	},

	/// Unknown pattern
	#[error("Unknown pattern {pattern_name}")]
	UnknownPattern {
		/// Pattern name
		pattern_name: String,
	},

	/// Unresolved alias or patterns
	#[error("Expression had unresolved alias or patterns: {expr_fmt} ({expr_cmpts_fmt:?})")]
	UnresolvedAliasOrPats {
		/// Formatted expression
		expr_fmt: String,

		/// Components
		expr_cmpts_fmt: Vec<String>,
	},

	/// Alias operation
	#[error("Unable to apply alias operation `{op}`")]
	AliasOp {
		/// Operation
		op: AliasOp,

		/// Underlying error
		#[source]
		err: Box<Self>,
	},

	/// Other error
	// TODO: Remove this once we're ported all errors
	#[error(transparent)]
	Other(#[from] anyhow::Error),
}

/// Error shortcuts
///
/// These are functions that return functions to pass to `.map_err` to
/// specify a certain error.
impl AppError {
	pub fn get_current_dir() -> impl FnOnce(io::Error) -> Self {
		move |err| Self::GetCurrentDir { err }
	}

	pub fn set_current_dir(dir_path: impl Into<PathBuf>) -> impl FnOnce(io::Error) -> Self {
		move |err| Self::SetCurrentDir {
			dir_path: dir_path.into(),
			err,
		}
	}

	pub fn open_file(file_path: impl Into<PathBuf>) -> impl FnOnce(io::Error) -> Self {
		move |err| Self::OpenFile {
			file_path: file_path.into(),
			err,
		}
	}

	pub fn check_file_exists(file_path: impl Into<PathBuf>) -> impl FnOnce(io::Error) -> Self {
		move |err| Self::CheckFileExists {
			file_path: file_path.into(),
			err,
		}
	}

	pub fn parse_yaml(yaml_path: impl Into<PathBuf>) -> impl FnOnce(serde_yaml::Error) -> Self {
		move |err| Self::ParseYaml {
			yaml_path: yaml_path.into(),
			err,
		}
	}

	pub fn get_default_jobs() -> impl FnOnce(io::Error) -> Self {
		move |err| Self::GetDefaultJobs { err }
	}

	pub fn alias_op(op: impl Into<AliasOp>) -> impl FnOnce(AppError) -> Self {
		move |err| Self::AliasOp {
			op:  op.into(),
			err: Box::new(err),
		}
	}

	pub fn build_target<T: fmt::Display>(target: &Target<T>) -> impl FnOnce(AppError) -> Self + '_ {
		move |err| Self::BuildTarget {
			target_fmt: target.to_string(),
			err:        Box::new(err),
		}
	}
}
