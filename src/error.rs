//! Errors

// Imports
use {
	crate::rules::{AliasOp, Command, Expr, Target},
	itertools::Itertools,
	std::{fmt, io, path::PathBuf, process::ExitStatusError, sync::Arc},
};

/// App error
///
/// Error that will be bubbled up to main when a fatal error occurs
// TODO: Not use debug output sometimes?
#[derive(Debug, thiserror::Error)]
#[non_exhaustive]
pub enum AppError {
	/// Shared
	// TODO: Is this a good idea? Should we just use `Arc<AppError>` here relevant?
	#[error(transparent)]
	Shared(Arc<Self>),

	/// Other
	// TODO: Removes usages of this, it's for quick prototyping
	#[error(transparent)]
	Other(anyhow::Error),

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

	/// Read file
	#[error("Unable to read file {file_path:?}")]
	ReadFile {
		/// File we failed to read
		file_path: PathBuf,

		/// Underlying error
		#[source]
		err: io::Error,
	},

	/// Read file metadata
	#[error("Unable to read file metadata {file_path:?}")]
	ReadFileMetadata {
		/// File we failed to read metadata of
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

	/// Missing file
	#[error("Missing file {file_path:?} and no rule to build it found")]
	MissingFile {
		/// File that is missing
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

	/// Spawn command
	#[error("Unable to spawn {cmd_fmt}")]
	SpawnCommand {
		/// Command formatted
		cmd_fmt: String,

		/// Underlying error
		#[source]
		err: io::Error,
	},

	/// Wait for command
	#[error("Unable to wait for {cmd_fmt}")]
	WaitCommand {
		/// Command formatted
		cmd_fmt: String,

		/// Underlying error
		#[source]
		err: io::Error,
	},

	/// Command failed
	#[error("Command failed {cmd_fmt}")]
	CommandFailed {
		/// Command formatted
		cmd_fmt: String,

		/// Underlying error
		#[source]
		err: ExitStatusError,
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

	/// Build rule
	#[error("Unable to build rule {rule_name}")]
	BuildRule {
		/// Rule name
		rule_name: String,

		/// Underlying error
		#[source]
		err: Box<Self>,
	},

	/// Build dependency file
	#[error("Unable to build dependency file {dep_file:?}")]
	BuildDepFile {
		/// Dependency file
		dep_file: PathBuf,

		/// Underlying error
		#[source]
		err: Box<Self>,
	},

	/// Expand rule
	#[error("Unable to expand rule {rule_name}")]
	ExpandRule {
		/// Rule name
		rule_name: String,

		/// Underlying error
		#[source]
		err: Box<Self>,
	},

	/// Expand target
	#[error("Unable to expand target {target_fmt}")]
	ExpandTarget {
		/// Formatted target
		target_fmt: String,

		/// Underlying error
		#[source]
		err: Box<Self>,
	},

	/// Expand expression
	#[error("Unable to expand expression {expr_fmt}")]
	ExpandExpr {
		/// Formatted expression
		expr_fmt: String,

		/// Underlying error
		#[source]
		err: Box<Self>,
	},

	/// Unknown rule
	#[error("Unknown rule {rule_name}")]
	UnknownRule {
		/// Rule name
		rule_name: String,
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

	/// Match expression had 2 or moore patterns
	#[error("Match expression had 2 or more patterns: {expr_fmt} ({expr_cmpts_fmt:?})")]
	MatchExprTooManyPats {
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

	/// Dependency file missing `:`
	#[error("Dependency file {dep_file_path:?} was missing a `:`")]
	DepFileMissingColon {
		/// Dep file path
		dep_file_path: PathBuf,
	},

	/// Dependency file missing rule name
	#[error("Dependency file {dep_file_path:?} is missing the rule name {rule_name}, found {dep_output}")]
	DepFileMissingRuleName {
		/// Dep file path
		dep_file_path: PathBuf,

		/// Rule name
		rule_name: String,

		/// Dependency file output
		dep_output: String,
	},

	/// Dependency file missing rule name
	#[error("Dependency file {dep_file_path:?} is missing any output of {rule_outputs:?}, found {dep_output}")]
	DepFileMissingOutputs {
		/// Dep file path
		dep_file_path: PathBuf,

		/// Rule outputs
		rule_outputs: Vec<String>,

		/// Dependency
		dep_output: String,
	},

	/// Rule executable was empty
	#[error("Rule {rule_name} executable as empty")]
	RuleExecEmpty {
		/// Rule name
		rule_name: String,
	},
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

	pub fn read_file(file_path: impl Into<PathBuf>) -> impl FnOnce(io::Error) -> Self {
		move |err| Self::ReadFile {
			file_path: file_path.into(),
			err,
		}
	}

	pub fn read_file_metadata(file_path: impl Into<PathBuf>) -> impl FnOnce(io::Error) -> Self {
		move |err| Self::ReadFileMetadata {
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

	pub fn missing_file(file_path: impl Into<PathBuf>) -> impl FnOnce(io::Error) -> Self {
		move |err| Self::MissingFile {
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

	pub fn spawn_command<T: fmt::Display>(cmd: &Command<T>) -> impl FnOnce(io::Error) -> Self + '_ {
		move |err| Self::SpawnCommand {
			cmd_fmt: cmd.args.iter().join(" "),
			err,
		}
	}

	pub fn wait_command<T: fmt::Display>(cmd: &Command<T>) -> impl FnOnce(io::Error) -> Self + '_ {
		move |err| Self::WaitCommand {
			cmd_fmt: cmd.args.iter().join(" "),
			err,
		}
	}

	pub fn command_failed<T: fmt::Display>(cmd: &Command<T>) -> impl FnOnce(ExitStatusError) -> Self + '_ {
		move |err| Self::CommandFailed {
			cmd_fmt: cmd.args.iter().join(" "),
			err,
		}
	}

	pub fn get_default_jobs() -> impl FnOnce(io::Error) -> Self {
		move |err| Self::GetDefaultJobs { err }
	}

	pub fn alias_op(op: impl Into<AliasOp>) -> impl FnOnce(Self) -> Self {
		move |err| Self::AliasOp {
			op:  op.into(),
			err: Box::new(err),
		}
	}

	pub fn build_target<T: fmt::Display>(target: &Target<T>) -> impl FnOnce(Self) -> Self + '_ {
		move |err| Self::BuildTarget {
			target_fmt: target.to_string(),
			err:        Box::new(err),
		}
	}

	pub fn build_rule(rule_name: impl Into<String>) -> impl FnOnce(Self) -> Self {
		move |err| Self::BuildRule {
			rule_name: rule_name.into(),
			err:       Box::new(err),
		}
	}

	pub fn build_dep_file(dep_file: impl Into<PathBuf>) -> impl FnOnce(Self) -> Self {
		move |err| Self::BuildDepFile {
			dep_file: dep_file.into(),
			err:      Box::new(err),
		}
	}

	pub fn expand_rule(rule_name: impl Into<String>) -> impl FnOnce(Self) -> Self {
		move |err| Self::ExpandRule {
			rule_name: rule_name.into(),
			err:       Box::new(err),
		}
	}

	pub fn expand_target<T: fmt::Display>(target: &Target<T>) -> impl FnOnce(Self) -> Self + '_ {
		move |err| Self::ExpandTarget {
			target_fmt: target.to_string(),
			err:        Box::new(err),
		}
	}

	pub fn expand_expr(expr: &Expr) -> impl FnOnce(Self) -> Self + '_ {
		move |err| Self::ExpandExpr {
			expr_fmt: expr.to_string(),
			err:      Box::new(err),
		}
	}
}
