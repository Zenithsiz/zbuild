//! Errors

// Imports
use {
	crate::rules::{self, AliasOp, Command, Expr, Target},
	itertools::Itertools,
	std::{fmt, io, path::PathBuf, process::ExitStatusError, sync::Arc},
};

/// Generates the error enum
macro decl_error(
	$(#[$meta:meta])*
	$Name:ident;
	$Shared:ident;
	$Other:ident;

	$(
		$( #[doc = $variant_doc:expr] )*
		$(
			#[from_fn(
				// Function definition
				$(#[$variant_fn_meta:meta])*
				fn $variant_fn:ident

				// Generics
				$( <
					$( $VariantLifetimes:lifetime, )*
					$( $VariantGenerics:ident $(: $VariantBound:path )? ),* $(,)?
				> )?

				// Error
				(
					$variant_fn_err:ident: $VariantFnErr:ty $( => $variant_fn_err_expr:expr )?
				)

				// Args
				(
					$(
						$variant_fn_arg:ident: $VariantFnArg:ty $( => $variant_fn_arg_expr:expr )?
					),*
					$(,)?
				)

				// Return type lifetimes
				$(
					+ $VariantFnLifetime:lifetime
				)?
			)]
		)?
		$Variant:ident( $($variant_fmt:tt)* ) {
			$(
				$( #[$variant_field_meta:meta] )*
				$variant_field:ident: $VariantField:ty
			),*
			$(,)?
		},
	)*
) {
	$( #[ $meta ] )*
	#[derive(Debug, thiserror::Error)]
	#[non_exhaustive]
	pub enum $Name {
		/// Shared
		// TODO: Is this a good idea? Should we just use `Arc<AppError>` where relevant?
		#[error(transparent)]
		$Shared(Arc<Self>),

		/// Other
		// TODO: Removes usages of this, it's for quick prototyping
		#[error(transparent)]
		$Other(anyhow::Error),

		$(
			$( #[doc = $variant_doc] )*
			#[error( $($variant_fmt)* )]
			$Variant {
				$(
					$( #[$variant_field_meta] )*
					$variant_field: $VariantField,
				)*
			},
		)*
	}

	impl $Name {
		$(
			$(
				#[doc = concat!("Returns a function to create a [`Self::", stringify!($Variant) ,"`] error from it's inner error.")]
				$( #[$variant_fn_meta] )*
				pub fn $variant_fn

				// Generics
				$( <
					$( $VariantLifetimes, )*
					$( $VariantGenerics $(: $VariantBound )?, )*
				> )?

				// Arguments
				( $(
					$variant_fn_arg: $VariantFnArg,
				)* )

				// Return type
				-> impl FnOnce($VariantFnErr) -> Self $( + $VariantFnLifetime )?

				{
					move |$variant_fn_err| Self::$Variant {
						$variant_fn_err $(: $variant_fn_err_expr )?,
						$(
							$variant_fn_arg $(: $variant_fn_arg_expr )?,
						)*
					}
				}
			)?
		)*
	}
}

decl_error! {
	/// Test
	AppError;
	Shared;
	Other;

	/// Get current directory
	#[from_fn( fn get_current_dir(source: io::Error)() )]
	GetCurrentDir("Unable to get current directory") {
		/// Underlying error
		source: io::Error
	},

	/// Set current directory
	#[from_fn(
		fn set_current_dir<P: Into<PathBuf>>(source: io::Error)(
			dir: P => dir.into()
		)
	)]
	SetCurrentDir("Unable to set current directory to {dir:?}") {
		/// Underlying error
		source: io::Error,

		/// Path we tried to set as current directory
		dir: PathBuf
	},

	/// Read file
	#[from_fn(
		fn read_file<P: Into<PathBuf>>(source: io::Error)(
			file_path: P => file_path.into()
		)
	)]
	ReadFile("Unable to read file {file_path:?}") {
		/// Underlying error
		source: io::Error,

		/// File we failed to read
		file_path: PathBuf,
	},

	/// Read file metadata
	#[from_fn(
		fn read_file_metadata<P: Into<PathBuf>>(source: io::Error)(
			file_path: P => file_path.into()
		)
	)]
	ReadFileMetadata("Unable to read file metadata {file_path:?}") {
		/// Underlying error
		source: io::Error,

		/// File we failed to read metadata of
		file_path: PathBuf,
	},

	/// Get file modified time
	#[from_fn(
		fn get_file_modified_time<P: Into<PathBuf>>(source: io::Error)(
			file_path: P => file_path.into()
		)
	)]
	GetFileModifiedTime("Unable to get file modified time {file_path:?}") {
		/// Underlying error
		source: io::Error,

		/// File we failed to get the modified time of
		file_path: PathBuf,
	},

	/// Check if file exists
	#[from_fn(
		fn check_file_exists<P: Into<PathBuf>>(source: io::Error)(
			file_path: P => file_path.into()
		)
	)]
	CheckFileExists("Unable to check if file exists {file_path:?}") {
		/// Underlying error
		source: io::Error,

		/// File we failed to check
		file_path: PathBuf,
	},

	/// Missing file
	#[from_fn(
		#[expect(dead_code, reason = "Not used yet")]
		fn missing_file<P: Into<PathBuf>>(source: io::Error)(
			file_path: P => file_path.into()
		)
	)]
	MissingFile("Missing file {file_path:?} and no rule to build it found") {
		/// Underlying error
		source: io::Error,

		/// File that is missing
		file_path: PathBuf,
	},

	/// Parse yaml
	#[from_fn(
		fn parse_yaml<P: Into<PathBuf>>(source: serde_yaml::Error)(
			yaml_path: P => yaml_path.into()
		)
	)]
	ParseYaml("Unable to parse yaml file {yaml_path:?}") {
		/// Underlying error
		source: serde_yaml::Error,

		/// Yaml path
		yaml_path: PathBuf,
	},

	/// Spawn command
	#[from_fn(
		fn spawn_command<T: fmt::Display>(source: io::Error)(
			cmd: &Command<T> => self::cmd_to_string(cmd)
		) + '_
	)]
	SpawnCommand("Unable to spawn {cmd}") {
		/// Underlying error
		source: io::Error,

		/// Command formatted
		cmd: String,
	},

	/// Wait for command
	#[from_fn(
		fn wait_command<T: fmt::Display>(source: io::Error)(
			cmd: &Command<T> => self::cmd_to_string(cmd)
		) + '_
	)]
	WaitCommand("Unable to wait for {cmd}") {
		/// Underlying error
		source: io::Error,

		/// Command formatted
		cmd: String,
	},

	/// Command failed
	#[from_fn(
		fn command_failed<T: fmt::Display>(source: ExitStatusError)(
			cmd: &Command<T> => self::cmd_to_string(cmd)
		) + '_
	)]
	CommandFailed("Command failed {cmd}") {
		/// Underlying error
		source: ExitStatusError,

		/// Command formatted
		cmd: String,
	},

	/// Get default jobs
	#[from_fn( fn get_default_jobs(source: io::Error)() )]
	GetDefaultJobs("Unable to query system for available parallelism for default number of jobs") {
		/// Underlying error
		source: io::Error
	},

	/// Zbuild not found
	ZBuildNotFound(
		"No `zbuild.yaml` file found in current or parent directories.\n\
		You can use `--path {{zbuild-path}}` in order to specify the manifest's path"
	) {

	},

	/// Path had no parent
	PathParent("Path had no parent directory {path:?}") {
		/// Path that had no parent
		path: PathBuf,
	},

	/// Build target
	#[from_fn(
		fn build_target<'target, T: fmt::Display>(source: Self => Box::new(source))(
			target: &'target Target<'_, T> => target.to_string()
		) + 'target
	)]
	BuildTarget("Unable to build target {target}") {
		/// Underlying error
		source: Box<Self>,

		/// Formatted target
		target: String,
	},

	/// Build rule
	#[from_fn(
		fn build_rule<S: Into<String>>(source: Self => Box::new(source))(
			rule_name: S => rule_name.into()
		)
	)]
	BuildRule("Unable to build rule {rule_name}") {
		/// Underlying error
		source: Box<Self>,

		/// Rule name
		rule_name: String,
	},

	/// Build dependency file
	#[from_fn(
		fn build_dep_file<P: Into<PathBuf>>(source: Self => Box::new(source))(
			dep_file: P => dep_file.into()
		)
	)]
	BuildDepFile("Unable to build dependency file {dep_file:?}") {
		/// Underlying error
		source: Box<Self>,

		/// Dependency file
		dep_file: PathBuf,
	},

	/// Expand rule
	#[from_fn(
		fn expand_rule<T: Into<String>>(source: Self => Box::new(source))(
			rule_name: T => rule_name.into()
		)
	)]
	ExpandRule("Unable to expand rule {rule_name}") {
		/// Underlying error
		source: Box<Self>,

		/// Rule name
		rule_name: String,
	},

	/// Expand target
	#[from_fn(
		fn expand_target<'target, T: fmt::Display>(source: Self => Box::new(source))(
			target: &'target Target<'_, T> => target.to_string()
		) + 'target
	)]
	ExpandTarget("Unable to expand target {target}") {
		/// Underlying error
		source: Box<Self>,

		/// Formatted target
		target: String,
	},

	/// Expand expression
	#[from_fn(
		fn expand_expr<'expr,>(source: Self => Box::new(source))(
			expr: &'expr Expr<'_> => expr.to_string()
		) + 'expr
	)]
	ExpandExpr("Unable to expand expression {expr}") {
		/// Underlying error
		source: Box<Self>,

		/// Formatted expression
		expr: String,
	},

	/// Unknown rule
	UnknownRule("Unknown rule {rule_name}") {
		/// Rule name
		rule_name: String,
	},

	/// Unknown alias
	UnknownAlias("Unknown alias {alias_name}") {
		/// Alias name
		alias_name: String,
	},

	/// Unknown pattern
	UnknownPattern("Unknown pattern {pattern_name}") {
		/// Pattern name
		pattern_name: String,
	},

	/// Unresolved alias or patterns
	UnresolvedAliasOrPats("Expression had unresolved alias or patterns: {expr} ({expr_cmpts:?})") {
		/// Formatted expression
		expr: String,

		/// Components
		expr_cmpts: Vec<String>,
	},

	/// Match expression had 2 or moore patterns
	MatchExprTooManyPats("Match expression had 2 or more patterns: {expr} ({expr_cmpts:?})") {
		/// Formatted expression
		expr: String,

		/// Components
		expr_cmpts: Vec<String>,
	},

	/// Alias operation
	#[from_fn( fn alias_op(source: Self => Box::new(source))(op: AliasOp) )]
	AliasOp("Unable to apply alias operation `{op}`") {
		/// Underlying error
		source: Box<Self>,

		/// Operation
		op: AliasOp,
	},

	/// Dependency file missing `:`
	DepFileMissingColon("Dependency file {dep_file_path:?} was missing a `:`") {
		/// Dep file path
		dep_file_path: PathBuf,
	},

	/// Dependency file missing rule name
	DepFileMissingRuleName("Dependency file {dep_file_path:?} is missing the rule name {rule_name}, found {dep_output}") {
		/// Dep file path
		dep_file_path: PathBuf,

		/// Rule name
		rule_name: String,

		/// Dependency file output
		dep_output: String,
	},

	/// Dependency file missing rule name
	DepFileMissingOutputs("Dependency file {dep_file_path:?} is missing any output of {rule_outputs:?}, found {dep_output}") {
		/// Dep file path
		dep_file_path: PathBuf,

		/// Rule outputs
		rule_outputs: Vec<String>,

		/// Dependency
		dep_output: String,
	},

	/// Rule executable was empty
	RuleExecEmpty("Rule {rule_name} executable as empty") {
		/// Rule name
		rule_name: String,
	},
}

/// Helper function to format a `Command` for errors
fn cmd_to_string<T: fmt::Display>(cmd: &Command<T>) -> String {
	let inner = cmd
		.args
		.iter()
		.map(|arg| match arg {
			rules::CommandArg::Expr(expr) => format!("\"{expr}\""),
			rules::CommandArg::Command { cmd, .. } => self::cmd_to_string(cmd),
		})
		.join(" ");
	format!("[{inner}]")
}
