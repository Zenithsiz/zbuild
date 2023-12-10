//! Errors

// Imports
use {
	crate::rules::{self, AliasOp, Command, Expr, Target},
	itertools::{Itertools, Position as ItertoolsPos},
	std::{
		convert::Infallible,
		env,
		error::Error as StdError,
		fmt,
		io,
		ops::{ControlFlow, FromResidual, Try},
		path::PathBuf,
		process::{self, ExitStatusError, Termination},
		string::FromUtf8Error,
		vec,
	},
};

/// Generates the error enum
macro_rules! decl_error {
	(
		$(#[$meta:meta])*
		$Name:ident;
		$Multiple:ident($MultipleTy:ty);
		$Other:ident($OtherTy:ty);

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
			#[source($variant_source:expr)]
			#[fmt($($variant_fmt:tt)*)]
			$Variant:ident {
				$(
					$( #[$variant_field_meta:meta] )*
					$variant_field:ident: $VariantField:ty
				),*
				$(,)?
			},
		)*
	) => {
		$( #[ $meta ] )*
		#[derive(Debug)]
		#[non_exhaustive]
		pub enum $Name {
			/// Multiple
			$Multiple($MultipleTy),

			/// Other
			// TODO: Removes usages of this, it's for quick prototyping
			$Other($OtherTy),

			$(
				$( #[doc = $variant_doc] )*
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

			/// Returns an object that can be used for a pretty display of this error
			pub fn pretty(&self) -> PrettyDisplay<'_> {
				PrettyDisplay::new(self)
			}
		}

		impl StdError for AppError {
			fn source(&self) -> Option<&(dyn StdError + 'static)> {
				match self {
					// Note: We don't return any of the errors here, so that we can format
					//       it properly without duplicating errors.
					Self::$Multiple(_) => None,
					Self::$Other(source) => AsRef::<dyn StdError>::as_ref(source).source(),
					$(
						#[expect(clippy::allow_attributes, reason = "Auto-generated code")]
						#[allow(unused_variables, reason = "Auto-generated code")]
						Self::$Variant { $( $variant_field ),* } => $variant_source,
					)*
				}
			}
		}

		impl fmt::Display for AppError {
			fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
				// Display the main message
				match self {
					Self::$Multiple(errs) => write!(f, "Multiple errors ({})", errs.len()),
					Self::$Other(source) => source.fmt(f),
					$(
						#[expect(clippy::allow_attributes, reason = "Auto-generated code")]
						#[allow(unused_variables, reason = "Auto-generated code")]
						Self::$Variant { $( $variant_field ),* } => write!(f, $($variant_fmt)*),
					)*
				}
			}
		}
	}
}


decl_error! {
	/// Test
	AppError;
	Multiple(Vec<Self>);
	Other(anyhow::Error);

	/// Get current directory
	#[from_fn( fn get_current_dir(source: io::Error)() )]
	#[source(Some(source))]
	#[fmt("Unable to get current directory")]
	GetCurrentDir {
		/// Underlying error
		source: io::Error
	},

	/// Set current directory
	#[from_fn(
		fn set_current_dir<P: Into<PathBuf>>(source: io::Error)(
			dir: P => dir.into()
		)
	)]
	#[source(Some(source))]
	#[fmt("Unable to set current directory to {dir:?}")]
	SetCurrentDir {
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
	#[source(Some(source))]
	#[fmt("Unable to read file {file_path:?}")]
	ReadFile {
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
	#[source(Some(source))]
	#[fmt("Unable to read file metadata {file_path:?}")]
	ReadFileMetadata {
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
	#[source(Some(source))]
	#[fmt("Unable to get file modified time {file_path:?}")]
	GetFileModifiedTime {
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
	#[source(Some(source))]
	#[fmt("Unable to check if file exists {file_path:?}")]
	CheckFileExists {
		/// Underlying error
		source: io::Error,

		/// File we failed to check
		file_path: PathBuf,
	},

	/// Missing file
	#[from_fn(
		// TODO: For some reason, rustc thinks the following lint is
		//       unfulfilled, check why.
		//#[expect(dead_code, reason = "Not used yet")]
		fn missing_file<P: Into<PathBuf>>(source: io::Error)(
			file_path: P => file_path.into()
		)
	)]
	#[source(Some(source))]
	#[fmt("Missing file {file_path:?} and no rule to build it found")]
	MissingFile {
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
	#[source(Some(source))]
	#[fmt("Unable to parse yaml file {yaml_path:?}")]
	ParseYaml {
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
	#[source(Some(source))]
	#[fmt("Unable to spawn {cmd}")]
	SpawnCommand {
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
	#[source(Some(source))]
	#[fmt("Unable to wait for {cmd}")]
	WaitCommand {
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
	#[source(Some(source))]
	#[fmt("Command failed {cmd}")]
	CommandFailed {
		/// Underlying error
		source: ExitStatusError,

		/// Command formatted
		cmd: String,
	},

	/// Command output was non-utf8
	#[from_fn(
		fn command_output_non_utf8<T: fmt::Display>(source: FromUtf8Error)(
			cmd: &Command<T> => self::cmd_to_string(cmd)
		) + '_
	)]
	#[source(Some(source))]
	#[fmt("Command output was non-utf8 {cmd}")]
	CommandOutputNonUtf8 {
		/// Underlying error
		source: FromUtf8Error,

		/// Command formatted
		cmd: String,
	},

	/// Get default jobs
	#[from_fn( fn get_default_jobs(source: io::Error)() )]
	#[source(Some(source))]
	#[fmt("Unable to query system for available parallelism for default number of jobs")]
	GetDefaultJobs {
		/// Underlying error
		source: io::Error
	},

	/// Zbuild not found
	#[source(None)]
	#[fmt("No `zbuild.yaml` file found in current or parent directories.\nYou can use `--path {{zbuild-path}}` in order to specify the manifest's path")]
	ZBuildNotFound {},

	/// Path had no parent
	#[source(None)]
	#[fmt("Path had no parent directory {path:?}")]
	PathParent {
		/// Path that had no parent
		path: PathBuf,
	},

	/// Build target
	#[from_fn(
		fn build_target<'target, T: fmt::Display>(source: Self => Some(Box::new(source)))(
			target: &'target Target<'_, T> => target.to_string()
		) + 'target
	)]
	#[source(source.as_deref().map(|err: &AppError| <&dyn StdError>::from(err)))]
	#[fmt("Unable to build target {target}")]
	BuildTarget {
		/// Underlying error
		source: Option<Box<Self>>,

		/// Formatted target
		target: String,
	},

	/// Build rule
	#[from_fn(
		fn build_rule<S: Into<String>>(source: Self => Box::new(source))(
			rule_name: S => rule_name.into()
		)
	)]
	#[source(Some(&**source))]
	#[fmt("Unable to build rule {rule_name}")]
	BuildRule {
		/// Underlying error
		source: Box<Self>,

		/// Rule name
		rule_name: String,
	},

	/// Build dependencies file
	#[from_fn(
		fn build_deps_file<P: Into<PathBuf>>(source: Self => Box::new(source))(
			deps_file: P => deps_file.into()
		)
	)]
	#[source(Some(&**source))]
	#[fmt("Unable to build dependencies file {deps_file:?}")]
	BuildDepFile {
		/// Underlying error
		source: Box<Self>,

		/// Dependencies file
		deps_file: PathBuf,
	},

	/// Expand rule
	#[from_fn(
		fn expand_rule<T: Into<String>>(source: Self => Box::new(source))(
			rule_name: T => rule_name.into()
		)
	)]
	#[source(Some(&**source))]
	#[fmt("Unable to expand rule {rule_name}")]
	ExpandRule {
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
	#[source(Some(&**source))]
	#[fmt("Unable to expand target {target}")]
	ExpandTarget {
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
	#[source(Some(&**source))]
	#[fmt("Unable to expand expression {expr}")]
	ExpandExpr {
		/// Underlying error
		source: Box<Self>,

		/// Formatted expression
		expr: String,
	},

	/// Unknown rule
	#[source(None)]
	#[fmt("Unknown rule {rule_name}")]
	UnknownRule {
		/// Rule name
		rule_name: String,
	},

	/// Unknown alias
	#[source(None)]
	#[fmt("Unknown alias {alias_name}")]
	UnknownAlias {
		/// Alias name
		alias_name: String,
	},

	/// Unknown pattern
	#[source(None)]
	#[fmt("Unknown pattern {pattern_name}")]
	UnknownPattern {
		/// Pattern name
		pattern_name: String,
	},

	/// Unresolved alias or patterns
	#[source(None)]
	#[fmt("Expression had unresolved alias or patterns: {expr} ({expr_cmpts:?})")]
	UnresolvedAliasOrPats {
		/// Formatted expression
		expr: String,

		/// Components
		expr_cmpts: Vec<String>,
	},

	/// Match expression had 2 or moore patterns
	#[source(None)]
	#[fmt("Match expression had 2 or more patterns: {expr} ({expr_cmpts:?})")]
	MatchExprTooManyPats {
		/// Formatted expression
		expr: String,

		/// Components
		expr_cmpts: Vec<String>,
	},

	/// Alias operation
	#[from_fn( fn alias_op(source: Self => Box::new(source))(op: AliasOp) )]
	#[source(Some(&**source))]
	#[fmt("Unable to apply alias operation `{op}`")]
	AliasOp {
		/// Underlying error
		source: Box<Self>,

		/// Operation
		op: AliasOp,
	},

	/// Dependencies file missing `:`
	#[source(None)]
	#[fmt("Dependencies file {deps_file_path:?} was missing a `:`")]
	DepFileMissingColon {
		/// Dep file path
		deps_file_path: PathBuf,
	},

	/// Dependencies file missing rule name
	#[source(None)]
	#[fmt("Dependencies file {deps_file_path:?} is missing the rule name {rule_name}, found {dep_output}")]
	DepFileMissingRuleName {
		/// Dep file path
		deps_file_path: PathBuf,

		/// Rule name
		rule_name: String,

		/// Dependencies file output
		dep_output: String,
	},

	/// Dependencies file missing rule name
	#[source(None)]
	#[fmt("Dependencies file {deps_file_path:?} is missing any output of {rule_outputs:?}, found {dep_output}")]
	DepFileMissingOutputs {
		/// Dep file path
		deps_file_path: PathBuf,

		/// Rule outputs
		rule_outputs: Vec<String>,

		/// Dependency
		dep_output: String,
	},

	/// Rule executable was empty
	#[source(None)]
	#[fmt("Rule {rule_name} executable as empty")]
	RuleExecEmpty {
		/// Rule name
		rule_name: String,
	},

	/// Exit due to failed builds
	#[source(None)]
	#[fmt("Exiting with non-0 due to failed builds")]
	ExitDueToFailedBuilds {},

	/// Execution semaphore was closed
	#[source(None)]
	#[fmt("Execution semaphore was closed")]
	ExecSemaphoreClosed {},
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

/// Helper type to collect a `IntoIter<Item = Result<T, AppError>>`
/// into a `Result<C, AppError::Multiple>`.
#[derive(Debug)]
pub enum ResultMultiple<C> {
	Ok(C),
	Err(Vec<AppError>),
}

impl<C, T> FromIterator<Result<T, AppError>> for ResultMultiple<C>
where
	C: Default + Extend<T>,
{
	fn from_iter<I>(iter: I) -> Self
	where
		I: IntoIterator<Item = Result<T, AppError>>,
	{
		// TODO: If we get any errors, don't allocate memory for the rest of the values?
		let (values, errs) = iter.into_iter().partition_result::<C, Vec<_>, _, _>();
		match errs.is_empty() {
			true => Self::Ok(values),
			false => Self::Err(errs),
		}
	}
}

#[derive(Debug)]
pub struct ResultMultipleResidue(Vec<AppError>);

impl<C> Try for ResultMultiple<C> {
	type Output = C;
	type Residual = ResultMultipleResidue;

	fn from_output(output: Self::Output) -> Self {
		Self::Ok(output)
	}

	fn branch(self) -> ControlFlow<Self::Residual, Self::Output> {
		match self {
			Self::Ok(values) => ControlFlow::Continue(values),
			Self::Err(errs) => ControlFlow::Break(ResultMultipleResidue(errs)),
		}
	}
}

impl<T> FromResidual<ResultMultipleResidue> for ResultMultiple<T> {
	fn from_residual(residual: ResultMultipleResidue) -> Self {
		Self::Err(residual.0)
	}
}

impl<T> FromResidual<ResultMultipleResidue> for Result<T, AppError> {
	fn from_residual(residual: ResultMultipleResidue) -> Self {
		let err = match <[_; 1]>::try_from(residual.0) {
			Ok([err]) => err,
			Err(errs) => {
				assert!(!errs.is_empty(), "`ResultMultipleResidue` should hold at least 1 error");
				AppError::Multiple(errs)
			},
		};

		Err(err)
	}
}

/// Exit result
pub enum ExitResult {
	Ok,
	Err(AppError),
}

impl Termination for ExitResult {
	fn report(self) -> process::ExitCode {
		match self {
			Self::Ok => process::ExitCode::SUCCESS,
			Self::Err(err) => {
				eprintln!("Error: {}", err.pretty());
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

/// Pretty display for [`AppError`]
#[derive(Debug)]
pub struct PrettyDisplay<'a> {
	/// Root error
	root: &'a AppError,

	/// Whether we should show irrelevant errors.
	show_irrelevant: bool,
}

#[derive(PartialEq, Clone, Copy, Debug)]
enum Column {
	Line,
	Empty,
}

impl Column {
	/// Returns the string for this column
	const fn as_str(self) -> &'static str {
		match self {
			Self::Line => "│ ",
			Self::Empty => "  ",
		}
	}
}

impl<'a> PrettyDisplay<'a> {
	/// Creates a new pretty display
	pub(crate) fn new(root: &'a AppError) -> Self {
		// Get whether to show irrelevant errors from the environment
		let var = env::var("ZBUILD_SHOW_IRRELEVANT_ERRS");
		let show_irrelevant = match var {
			Ok(mut s) => {
				s.make_ascii_lowercase();
				matches!(s.as_str(), "1" | "y" | "yes" | "true")
			},
			Err(_) => false,
		};

		Self { root, show_irrelevant }
	}

	/// Sets if we should show irrelevant errors during formatting
	pub fn with_show_irrelevant(&mut self, show_irrelevant: bool) -> &mut Self {
		self.show_irrelevant = show_irrelevant;
		self
	}

	/// Formats a single error
	// Note: Always prints, even if irrelevant. Only `fmt_multiple` actually filters
	//       any of it's entries for being irrelevant.
	fn fmt_single(
		&self,
		f: &mut fmt::Formatter<'_>,
		err: &AppError,
		columns: &mut Vec<Column>,
		total_ignored_errs: &mut usize,
	) -> fmt::Result {
		// If it's multiple, display it as multiple
		if let AppError::Multiple(errs) = err {
			return self.fmt_multiple(f, errs, columns, total_ignored_errs);
		}

		// Else write the top-level error
		write!(f, "{err}")?;

		// Then, if there's a cause, write the rest
		if let Some(mut cur_source) = err.source() {
			let starting_columns = columns.len();
			loop {
				// Print the pre-amble
				f.pad("\n")?;
				for c in &*columns {
					f.pad(c.as_str())?;
				}
				f.pad("└─")?;
				columns.push(Column::Empty);

				// Then check if we got to a multiple.
				match cur_source.downcast_ref::<AppError>() {
					Some(AppError::Multiple(errs)) => {
						self.fmt_multiple(f, errs, columns, total_ignored_errs)?;
						break;
					},
					_ => write!(f, "{cur_source}",)?,
				}

				// And descend
				cur_source = match cur_source.source() {
					Some(source) => source,
					_ => break,
				};
			}
			let _: vec::Drain<'_, _> = columns.drain(starting_columns..);
		}

		Ok(())
	}

	/// Formats multiple errors
	fn fmt_multiple(
		&self,
		f: &mut fmt::Formatter<'_>,
		errs: &[AppError],
		columns: &mut Vec<Column>,
		total_ignored_errs: &mut usize,
	) -> fmt::Result {
		// Write the top-level error
		write!(f, "Multiple errors:")?;

		// For each error, write it
		let mut ignored_errs = 0;
		for (pos, err) in errs.iter().with_position() {
			// If this error is irrelevant, continue
			if !self.show_irrelevant && !self::err_contains_relevant(err) {
				ignored_errs += 1;
				continue;
			}

			f.pad("\n")?;
			for c in &*columns {
				f.pad(c.as_str())?;
			}

			// Note: We'll only print `└─` if we have no ignored errors, since if we do,
			//       we need that to print the final line showcasing how many we ignored
			match ignored_errs == 0 && matches!(pos, ItertoolsPos::Last | ItertoolsPos::Only) {
				true => {
					f.pad("└─")?;
					columns.push(Column::Empty);
				},
				false => {
					f.pad("├─")?;
					columns.push(Column::Line);
				},
			}

			self.fmt_single(f, err, columns, total_ignored_errs)?;
			let _: Option<_> = columns.pop();
		}

		if ignored_errs != 0 {
			*total_ignored_errs += ignored_errs;

			f.pad("\n")?;
			for c in &*columns {
				f.pad(c.as_str())?;
			}
			f.pad("└─")?;
			write!(f, "({ignored_errs} irrelevant errors)")?;
		}

		Ok(())
	}
}

impl fmt::Display for PrettyDisplay<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let mut columns = vec![];
		let mut total_ignored_errs = 0;
		self.fmt_single(f, self.root, &mut columns, &mut total_ignored_errs)?;
		assert_eq!(columns.len(), 0, "There should be no columns after formatting");

		if total_ignored_errs != 0 {
			f.pad("\n")?;
			write!(
				f,
				"Note: {total_ignored_errs} irrelevant errors were hidden, set `ZBUILD_SHOW_IRRELEVANT_ERRS=1` to \
				 show them"
			)?;
		}

		Ok(())
	}
}

/// Returns if this error contains any "relevant" errors.
///
/// In our case, the following cases are considered *irrelevant*:
/// - Is an [`AppError::BuildTarget`] with no source.
/// - Is an [`AppError::ExecSemaphoreClosed`].
fn err_contains_relevant(err: &AppError) -> bool {
	// If we're multiple errors, return if any of them are relevant
	if let AppError::Multiple(errs) = err {
		return errs.iter().any(self::err_contains_relevant);
	}

	// Else if the error itself is irrelevant, return
	if matches!(
		err,
		AppError::BuildTarget { source: None, .. } | AppError::ExecSemaphoreClosed {}
	) {
		return false;
	}

	// Else check the inner error, if any
	if let Some(source) = err.source() &&
		let Some(err) = source.downcast_ref::<AppError>()
	{
		return self::err_contains_relevant(err);
	}

	// Else, we're relevant
	true
}
