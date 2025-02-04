//! Expander

// Imports
use {
	crate::{
		rules::{Command, DepItem, Exec, Expr, ExprCmpt, ExprOp, OutItem, Pattern, Rule, Target},
		util::ArcStr,
		AppError,
	},
	smallvec::SmallVec,
	std::{collections::HashMap, mem, path::PathBuf},
	zutil_app_error::{app_error, AllErrs, Context},
};

/// Expander
#[derive(Debug)]
pub struct Expander {}

#[expect(clippy::unused_self, reason = "Currently expander doesn't do anything")]
impl Expander {
	/// Creates a new expander
	pub const fn new() -> Self {
		Self {}
	}

	/// Expands an expression to it's components
	pub fn expand_expr<T>(&self, expr: &Expr, visitor: &Visitor<'_>) -> Result<T, AppError>
	where
		T: TryFromExpr,
	{
		// First convert everything into an expression
		let expr = expr
			.cmpts
			.iter()
			.try_fold::<_, _, Result<_, _>>(Expr::new(), |mut expr, cmpt| {
				match cmpt {
					// If it's a string, we keep it
					ExprCmpt::String(s) => expr.push_str(s),

					// If it's a pattern, we visit it
					ExprCmpt::Ident { name, ops } => match visitor.visit_ident(name) {
						// If expanded, check if we need to apply any operations
						FlowControl::ExpandTo(expand_expr) => match ops.is_empty() {
							// If not, just recursively expand it
							true => expr.extend(self.expand_expr::<Expr>(&expand_expr, visitor)?.cmpts),

							// Else expand it to a string, then apply all operations
							// Note: We expand to string even if we don't *need* to to ensure the user doesn't
							//       add a dependency at some point, which needs a string output and suddenly
							//       we can't resolve the operations.
							false => {
								// Expand
								let value = self.expand_expr::<ArcStr>(&expand_expr, visitor)?;

								// Then apply all
								let value = ops.iter().try_fold(value, |mut value, &op| {
									value
										.with_mut(|s| self.expand_expr_op(op, s))
										.with_context(|| format!("Unable to apply expression operator `{op}`"))?;

									Ok::<_, AppError>(value)
								})?;

								expr.push_str(&value);
							},
						},

						// Else keep on Keep and error on Error
						FlowControl::Keep => expr.push(cmpt),
						FlowControl::Error => zutil_app_error::bail!("Unknown expression {name:?}"),
					},
				};

				Ok::<_, AppError>(expr)
			})?;

		// Then try to parse from the expression
		T::try_from_expr(expr)
	}

	/// Expands an expression operation on the value of that expression
	fn expand_expr_op(&self, op: ExprOp, value: &mut String) -> Result<(), AppError> {
		match op {
			ExprOp::DirName => {
				// Get the path and try to pop the last segment
				let mut path = PathBuf::from(mem::take(value));
				zutil_app_error::ensure!(path.pop(), "Path had no parent directory {path:?}");

				// Then convert it back to a string
				// Note: This should technically never fail, since the path was originally
				//       utf-8
				*value = path
					.into_os_string()
					.into_string()
					.expect("utf-8 path was no longer utf-8 after getting dir-name");
			},
		};

		Ok(())
	}

	/// Expands a rule of all it's aliases and patterns
	pub fn expand_rule<T>(&self, rule: &Rule<Expr>, visitor: &Visitor<'_>) -> Result<Rule<T>, AppError>
	where
		T: TryFromExpr + Ord,
	{
		let aliases = rule
			.aliases
			.iter()
			.map(|(name, expr)| Ok((name.clone(), self.expand_expr(expr, visitor)?)))
			.collect::<AllErrs<_, _>>()?;

		let output = rule
			.output
			.iter()
			.map(|item: &OutItem<Expr>| match *item {
				OutItem::File { ref file, is_deps_file } => Ok::<_, AppError>(OutItem::File {
					file: self.expand_expr(file, visitor)?,
					is_deps_file,
				}),
			})
			.collect::<AllErrs<_, _>>()?;

		let deps = rule
			.deps
			.iter()
			.map(|item: &DepItem<Expr>| match *item {
				DepItem::File {
					ref file,
					is_optional,
					is_static,
					is_deps_file,
				} => Ok::<_, AppError>(DepItem::File {
					file: self.expand_expr(file, visitor)?,
					is_optional,
					is_static,
					is_deps_file,
				}),
			})
			.collect::<AllErrs<_, _>>()?;

		let exec = Exec {
			cmds: rule
				.exec
				.cmds
				.iter()
				.map(|cmd| self.expand_cmd(cmd, visitor))
				.collect::<AllErrs<_, _>>()?,
		};

		Ok(Rule {
			name: rule.name.clone(),
			aliases,
			pats: rule.pats.clone(),
			output,
			deps,
			exec,
		})
	}

	/// Expands a command
	pub fn expand_cmd<T>(&self, cmd: &Command<Expr>, visitor: &Visitor<'_>) -> Result<Command<T>, AppError>
	where
		T: TryFromExpr,
	{
		Ok(Command {
			cwd:  cmd.cwd.as_ref().map(|cwd| self.expand_expr(cwd, visitor)).transpose()?,
			args: cmd
				.args
				.iter()
				.map(|arg| self.expand_expr(arg, visitor))
				.collect::<AllErrs<_, _>>()?,
		})
	}

	/// Expands a target expression
	pub fn expand_target<T>(&self, target: &Target<Expr>, visitor: &Visitor<'_>) -> Result<Target<T>, AppError>
	where
		T: TryFromExpr,
	{
		let target = match *target {
			Target::File { ref file, is_static } => Target::File {
				file: self
					.expand_expr(file, visitor)
					.with_context(|| format!("Unable to expand expression {file}"))?,
				is_static,
			},

			Target::Rule { ref rule, ref pats } => {
				let pats = pats
					.iter()
					.map(|(pat, expr)| {
						Ok((
							pat.clone(),
							self.expand_expr(expr, visitor)
								.with_context(|| format!("Unable to expand expression {expr}"))?,
						))
					})
					.collect::<AllErrs<_, _>>()?;
				Target::Rule {
					rule: self
						.expand_expr(rule, visitor)
						.with_context(|| format!("Unable to expand expression {rule}"))?,
					pats,
				}
			},
		};

		Ok(target)
	}
}

/// Flow control for [`Expander::expand_expr`]
#[derive(Clone, Copy, Debug)]
pub enum FlowControl<T> {
	/// Expand to
	ExpandTo(T),

	/// Keep
	Keep,

	/// Error
	Error,
}

impl<T> FlowControl<T> {
	/// Converts a `&FlowControl<T>` to `FlowControl<&T>`
	pub const fn _as_ref(&self) -> FlowControl<&T> {
		match self {
			Self::ExpandTo(value) => FlowControl::ExpandTo(value),
			Self::Keep => FlowControl::Keep,
			Self::Error => FlowControl::Error,
		}
	}
}

/// Trait for converting from an expression
pub trait TryFromExpr: Sized {
	fn try_from_expr(expr: Expr) -> Result<Self, AppError>;
}

impl TryFromExpr for Expr {
	fn try_from_expr(expr: Expr) -> Result<Self, AppError> {
		Ok(expr)
	}
}

impl TryFromExpr for ArcStr {
	fn try_from_expr(expr: Expr) -> Result<Self, AppError> {
		expr.try_into_string().map_err(|expr| {
			app_error!(
				"Expression had unresolved aliases or patterns: {expr} ({:?})",
				expr.cmpts.iter().map(ExprCmpt::to_string).collect::<Vec<_>>()
			)
		})
	}
}

/// Visitor for [`Expander`]
#[derive(Clone, Debug)]
pub struct Visitor<'a> {
	/// All aliases, in order to check
	aliases: SmallVec<[&'a HashMap<ArcStr, Expr>; 2]>,

	/// All unresolved patterns, in order to check
	unresolved_pats: SmallVec<[&'a HashMap<ArcStr, Pattern>; 2]>,

	/// All resolved patterns
	resolved_pats: SmallVec<[(ArcStr, ArcStr); 1]>,
}

impl<'a> Visitor<'a> {
	/// Creates a new visitor with aliases and patterns
	pub fn new<A, UP, RP>(aliases: A, unresolved_pats: UP, resolved_pats: RP) -> Self
	where
		A: IntoIterator<Item = &'a HashMap<ArcStr, Expr>>,
		UP: IntoIterator<Item = &'a HashMap<ArcStr, Pattern>>,
		RP: IntoIterator<Item = SmallVec<[(ArcStr, ArcStr); 1]>>,
	{
		Self {
			aliases:         aliases.into_iter().collect(),
			unresolved_pats: unresolved_pats.into_iter().collect(),
			resolved_pats:   resolved_pats.into_iter().flatten().collect(),
		}
	}

	/// Visits an identifier
	fn visit_ident(&self, name: &str) -> FlowControl<Expr> {
		for (pat_name, pat) in &self.resolved_pats {
			if name == &**pat_name {
				return FlowControl::ExpandTo(Expr::string(pat.clone()));
			}
		}

		for pats in &self.unresolved_pats {
			if pats.contains_key(name) {
				return FlowControl::Keep;
			}
		}

		for aliases in &self.aliases {
			if let Some(alias) = aliases.get(name) {
				return FlowControl::ExpandTo(alias.clone());
			}
		}

		FlowControl::Error
	}
}
