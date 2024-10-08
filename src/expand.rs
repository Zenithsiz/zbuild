//! Expander

// Imports
use {
	crate::{
		error::{AppError, ResultMultiple},
		rules::{AliasOp, Command, DepItem, Exec, Expr, ExprCmpt, OutItem, Rule, Target},
		util::ArcStr,
	},
	indexmap::IndexMap,
	smallvec::SmallVec,
	std::{collections::BTreeMap, marker::PhantomData, mem, path::PathBuf, sync::Arc},
};

/// Expander
#[derive(Debug)]
pub struct Expander {
	/// Phantom for `'s`
	_phantom: PhantomData<&'static ()>,
}

#[expect(clippy::unused_self, reason = "Currently expander doesn't do anything")]
impl Expander {
	/// Creates a new expander
	pub const fn new() -> Self {
		Self { _phantom: PhantomData }
	}

	/// Expands an expression to it's components
	pub fn expand_expr<T>(&self, expr: &Expr, visitor: &Visitor) -> Result<T, AppError>
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
					// Note: We don't care about the operations on patterns, those are for matching
					ExprCmpt::Pattern(pat) => match visitor.visit_pat(&pat.name) {
						// If expanded, just replace it with a string
						FlowControl::ExpandTo(value) => expr.push_str(value),

						// Else keep on Keep and error on Error
						FlowControl::Keep => expr.push(cmpt),
						FlowControl::Error =>
							return Err(AppError::UnknownPattern {
								pattern_name: pat.name.to_string(),
							}),
					},

					// If it's an alias, we visit and then expand it
					ExprCmpt::Alias(alias) => match visitor.visit_alias(&alias.name) {
						// If expanded, check if we need to apply any operations
						FlowControl::ExpandTo(alias_expr) => match alias.ops.is_empty() {
							// If not, just recursively expand it
							true => expr.extend(self.expand_expr::<Expr>(alias_expr, visitor)?.cmpts),

							// Else expand it to a string, then apply all operations
							// Note: We expand to string even if we don't *need* to to ensure the user doesn't
							//       add a dependency at some point, which needs a string output and suddenly
							//       we can't resolve the operations.
							false => {
								// Expand
								let value = self.expand_expr::<ArcStr>(alias_expr, visitor)?;

								// Then apply all
								let value = alias.ops.iter().try_fold(value, |mut value, &op| {
									value
										.with_mut(|s| self.expand_alias_op(op, s))
										.map_err(AppError::alias_op(op))?;

									Ok(value)
								})?;

								expr.push_str(&value);
							},
						},

						// Else keep on Keep and error on Error
						FlowControl::Keep => expr.push(cmpt),
						FlowControl::Error =>
							return Err(AppError::UnknownAlias {
								alias_name: alias.name.to_string(),
							}),
					},
				};

				Ok(expr)
			})?;

		// Then try to parse from the expression
		T::try_from_expr(expr)
	}

	/// Expands an alias operation on the value of that alias
	fn expand_alias_op(&self, op: AliasOp, value: &mut String) -> Result<(), AppError> {
		match op {
			AliasOp::DirName => {
				// Get the path and try to pop the last segment
				let mut path = PathBuf::from(mem::take(value));
				if !path.pop() {
					return Err(AppError::PathParent { path });
				}

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
	pub fn expand_rule<T>(&self, rule: &Rule<Expr>, visitor: &Visitor) -> Result<Rule<T>, AppError>
	where
		T: TryFromExpr + Ord,
	{
		let aliases = rule
			.aliases
			.iter()
			.map(|(name, expr)| Ok((name.clone(), self.expand_expr(expr, visitor)?)))
			.collect::<ResultMultiple<_>>()?;

		let output = rule
			.output
			.iter()
			.map(|item: &OutItem<Expr>| match *item {
				OutItem::File { ref file, is_deps_file } => Ok::<_, AppError>(OutItem::File {
					file: self.expand_expr(file, visitor)?,
					is_deps_file,
				}),
			})
			.collect::<ResultMultiple<_>>()?;

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
				DepItem::Rule { ref name, ref pats } => {
					let pats = pats
						.iter()
						.map(|(pat, expr)| Ok((self.expand_expr(pat, visitor)?, self.expand_expr(expr, visitor)?)))
						.collect::<ResultMultiple<_>>()?;
					Ok::<_, AppError>(DepItem::Rule {
						name: self.expand_expr(name, visitor)?,
						pats: Arc::new(pats),
					})
				},
			})
			.collect::<ResultMultiple<_>>()?;

		let exec = Exec {
			cmds: rule
				.exec
				.cmds
				.iter()
				.map(|cmd| self.expand_cmd(cmd, visitor))
				.collect::<ResultMultiple<_>>()?,
		};

		Ok(Rule {
			name: rule.name.clone(),
			aliases: Arc::new(aliases),
			output,
			deps,
			exec,
		})
	}

	/// Expands a command
	pub fn expand_cmd<T>(&self, cmd: &Command<Expr>, visitor: &Visitor) -> Result<Command<T>, AppError>
	where
		T: TryFromExpr,
	{
		Ok(Command {
			cwd:  cmd.cwd.as_ref().map(|cwd| self.expand_expr(cwd, visitor)).transpose()?,
			args: cmd
				.args
				.iter()
				.map(|arg| self.expand_expr(arg, visitor))
				.collect::<ResultMultiple<_>>()?,
		})
	}

	/// Expands a target expression
	pub fn expand_target<T>(&self, target: &Target<Expr>, visitor: &Visitor) -> Result<Target<T>, AppError>
	where
		T: TryFromExpr,
	{
		let target = match *target {
			Target::File { ref file, is_static } => Target::File {
				file: self.expand_expr(file, visitor).map_err(AppError::expand_expr(file))?,
				is_static,
			},

			Target::Rule { ref rule, ref pats } => {
				let pats = pats
					.iter()
					.map(|(pat, expr)| {
						Ok((
							pat.clone(),
							self.expand_expr(expr, visitor).map_err(AppError::expand_expr(expr))?,
						))
					})
					.collect::<ResultMultiple<_>>()?;
				Target::Rule {
					rule: self.expand_expr(rule, visitor).map_err(AppError::expand_expr(rule))?,
					pats: Arc::new(pats),
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
	pub const fn as_ref(&self) -> FlowControl<&T> {
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
		expr.try_into_string().map_err(|expr| AppError::UnresolvedAliasOrPats {
			expr:       expr.to_string(),
			expr_cmpts: expr.cmpts.into_iter().map(|cmpt| cmpt.to_string()).collect(),
		})
	}
}

/// Visitor for [`Expander`]
#[derive(Clone, Debug)]
pub struct Visitor {
	/// All aliases, in order to check
	aliases: SmallVec<[Arc<IndexMap<ArcStr, Expr>>; 2]>,

	/// All patterns, in order to check
	pats: SmallVec<[Arc<BTreeMap<ArcStr, ArcStr>>; 1]>,

	/// Default alias action
	default_alias: FlowControl<Expr>,

	/// Default pattern action
	default_pat: FlowControl<ArcStr>,
}

impl Visitor {
	/// Creates a new visitor with aliases and patterns
	pub fn new<'a, A, P>(aliases: A, pats: P) -> Self
	where
		A: IntoIterator<Item = &'a Arc<IndexMap<ArcStr, Expr>>>,
		P: IntoIterator<Item = &'a Arc<BTreeMap<ArcStr, ArcStr>>>,
	{
		Self {
			aliases:       aliases.into_iter().map(Arc::clone).collect(),
			pats:          pats.into_iter().map(Arc::clone).collect(),
			default_alias: FlowControl::Error,
			default_pat:   FlowControl::Error,
		}
	}

	/// Creates a visitor from aliases
	pub fn from_aliases<'a, A>(aliases: A) -> Self
	where
		A: IntoIterator<Item = &'a Arc<IndexMap<ArcStr, Expr>>>,
	{
		Self::new(aliases, [])
	}

	/// Sets the default pattern
	pub fn with_default_pat(self, default_pat: FlowControl<ArcStr>) -> Self {
		Self { default_pat, ..self }
	}

	/// Visits an alias
	fn visit_alias(&self, alias_name: &str) -> FlowControl<&Expr> {
		for aliases in &self.aliases {
			if let Some(alias) = aliases.get(alias_name) {
				return FlowControl::ExpandTo(alias);
			}
		}

		self.default_alias.as_ref()
	}

	/// Visits a pattern
	fn visit_pat(&self, pat_name: &str) -> FlowControl<&ArcStr> {
		for pats in &self.pats {
			if let Some(pat) = pats.get(pat_name) {
				return FlowControl::ExpandTo(pat);
			}
		}

		self.default_pat.as_ref()
	}
}
