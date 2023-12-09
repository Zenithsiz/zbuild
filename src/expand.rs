//! Expander

// Imports
use {
	crate::{
		error::AppError,
		rules::{AliasOp, Command, CommandArg, DepItem, Exec, Expr, ExprCmpt, OutItem, Rule, Target},
		util::CowStr,
	},
	itertools::Itertools,
	std::{marker::PhantomData, path::PathBuf},
};

/// Expander
#[derive(Debug)]
pub struct Expander<'s> {
	/// Phantom for `'s`
	_phantom: PhantomData<&'s ()>,
}

#[expect(clippy::unused_self, reason = "Currently expander doesn't do anything")]
impl<'s> Expander<'s> {
	/// Creates a new expander
	pub const fn new() -> Self {
		Self { _phantom: PhantomData }
	}

	/// Expands an expression to it's components
	pub fn expand_expr(&self, expr: &Expr<'s>, visitor: &mut impl Visitor<'s>) -> Result<Expr<'s>, AppError> {
		// Go through all components
		let cmpts = expr
			.cmpts
			.iter()
			.try_fold::<_, _, Result<_, _>>(vec![], |mut cmpts, cmpt| {
				match cmpt {
					// If it's a string, we keep it
					ExprCmpt::String(_) => cmpts.push(cmpt.clone()),

					// If it's a pattern, we visit it
					// Note: We don't care about the operations on patterns, those are for matching
					ExprCmpt::Pattern(pat) => match visitor.visit_pat(pat.name) {
						// If expanded, just replace it with a string
						FlowControl::ExpandTo(value) => cmpts.push(ExprCmpt::String(value)),

						// Else keep on Keep and error on Error
						FlowControl::Keep => cmpts.push(cmpt.clone()),
						FlowControl::Error =>
							return Err(AppError::UnknownPattern {
								pattern_name: pat.name.to_owned(),
							}),
					},

					// If it's an alias, we visit and then expand it
					ExprCmpt::Alias(alias) => match visitor.visit_alias(alias.name) {
						// If expanded, check if we need to apply any operations
						FlowControl::ExpandTo(alias_expr) => match alias.ops.is_empty() {
							// If not, just recursively expand it
							true => cmpts.extend(self.expand_expr(&alias_expr, visitor)?.cmpts),

							// Else expand it to a string, then apply all operations
							// Note: We expand to string even if we don't *need* to to ensure the user doesn't
							//       add a dependency at some point, which needs a string output and suddenly
							//       we can't resolve the operations.
							false => {
								// Expand
								let value = self.expand_expr_string(&alias_expr, visitor)?;

								// Then apply all
								#[expect(clippy::shadow_unrelated, reason = "They are the same value")]
								let value = alias.ops.iter().try_fold(value, |value, &op| {
									let s = CowStr::into_owned(value);
									self.expand_alias_op(op, s)
										.map(CowStr::from)
										.map_err(AppError::alias_op(op))
								})?;

								cmpts.push(ExprCmpt::String(value));
							},
						},

						// Else keep on Keep and error on Error
						FlowControl::Keep => cmpts.push(cmpt.clone()),
						FlowControl::Error =>
							return Err(AppError::UnknownAlias {
								alias_name: alias.name.to_owned(),
							}),
					},
				};

				Ok(cmpts)
			})?;

		// Then merge neighboring strings
		// TODO: Do this in the above pass
		let cmpts = cmpts
			.into_iter()
			.coalesce(|prev, next| match (prev, next) {
				// Merge strings
				(ExprCmpt::String(prev), ExprCmpt::String(next)) => Ok(ExprCmpt::String(prev + next)),

				// Everything else leave
				(prev, next) => Err((prev, next)),
			})
			.collect();

		Ok(Expr { cmpts })
	}

	/// Expands an expression into a string
	pub fn expand_expr_string(&self, expr: &Expr<'s>, visitor: &mut impl Visitor<'s>) -> Result<CowStr<'s>, AppError> {
		let expr_cmpts = self.expand_expr(expr, visitor)?.cmpts.into_boxed_slice();
		let res = match Box::<[_; 0]>::try_from(expr_cmpts) {
			Ok(box []) => Ok("".into()),
			Err(cmpts) => match Box::<[_; 1]>::try_from(cmpts) {
				Ok(box [ExprCmpt::String(s)]) => Ok(s),
				Ok(box [cmpt]) => Err(vec![cmpt]),
				Err(cmpts) => Err(cmpts.into_vec()),
			},
		};

		res.map_err(|cmpts| AppError::UnresolvedAliasOrPats {
			expr_fmt:       expr.to_string(),
			expr_cmpts_fmt: cmpts.into_iter().map(|cmpt| cmpt.to_string()).collect(),
		})
	}

	/// Expands an alias operation on the value of that alias
	fn expand_alias_op(&self, op: AliasOp, value: String) -> Result<String, AppError> {
		let value = match op {
			AliasOp::DirName => {
				// Get the path and try to pop the last segment
				let mut path = PathBuf::from(value);
				if !path.pop() {
					return Err(AppError::PathParent { path });
				}

				// Then convert it back to a string
				// Note: This should technically never fail, since the path was originally
				//       utf-8
				path.into_os_string()
					.into_string()
					.expect("utf-8 path was no longer utf-8 after getting dir-name")
			},
		};

		Ok(value)
	}

	/// Expands a rule of all it's aliases and patterns
	pub fn expand_rule(
		&self,
		rule: &Rule<'s, Expr<'s>>,
		visitor: &mut impl Visitor<'s>,
	) -> Result<Rule<'s, CowStr<'s>>, AppError> {
		let aliases = rule
			.aliases
			.iter()
			.map(|(&name, expr)| Ok((name, self.expand_expr_string(expr, visitor)?)))
			.collect::<Result<_, AppError>>()?;

		let output = rule
			.output
			.iter()
			.map(|item: &OutItem<Expr<'_>>| match item {
				OutItem::File { file } => Ok::<_, AppError>(OutItem::File {
					file: self.expand_expr_string(file, visitor)?,
				}),
				OutItem::DepsFile { file } => Ok::<_, AppError>(OutItem::DepsFile {
					file: self.expand_expr_string(file, visitor)?,
				}),
			})
			.collect::<Result<_, _>>()?;

		let deps = rule
			.deps
			.iter()
			.map(|item: &DepItem<Expr<'_>>| match *item {
				DepItem::File {
					ref file,
					is_optional,
					is_static,
				} => Ok::<_, AppError>(DepItem::File {
					file: self.expand_expr_string(file, visitor)?,
					is_optional,
					is_static,
				}),
				DepItem::DepsFile {
					ref file,
					is_optional,
					is_static,
				} => Ok::<_, AppError>(DepItem::DepsFile {
					file: self.expand_expr_string(file, visitor)?,
					is_optional,
					is_static,
				}),
				DepItem::Rule { ref name, ref pats } => Ok::<_, AppError>(DepItem::Rule {
					name: self.expand_expr_string(name, visitor)?,
					pats: pats
						.iter()
						.map(|(pat, expr)| {
							Ok((
								self.expand_expr_string(pat, visitor)?,
								self.expand_expr_string(expr, visitor)?,
							))
						})
						.collect::<Result<_, AppError>>()?,
				}),
			})
			.collect::<Result<_, _>>()?;

		let exec = Exec {
			cmds: rule
				.exec
				.cmds
				.iter()
				.map(|cmd| self.expand_cmd(cmd, visitor))
				.collect::<Result<_, _>>()?,
		};

		Ok(Rule {
			name: rule.name,
			aliases,
			output,
			deps,
			exec,
		})
	}

	/// Expands a command
	pub fn expand_cmd(
		&self,
		cmd: &Command<Expr<'s>>,
		visitor: &mut impl Visitor<'s>,
	) -> Result<Command<CowStr<'s>>, AppError> {
		Ok(Command {
			cwd:  cmd
				.cwd
				.as_ref()
				.map(|cwd| self.expand_expr_string(cwd, visitor))
				.transpose()?,
			args: cmd
				.args
				.iter()
				.map(|arg| {
					let arg = match *arg {
						CommandArg::Expr(ref expr) => CommandArg::Expr(self.expand_expr_string(expr, visitor)?),
						CommandArg::Command { strip_on_fail, ref cmd } => CommandArg::Command {
							strip_on_fail,
							cmd: self.expand_cmd(cmd, visitor)?,
						},
					};
					Ok(arg)
				})
				.collect::<Result<_, _>>()?,
		})
	}

	/// Expands a target expression
	pub fn expand_target(
		&self,
		target: &Target<'s, Expr<'s>>,
		visitor: &mut impl Visitor<'s>,
	) -> Result<Target<'s, CowStr<'s>>, AppError> {
		let target = match *target {
			Target::File { ref file, is_static } => Target::File {
				file: self
					.expand_expr_string(file, visitor)
					.map_err(AppError::expand_expr(file))?,
				is_static,
			},

			Target::Rule { ref rule, ref pats } => Target::Rule {
				rule: self
					.expand_expr_string(rule, visitor)
					.map_err(AppError::expand_expr(rule))?,
				pats: pats
					.iter()
					.map(|(pat, expr)| {
						Ok((
							pat.clone(),
							self.expand_expr_string(expr, visitor)
								.map_err(AppError::expand_expr(expr))?,
						))
					})
					.collect::<Result<_, AppError>>()?,
			},
		};

		Ok(target)
	}
}

/// Flow control for [`Expander::expand_expr_string`]
pub enum FlowControl<T> {
	/// Expand to
	ExpandTo(T),

	/// Keep
	Keep,

	/// Error
	Error,
}

/// Visitor for [`Expander`]
pub trait Visitor<'s> {
	/// Visits an alias
	fn visit_alias(&mut self, alias_name: &str) -> FlowControl<Expr<'s>>;

	/// Visits a pattern
	fn visit_pat(&mut self, pat_name: &str) -> FlowControl<CowStr<'s>>;
}
