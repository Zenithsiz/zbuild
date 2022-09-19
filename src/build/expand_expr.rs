//! Expression expansion

// Imports
use {
	crate::rules::{AliasOp, Expr, ExprCmpt},
	anyhow::Context,
	itertools::Itertools,
	std::{collections::HashMap, path::PathBuf},
};

/// Expands an expression to it's components
pub fn expand_expr(expr: &Expr, visitor: &mut impl Visitor) -> Result<Vec<ExprCmpt>, anyhow::Error> {
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
				ExprCmpt::Pattern(pat) => match visitor.visit_pat(&pat.name) {
					// If expanded, just replace it with a string
					FlowControl::ExpandTo(value) => cmpts.push(ExprCmpt::String(value)),

					// Else keep on Keep and error on Error
					FlowControl::Keep => cmpts.push(cmpt.clone()),
					FlowControl::Error => anyhow::bail!("Unknown pattern {pat:?}"),
				},

				// If it's an alias, we visit and then expand it
				ExprCmpt::Alias(alias) => match visitor.visit_alias(&alias.name) {
					// If expanded, check if we need to apply any operations
					FlowControl::ExpandTo(expr) => match alias.ops.is_empty() {
						// If not, just recursively expand it
						true => cmpts.extend(self::expand_expr(&expr, visitor)?),

						// Else expand it to a string, then apply all operations
						// Note: We expand to string even if we don't *need* to to ensure the user doesn't
						//       add a dependency at some point, which needs a string output and suddenly
						//       we can't resolve the operations.
						false => {
							// Expand
							let value = self::expand_expr_string(&expr, visitor)?;

							// Then apply all
							let value = alias.ops.iter().try_fold(value, |value, &op| {
								self::expand_alias_op(op, value)
									.with_context(|| format!("Unable to apply alias operator {op:?}"))
							})?;

							cmpts.push(ExprCmpt::String(value))
						},
					},

					// Else keep on Keep and error on Error
					FlowControl::Keep => cmpts.push(cmpt.clone()),
					FlowControl::Error => anyhow::bail!("Unknown alias {alias:?}"),
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
			(ExprCmpt::String(prev), ExprCmpt::String(next)) => Ok(ExprCmpt::String(prev + &next)),

			// Everything else leave
			(prev, next) => Err((prev, next)),
		})
		.collect();

	Ok(cmpts)
}

/// Expands an expression into a string
pub fn expand_expr_string(expr: &Expr, visitor: &mut impl Visitor) -> Result<String, anyhow::Error> {
	let cmpts = self::expand_expr(expr, visitor)?.into_boxed_slice();

	let s = match Box::<[_; 0]>::try_from(cmpts) {
		Ok(box []) => "".to_owned(),
		Err(cmpts) => match Box::<[_; 1]>::try_from(cmpts) {
			Ok(box [ExprCmpt::String(s)]) => s,
			Ok(box [cmpt]) => anyhow::bail!("Expression had unresolved aliases or patterns: {cmpt:?}"),
			Err(cmpts) => anyhow::bail!("Expression had unresolved aliases or patterns: {cmpts:?}"),
		},
	};

	Ok(s)
}

/// Expands an alias operation on the value of that alias
fn expand_alias_op(op: AliasOp, value: String) -> Result<String, anyhow::Error> {
	let value = match op {
		AliasOp::DirName => {
			// Get the path and try to pop the last segment
			let mut path = PathBuf::from(value);
			anyhow::ensure!(path.pop(), "Path {path:?} had no directory name");

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

/// Flow control for [`expand_expr_string`]
pub enum FlowControl<T> {
	/// Expand to
	ExpandTo(T),

	/// Keep
	Keep,

	/// Error
	Error,
}

/// Visitor for [`expand_expr_string`]
pub trait Visitor {
	/// Visits an alias
	fn visit_alias(&mut self, alias_name: &str) -> FlowControl<Expr>;

	/// Visits a pattern
	fn visit_pat(&mut self, pat_name: &str) -> FlowControl<String>;
}

/// Visitor for global aliases.
///
/// Expands any defined aliases, errors on undefined ones
/// Errors any patterns.
#[derive(Clone, Copy, Debug)]
pub struct GlobalVisitor<'global> {
	/// Aliases
	aliases: &'global HashMap<String, Expr>,
}

impl<'global> GlobalVisitor<'global> {
	/// Creates a new global visitor
	pub fn new(aliases: &'global HashMap<String, Expr>) -> Self {
		Self { aliases }
	}
}

impl<'global> Visitor for GlobalVisitor<'global> {
	fn visit_alias(&mut self, alias_name: &str) -> FlowControl<Expr> {
		match self.aliases.get(alias_name).cloned() {
			Some(expr) => FlowControl::ExpandTo(expr),
			None => FlowControl::Error,
		}
	}

	fn visit_pat(&mut self, _pat: &str) -> FlowControl<String> {
		FlowControl::Error
	}
}

/// Visitor for rule output
///
/// Expands any defined aliases (first with the rule aliases, then global), errors on undefined ones
/// Keeps any patterns.
#[derive(Clone, Copy, Debug)]
pub struct RuleOutputVisitor<'global, 'rule> {
	/// Global visitor
	global_visitor: GlobalVisitor<'global>,

	/// Rule aliases
	rule_aliases: &'rule HashMap<String, Expr>,
}

impl<'global, 'rule> RuleOutputVisitor<'global, 'rule> {
	/// Creates a new rule output expression visitor
	pub fn new(global_visitor: GlobalVisitor<'global>, rule_aliases: &'rule HashMap<String, Expr>) -> Self {
		Self {
			global_visitor,
			rule_aliases,
		}
	}
}

impl<'global, 'rule> Visitor for RuleOutputVisitor<'global, 'rule> {
	fn visit_alias(&mut self, alias_name: &str) -> FlowControl<Expr> {
		match self.rule_aliases.get(alias_name).cloned() {
			Some(expr) => FlowControl::ExpandTo(expr),
			None => self.global_visitor.visit_alias(alias_name),
		}
	}

	fn visit_pat(&mut self, _pat: &str) -> FlowControl<String> {
		FlowControl::Keep
	}
}

/// Visitor for rule
///
/// Expands any defined aliases (first with the rule aliases, then global), errors on undefined ones
/// Expands any defined patterns, errors on undefined
#[derive(Clone, Copy, Debug)]
pub struct RuleVisitor<'global, 'rule, 'pats> {
	/// Rule output visitor
	rule_output_visitor: RuleOutputVisitor<'global, 'rule>,

	/// Patterns
	pats: &'pats HashMap<String, String>,
}

impl<'global, 'rule, 'pats> RuleVisitor<'global, 'rule, 'pats> {
	/// Creates a new rule visitor
	pub fn new(rule_output_visitor: RuleOutputVisitor<'global, 'rule>, pats: &'pats HashMap<String, String>) -> Self {
		Self {
			rule_output_visitor,
			pats,
		}
	}
}

impl<'global, 'rule, 'pats> Visitor for RuleVisitor<'global, 'rule, 'pats> {
	fn visit_alias(&mut self, alias_name: &str) -> FlowControl<Expr> {
		self.rule_output_visitor.visit_alias(alias_name)
	}

	fn visit_pat(&mut self, pat_name: &str) -> FlowControl<String> {
		match self.pats.get(pat_name).cloned() {
			Some(value) => FlowControl::ExpandTo(value),
			None => FlowControl::Error,
		}
	}
}
