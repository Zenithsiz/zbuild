//! Expression expansion

// Imports
use {
	crate::rules::{Alias, Expr, ExprCmpt, ExprOp, Pattern},
	anyhow::Context,
	std::{borrow::Cow, collections::HashMap, path::PathBuf},
};

/// Expands an expression to it's components
pub fn expand_expr(expr: &Expr, visitor: &mut impl Visitor) -> Result<Vec<ExprCmpt>, anyhow::Error> {
	let cmpts = match expr {
		Expr::Op { op, expr } => {
			let s = self::expand_expr_string(expr, visitor)
				.with_context(|| format!("Unable to expand expression to string for operator {expr:?}"))?;
			let s = self::expand_op(op, s)?;

			vec![ExprCmpt::String(s)]
		},

		// If we got a string, simply expand it
		Expr::String(cmpts) => cmpts.iter().try_fold::<_, _, Result<_, _>>(vec![], |mut cmpts, cmpt| {
			match cmpt {
				ExprCmpt::String(_) => cmpts.push(cmpt.clone()),
				ExprCmpt::Pattern(pat) => match visitor.visit_pat(pat) {
					FlowControl::ExpandTo(s) => cmpts.push(ExprCmpt::String(s)),
					FlowControl::Keep => cmpts.push(cmpt.clone()),
					FlowControl::Error => anyhow::bail!("Unknown pattern {pat:?}"),
				},
				ExprCmpt::Alias(alias) => match visitor.visit_alias(alias) {
					FlowControl::ExpandTo(expr) => cmpts.extend(self::expand_expr(&expr, visitor)?),
					FlowControl::Keep => cmpts.push(cmpt.clone()),
					FlowControl::Error => anyhow::bail!("Unknown alias {alias:?}"),
				},
			};

			Ok(cmpts)
		})?,
	};

	Ok(cmpts)
}

/// Expands an expression into a string
///
/// Panics if `visitor` returns `Keep`.
// TODO: Merge neighboring strings in output.
pub fn expand_expr_string(expr: &Expr, visitor: &mut impl Visitor) -> Result<String, anyhow::Error> {
	let s = match expr {
		Expr::Op { op, expr } => {
			let expr = self::expand_expr_string(expr, visitor)?;
			self::expand_op(op, expr)?
		},

		// If we got a string, simply expand it
		Expr::String(cmpts) => cmpts
			.iter()
			.try_fold::<_, _, Result<_, _>>(String::new(), |mut string, cmpt| {
				let s = match cmpt {
					ExprCmpt::String(s) => Cow::Borrowed(s),
					ExprCmpt::Pattern(pat) => match visitor.visit_pat(pat) {
						FlowControl::ExpandTo(s) => Cow::Owned(s),
						FlowControl::Keep => panic!("Cannot keep unknown pattern when expanding to string"),
						FlowControl::Error => anyhow::bail!("Unknown pattern {pat:?}"),
					},
					ExprCmpt::Alias(alias) => {
						let expr = match visitor.visit_alias(alias) {
							FlowControl::ExpandTo(expr) => expr,
							FlowControl::Keep => panic!("Cannot keep unknown pattern when expanding to string"),
							FlowControl::Error => anyhow::bail!("Unknown alias {alias:?}"),
						};
						let s = self::expand_expr_string(&expr, visitor)?;
						Cow::Owned(s)
					},
				};

				string.push_str(&s);
				Ok(string)
			})?,
	};

	Ok(s)
}

/// Expands an operation on an expression
fn expand_op(op: &ExprOp, expr: String) -> Result<String, anyhow::Error> {
	let expr = match op {
		// TODO: Not add `/` here.
		ExprOp::DirName => {
			// Get the path and try to pop the last segment
			let mut path = PathBuf::from(expr);
			anyhow::ensure!(path.pop(), "Path {path:?} had no directory name");

			// Then convert it back to a string
			// Note: This should technically never fail, since the path was originally
			//       utf-8
			let mut path = path
				.into_os_string()
				.into_string()
				.expect("utf-8 path was no longer utf-8 after getting dir-name");
			path.push('/');
			path
		},
	};

	Ok(expr)
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
	fn visit_alias(&mut self, alias: &Alias) -> FlowControl<Expr>;

	/// Visits a pattern
	fn visit_pat(&mut self, pat: &Pattern) -> FlowControl<String>;
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
	fn visit_alias(&mut self, alias: &Alias) -> FlowControl<Expr> {
		match self.aliases.get(&alias.name).cloned() {
			Some(expr) => FlowControl::ExpandTo(expr),
			None => FlowControl::Error,
		}
	}

	fn visit_pat(&mut self, _pat: &Pattern) -> FlowControl<String> {
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
	fn visit_alias(&mut self, alias: &Alias) -> FlowControl<Expr> {
		match self.rule_aliases.get(&alias.name).cloned() {
			Some(expr) => FlowControl::ExpandTo(expr),
			None => self.global_visitor.visit_alias(alias),
		}
	}

	fn visit_pat(&mut self, _pat: &Pattern) -> FlowControl<String> {
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
	fn visit_alias(&mut self, alias: &Alias) -> FlowControl<Expr> {
		self.rule_output_visitor.visit_alias(alias)
	}

	fn visit_pat(&mut self, pat: &Pattern) -> FlowControl<String> {
		match self.pats.get(&pat.name).cloned() {
			Some(value) => FlowControl::ExpandTo(value),
			None => FlowControl::Error,
		}
	}
}
