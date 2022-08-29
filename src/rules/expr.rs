//! Expressions

// Imports
use {
	super::{
		alias::{Alias, AliasOp},
		pattern::{Pattern, PatternOp},
	},
	crate::ast,
};

/// Expression
#[derive(Clone, Debug)]
pub struct Expr {
	/// Components
	pub cmpts: Vec<ExprCmpt>,
}

/// Expression component
#[derive(Clone, Debug)]
pub enum ExprCmpt {
	/// String
	String(String),

	/// Pattern
	Pattern(Pattern),

	/// Alias
	Alias(Alias),
}

impl Expr {
	/// Creates a new expression from it's ast
	pub fn new(expr: ast::Expr) -> Self {
		let cmpts = expr
			.cmpts
			.into_iter()
			.map(|cmpt| match cmpt {
				ast::ExprCmpt::String(s) => ExprCmpt::String(s),
				ast::ExprCmpt::Pattern { name, ops } => ExprCmpt::Pattern(Pattern {
					name,
					ops: ops
						.into_iter()
						.map(|op| match op {
							ast::PatternOp::NonEmpty => PatternOp::NonEmpty,
						})
						.collect(),
				}),
				ast::ExprCmpt::Alias { name, ops } => ExprCmpt::Alias(Alias {
					name,
					ops: ops
						.into_iter()
						.map(|op| match op {
							ast::AliasOp::DirName => AliasOp::DirName,
						})
						.collect(),
				}),
			})
			.collect();

		Self { cmpts }
	}

	/// Returns an expression that's just a string
	pub fn string(value: String) -> Self {
		Self {
			cmpts: vec![ExprCmpt::String(value)],
		}
	}
}
