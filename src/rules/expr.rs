//! Expressions

// Imports
use {
	super::{
		alias::{Alias, AliasOp},
		pattern::{Pattern, PatternOp},
	},
	crate::ast,
	std::fmt,
};

/// Expression
#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub struct Expr {
	/// Components
	pub cmpts: Vec<ExprCmpt>,
}

/// Expression component
#[derive(PartialEq, Eq, Clone, Hash, Debug)]
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
				ast::ExprCmpt::String(s) => ExprCmpt::String(s.into_owned()),
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

impl fmt::Display for Expr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		for cmpt in &self.cmpts {
			write!(f, "{cmpt}")?;
		}

		Ok(())
	}
}

impl fmt::Display for ExprCmpt {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::String(s) => write!(f, "{s}"),
			Self::Pattern(pat) => write!(f, "{pat}"),
			Self::Alias(alias) => write!(f, "{alias}"),
		}
	}
}
