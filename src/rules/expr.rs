//! Expressions

// Imports
use {
	super::{
		alias::{Alias, AliasOp},
		pattern::{Pattern, PatternOp},
	},
	crate::{ast, util::CowStr},
	std::fmt,
};

/// Expression
#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub struct Expr<'s> {
	/// Components
	pub cmpts: Vec<ExprCmpt<'s>>,
}

/// Expression component
#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub enum ExprCmpt<'s> {
	/// String
	String(CowStr<'s>),

	/// Pattern
	Pattern(Pattern<'s>),

	/// Alias
	Alias(Alias<'s>),
}

impl<'s> Expr<'s> {
	/// Creates a new expression from it's ast
	pub fn new(expr: ast::Expr<'s>) -> Self {
		let cmpts = expr
			.cmpts
			.into_iter()
			.map(|cmpt| match cmpt {
				ast::ExprCmpt::String(s) => ExprCmpt::String(CowStr::Borrowed(s)),
				ast::ExprCmpt::Pattern(ast::Pattern { name, ops }) => ExprCmpt::Pattern(Pattern {
					name,
					ops: ops
						.into_iter()
						.map(|op| match op {
							ast::PatternOp::NonEmpty => PatternOp::NonEmpty,
						})
						.collect(),
				}),
				ast::ExprCmpt::Alias(ast::Alias { name, ops }) => ExprCmpt::Alias(Alias {
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
	pub fn string(value: impl Into<CowStr<'s>>) -> Self {
		Self {
			cmpts: vec![ExprCmpt::String(value.into())],
		}
	}
}

impl<'s> fmt::Display for Expr<'s> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		for cmpt in &self.cmpts {
			write!(f, "{cmpt}")?;
		}

		Ok(())
	}
}

impl<'s> fmt::Display for ExprCmpt<'s> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::String(s) => write!(f, "{s}"),
			Self::Pattern(pat) => write!(f, "{pat}"),
			Self::Alias(alias) => write!(f, "{alias}"),
		}
	}
}
