//! Expressions

// Imports
use {
	super::{alias::Alias, pattern::Pattern},
	crate::ast,
};

/// Expression
#[derive(Clone, Debug)]
pub enum Expr {
	/// Operation
	Op {
		/// Operation
		op: ExprOp,

		/// Expression
		expr: Box<Self>,
	},

	/// String
	String(Vec<ExprCmpt>),
}

/// Expression operator
#[derive(Clone, Debug)]
pub enum ExprOp {
	/// Dir name
	DirName,
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
		match expr {
			ast::Expr::Op { op, expr } => Self::Op {
				op:   match op {
					ast::ExprOp::DirName => ExprOp::DirName,
				},
				expr: Box::new(Self::new(*expr)),
			},
			ast::Expr::String(cmpts) => Self::String(
				cmpts
					.into_iter()
					.map(|cmpt| match cmpt {
						ast::ExprCmpt::String(s) => ExprCmpt::String(s),
						ast::ExprCmpt::Pattern(name) => ExprCmpt::Pattern(Pattern { name }),
						ast::ExprCmpt::Alias(name) => ExprCmpt::Alias(Alias { name }),
					})
					.collect(),
			),
		}
	}

	/// Returns an expression that's just a string
	pub fn string(value: String) -> Self {
		Self::String(vec![ExprCmpt::String(value)])
	}
}
