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

/// Expression component
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Debug)]
pub enum ExprCmpt {
	/// String
	String(CowStr),

	/// Pattern
	Pattern(Pattern),

	/// Alias
	Alias(Alias),
}

/// Expression
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Debug)]
pub struct Expr {
	/// Components
	pub cmpts: Vec<ExprCmpt>,
}

impl Expr {
	/// Creates an empty expression
	// TODO: Rename this to `new` and have `new` be `from_ast`?
	pub const fn empty() -> Self {
		Self { cmpts: vec![] }
	}

	/// Pushes a string into this expression
	pub fn push_str(&mut self, s: &CowStr) {
		match self.cmpts.last_mut() {
			Some(ExprCmpt::String(last)) => {
				last.to_mut().push_str(s);
			},
			_ => self.cmpts.push(ExprCmpt::String(s.clone())),
		}
	}

	/// Pushes a component into this expression
	pub fn push(&mut self, cmpt: &ExprCmpt) {
		#[expect(clippy::wildcard_enum_match_arm, reason = "The wildcard matches all variants")]
		match cmpt {
			// If it's a string, try to use `push_str` for merging strings.
			ExprCmpt::String(s) if let Some(ExprCmpt::String(last)) = self.cmpts.last_mut() =>
				last.to_mut().push_str(s),

			cmpt => self.cmpts.push(cmpt.clone()),
		}
	}

	/// Extends this expression with an iterator of components
	pub fn extend<I>(&mut self, cmpts: I)
	where
		I: IntoIterator<Item = ExprCmpt, IntoIter: ExactSizeIterator>,
	{
		let cmpts = cmpts.into_iter();

		self.cmpts.reserve(cmpts.len());
		for cmpt in cmpts {
			self.push(&cmpt);
		}
	}

	/// Creates a new expression from it's ast
	pub fn new(expr: ast::Expr<'static>) -> Self {
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
	pub fn string(value: impl Into<CowStr>) -> Self {
		Self {
			cmpts: vec![ExprCmpt::String(value.into())],
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
