//! Expressions

// Imports
use {
	super::{
		alias::{Alias, AliasOp},
		pattern::{Pattern, PatternOp},
	},
	crate::{ast, util::ArcStr},
	std::fmt,
};

/// Expression component
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Debug)]
pub enum ExprCmpt {
	/// String
	String(ArcStr),

	/// Pattern
	Pattern(Pattern),

	/// Alias
	Alias(Alias),
}

impl ExprCmpt {
	/// Converts this expression into a string, if it's a string.
	pub fn try_into_string(self) -> Result<ArcStr, Self> {
		#[expect(clippy::wildcard_enum_match_arm, reason = "We only care about a specific variant")]
		match self {
			Self::String(v) => Ok(v),
			_ => Err(self),
		}
	}
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
	pub fn push_str(&mut self, s: &ArcStr) {
		self.cmpts.push(ExprCmpt::String(s.clone()));
	}

	/// Pushes a component into this expression
	pub fn push(&mut self, cmpt: &ExprCmpt) {
		self.cmpts.push(cmpt.clone());
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

	/// Converts this expression into a string, if it's compromised of only string components
	pub fn try_into_string(self) -> Result<ArcStr, Self> {
		// If all components aren't strings, return Err
		if !self.cmpts.iter().all(|cmpt| matches!(cmpt, ExprCmpt::String(_))) {
			return Err(self);
		}

		// Otherwise, get the first string, if any, then push all other strings
		let mut cmpts = self.cmpts.into_iter();
		let Some(output) = cmpts.next() else {
			return Ok("".into());
		};
		let output = output.try_into_string().expect("Component wasn't a string");

		enum ArcStrCow {
			String(String),
			ArcStr(ArcStr),
		}
		let mut output = ArcStrCow::ArcStr(output);

		// Then add all other strings, if non-empty
		for cmpt in cmpts {
			let cmpt = cmpt.try_into_string().expect("Component wasn't a string");
			if !cmpt.is_empty() {
				let output = match &mut output {
					ArcStrCow::String(s) => s,
					ArcStrCow::ArcStr(s) => {
						output = ArcStrCow::String(s.to_string());
						match &mut output {
							ArcStrCow::String(s) => s,
							ArcStrCow::ArcStr(_) => unreachable!("Just converted"),
						}
					},
				};

				output.push_str(&cmpt);
			}
		}

		let output = match output {
			ArcStrCow::String(s) => ArcStr::from(s),
			ArcStrCow::ArcStr(s) => s,
		};

		Ok(output)
	}

	/// Creates a new expression from it's ast
	pub fn new(expr: ast::Expr<'static>) -> Self {
		let cmpts = expr
			.cmpts
			.into_iter()
			.map(|cmpt| match cmpt {
				ast::ExprCmpt::String(s) => ExprCmpt::String(ArcStr::from(s)),
				ast::ExprCmpt::Pattern(ast::Pattern { name, ops }) => ExprCmpt::Pattern(Pattern {
					name: name.into(),
					ops:  ops
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
	pub fn string(value: impl Into<ArcStr>) -> Self {
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
