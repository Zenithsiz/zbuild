//! Expressions

// Modules
pub mod expr_tree;

// Exports
pub use self::expr_tree::ExprTree;

// Imports
use {
	crate::{ast, util::ArcStr},
	std::fmt,
};

/// Expression component
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Debug)]
pub enum ExprCmpt {
	/// String
	String(ArcStr),

	/// Identifier
	Ident {
		/// Name
		name: ArcStr,

		/// Operators
		ops: Vec<ExprOp>,
	},
}

impl ExprCmpt {
	/// Returns `true` if the component is [`ExprCmpt::String`].
	#[must_use]
	pub const fn is_string(&self) -> bool {
		matches!(self, Self::String(_))
	}

	/// Returns this expression as a string, if it is one.
	#[must_use]
	pub const fn as_string(&self) -> Option<&ArcStr> {
		match self {
			Self::String(v) => Some(v),
			Self::Ident { .. } => None,
		}
	}

	/// Converts this component into a string, if it's a string.
	pub fn try_into_string(self) -> Result<ArcStr, Self> {
		match self {
			Self::String(v) => Ok(v),
			Self::Ident { .. } => Err(self),
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
	/// Creates a new, empty, expression
	pub const fn new() -> Self {
		Self { cmpts: vec![] }
	}

	/// Pushes a string into this expression
	pub fn push_str(&mut self, s: &ArcStr) {
		match self.cmpts.last_mut() {
			Some(ExprCmpt::String(last)) => last.with_mut(|last| last.push_str(s)),
			_ => self.cmpts.push(ExprCmpt::String(s.clone())),
		}
	}

	/// Pushes a component into this expression
	pub fn push(&mut self, cmpt: &ExprCmpt) {
		match cmpt {
			// If it's a string, try to use `push_str` for merging strings.
			ExprCmpt::String(s) => self.push_str(s),

			cmpt @ ExprCmpt::Ident { .. } => self.cmpts.push(cmpt.clone()),
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

	/// Converts this expression into a string, if it's compromised of only string components
	pub fn try_into_string(self) -> Result<ArcStr, Self> {
		// If all components aren't strings, return Err
		if !self.cmpts.iter().all(|cmpt| matches!(cmpt, ExprCmpt::String(_))) {
			return Err(self);
		}

		// Otherwise, get the first string, if any, then push all other strings
		let mut cmpts = self.cmpts.into_iter();
		let Some(output) = cmpts.next() else {
			return Ok(String::new().into());
		};
		let mut output = output.try_into_string().expect("Component wasn't a string");

		// Then add all other strings, if non-empty
		for cmpt in cmpts {
			let cmpt = cmpt.try_into_string().expect("Component wasn't a string");
			if !cmpt.is_empty() {
				output.with_mut(|output| output.push_str(&cmpt));
			}
		}

		Ok(output)
	}

	/// Creates a new expression from it's ast
	pub fn from_ast(expr: ast::Expr) -> Self {
		let cmpts = expr
			.cmpts
			.into_iter()
			.map(|cmpt| match cmpt {
				ast::ExprCmpt::String(s) => ExprCmpt::String(s),
				ast::ExprCmpt::Ident { ident, ops } => ExprCmpt::Ident {
					name: ident.0,
					ops:  ops
						.into_iter()
						.map(|op| match op {
							ast::ExprOp::DirName => ExprOp::DirName,
						})
						.collect(),
				},
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

impl Default for Expr {
	fn default() -> Self {
		Self::new()
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
			Self::Ident { name, ops } => {
				write!(f, "{{{name}")?;
				for op in ops {
					match op {
						ExprOp::DirName => write!(f, ".dir_name")?,
					}
				}
				write!(f, "}}")?;
				Ok(())
			},
		}
	}
}

/// Expression operators
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash, Debug)]
pub enum ExprOp {
	/// Directory name, `.dir_name`.
	DirName,
}

impl fmt::Display for ExprOp {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::DirName => write!(f, ".dir_name"),
		}
	}
}
