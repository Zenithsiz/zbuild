//! Pattern

// Imports
use std::fmt;

/// Pattern
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Debug)]
pub struct Pattern<'s> {
	/// Pattern name
	pub name: &'s str,

	/// Operators
	pub ops: Vec<PatternOp>,
}

/// Pattern operator
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash, Debug)]
pub enum PatternOp {
	/// Non-empty
	NonEmpty,
}

impl fmt::Display for Pattern<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "^({}", self.name)?;

		for op in &self.ops {
			write!(f, "::{op}")?;
		}

		write!(f, ")")?;


		Ok(())
	}
}

impl fmt::Display for PatternOp {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::NonEmpty => write!(f, "non_empty"),
		}
	}
}
