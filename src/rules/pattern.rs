//! Pattern

// Imports
use {crate::util::ArcStr, std::fmt};

/// Pattern
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Debug)]
pub struct Pattern {
	/// Pattern name
	pub name: ArcStr,

	/// Operators
	pub ops: Vec<PatternOp>,
}

/// Pattern operator
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash, Debug)]
pub enum PatternOp {
	/// Non-empty
	NonEmpty,
}

impl fmt::Display for Pattern {
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
