//! Pattern

// Imports
use std::fmt;

/// Alias
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Debug)]
pub struct Alias<'s> {
	/// Alias name
	pub name: &'s str,

	/// Operators
	pub ops: Vec<AliasOp>,
}

/// Alias operator
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash, Debug)]
pub enum AliasOp {
	/// Directory name
	DirName,
}


impl fmt::Display for Alias<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "$({}", self.name)?;

		for op in &self.ops {
			write!(f, "::{op}")?;
		}

		write!(f, ")")?;


		Ok(())
	}
}

impl fmt::Display for AliasOp {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::DirName => write!(f, "dir_name"),
		}
	}
}
