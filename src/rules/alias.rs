//! Pattern

// Imports
use std::fmt;

/// Alias
#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub struct Alias {
	/// Alias name
	pub name: String,

	/// Operators
	pub ops: Vec<AliasOp>,
}

/// Alias operator
#[derive(PartialEq, Eq, Clone, Copy, Hash, Debug)]
pub enum AliasOp {
	/// Directory name
	DirName,
}


impl fmt::Display for Alias {
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
			AliasOp::DirName => write!(f, "dir_name"),
		}
	}
}
