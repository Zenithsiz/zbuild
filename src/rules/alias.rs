//! Pattern

// Imports
use {crate::util::ArcStr, std::fmt};

/// Alias
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Debug)]
pub struct Alias {
	/// Alias name
	pub name: ArcStr,

	/// Operators
	pub ops: Vec<AliasOp>,
}

/// Alias operator
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash, Debug)]
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
			Self::DirName => write!(f, "dir_name"),
		}
	}
}
