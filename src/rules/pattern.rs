//! Pattern

// Imports
use {crate::util::ArcStr, std::fmt};

/// Pattern
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Debug)]
pub struct Pattern {
	/// Pattern name
	pub name: ArcStr,

	/// Non-empty
	pub non_empty: bool,
}

impl fmt::Display for Pattern {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "^({}", self.name)?;

		if self.non_empty {
			write!(f, "::non_empty")?;
		}

		write!(f, ")")?;


		Ok(())
	}
}
