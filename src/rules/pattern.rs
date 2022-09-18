//! Pattern

/// Pattern
#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub struct Pattern {
	/// Pattern name
	pub name: String,

	/// Operators
	pub ops: Vec<PatternOp>,
}

/// Pattern operator
#[derive(PartialEq, Eq, Clone, Copy, Hash, Debug)]
pub enum PatternOp {
	/// Non-empty
	NonEmpty,
}
