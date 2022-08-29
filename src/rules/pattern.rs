//! Pattern

/// Pattern
#[derive(Clone, Debug)]
pub struct Pattern {
	/// Pattern name
	pub name: String,

	/// Operators
	pub ops: Vec<PatternOp>,
}

/// Pattern operator
#[derive(Clone, Copy, Debug)]
pub enum PatternOp {
	/// Non-empty
	NonEmpty,
}
