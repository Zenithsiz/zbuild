//! Pattern

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
