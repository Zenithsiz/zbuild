//! Pattern

/// Alias
#[derive(Clone, Debug)]
pub struct Alias {
	/// Alias name
	pub name: String,

	/// Operators
	pub ops: Vec<AliasOp>,
}

/// Alias operator
#[derive(Clone, Copy, Debug)]
pub enum AliasOp {
	/// Directory name
	DirName,
}
