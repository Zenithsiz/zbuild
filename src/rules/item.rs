//! Items

// Imports
use {super::Expr, crate::ast, std::collections::HashMap};

/// Item
#[derive(Clone, Debug)]
pub enum Item<T> {
	/// File
	File(T),

	/// Dependencies file
	DepsFile(T),
}

impl Item<Expr> {
	/// Creates a new item from it's `ast`.
	pub fn new(item: ast::Item) -> Self {
		match item {
			ast::Item::File(file) => {
				let file = Expr::new(file);
				Item::File(file)
			},
			ast::Item::DepsFile { deps_file } => {
				let deps_file = Expr::new(deps_file);
				Item::DepsFile(deps_file)
			},
		}
	}
}

impl<T> Item<T> {
	/// Returns the file of this item
	pub fn file(&self) -> &T {
		match self {
			Item::File(file) | Item::DepsFile(file) => file,
		}
	}
}

/// Rule Item
#[derive(Clone, Debug)]
pub struct RuleItem<T> {
	/// Name
	pub name: T,

	/// Patterns
	pub pats: HashMap<T, T>,
}

impl RuleItem<Expr> {
	/// Creates a new item from it's `ast`.
	pub fn new(item: ast::RuleItem) -> Self {
		Self {
			name: Expr::new(item.name),
			pats: item
				.pats
				.into_iter()
				.map(|(pat, expr)| (Expr::new(pat), Expr::new(expr)))
				.collect(),
		}
	}
}
