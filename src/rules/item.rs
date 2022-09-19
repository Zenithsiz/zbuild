//! Items

// Imports
use {
	super::Expr,
	crate::ast,
	std::{collections::HashMap, fmt},
};


/// Output Item
#[derive(Clone, Debug)]
pub enum OutItem<T> {
	/// File
	File { file: T },

	/// Dependencies file
	DepsFile { file: T },
}

impl OutItem<Expr> {
	/// Creates a new item from it's `ast`.
	pub fn new(item: ast::OutItem) -> Self {
		match item {
			ast::OutItem::File(file) => Self::File { file: Expr::new(file) },
			ast::OutItem::DepsFile { deps_file } => Self::DepsFile {
				file: Expr::new(deps_file),
			},
		}
	}
}

impl<T: fmt::Display> fmt::Display for OutItem<T> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			OutItem::File { file } => write!(f, "{file}"),
			OutItem::DepsFile { file } => write!(f, "dep_file: {file}"),
		}
	}
}


/// Dependency Item
#[derive(Clone, Debug)]
pub enum DepItem<T> {
	/// File
	File { file: T, is_static: bool },

	/// Dependencies file
	DepsFile { file: T, is_static: bool },

	/// Rule
	Rule { name: T, pats: HashMap<T, T> },
}

impl DepItem<Expr> {
	/// Creates a new item from it's `ast`.
	pub fn new(item: ast::DepItem) -> Self {
		match item {
			ast::DepItem::File(file) => Self::File {
				file:      Expr::new(file),
				is_static: false,
			},
			ast::DepItem::Rule { rule, pats } => Self::Rule {
				name: Expr::new(rule),
				pats: pats
					.into_iter()
					.map(|(pat, value)| (Expr::new(pat), Expr::new(value)))
					.collect(),
			},
			ast::DepItem::DepsFile { deps_file } => Self::DepsFile {
				file:      Expr::new(deps_file),
				is_static: false,
			},
			ast::DepItem::Static { item } => match item {
				ast::StaticDepItem::File(file) => Self::File {
					file:      Expr::new(file),
					is_static: false,
				},
				ast::StaticDepItem::DepsFile { deps_file } => Self::DepsFile {
					file:      Expr::new(deps_file),
					is_static: false,
				},
			},
		}
	}
}

impl<T: fmt::Display> fmt::Display for DepItem<T> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			DepItem::File { file, is_static } => match is_static {
				true => write!(f, "static: {file}"),
				false => write!(f, "{file}"),
			},
			DepItem::DepsFile { file, is_static } => match is_static {
				true => write!(f, "static: dep_file: {file}"),
				false => write!(f, "dep_file: {file}"),
			},
			DepItem::Rule { name, pats } => {
				write!(f, "rule: {}", name)?;

				if !pats.is_empty() {
					write!(f, " (")?;

					for (pat, value) in pats {
						write!(f, "{pat}={value}, ")?;
					}

					write!(f, ")")?;
				}

				Ok(())
			},
		}
	}
}
