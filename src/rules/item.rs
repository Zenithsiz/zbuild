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

impl<'s> OutItem<Expr<'s>> {
	/// Creates a new item from it's `ast`.
	pub fn new(item: ast::OutItem<'s>) -> Self {
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
			Self::File { file } => write!(f, "{file}"),
			Self::DepsFile { file } => write!(f, "dep_file: {file}"),
		}
	}
}


/// Dependency Item
#[derive(Clone, Debug)]
pub enum DepItem<T> {
	/// File
	File {
		file:        T,
		is_optional: bool,
		is_static:   bool,
	},

	/// Dependencies file
	DepsFile {
		file:        T,
		is_optional: bool,
		is_static:   bool,
	},

	/// Rule
	Rule { name: T, pats: HashMap<T, T> },
}

impl<'s> DepItem<Expr<'s>> {
	/// Creates a new item from it's `ast`.
	pub fn new(item: ast::DepItem<'s>) -> Self {
		match item {
			ast::DepItem::File(file) => Self::File {
				file:        Expr::new(file),
				is_optional: false,
				is_static:   false,
			},
			ast::DepItem::Rule { rule, pats } => Self::Rule {
				name: Expr::new(rule),
				pats: pats
					.into_iter()
					.map(|(pat, value)| (Expr::new(pat), Expr::new(value)))
					.collect(),
			},
			ast::DepItem::DepsFile { deps_file } => Self::DepsFile {
				file:        Expr::new(deps_file),
				is_optional: false,
				is_static:   false,
			},
			ast::DepItem::Static { item: static_item } => match static_item {
				ast::StaticDepItem::File(file) => Self::File {
					file:        Expr::new(file),
					is_optional: false,
					is_static:   true,
				},
				ast::StaticDepItem::DepsFile { deps_file } => Self::DepsFile {
					file:        Expr::new(deps_file),
					is_optional: false,
					is_static:   true,
				},
			},
			ast::DepItem::Opt { item: opt_item } => match opt_item {
				ast::OptDepItem::File(file) => Self::File {
					file:        Expr::new(file),
					is_optional: true,
					is_static:   true,
				},
				ast::OptDepItem::DepsFile { deps_file } => Self::DepsFile {
					file:        Expr::new(deps_file),
					is_optional: true,
					is_static:   true,
				},
			},
		}
	}
}

impl<T: fmt::Display> fmt::Display for DepItem<T> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::File {
				file,
				is_optional,
				is_static,
			} => match (is_optional, is_static) {
				(true, true) => write!(f, "opt: static: {file}"),
				(true, false) => write!(f, "opt: {file}"),
				(false, true) => write!(f, "static: {file}"),
				(false, false) => write!(f, "{file}"),
			},
			Self::DepsFile {
				file,
				is_optional,
				is_static,
			} => match (is_optional, is_static) {
				(true, true) => write!(f, "opt: static: dep_file: {file}"),
				(true, false) => write!(f, "opt: dep_file: {file}"),
				(false, true) => write!(f, "static: dep_file: {file}"),
				(false, false) => write!(f, "dep_file: {file}"),
			},
			Self::Rule { name, pats } => {
				write!(f, "rule: {name}")?;

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
