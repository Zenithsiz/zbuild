//! Items

// Imports
use {
	super::Expr,
	crate::ast,
	std::{collections::BTreeMap, fmt, sync::Arc},
};


/// Output Item
#[derive(Clone, Debug)]
pub enum OutItem<T> {
	/// File
	File {
		/// File that will be built
		file: T,

		/// If the file is a dependencies file
		is_deps_file: bool,
	},
}

impl OutItem<Expr> {
	/// Creates a new item from it's `ast`.
	pub fn new(item: ast::OutItem<'static>) -> Self {
		match item {
			ast::OutItem::File(file) => Self::File {
				file:         Expr::new(file),
				is_deps_file: false,
			},
			ast::OutItem::DepsFile { deps_file } => Self::File {
				file:         Expr::new(deps_file),
				is_deps_file: true,
			},
		}
	}
}

impl<T: fmt::Display> fmt::Display for OutItem<T> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match *self {
			Self::File { ref file, is_deps_file } => {
				if is_deps_file {
					write!(f, "deps_file: ")?;
				}

				write!(f, "{file}")?;
				Ok(())
			},
		}
	}
}


/// Dependency Item
#[derive(Clone, Debug)]
pub enum DepItem<T> {
	/// File
	File {
		/// File dependency
		file: T,

		/// If optional
		is_optional: bool,

		/// If static
		is_static: bool,

		/// If a dependencies file
		is_deps_file: bool,
	},

	/// Rule
	Rule {
		/// Rule name
		name: T,

		/// All rule patterns
		pats: Arc<BTreeMap<T, T>>,
	},
}

impl DepItem<Expr> {
	/// Creates a new item from it's `ast`.
	pub fn new(item: ast::DepItem<'static>) -> Self {
		match item {
			ast::DepItem::File(file) => Self::File {
				file:         Expr::new(file),
				is_optional:  false,
				is_static:    false,
				is_deps_file: false,
			},
			ast::DepItem::Rule { rule, pats } => {
				let pats = pats
					.into_iter()
					.map(|(pat, value)| (Expr::new(pat), Expr::new(value)))
					.collect();
				Self::Rule {
					name: Expr::new(rule),
					pats: Arc::new(pats),
				}
			},
			ast::DepItem::DepsFile { deps_file } => Self::File {
				file:         Expr::new(deps_file),
				is_optional:  false,
				is_static:    false,
				is_deps_file: true,
			},
			ast::DepItem::Static { item: static_item } => match static_item {
				ast::StaticDepItem::File(file) => Self::File {
					file:         Expr::new(file),
					is_optional:  false,
					is_static:    true,
					is_deps_file: false,
				},
				ast::StaticDepItem::DepsFile { deps_file } => Self::File {
					file:         Expr::new(deps_file),
					is_optional:  false,
					is_static:    true,
					is_deps_file: true,
				},
			},
			ast::DepItem::Opt { item: opt_item } => match opt_item {
				ast::OptDepItem::File(file) => Self::File {
					file:         Expr::new(file),
					is_optional:  true,
					is_static:    true,
					is_deps_file: false,
				},
				ast::OptDepItem::DepsFile { deps_file } => Self::File {
					file:         Expr::new(deps_file),
					is_optional:  true,
					is_static:    true,
					is_deps_file: true,
				},
			},
		}
	}
}

impl<T: fmt::Display> fmt::Display for DepItem<T> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match *self {
			Self::File {
				ref file,
				is_optional,
				is_static,
				is_deps_file,
			} => {
				if is_optional {
					write!(f, "opt: ")?;
				}

				if is_static {
					write!(f, "static: ")?;
				}

				if is_deps_file {
					write!(f, "deps_file: ")?;
				}

				write!(f, "{file}")?;
				Ok(())
			},
			Self::Rule { ref name, ref pats } => {
				write!(f, "rule: {name}")?;

				if !pats.is_empty() {
					write!(f, " (")?;

					for (pat, value) in &**pats {
						write!(f, "{pat}={value}, ")?;
					}

					write!(f, ")")?;
				}

				Ok(())
			},
		}
	}
}
