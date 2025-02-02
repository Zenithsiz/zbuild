//! Items

// Imports
use {
	super::Expr,
	crate::{ast, AppError},
	std::fmt,
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
	pub fn from_ast(item: ast::Expr) -> Result<Self, AppError> {
		let is_deps_file = item.is_deps_file;
		zutil_app_error::ensure!(!item.is_opt, "Output items cannot be optional");
		zutil_app_error::ensure!(!item.is_static, "Output items cannot be static");

		Ok(Self::File {
			file: Expr::from_ast(item),
			is_deps_file,
		})
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
}

impl DepItem<Expr> {
	/// Creates a new item from it's `ast`.
	pub fn from_ast(item: ast::Expr) -> Self {
		let is_optional = item.is_opt;
		let is_static = item.is_static;
		let is_deps_file = item.is_deps_file;

		Self::File {
			file: Expr::from_ast(item),
			is_optional,
			is_static,
			is_deps_file,
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
		}
	}
}
