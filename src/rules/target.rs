//! Target

// Imports
use {
	super::Expr,
	crate::ast,
	itertools::Itertools,
	std::{
		collections::HashMap,
		fmt,
		hash::{Hash, Hasher},
	},
};

/// Target
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Target<T> {
	/// File
	File {
		/// Target file
		file: T,

		/// If static
		is_static: bool,
	},

	/// Rule
	Rule {
		/// Target rule name
		rule: T,

		/// Patterns
		pats: HashMap<String, T>,
	},
}

impl<T> Target<T> {
	/// Returns if this target is static
	pub fn is_static(&self) -> bool {
		match *self {
			Self::File { is_static, .. } => is_static,
			Self::Rule { .. } => false,
		}
	}
}

impl Target<Expr> {
	/// Creates a new target from it's ast
	pub fn new(ast: ast::Target) -> Self {
		match ast {
			ast::Target::File(file) => Self::File {
				file:      Expr::new(file),
				is_static: false,
			},
			ast::Target::Rule { rule } => Self::Rule {
				rule: Expr::new(rule),
				pats: HashMap::new(),
			},
		}
	}
}

impl Hash for Target<String> {
	fn hash<H: Hasher>(&self, state: &mut H) {
		core::mem::discriminant(self).hash(state);
		match self {
			Target::File { file, is_static } => {
				file.hash(state);
				is_static.hash(state);
			},
			Target::Rule { rule, pats } => {
				rule.hash(state);
				// TODO: Not have to sort the patterns
				for (pat, value) in pats.iter().sorted() {
					pat.hash(state);
					value.hash(state);
				}
			},
		}
	}
}


impl<T: fmt::Display> fmt::Display for Target<T> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Target::File { file, is_static } => match is_static {
				true => write!(f, "static: file: {file}"),
				false => write!(f, "file: {file}"),
			},
			Target::Rule { rule, pats } => {
				write!(f, "rule: {rule}")?;

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
