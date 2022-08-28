//! Target

use {
	itertools::Itertools,
	std::hash::{Hash, Hasher},
};

// Imports
use {super::Expr, crate::ast, std::collections::HashMap};

/// Target
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Target<T> {
	/// File
	File {
		/// Target file
		file: T,
	},

	/// Rule
	Rule {
		/// Target rule name
		rule: T,

		/// Patterns
		pats: HashMap<String, T>,
	},
}

impl Target<Expr> {
	/// Creates a new target from it's ast
	pub fn new(ast: ast::Target) -> Self {
		match ast {
			ast::Target::File(file) => Self::File { file: Expr::new(file) },
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
			Target::File { file } => file.hash(state),
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
