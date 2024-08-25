//! Target

// Imports
use {
	super::Expr,
	crate::{ast, util::CowStr},
	std::{
		collections::BTreeMap,
		fmt,
		hash::{Hash, Hasher},
		mem,
		sync::Arc,
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
		pats: Arc<BTreeMap<CowStr, T>>,
	},
}

impl<T> Target<T> {
	/// Returns if this target is static
	pub const fn is_static(&self) -> bool {
		match *self {
			Self::File { is_static, .. } => is_static,
			Self::Rule { .. } => false,
		}
	}
}

impl Target<Expr> {
	/// Creates a new target from it's ast
	pub fn new(ast: ast::Target<'static>) -> Self {
		match ast {
			ast::Target::File(file) => Self::File {
				file:      Expr::new(file),
				is_static: false,
			},
			ast::Target::Rule { rule } => Self::Rule {
				rule: Expr::new(rule),
				pats: Arc::new(BTreeMap::new()),
			},
		}
	}
}

impl<T: Hash + Ord> Hash for Target<T> {
	fn hash<H: Hasher>(&self, state: &mut H) {
		mem::discriminant(self).hash(state);
		match self {
			Self::File { file, is_static } => {
				file.hash(state);
				is_static.hash(state);
			},
			Self::Rule { rule, pats } => {
				rule.hash(state);
				for (pat, value) in &**pats {
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
			Self::File { file, is_static } => match is_static {
				true => write!(f, "static: file: {file}"),
				false => write!(f, "file: {file}"),
			},
			Self::Rule { rule, pats } => {
				write!(f, "rule: {rule}")?;

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
