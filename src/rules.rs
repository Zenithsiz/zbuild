//! Rules

// Modules
mod expr;
mod item;
mod pattern;
mod rule;
mod target;

// Exports
pub use {
	expr::{Expr, ExprCmpt, ExprOp, ExprTree},
	item::{DepItem, OutItem},
	pattern::Pattern,
	rule::{Command, Exec, Rule},
	target::Target,
};

// Imports
use {
	crate::{util::ArcStr, AppError, Ast},
	indexmap::IndexMap,
	std::sync::Arc,
};

/// Rules.
///
/// Stores all rules, along with associated information, such as
/// global aliases and the default target.
#[derive(Clone, Debug)]
pub struct Rules {
	/// Global aliases.
	///
	/// These are available for the whole program to
	/// use.
	pub aliases: Arc<IndexMap<ArcStr, Expr>>,

	/// Patterns.
	///
	/// These are available for the whole program to
	/// use.
	pub pats: Arc<IndexMap<ArcStr, Pattern>>,

	/// Default targets to build
	pub default: Vec<Target<Expr>>,

	/// Rules
	pub rules: IndexMap<ArcStr, Rule<Expr>>,
}

impl Rules {
	/// Creates all rules from the ast
	pub fn from_ast(zbuild_file: &ArcStr, ast: Ast<'_>) -> Result<Self, AppError> {
		let aliases = ast
			.aliases
			.into_iter()
			.map(|alias| {
				(
					zbuild_file.slice_from_str(alias.name.0),
					Expr::from_ast(zbuild_file, alias.value),
				)
			})
			.collect();
		let pats = ast
			.pats
			.into_iter()
			.map(|pat| {
				let name = zbuild_file.slice_from_str(pat.name.0);
				(name.clone(), Pattern {
					name,
					non_empty: pat.non_empty,
				})
			})
			.collect();
		let default = ast
			.defaults
			.into_iter()
			.map(|target| Target::from_ast(zbuild_file, target.default))
			.collect();
		let rules = ast
			.rules
			.into_iter()
			.map(|rule| try {
				let name = zbuild_file.slice_from_str(rule.name.0);
				(name, Rule::from_ast(zbuild_file, rule)?)
			})
			.collect::<Result<_, AppError>>()?;

		Ok(Self {
			aliases: Arc::new(aliases),
			pats: Arc::new(pats),
			default,
			rules,
		})
	}
}
