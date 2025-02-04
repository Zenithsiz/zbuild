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
	std::collections::HashMap,
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
	pub aliases: HashMap<ArcStr, Expr>,

	/// Patterns.
	///
	/// These are available for the whole program to
	/// use.
	pub pats: HashMap<ArcStr, Pattern>,

	/// Default targets to build
	pub default: Vec<Target<Expr>>,

	/// Rules
	#[expect(clippy::struct_field_names, reason = "TODO: Rename struct name")]
	pub rules: HashMap<ArcStr, Rule<Expr>>,
}

impl Rules {
	/// Creates all rules from the ast
	pub fn from_ast(ast: Ast) -> Result<Self, AppError> {
		let aliases = ast
			.aliases
			.into_iter()
			.map(|alias| (alias.name.0, Expr::from_ast(alias.value)))
			.collect();
		let pats = ast
			.pats
			.into_iter()
			.map(|pat| {
				(pat.name.0.clone(), Pattern {
					name:      pat.name.0,
					non_empty: pat.non_empty,
				})
			})
			.collect();
		let default = ast
			.defaults
			.into_iter()
			.map(|target| Target::from_ast(target.default))
			.collect();
		let rules = ast
			.rules
			.into_iter()
			.map(|rule| try { (rule.name.0.clone(), Rule::from_ast(rule)?) })
			.collect::<Result<_, AppError>>()?;

		Ok(Self {
			aliases,
			pats,
			default,
			rules,
		})
	}
}
