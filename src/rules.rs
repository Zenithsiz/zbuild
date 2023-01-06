//! Rules

// Modules
mod alias;
mod expr;
mod item;
mod pattern;
mod rule;
mod target;

// Exports
pub use {
	alias::{Alias, AliasOp},
	expr::{Expr, ExprCmpt},
	item::{DepItem, OutItem},
	pattern::{Pattern, PatternOp},
	rule::{Command, Exec, Rule},
	target::Target,
};

// Imports
use {
	crate::{util::CowArcStr, Ast},
	std::collections::HashMap,
};

/// Rules.
///
/// Stores all rules, along with associated information, such as
/// global aliases and the default target.
#[derive(Clone, Debug)]
pub struct Rules<'s> {
	/// Global aliases.
	///
	/// These are available for the whole program to
	/// use.
	pub aliases: HashMap<CowArcStr<'s>, Expr>,

	/// Default targets to build
	pub default: Vec<Target<Expr>>,

	/// Rules
	pub rules: HashMap<String, Rule<Expr>>,
}

impl<'s> Rules<'s> {
	/// Creates all rules from the ast
	#[must_use]
	pub fn new(ast: Ast<'s>) -> Self {
		let aliases = ast
			.aliases
			.into_iter()
			.map(|(alias, value)| (CowArcStr::from_cow(alias), Expr::new(value)))
			.collect();
		let default = ast.default.into_iter().map(Target::new).collect();
		let rules = ast
			.rules
			.into_iter()
			.map(|(name, rule)| (name.clone().into_owned(), Rule::new(name.into_owned(), rule)))
			.collect();

		Self {
			aliases,
			default,
			rules,
		}
	}
}
