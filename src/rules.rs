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
	rule::{Command, CommandArg, Exec, Rule},
	target::Target,
};

// Imports
use {crate::Ast, std::collections::HashMap};

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
	pub aliases: HashMap<&'s str, Expr<'s>>,

	/// Default targets to build
	pub default: Vec<Target<'s, Expr<'s>>>,

	/// Rules
	pub rules: HashMap<&'s str, Rule<'s, Expr<'s>>>,
}

impl<'s> Rules<'s> {
	/// Creates all rules from the ast
	#[must_use]
	pub fn new(ast: Ast<'s>) -> Self {
		let aliases = ast
			.aliases
			.into_iter()
			.map(|(alias, value)| (alias, Expr::new(value)))
			.collect();
		let default = ast.default.into_iter().map(Target::new).collect();
		let rules = ast
			.rules
			.into_iter()
			.map(|(name, rule)| (name, Rule::new(name, rule)))
			.collect();

		Self {
			aliases,
			default,
			rules,
		}
	}
}
