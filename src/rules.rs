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
	alias::AliasOp,
	expr::{Expr, ExprCmpt},
	item::{DepItem, OutItem},
	pattern::PatternOp,
	rule::{Command, CommandArg, Exec, Rule},
	target::Target,
};

// Imports
use {crate::Ast, indexmap::IndexMap, std::sync::Arc};

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
	pub aliases: Arc<IndexMap<&'static str, Expr>>,

	/// Default targets to build
	pub default: Vec<Target<Expr>>,

	/// Rules
	pub rules: IndexMap<&'static str, Rule<Expr>>,
}

impl Rules {
	/// Creates all rules from the ast
	#[must_use]
	pub fn new(ast: Ast<'static>) -> Self {
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
			aliases: Arc::new(aliases),
			default,
			rules,
		}
	}
}
