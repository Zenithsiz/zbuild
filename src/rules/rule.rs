//! Rule

// Imports
use {
	super::{Expr, Item, RuleItem},
	crate::ast,
	std::collections::HashMap,
};

/// Rule
#[derive(Clone, Debug)]
pub struct Rule<T> {
	/// Name
	pub name: String,

	/// Aliases
	pub aliases: HashMap<String, T>,

	/// Output items
	pub output: Vec<Item<T>>,

	/// Dependencies
	pub deps: Vec<Item<T>>,

	/// Static dependencies
	pub static_deps: Vec<Item<T>>,

	/// Rule dependencies
	pub rule_deps: Vec<RuleItem<T>>,

	/// Execution working directory
	pub exec_cwd: Option<T>,

	/// Execution
	pub exec: Vec<Command<T>>,
}

impl Rule<Expr> {
	/// Creates a new rule from it's ast
	pub fn new(name: String, rule: ast::Rule) -> Self {
		let aliases = rule
			.alias
			.into_iter()
			.map(|(name, expr)| (name, Expr::new(expr)))
			.collect();
		let output = rule.out.into_iter().map(Item::new).collect();
		let deps = rule.deps.into_iter().map(Item::new).collect();
		let static_deps = rule.static_deps.into_iter().map(Item::new).collect();
		let rule_deps = rule.rule_deps.into_iter().map(RuleItem::new).collect();
		let exec_cwd = rule.exec_cwd.map(Expr::new);
		let exec = rule
			.exec
			.into_iter()
			.map(|command| Command {
				args: command.args.into_iter().map(Expr::new).collect(),
			})
			.collect();

		Self {
			name,
			aliases,
			output,
			deps,
			static_deps,
			rule_deps,
			exec_cwd,
			exec,
		}
	}
}

/// Command
#[derive(Clone, Debug)]
pub struct Command<T> {
	/// All arguments
	pub args: Vec<T>,
}
