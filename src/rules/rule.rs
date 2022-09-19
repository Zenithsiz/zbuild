//! Rule

// Imports
use {
	super::{DepItem, Expr, OutItem},
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
	pub output: Vec<OutItem<T>>,

	/// Dependencies
	pub deps: Vec<DepItem<T>>,

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
		let output = rule.out.into_iter().map(OutItem::new).collect();
		let deps = rule.deps.into_iter().map(DepItem::new).collect();
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
