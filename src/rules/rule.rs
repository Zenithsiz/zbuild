//! Rule

// Imports
use {
	super::{DepItem, Expr, OutItem},
	crate::ast,
	std::collections::HashMap,
};

/// Rule
#[derive(Clone, Debug)]
pub struct Rule<'s, T> {
	/// Name
	pub name: &'s str,

	/// Aliases
	pub aliases: HashMap<&'s str, T>,

	/// Output items
	pub output: Vec<OutItem<T>>,

	/// Dependencies
	pub deps: Vec<DepItem<T>>,

	/// Execution
	pub exec: Exec<T>,
}

impl<'s> Rule<'s, Expr<'s>> {
	/// Creates a new rule from it's ast
	pub fn new(name: &'s str, rule: ast::Rule<'s>) -> Self {
		let aliases = rule
			.alias
			.into_iter()
			.map(|(alias, expr)| (alias, Expr::new(expr)))
			.collect();
		let output = rule.out.into_iter().map(OutItem::new).collect();
		let deps = rule.deps.into_iter().map(DepItem::new).collect();
		let exec = match rule.exec {
			ast::Exec::OnlyCmds(cmds) => Exec {
				cwd:  None,
				cmds: cmds
					.into_iter()
					.map(|cmd| Command {
						args: cmd.args.into_iter().map(Expr::new).collect(),
					})
					.collect(),
			},
			ast::Exec::Full { cwd, cmds } => Exec {
				cwd:  cwd.map(Expr::new),
				cmds: cmds
					.into_iter()
					.map(|cmd| Command {
						args: cmd.args.into_iter().map(Expr::new).collect(),
					})
					.collect(),
			},
		};

		Self {
			name,
			aliases,
			output,
			deps,
			exec,
		}
	}
}


/// Exec
#[derive(Clone, Debug)]
pub struct Exec<T> {
	/// Working directory
	pub cwd: Option<T>,

	/// Commands
	pub cmds: Vec<Command<T>>,
}


/// Command
#[derive(Clone, Debug)]
pub struct Command<T> {
	/// All arguments
	pub args: Vec<T>,
}
