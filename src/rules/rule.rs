//! Rule

// Imports
use {
	super::{DepItem, Expr, OutItem},
	crate::ast,
	indexmap::IndexMap,
	std::sync::Arc,
};

/// Rule
#[derive(Clone, Debug)]
pub struct Rule<T> {
	/// Name
	pub name: &'static str,

	/// Aliases
	pub aliases: Arc<IndexMap<&'static str, T>>,

	/// Output items
	pub output: Vec<OutItem<T>>,

	/// Dependencies
	pub deps: Vec<DepItem<T>>,

	/// Execution
	pub exec: Exec<T>,
}

impl Rule<Expr> {
	/// Creates a new rule from it's ast
	pub fn new(name: &'static str, rule: ast::Rule<'static>) -> Self {
		let aliases = rule
			.aliases
			.into_iter()
			.map(|(alias, expr)| (alias, Expr::new(expr)))
			.collect();
		let output = rule.out.into_iter().map(OutItem::new).collect();
		let deps = rule.deps.into_iter().map(DepItem::new).collect();
		let exec = Exec::new(rule.exec);

		Self {
			name,
			aliases: Arc::new(aliases),
			output,
			deps,
			exec,
		}
	}
}


/// Exec
#[derive(Clone, Debug)]
pub struct Exec<T> {
	/// Commands
	pub cmds: Vec<Command<T>>,
}

impl Exec<Expr> {
	/// Creates a new exec from it's ast
	pub fn new(exec: ast::Exec<'static>) -> Self {
		Self {
			cmds: exec.cmds.into_iter().map(Command::new).collect(),
		}
	}
}


/// Command
#[derive(Clone, Debug)]
pub struct Command<T> {
	/// Working directory
	pub cwd: Option<T>,

	/// All arguments
	pub args: Vec<CommandArg<T>>,
}

impl Command<Expr> {
	/// Creates a new command from it's ast
	pub fn new(cmd: ast::Command<'static>) -> Self {
		match cmd {
			ast::Command::OnlyArgs(args) => Self {
				cwd:  None,
				args: args.into_iter().map(CommandArg::new).collect(),
			},
			ast::Command::Full { cwd, args } => Self {
				cwd:  cwd.map(Expr::new),
				args: args.into_iter().map(CommandArg::new).collect(),
			},
		}
	}
}

/// Command argument
#[derive(Clone, Debug)]
pub enum CommandArg<T> {
	/// Expression
	Expr(T),
}

impl CommandArg<Expr> {
	/// Creates a new command argument from it's ast
	pub fn new(arg: ast::CommandArg<'static>) -> Self {
		match arg {
			ast::CommandArg::Expr(expr) => Self::Expr(Expr::new(expr)),
		}
	}
}
