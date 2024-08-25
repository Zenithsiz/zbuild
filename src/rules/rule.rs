//! Rule

// Imports
use {
	super::{DepItem, Expr, OutItem},
	crate::{ast, util::ArcStr},
	indexmap::IndexMap,
	std::sync::Arc,
};

/// Rule
#[derive(Clone, Debug)]
pub struct Rule<T> {
	/// Name
	pub name: ArcStr,

	/// Aliases
	pub aliases: Arc<IndexMap<ArcStr, T>>,

	/// Output items
	pub output: Vec<OutItem<T>>,

	/// Dependencies
	pub deps: Vec<DepItem<T>>,

	/// Execution
	pub exec: Exec<T>,
}

impl Rule<Expr> {
	/// Creates a new rule from it's ast
	pub fn new(zbuild_file: &ArcStr, name: ArcStr, rule: ast::Rule<'_>) -> Self {
		let aliases = rule
			.aliases
			.into_iter()
			.map(|(alias, expr)| (zbuild_file.slice_from_str(alias), Expr::new(zbuild_file, expr)))
			.collect();
		let output = rule.out.into_iter().map(|out| OutItem::new(zbuild_file, out)).collect();
		let deps = rule
			.deps
			.into_iter()
			.map(|dep| DepItem::new(zbuild_file, dep))
			.collect();
		let exec = Exec::new(zbuild_file, rule.exec);

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
	pub fn new(zbuild_file: &ArcStr, exec: ast::Exec<'_>) -> Self {
		Self {
			cmds: exec
				.cmds
				.into_iter()
				.map(|cmd| Command::new(zbuild_file, cmd))
				.collect(),
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
	pub fn new(zbuild_file: &ArcStr, cmd: ast::Command<'_>) -> Self {
		match cmd {
			ast::Command::OnlyArgs(args) => Self {
				cwd:  None,
				args: args.into_iter().map(|arg| CommandArg::new(zbuild_file, arg)).collect(),
			},
			ast::Command::Full { cwd, args } => Self {
				cwd:  cwd.map(|cwd| Expr::new(zbuild_file, cwd)),
				args: args.into_iter().map(|arg| CommandArg::new(zbuild_file, arg)).collect(),
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
	pub fn new(zbuild_file: &ArcStr, arg: ast::CommandArg<'_>) -> Self {
		match arg {
			ast::CommandArg::Expr(expr) => Self::Expr(Expr::new(zbuild_file, expr)),
		}
	}
}
