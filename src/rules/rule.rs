//! Rule

// Imports
use {
	super::{pattern::Pattern, DepItem, Expr, OutItem},
	crate::{ast, util::ArcStr, AppError},
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

	/// Patterns
	pub pats: Arc<IndexMap<ArcStr, Pattern>>,

	/// Output items
	pub output: Vec<OutItem<T>>,

	/// Dependencies
	pub deps: Vec<DepItem<T>>,

	/// Execution
	pub exec: Exec<T>,
}

impl Rule<Expr> {
	/// Creates a new rule from it's ast
	pub fn from_ast(rule: ast::RuleStmt) -> Result<Self, AppError> {
		let aliases = rule
			.aliases
			.into_iter()
			.map(|alias| (alias.name.0, Expr::from_ast(alias.value)))
			.collect();
		let pats = rule
			.pats
			.into_iter()
			.map(|pat| {
				(pat.name.0.clone(), Pattern {
					name:      pat.name.0,
					non_empty: false,
				})
			})
			.collect();
		let output = rule
			.out
			.into_iter()
			.map(OutItem::from_ast)
			.collect::<Result<_, AppError>>()?;
		let deps = rule.deps.into_iter().map(DepItem::from_ast).collect();
		let exec = Exec {
			cmds: rule.exec.into_iter().map(Command::from_ast).collect(),
		};

		Ok(Self {
			name: rule.name.0,
			aliases: Arc::new(aliases),
			pats: Arc::new(pats),
			output,
			deps,
			exec,
		})
	}
}


/// Exec
#[derive(Clone, Debug)]
pub struct Exec<T> {
	/// Commands
	pub cmds: Vec<Command<T>>,
}

/// Command
#[derive(Clone, Debug)]
pub struct Command<T> {
	/// Working directory
	pub cwd: Option<T>,

	/// All arguments
	pub args: Vec<T>,
}

impl Command<Expr> {
	/// Creates a new command from it's ast
	pub fn from_ast(cmd: ast::Command) -> Self {
		Self {
			cwd:  cmd.cwd.map(Expr::from_ast),
			args: cmd.args.0.into_iter().map(Expr::from_ast).collect(),
		}
	}
}
