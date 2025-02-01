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
	pub fn from_ast(zbuild_file: &ArcStr, rule: ast::RuleStmt<'_>) -> Result<Self, AppError> {
		let aliases = rule
			.aliases
			.into_iter()
			.map(|alias| {
				(
					zbuild_file.slice_from_str(alias.name.0),
					Expr::from_ast(zbuild_file, alias.value),
				)
			})
			.collect();
		let pats = rule
			.pats
			.into_iter()
			.map(|pat| {
				let name = zbuild_file.slice_from_str(pat.name.0);
				(name.clone(), Pattern { name, non_empty: false })
			})
			.collect();
		let output = rule
			.out
			.into_iter()
			.map(|out| OutItem::from_ast(zbuild_file, out))
			.collect::<Result<_, AppError>>()?;
		let deps = rule
			.deps
			.into_iter()
			.map(|dep| DepItem::from_ast(zbuild_file, dep))
			.collect();
		let exec = Exec {
			cmds: rule
				.exec
				.into_iter()
				.map(|cmd| Command::from_ast(zbuild_file, cmd))
				.collect(),
		};

		Ok(Self {
			name: zbuild_file.slice_from_str(rule.name.0),
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
	pub fn from_ast(zbuild_file: &ArcStr, cmd: ast::Command<'_>) -> Self {
		Self {
			cwd:  cmd.cwd.map(|cwd| Expr::from_ast(zbuild_file, cwd)),
			args: cmd
				.args
				.0
				.into_iter()
				.map(|arg| Expr::from_ast(zbuild_file, arg))
				.collect(),
		}
	}
}
