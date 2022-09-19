//! Expanding rule

// Imports
use {
	super::{expand_expr, expand_expr::expand_expr_string},
	crate::{
		rules::{Command, DepItem, Exec, Expr, OutItem, Rule},
		AppError,
	},
	std::collections::HashMap,
};

/// Expands a rule of all it's aliases and patterns
pub fn expand_rule(
	rule: &Rule<Expr>,
	global_aliases: &HashMap<String, Expr>,
	rule_aliases: &HashMap<String, Expr>,
	rule_pats: &HashMap<String, String>,
) -> Result<Rule<String>, AppError> {
	// Helper function to expand an expression
	let expand_expr = for<'a> move |expr: &'a Expr| -> Result<String, AppError> {
		self::expand_expr_string(
			expr,
			&mut expand_expr::RuleVisitor::new(global_aliases, rule_aliases, rule_pats),
		)
	};

	// Helper functions to expand items
	let expand_out_item = |item: &OutItem<Expr>| match item {
		OutItem::File { file } => Ok::<_, AppError>(OutItem::File {
			file: expand_expr(file)?,
		}),
		OutItem::DepsFile { file } => Ok::<_, AppError>(OutItem::DepsFile {
			file: expand_expr(file)?,
		}),
	};
	let expand_dep_item = |item: &DepItem<Expr>| match *item {
		DepItem::File { ref file, is_static } => Ok::<_, AppError>(DepItem::File {
			file: expand_expr(file)?,
			is_static,
		}),
		DepItem::DepsFile { ref file, is_static } => Ok::<_, AppError>(DepItem::DepsFile {
			file: expand_expr(file)?,
			is_static,
		}),
		DepItem::Rule { ref name, ref pats } => Ok::<_, AppError>(DepItem::Rule {
			name: expand_expr(name)?,
			pats: pats
				.iter()
				.map(|(pat, expr)| Ok((expand_expr(pat)?, expand_expr(expr)?)))
				.collect::<Result<_, AppError>>()?,
		}),
	};

	let aliases = rule
		.aliases
		.iter()
		.map(|(name, expr)| try { (name.clone(), expand_expr(expr)?) })
		.collect::<Result<_, AppError>>()?;
	let output = rule.output.iter().map(expand_out_item).collect::<Result<_, _>>()?;
	let deps = rule.deps.iter().map(expand_dep_item).collect::<Result<_, _>>()?;
	let exec = Exec {
		cwd:  rule.exec.cwd.as_ref().map(expand_expr).transpose()?,
		cmds: rule
			.exec
			.cmds
			.iter()
			.map(|cmd| {
				Ok::<_, AppError>(Command {
					args: cmd.args.iter().map(expand_expr).collect::<Result<_, _>>()?,
				})
			})
			.collect::<Result<_, _>>()?,
	};


	Ok(Rule {
		name: rule.name.clone(),
		aliases,
		output,
		deps,
		exec,
	})
}
