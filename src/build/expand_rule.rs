//! Expanding rule

// Imports
use {
	super::{expand_expr, expand_expr::expand_expr_string},
	crate::{
		rules::{Command, Expr, Item, Rule, RuleItem},
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
) -> Result<Rule<String>, anyhow::Error> {
	// Helper function to expand an expression
	let expand_expr = for<'a> move |expr: &'a Expr| -> Result<String, AppError> {
		self::expand_expr_string(
			expr,
			&mut expand_expr::RuleVisitor::new(global_aliases, rule_aliases, rule_pats),
		)
	};

	// Helper function to expand items
	let expand_item = |item: &Item<Expr>| match item {
		Item::File(file) => expand_expr(file).map(Item::File),
		Item::DepsFile(file) => expand_expr(file).map(Item::DepsFile),
	};
	let expand_rule_item = |item: &RuleItem<Expr>| {
		anyhow::Ok(RuleItem {
			name: expand_expr(&item.name)?,
			pats: item
				.pats
				.iter()
				.map(|(pat, expr)| Ok((expand_expr(pat)?, expand_expr(expr)?)))
				.collect::<Result<_, anyhow::Error>>()?,
		})
	};

	let aliases = rule
		.aliases
		.iter()
		.map(|(name, expr)| try { (name.clone(), expand_expr(expr)?) })
		.collect::<Result<_, anyhow::Error>>()?;
	let output = rule.output.iter().map(expand_item).collect::<Result<_, _>>()?;
	let deps = rule.deps.iter().map(expand_item).collect::<Result<_, _>>()?;
	let static_deps = rule.static_deps.iter().map(expand_item).collect::<Result<_, _>>()?;
	let rule_deps = rule.rule_deps.iter().map(expand_rule_item).collect::<Result<_, _>>()?;
	let exec_cwd = rule.exec_cwd.as_ref().map(expand_expr).transpose()?;
	let exec = rule
		.exec
		.iter()
		.map(|command| {
			Ok::<_, anyhow::Error>(Command {
				args: command.args.iter().map(expand_expr).collect::<Result<_, _>>()?,
			})
		})
		.collect::<Result<_, _>>()?;


	Ok(Rule {
		name: rule.name.clone(),
		aliases,
		output,
		deps,
		static_deps,
		rule_deps,
		exec_cwd,
		exec,
	})
}
