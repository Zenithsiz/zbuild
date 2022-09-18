//! Expanding rule

// Imports
use {
	super::{expand_expr, expand_expr::expand_expr_string},
	crate::rules::{Command, Expr, Item, Rule, RuleItem},
	std::collections::HashMap,
};

/// Expands a rule of all it's aliases and patterns
pub fn expand_rule(
	rule: &Rule<Expr>,
	rule_output_expr_visitor: expand_expr::RuleOutputVisitor,
	pats: &HashMap<String, String>,
) -> Result<Rule<String>, anyhow::Error> {
	// Helper function to expand an expression
	let mut rule_expr_visitor = expand_expr::RuleVisitor::new(rule_output_expr_visitor, pats);
	let mut expand_expr = |expr| self::expand_expr_string(expr, &mut rule_expr_visitor);

	// Helper function to expand an item
	let mut rule_expr_visitor = expand_expr::RuleVisitor::new(rule_output_expr_visitor, pats);
	let mut expand_item = |item: &Item<Expr>| match item {
		Item::File(file) => self::expand_expr_string(file, &mut rule_expr_visitor).map(Item::File),
		Item::DepsFile(file) => self::expand_expr_string(file, &mut rule_expr_visitor).map(Item::DepsFile),
	};
	let expand_rule_item = |item: &RuleItem<Expr>, rule_expr_visitor: &mut _| {
		Ok::<_, anyhow::Error>(RuleItem {
			name: self::expand_expr_string(&item.name, rule_expr_visitor)?,
			pats: item
				.pats
				.iter()
				.map(|(pat, expr)| {
					Ok((
						self::expand_expr_string(pat, rule_expr_visitor)?,
						self::expand_expr_string(expr, rule_expr_visitor)?,
					))
				})
				.collect::<Result<_, anyhow::Error>>()?,
		})
	};

	let aliases = rule
		.aliases
		.iter()
		.map(|(name, expr)| expand_expr(expr).map(|expr| (name.clone(), expr)))
		.collect::<Result<_, _>>()?;
	let output = rule.output.iter().map(&mut expand_item).collect::<Result<_, _>>()?;
	let deps = rule.deps.iter().map(&mut expand_item).collect::<Result<_, _>>()?;
	let static_deps = rule
		.static_deps
		.iter()
		.map(&mut expand_item)
		.collect::<Result<_, _>>()?;
	let rule_deps = rule
		.rule_deps
		.iter()
		.map(|item| expand_rule_item(item, &mut rule_expr_visitor))
		.collect::<Result<_, _>>()?;
	let exec_cwd = rule.exec_cwd.as_ref().map(&mut expand_expr).transpose()?;
	let exec = rule
		.exec
		.iter()
		.map(|command| {
			Ok::<_, anyhow::Error>(Command {
				args: command.args.iter().map(&mut expand_expr).collect::<Result<_, _>>()?,
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
