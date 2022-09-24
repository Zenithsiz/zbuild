//! Visitors for expanding

// Imports
use {
	crate::{
		expand::{FlowControl, Visitor},
		rules::Expr,
	},
	std::collections::HashMap,
};


/// Visitor for global aliases.
///
/// Expands any defined aliases, errors on undefined ones
/// Errors any patterns.
#[derive(Clone, Copy, Debug)]
pub struct GlobalVisitor<'global> {
	/// Aliases
	aliases: &'global HashMap<String, Expr>,
}

impl<'global> GlobalVisitor<'global> {
	/// Creates a new global visitor
	pub const fn new(aliases: &'global HashMap<String, Expr>) -> Self {
		Self { aliases }
	}
}

impl<'global> Visitor for GlobalVisitor<'global> {
	fn visit_alias(&mut self, alias_name: &str) -> FlowControl<Expr> {
		self.aliases
			.get(alias_name)
			.cloned()
			.map_or(FlowControl::Error, FlowControl::ExpandTo)
	}

	fn visit_pat(&mut self, _pat: &str) -> FlowControl<String> {
		FlowControl::Error
	}
}

/// Visitor for rule output
///
/// Expands any defined aliases (first with the rule aliases, then global), errors on undefined ones
/// Keeps any patterns.
#[derive(Clone, Copy, Debug)]
pub struct RuleOutputVisitor<'global, 'rule> {
	/// Global aliases
	global_aliases: &'global HashMap<String, Expr>,

	/// Rule aliases
	rule_aliases: &'rule HashMap<String, Expr>,
}

impl<'global, 'rule> RuleOutputVisitor<'global, 'rule> {
	/// Creates a new rule output expression visitor
	pub const fn new(
		global_aliases: &'global HashMap<String, Expr>,
		rule_aliases: &'rule HashMap<String, Expr>,
	) -> Self {
		Self {
			global_aliases,
			rule_aliases,
		}
	}
}

impl<'global, 'rule> Visitor for RuleOutputVisitor<'global, 'rule> {
	fn visit_alias(&mut self, alias_name: &str) -> FlowControl<Expr> {
		match self.rule_aliases.get(alias_name).cloned() {
			Some(expr) => FlowControl::ExpandTo(expr),
			None => self
				.global_aliases
				.get(alias_name)
				.cloned()
				.map_or(FlowControl::Error, FlowControl::ExpandTo),
		}
	}

	fn visit_pat(&mut self, _pat: &str) -> FlowControl<String> {
		FlowControl::Keep
	}
}

/// Visitor for rule
///
/// Expands any defined aliases (first with the rule aliases, then global), errors on undefined ones
/// Expands any defined patterns, errors on undefined
#[derive(Clone, Copy, Debug)]
pub struct RuleVisitor<'global, 'rule, 'pats> {
	/// Global aliases
	global_aliases: &'global HashMap<String, Expr>,

	/// Rule aliases
	rule_aliases: &'rule HashMap<String, Expr>,

	/// Rule patterns
	rule_pats: &'pats HashMap<String, String>,
}

impl<'global, 'rule, 'pats> RuleVisitor<'global, 'rule, 'pats> {
	/// Creates a new rule visitor
	pub const fn new(
		global_aliases: &'global HashMap<String, Expr>,
		rule_aliases: &'rule HashMap<String, Expr>,
		rule_pats: &'pats HashMap<String, String>,
	) -> Self {
		Self {
			global_aliases,
			rule_aliases,
			rule_pats,
		}
	}
}

impl<'global, 'rule, 'pats> Visitor for RuleVisitor<'global, 'rule, 'pats> {
	fn visit_alias(&mut self, alias_name: &str) -> FlowControl<Expr> {
		match self.rule_aliases.get(alias_name).cloned() {
			Some(expr) => FlowControl::ExpandTo(expr),
			None => self
				.global_aliases
				.get(alias_name)
				.cloned()
				.map_or(FlowControl::Error, FlowControl::ExpandTo),
		}
	}

	fn visit_pat(&mut self, pat_name: &str) -> FlowControl<String> {
		self.rule_pats
			.get(pat_name)
			.cloned()
			.map_or(FlowControl::Error, FlowControl::ExpandTo)
	}
}
