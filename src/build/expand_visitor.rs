//! Visitors for expanding

// Imports
use {
	crate::{
		expand::{FlowControl, Visitor},
		rules::Expr,
		util::CowStr,
	},
	std::collections::HashMap,
};


/// Visitor for global aliases.
///
/// Expands any defined aliases, errors on undefined ones
/// Errors any patterns.
#[derive(Clone, Copy, Debug)]
pub struct GlobalVisitor<'s, 'global> {
	/// Aliases
	aliases: &'global HashMap<&'s str, Expr<'s>>,
}

impl<'s, 'global> GlobalVisitor<'s, 'global> {
	/// Creates a new global visitor
	pub const fn new(aliases: &'global HashMap<&'s str, Expr<'s>>) -> Self {
		Self { aliases }
	}
}

impl<'s, 'global> Visitor<'s> for GlobalVisitor<'s, 'global> {
	fn visit_alias(&mut self, alias_name: &str) -> FlowControl<Expr<'s>> {
		self.aliases
			.get(alias_name)
			.cloned()
			.map_or(FlowControl::Error, FlowControl::ExpandTo)
	}

	fn visit_pat(&mut self, _pat: &str) -> FlowControl<CowStr<'s>> {
		FlowControl::Error
	}
}

/// Visitor for rule output
///
/// Expands any defined aliases (first with the rule aliases, then global), errors on undefined ones
/// Keeps any patterns.
#[derive(Clone, Copy, Debug)]
pub struct RuleOutputVisitor<'s, 'global, 'rule> {
	/// Global aliases
	global_aliases: &'global HashMap<&'s str, Expr<'s>>,

	/// Rule aliases
	rule_aliases: &'rule HashMap<&'s str, Expr<'s>>,
}

impl<'s, 'global, 'rule> RuleOutputVisitor<'s, 'global, 'rule> {
	/// Creates a new rule output expression visitor
	pub const fn new(
		global_aliases: &'global HashMap<&'s str, Expr<'s>>,
		rule_aliases: &'rule HashMap<&'s str, Expr<'s>>,
	) -> Self {
		Self {
			global_aliases,
			rule_aliases,
		}
	}
}

impl<'s, 'global, 'rule> Visitor<'s> for RuleOutputVisitor<'s, 'global, 'rule> {
	fn visit_alias(&mut self, alias_name: &str) -> FlowControl<Expr<'s>> {
		match self.rule_aliases.get(alias_name).cloned() {
			Some(expr) => FlowControl::ExpandTo(expr),
			None => self
				.global_aliases
				.get(alias_name)
				.cloned()
				.map_or(FlowControl::Error, FlowControl::ExpandTo),
		}
	}

	fn visit_pat(&mut self, _pat: &str) -> FlowControl<CowStr<'s>> {
		FlowControl::Keep
	}
}

/// Visitor for rule
///
/// Expands any defined aliases (first with the rule aliases, then global), errors on undefined ones
/// Expands any defined patterns, errors on undefined
#[derive(Clone, Copy, Debug)]
pub struct RuleVisitor<'s, 'global, 'rule, 'pats> {
	/// Global aliases
	global_aliases: &'global HashMap<&'s str, Expr<'s>>,

	/// Rule aliases
	rule_aliases: &'rule HashMap<&'s str, Expr<'s>>,

	/// Rule patterns
	rule_pats: &'pats HashMap<CowStr<'s>, CowStr<'s>>,
}

impl<'s, 'global, 'rule, 'pats> RuleVisitor<'s, 'global, 'rule, 'pats> {
	/// Creates a new rule visitor
	pub const fn new(
		global_aliases: &'global HashMap<&'s str, Expr<'s>>,
		rule_aliases: &'rule HashMap<&'s str, Expr<'s>>,
		rule_pats: &'pats HashMap<CowStr<'s>, CowStr<'s>>,
	) -> Self {
		Self {
			global_aliases,
			rule_aliases,
			rule_pats,
		}
	}
}

impl<'s, 'global, 'rule, 'pats> Visitor<'s> for RuleVisitor<'s, 'global, 'rule, 'pats> {
	fn visit_alias(&mut self, alias_name: &str) -> FlowControl<Expr<'s>> {
		match self.rule_aliases.get(alias_name).cloned() {
			Some(expr) => FlowControl::ExpandTo(expr),
			None => self
				.global_aliases
				.get(alias_name)
				.cloned()
				.map_or(FlowControl::Error, FlowControl::ExpandTo),
		}
	}

	fn visit_pat(&mut self, pat_name: &str) -> FlowControl<CowStr<'s>> {
		self.rule_pats
			.get(pat_name)
			.cloned()
			.map_or(FlowControl::Error, FlowControl::ExpandTo)
	}
}
