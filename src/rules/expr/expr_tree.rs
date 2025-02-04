//! Expression tree

use {
	super::Expr,
	crate::{rules::pattern::Pattern, util::ArcStr, AppError},
	itertools::PeekingNext,
	smallvec::SmallVec,
	std::collections::HashMap,
};

/// An expression tree.
// TODO: Currently we just match against all possible prefixes and suffixes.
//       Try to actually create a fast, non `O(n)`, algorithm?
#[derive(Debug)]
pub struct ExprTree<K> {
	/// Matches
	matches: HashMap<(ArcStr, ArcStr), (K, Option<Pattern>)>,
}

impl<K> ExprTree<K> {
	/// Creates a new, empty, expression tree
	pub fn new() -> Self {
		Self {
			matches: HashMap::new(),
		}
	}

	/// Adds an expression to the expression tree, associated with a key.
	///
	/// The expression must not contain any aliases.
	///
	/// Returns the old key if the expression already existed.
	pub fn insert(&mut self, expr: &Expr, key: K, pats: &[&HashMap<ArcStr, Pattern>]) -> Result<Option<K>, AppError> {
		let mut cmpts = expr.cmpts.iter();

		// Get all components from the start that are strings
		let prefix = cmpts
			.peeking_next(|cmpt| cmpt.is_string())
			.map(|cmpt| cmpt.as_string().expect("Just checked"))
			.cloned()
			.unwrap_or_default();

		// Get the (possible) pattern in the middle
		let pat = cmpts
			.peeking_next(|cmpt| match cmpt {
				super::ExprCmpt::String(_) => false,
				super::ExprCmpt::Ident { name, .. } => pats.iter().any(|pats| pats.contains_key(name)),
			})
			.map(|cmpt| match cmpt {
				super::ExprCmpt::String(_) => unreachable!("Just checked"),
				super::ExprCmpt::Ident { name, .. } => {
					for pats in pats {
						if let Some(pat) = pats.get(name) {
							return pat;
						}
					}

					unreachable!("Just checked")
				},
			})
			.cloned();

		// Then get the rest of the string
		let suffix = cmpts
			.peeking_next(|cmpt| cmpt.is_string())
			.map(|cmpt| cmpt.as_string().expect("Just checked"))
			.cloned()
			.unwrap_or_default();

		// After this the expression should be empty
		if let Some(cmpt) = cmpts.next() {
			zutil_app_error::bail!("Unexpected component in expression {expr}: {cmpt}");
		}

		// Finally try to insert and retrieve the old key, if any.
		let old_key = self
			.matches
			.insert((prefix, suffix), (key, pat))
			.map(|(old_key, _)| old_key);

		Ok(old_key)
	}

	/// Matches a string against this expression tree.
	///
	/// Returns the first match with patterns resolved.
	pub fn find(&self, value: &ArcStr) -> Option<(K, Patterns)>
	where
		K: Clone,
	{
		for ((prefix, suffix), (key, pat)) in &self.matches {
			// If the prefix no longer matches, try the next
			let Some(value) = value.strip_prefix(&**prefix) else {
				continue;
			};

			// If the prefix no longer matches, try the next
			let Some(value) = value.strip_suffix(&**suffix) else {
				continue;
			};

			// Otherwise, we might have found the final value, so test it
			if let Some(pats) = Self::find_match_pat(&value, pat.as_ref()) {
				return Some((key.clone(), pats));
			}
		}

		None
	}

	/// Matches a pattern against a remaining value after it's prefix and suffix have been stripped
	fn find_match_pat(value: &ArcStr, pat: Option<&Pattern>) -> Option<Patterns> {
		let pats = match pat {
			// If there is any pattern, try to match it
			Some(pat) => {
				if pat.non_empty && value.is_empty() {
					return None;
				}

				SmallVec::from([(pat.name.clone(), value.clone())])
			},

			// Otherwise, we match if the value is empty
			None => match value.is_empty() {
				true => SmallVec::new(),
				false => return None,
			},
		};

		Some(pats)
	}
}

/// Patterns
pub type Patterns = SmallVec<[(ArcStr, ArcStr); 1]>;
