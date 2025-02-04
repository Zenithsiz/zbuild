//! Expression tree

use {
	super::Expr,
	crate::{rules::pattern::Pattern, util::ArcStr, AppError},
	indexmap::IndexMap,
	itertools::{Itertools, PeekingNext},
	smallvec::SmallVec,
	std::collections::BTreeMap,
};

/// An expression tree.
// TODO: Currently we just match against all possible prefixes and suffixes.
//       Try to actually create a fast, non `O(n)`, algorithm?
#[derive(Debug)]
pub struct ExprTree<K> {
	/// Prefixes
	prefixes: PrefixTree<K>,
}

// TODO: Flatten this?
type PrefixTree<K> = BTreeMap<ArcStr, SuffixTree<K>>;
type SuffixTree<K> = BTreeMap<ArcStr, (Option<Pattern>, K)>;

impl<K> ExprTree<K> {
	/// Creates a new, empty, expression tree
	pub const fn new() -> Self {
		Self {
			prefixes: BTreeMap::new(),
		}
	}

	/// Adds an expression to the suffix tree, associated with a key.
	///
	/// The expression must not contain any aliases.
	///
	/// Returns the old key if the expression already existed.
	pub fn insert(&mut self, expr: &Expr, key: K, pats: &[&IndexMap<ArcStr, Pattern>]) -> Result<Option<K>, AppError> {
		let mut cmpts = expr.cmpts.iter();

		// Get all components from the start that are strings
		let prefix = cmpts
			.by_ref()
			.peeking_take_while(|cmpt| cmpt.is_string())
			.map(|cmpt| &**cmpt.as_string().expect("Just checked"))
			.collect::<String>();
		let prefix = ArcStr::from(prefix);

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
			.peeking_take_while(|cmpt| cmpt.is_string())
			.map(|cmpt| &**cmpt.as_string().expect("Just checked"))
			.collect::<String>();
		let suffix = ArcStr::from(suffix);

		// After this the expression should be empty
		if let Some(cmpt) = cmpts.next() {
			zutil_app_error::bail!("Unexpected component in expression {expr}: {cmpt}");
		}

		// Finally try to insert and retrieve the old key, if any.
		let old_key = self
			.prefixes
			.entry(prefix)
			.or_default()
			.insert(suffix, (pat, key))
			.map(|(_, old_key)| old_key);

		Ok(old_key)
	}

	/// Matches a string against this expression tree.
	///
	/// Returns the first match with patterns resolved.
	pub fn find(&self, value: &ArcStr) -> Option<(K, Patterns)>
	where
		K: Clone,
	{
		for (prefix, suffixes) in &self.prefixes {
			// If the prefix no longer matches, try the next
			let Some(value_rest) = value.strip_prefix(&**prefix) else {
				continue;
			};

			// Try to find match the suffixes
			if let Some((key, pats)) = Self::find_match_suffix(&value_rest, suffixes) {
				return Some((key, pats));
			}
		}

		None
	}

	/// Finds a matching suffix for `value` from the suffix map.
	fn find_match_suffix(value: &ArcStr, suffixes: &SuffixTree<K>) -> Option<(K, Patterns)>
	where
		K: Clone,
	{
		// Otherwise, match against all other suffixes
		for (suffix, (pat, key)) in suffixes {
			// If the prefix no longer matches, try the next
			let Some(pat_value) = value.strip_suffix(&**suffix) else {
				continue;
			};

			// Otherwise, we might have found the final value, so test it
			if let Some(pats) = Self::find_match_pat(&pat_value, pat.as_ref()) {
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
