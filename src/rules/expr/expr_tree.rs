//! Expression tree

use {
	super::Expr,
	crate::{
		error::AppError,
		rules::{pattern::Pattern, PatternOp},
		util::ArcStr,
	},
	itertools::{Itertools, PeekingNext},
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
	pub fn insert(&mut self, expr: &Expr, key: K) -> Result<Option<K>, AppError> {
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
			.peeking_next(|cmpt| cmpt.is_pattern())
			.map(|cmpt| cmpt.as_pattern().expect("Just checked"))
			.cloned();

		// Then get the rest of the string
		let suffix = cmpts
			.peeking_take_while(|cmpt| cmpt.is_string())
			.map(|cmpt| &**cmpt.as_string().expect("Just checked"))
			.collect::<String>();
		let suffix = ArcStr::from(suffix);

		// After this the expression should be empty
		if let Some(cmpt) = cmpts.next() {
			return Err(AppError::Other(anyhow::anyhow!(
				"Unexpected component in expression {expr}: {cmpt}"
			)));
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
	// TODO: Since we only support a single pattern, return `Option<ArcStr>` for it instead.
	pub fn find(&self, value: &str) -> Option<(K, BTreeMap<ArcStr, ArcStr>)>
	where
		K: Clone,
	{
		for (prefix, suffixes) in &self.prefixes {
			// If the prefix no longer matches, try the next
			let Some(value_rest) = value.strip_prefix(&**prefix) else {
				continue;
			};

			// Try to find match the suffixes
			if let Some((key, pats)) = Self::find_match_suffix(value_rest, suffixes) {
				return Some((key, pats));
			}
		}

		None
	}

	/// Finds a matching suffix for `value` from the suffix map.
	fn find_match_suffix(value: &str, suffixes: &SuffixTree<K>) -> Option<(K, BTreeMap<ArcStr, ArcStr>)>
	where
		K: Clone,
	{
		// Try to match against an empty suffix
		// Note: We always do this, since some expressions could be
		//       of the form `(String, Pat, Empty)`, with `Pat` matching
		//       the rest.
		if let Some((pat, key)) = suffixes.get("") &&
			let Some(pats) = Self::find_match_pat("", pat)
		{
			return Some((key.clone(), pats));
		}

		// Otherwise, match against all other suffixes
		for (suffix, (pat, key)) in suffixes {
			// If the prefix no longer matches, try the next
			let Some(pat_value) = value.strip_suffix(&**suffix) else {
				continue;
			};

			// Otherwise, we might have found the final value, so test it
			if let Some(pats) = Self::find_match_pat(pat_value, pat) {
				return Some((key.clone(), pats));
			}
		}

		None
	}

	/// Matches a pattern against a remaining value after it's prefix and suffix have been stripped
	fn find_match_pat(value: &str, pat: &Option<Pattern>) -> Option<BTreeMap<ArcStr, ArcStr>> {
		let pats = match pat {
			// If there is any pattern, try to match it
			Some(pat) => {
				for op in &pat.ops {
					match op {
						// If it needs to be non-empty, check
						PatternOp::NonEmpty => match value.is_empty() {
							true => return None,
							false => continue,
						},
					}
				}

				BTreeMap::from([(pat.name.clone(), value.into())])
			},

			// Otherwise, we match if the value is empty
			_ => match value.is_empty() {
				true => BTreeMap::new(),
				false => return None,
			},
		};

		Some(pats)
	}
}
