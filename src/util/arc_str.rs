//! Arc string

// Imports
use std::{
	borrow::Borrow,
	cmp,
	fmt,
	hash::{Hash, Hasher},
	ops::{Deref, Range},
	str::pattern::{Pattern, ReverseSearcher},
	sync::Arc,
};

/// Arc string.
///
/// Stores a string as a `(Arc<str>, Range<usize>)`,
/// to allow for fast indexing and cloning.
#[derive(Clone)]
pub struct ArcStr {
	/// Base string
	base: Arc<str>,

	/// Range
	range: Range<usize>,
}

impl ArcStr {
	/// Creates a sub-slice of `self` containing `s`.
	///
	/// # Panics
	/// `s` must be derived from this string, else this method panics.
	fn slice_from_str(&self, s: &str) -> Self {
		// Get pointer ranges
		let this = self.as_bytes().as_ptr_range();
		let s = s.as_bytes().as_ptr_range();

		let start_offset = s
			.start
			.addr()
			.checked_signed_diff(this.start.addr())
			.expect("Pointer difference overflowed");
		let end_offset = s
			.end
			.addr()
			.checked_signed_diff(this.end.addr())
			.expect("Pointer difference overflowed");
		assert!(start_offset >= 0 && end_offset <= 0, "Substring was larger than string");

		let start = self
			.range
			.start
			.checked_add_signed(start_offset)
			.expect("Pointer addition overflowed");
		let end = self
			.range
			.end
			.checked_add_signed(end_offset)
			.expect("Pointer addition overflowed");

		Self {
			base:  Arc::clone(&self.base),
			range: start..end,
		}
	}

	/// Wrapper for [`str::strip_prefix`]
	pub fn strip_prefix<P: Pattern>(&self, prefix: P) -> Option<Self> {
		(**self).strip_prefix(prefix).map(|s| self.slice_from_str(s))
	}

	/// Wrapper for [`str::strip_suffix`]
	pub fn strip_suffix<P: Pattern>(&self, suffix: P) -> Option<Self>
	where
		for<'a> P::Searcher<'a>: ReverseSearcher<'a>,
	{
		(**self).strip_suffix(suffix).map(|s| self.slice_from_str(s))
	}
}

impl PartialEq for ArcStr {
	fn eq(&self, other: &Self) -> bool {
		self.cmp(other).is_eq()
	}
}
impl Eq for ArcStr {}

impl PartialOrd for ArcStr {
	fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
		Some(self.cmp(other))
	}
}
impl Ord for ArcStr {
	fn cmp(&self, other: &Self) -> cmp::Ordering {
		(**self).cmp(&**other)
	}
}

impl Hash for ArcStr {
	fn hash<H: Hasher>(&self, state: &mut H) {
		(**self).hash(state);
	}
}

impl Default for ArcStr {
	fn default() -> Self {
		String::new().into()
	}
}

impl fmt::Display for ArcStr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		(**self).fmt(f)
	}
}
impl fmt::Debug for ArcStr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		(**self).fmt(f)
	}
}

impl Deref for ArcStr {
	type Target = str;

	fn deref(&self) -> &Self::Target {
		&self.base[self.range.clone()]
	}
}

impl Borrow<str> for ArcStr {
	fn borrow(&self) -> &str {
		self
	}
}

// TODO: Optimize both of these conversions for
//       when the `Arc` has no other strong references?
impl From<String> for ArcStr {
	fn from(s: String) -> Self {
		Self {
			range: 0..s.len(),
			base:  s.into(),
		}
	}
}

impl From<ArcStr> for String {
	fn from(s: ArcStr) -> Self {
		(*s).to_string()
	}
}

impl From<&str> for ArcStr {
	fn from(s: &str) -> Self {
		Self {
			base:  Arc::from(s),
			range: 0..s.len(),
		}
	}
}
