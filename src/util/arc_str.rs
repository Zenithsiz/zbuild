//! Arc string

// Imports
use {
	std::{
		borrow::Borrow,
		cmp,
		fmt,
		hash::{Hash, Hasher},
		mem,
		ops::{Deref, Range},
		str::pattern::{Pattern, ReverseSearcher},
		sync::Arc,
	},
	yoke::Yoke,
};

/// Arc string.
///
/// Stores a string as a (theoretical) `(Arc<str>, Range<usize>)`,
/// to allow for fast indexing and cloning.
///
/// The actual implementation stores a `(&str, Arc<String>)` for fast
/// access to the string, while retaining the ability to be cheaply
/// accessible as a `String`.
#[derive(Clone)]
pub struct ArcStr {
	/// Inner
	// Note: We need an `Arc<String>` for efficient conversion to/from `String`
	inner: Yoke<&'static str, Arc<String>>,
}

impl ArcStr {
	/// Returns the range of this string compared to the base
	fn base_range(&self) -> Range<usize> {
		self.inner
			.backing_cart()
			.substr_range(self)
			.expect("String pointer should be within allocation")
	}

	/// Updates this string as a `&mut String`.
	///
	/// Copies the string unless no other copies exist
	pub fn with_mut<F, R>(&mut self, f: F) -> R
	where
		F: FnOnce(&mut String) -> R,
	{
		// Get the range of our specific string
		let range = self.base_range();

		// Get the inner string
		let mut inner = mem::take(self).inner.into_backing_cart();
		let s = match Arc::get_mut(&mut inner) {
			// If we're unique, slice the parts we don't care about and return
			Some(s) => {
				s.truncate(range.end);
				let _ = s.drain(..range.start);

				s
			},

			// Otherwise copy
			None => {
				inner = Arc::new(inner[range].to_owned());
				Arc::get_mut(&mut inner).expect("Should be unique")
			},
		};

		// Then mutate
		let output = f(s);

		// And finally, reconstruct ourselves
		*self = Self::from(inner);

		output
	}

	/// Creates a sub-slice of `self` containing `s`.
	///
	/// # Panics
	/// `s` must be derived from this string, else this method panics.
	pub fn slice_from_str(&self, s: &str) -> Self {
		let range = self.substr_range(s).expect("Input was not a substring of this string");
		let inner = self.inner.map_project_cloned(|s, _| &s[range]);
		Self { inner }
	}

	/// Slices this string
	pub fn slice<S>(&self, slice: S) -> Self
	where
		str: std::ops::Index<S, Output = str>,
	{
		self.slice_from_str(&self[slice])
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
		self.inner.get()
	}
}

impl Borrow<str> for ArcStr {
	fn borrow(&self) -> &str {
		self
	}
}

impl From<String> for ArcStr {
	fn from(s: String) -> Self {
		Self::from(Arc::new(s))
	}
}

impl From<Arc<String>> for ArcStr {
	fn from(s: Arc<String>) -> Self {
		Self {
			inner: Yoke::attach_to_cart(s, |s| &**s),
		}
	}
}

impl From<ArcStr> for String {
	fn from(s: ArcStr) -> Self {
		// Get the range of our specific string
		let range = s.base_range();

		let inner = s.inner.into_backing_cart();
		match Arc::try_unwrap(inner) {
			// If we're unique, slice the parts we don't care about and return
			Ok(mut inner) => {
				inner.truncate(range.end);
				let _ = inner.drain(..range.start);

				inner
			},

			// Otherwise copy
			Err(inner) => inner[range].to_owned(),
		}
	}
}
