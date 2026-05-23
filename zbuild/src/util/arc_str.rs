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
	inner: Yoke<&'static str, Option<Arc<String>>>,
}

impl ArcStr {
	/// Creates a new arc string from a static string
	pub const fn from_static(s: &'static str) -> Self {
		Self {
			inner: Yoke::new_owned(s),
		}
	}

	/// Returns the range of this string compared to the base
	fn base_range(&self) -> Range<usize> {
		match self.inner.backing_cart() {
			// If we're backed by anything, check in relation to it
			Some(inner) => inner
				.substr_range(self)
				.expect("String pointer should be within allocation")
				.into(),

			// Otherwise, we're all of ourselves
			None => 0..self.len(),
		}
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
		let inner = mem::take(self).inner;
		let output;
		*self = match inner.try_into_yokeable() {
			Ok(s) => {
				let mut s = s.to_owned();
				output = f(&mut s);
				Self::from(s)
			},
			Err(inner) => {
				// TODO: Get rid of this panic here?
				let mut inner = inner.into_backing_cart().expect("Should have a cart");
				let s = match Arc::get_mut(&mut inner) {
					// If we're unique, slice the parts we don't care about and return
					Some(s) => {
						s.truncate(range.end);
						drop(s.drain(..range.start));

						s
					},

					// Otherwise copy
					None => {
						inner = Arc::new(inner[range].to_owned());
						Arc::get_mut(&mut inner).expect("Should be unique")
					},
				};
				output = f(s);
				Self::from(inner)
			},
		};

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

impl From<&'_ str> for ArcStr {
	fn from(s: &str) -> Self {
		Self::from(s.to_owned())
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
			inner: Yoke::attach_to_cart(s, |s| &**s).wrap_cart_in_option(),
		}
	}
}

impl From<ArcStr> for String {
	fn from(s: ArcStr) -> Self {
		// Get the range of our specific string
		let range = s.base_range();

		match s.inner.try_into_yokeable() {
			// If we're not backed by anything, just extend the string
			Ok(s) => s.to_owned(),

			// Otherwise, wrap it
			Err(inner) => match Arc::try_unwrap(inner.into_backing_cart().expect("Should have a cart")) {
				// If we're unique, slice the parts we don't care about and return
				Ok(mut inner) => {
					inner.truncate(range.end);
					drop(inner.drain(..range.start));

					inner
				},

				// Otherwise copy
				Err(inner) => inner[range].to_owned(),
			},
		}
	}
}

#[cfg(test)]
#[coverage(off)]
mod tests {
	use {super::*, std::collections::HashSet};

	#[test]
	fn from_static() {
		let mut s = ArcStr::from_static("abc");
		assert_eq!(&*s, "abc");
		assert_eq!(&*s.slice(1..2), "b");
		assert_eq!(&*s.slice_from_str(&s[1..2]), "b");

		s.with_mut(|s| s.push('d'));
		assert_eq!(&*s, "abcd");
	}

	#[test]
	fn mutate() {
		let mut s = ArcStr::from("abc");
		s.with_mut(|s| s.push_str("def"));
		assert_eq!(&*s, "abcdef");

		let s2 = s.clone();
		s.with_mut(|s| s.push_str("ghi"));
		assert_eq!(&*s, "abcdefghi");
		assert_eq!(&*s2, "abcdef");

		let output = s.with_mut(|_s| 123);
		assert_eq!(output, 123);

		let output = ArcStr::from_static("abc").with_mut(|_s| 456);
		assert_eq!(output, 456);
	}

	#[test]
	fn strip() {
		let s = ArcStr::from("abc");
		assert_eq!(s.strip_prefix("ab").as_deref(), Some("c"));
		assert_eq!(s.strip_prefix("ac").as_deref(), None);

		assert_eq!(s.strip_suffix("bc").as_deref(), Some("a"));
		assert_eq!(s.strip_suffix("ac").as_deref(), None);
	}

	#[test]
	fn eq_cmp() {
		let a = ArcStr::from("a");
		let b = ArcStr::from("b");
		assert_eq!(a, ArcStr::from("a"));
		assert_ne!(a, b);
		assert!(b > a);
	}

	#[test]
	fn hash() {
		let s = ArcStr::from("abc");
		let mut h = HashSet::new();
		assert!(h.insert(s.clone()));
		assert!(!h.insert(s));
		assert!(!h.insert(ArcStr::from("abc")));
	}

	#[test]
	fn to_string() {
		assert_eq!(String::from(ArcStr::from_static("abc")), "abc");

		let mut s = ArcStr::from("abc");
		assert_eq!(String::from(s.clone()), "abc");

		s.with_mut(|s| s.push('d'));
		assert_eq!(String::from(s), "abcd");
	}

	#[test]
	fn borrow() {
		let s = ArcStr::from("abc");
		assert_eq!(Borrow::<str>::borrow(&s), "abc");
	}

	#[test]
	fn fmt() {
		let s = ArcStr::from("abc");

		let mut output = String::new();
		fmt::write(&mut output, format_args!("{s}")).expect("Unable to write");
		assert_eq!(output, "abc");

		output.clear();
		fmt::write(&mut output, format_args!("{s:?}")).expect("Unable to write");
		assert_eq!(output, r#""abc""#);
	}
}
