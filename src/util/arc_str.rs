//! Arc string

// Lints
#![expect(unsafe_code, reason = "We need unsafe to implement our string 'cached' pointer")]

// Imports
use std::{
	borrow::Borrow,
	cmp,
	fmt,
	hash::{Hash, Hasher},
	ops::Deref,
	ptr::NonNull,
	str::pattern::{Pattern, ReverseSearcher},
	sync::Arc,
};

/// Arc string.
///
/// Stores a string as a `(Arc<str>, Range<usize>)`,
/// to allow for fast indexing and cloning.
#[derive(Clone)]
pub struct ArcStr {
	/// This string's pointer
	///
	/// The string must *never* be mutated through this pointer,
	/// due to it being possibly derived from a `&str`.
	ptr: NonNull<str>,

	/// Inner
	#[expect(clippy::rc_buffer, reason = "We need it for efficient conversion to/from `String`")]
	inner: Arc<String>,
}

impl ArcStr {
	/// Returns the offset of this string compared to the base
	fn base_offset(&self) -> usize {
		// SAFETY: `self.ptr` was derived from `inner.base_ptr`
		let start = unsafe { self.ptr.as_ptr().byte_offset_from(self.inner.as_ptr()) };
		usize::try_from(start).expect("String pointer was before base pointer")
	}

	/// Updates this string as a `&mut String`.
	///
	/// Copies the string unless no other copies exist
	pub fn with_mut<F, R>(&mut self, f: F) -> R
	where
		F: FnOnce(&mut String) -> R,
	{
		// Get the offset and length of our specific string
		let start = self.base_offset();
		let len = self.len();

		// Get the inner string
		let s = match Arc::get_mut(&mut self.inner) {
			// If we're unique, slice the parts we don't care about and return
			Some(s) => {
				// Since we're invalidating `self.inner`, replace `ptr`
				// with a dummy value in case of panics.
				self.ptr = NonNull::from("");

				s.truncate(start + len);
				let _ = s.drain(..start);

				s
			},

			// Otherwise copy
			None => {
				self.inner = Arc::new(self.to_string());
				self.ptr = NonNull::from(self.inner.as_str());

				Arc::get_mut(&mut self.inner).expect("Should be unique")
			},
		};

		// Then mutate
		let output = f(s);

		// And finally, reconstruct ourselves
		self.ptr = NonNull::from(s.as_str());

		output
	}

	/// Creates a sub-slice of `self` containing `s`.
	///
	/// # Panics
	/// `s` must be derived from this string, else this method panics.
	pub fn slice_from_str(&self, s: &str) -> Self {
		// Get pointer ranges
		let self_range = self.as_bytes().as_ptr_range();
		let s_range = s.as_bytes().as_ptr_range();

		assert!(
			self_range.contains(&s_range.start) || s_range.start == self_range.end,
			"String start was before this string"
		);
		assert!(
			self_range.contains(&s_range.end) || s_range.end == self_range.end,
			"String end was past this string"
		);

		Self {
			ptr:   NonNull::from(s),
			inner: Arc::clone(&self.inner),
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

// SAFETY: We're immutable, like `Arc`
unsafe impl Send for ArcStr {}

// SAFETY: See above in [`Send`] impl
unsafe impl Sync for ArcStr {}


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
		// SAFETY: `self.ptr` always contains a valid `str`.
		unsafe { self.ptr.as_ref() }
	}
}

impl Borrow<str> for ArcStr {
	fn borrow(&self) -> &str {
		self
	}
}

impl From<String> for ArcStr {
	fn from(s: String) -> Self {
		Self {
			ptr:   NonNull::from(s.as_str()),
			inner: Arc::new(s),
		}
	}
}

impl From<ArcStr> for String {
	fn from(s: ArcStr) -> Self {
		// Get the offset and length of our specific string
		let start = s.base_offset();
		let len = s.len();

		match Arc::try_unwrap(s.inner) {
			Ok(mut inner) => {
				// Slice the string to fit
				let _ = inner.drain(start + len..);
				let _ = inner.drain(..start);

				inner
			},
			Err(inner) => ArcStr { inner, ..s }.to_string(),
		}
	}
}

impl From<&str> for ArcStr {
	fn from(s: &str) -> Self {
		s.to_owned().into()
	}
}
