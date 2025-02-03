//! Arc string

// Lints
#![expect(unsafe_code, reason = "We need unsafe to implement our string 'cached' pointer")]

// Imports
use std::{
	borrow::Borrow,
	cmp,
	fmt,
	hash::{Hash, Hasher},
	mem,
	ops::{Deref, Range},
	str::pattern::{Pattern, ReverseSearcher},
	sync::Arc,
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
	/// This string's pointer
	///
	/// The `'static` lifetime is a lie, but we never hand it out as `'static`,
	/// only as `'self`, so this is fine.
	ptr: &'static str,

	/// Inner
	// Note: We need an `Arc<String>` for efficient conversion to/from `String`
	inner: Arc<String>,
}

impl ArcStr {
	/// Returns the range of this string compared to the base
	fn base_range(&self) -> Range<usize> {
		self.inner
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
		// Get the offset and length of our specific string
		let range = self.base_range();

		// Get the inner string
		let s = match Arc::get_mut(&mut self.inner) {
			// If we're unique, slice the parts we don't care about and return
			Some(s) => {
				s.truncate(range.end);
				let _ = s.drain(..range.start);

				s
			},

			// Otherwise copy
			None => {
				self.inner = Arc::new(self.to_string());
				Arc::get_mut(&mut self.inner).expect("Should be unique")
			},
		};

		// Invalidate our string pointer in case of a panic.
		self.ptr = "";

		// Then mutate
		let output = f(s);

		// And finally, reconstruct ourselves
		// SAFETY: We never hand out the `'static` string, and we ensure
		//         it's kept alive, as it's derived from our `inner` field,
		//         which we own.
		self.ptr = unsafe { self::extend_static(s.as_str()) };

		output
	}

	/// Creates a sub-slice of `self` containing `s`.
	///
	/// # Panics
	/// `s` must be derived from this string, else this method panics.
	pub fn slice_from_str(&self, s: &str) -> Self {
		let range = self.substr_range(s).expect("Input was not a substring of this string");
		Self {
			ptr:   &self.ptr[range],
			inner: Arc::clone(&self.inner),
		}
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

/// Extends the lifetime of `s` to be static.
///
/// # Safety
/// This can only be used for strings that are assigned to `ArcStr::ptr`
unsafe fn extend_static(s: &str) -> &'static str {
	unsafe { mem::transmute::<&str, &'static str>(s) }
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
		self.ptr
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
			// SAFETY: We never hand out the `'static` string, and we ensure
			//         it's kept alive, as it's derived from our `inner` field,
			//         which we own.
			ptr:   unsafe { self::extend_static(s.as_str()) },
			inner: Arc::new(s),
		}
	}
}

impl From<ArcStr> for String {
	fn from(s: ArcStr) -> Self {
		// Get the offset and length of our specific string
		let range = s.base_range();

		match Arc::try_unwrap(s.inner) {
			// If we're unique, slice the parts we don't care about and return
			Ok(mut inner) => {
				inner.truncate(range.end);
				let _ = inner.drain(..range.start);

				inner
			},

			// Otherwise copy
			Err(inner) => ArcStr { inner, ..s }.to_string(),
		}
	}
}

#[cfg(test)]
mod tests {
	use {
		super::*,
		std::{hint::black_box, sync::Mutex},
	};

	#[test]
	fn create() {
		let s = ArcStr::from("Test".to_owned());
		_ = black_box(&*s);
	}

	#[test]
	fn mutate() {
		let mut s1 = ArcStr::from("Test".to_owned());
		let s2 = s1.clone();
		s1.with_mut(|s| {
			let cap = s.capacity();
			s.push_str(&"A".repeat(100));

			assert!(s.capacity() > cap, "Did not re-allocate");
		});
		_ = black_box(&*s1);
		_ = black_box(&*s2);
	}

	#[test]
	fn slice_from_str() {
		let s1 = ArcStr::from("Test".to_owned());
		let s2 = s1.slice_from_str(&s1[1..2]);
		_ = black_box(&*s1);
		_ = black_box(&*s2);
	}

	#[test]
	fn panic() {
		let s1 = Mutex::new(ArcStr::from("Test".to_owned()));

		let _: Box<_> = std::panic::catch_unwind(|| {
			s1.lock().expect("Poisoned").with_mut(|s| {
				s.push_str(&"A".repeat(100));
				panic!();
			});
		})
		.expect_err("Did not panic");

		s1.clear_poison();
		let s = s1.lock().expect("Poisoned");
		_ = black_box(&**s);
	}
}
