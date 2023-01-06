//! `Cow-Arc str`

// Imports
use std::{
	borrow::{Borrow, Cow},
	hash::{Hash, Hasher},
	sync::Arc,
};

/// A `Cow<'a, str> | Arc<str>`-like type.
///
/// Can either borrow from a `&'a str`, or have shared ownership of an `Arc<str>`
#[derive(Clone, Debug)]
pub enum CowArcStr<'a> {
	/// Borrowed
	Borrowed(&'a str),

	/// Owned
	Owned(Arc<str>),
}

impl<'a> PartialEq for CowArcStr<'a> {
	fn eq(&self, other: &Self) -> bool {
		self.as_str().eq(other.as_str())
	}
}

impl<'a> Eq for CowArcStr<'a> {}

impl<'a> PartialOrd for CowArcStr<'a> {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		Some(self.cmp(other))
	}
}

impl<'a> Ord for CowArcStr<'a> {
	fn cmp(&self, other: &Self) -> std::cmp::Ordering {
		self.as_str().cmp(other.as_str())
	}
}

impl<'a> Hash for CowArcStr<'a> {
	fn hash<H: Hasher>(&self, state: &mut H) {
		self.as_str().hash(state);
	}
}

impl<'a> CowArcStr<'a> {
	/// Creates a `CowArcStr` from a `Cow<'a, str>`.
	///
	/// This will either borrow if the `Cow` is borrowed, or convert
	/// the underlying string into an `Arc<str>` otherwise
	// TODO: `String` -> `Arc<str>` isn't free, how to improve this?
	pub fn from_cow(cow: Cow<'a, str>) -> Self {
		match cow {
			Cow::Borrowed(s) => Self::Borrowed(s),
			Cow::Owned(s) => Self::Owned(s.into()),
		}
	}

	/// Returns this as a string.
	///
	/// Note: We can't return a `&'a str`, as we could be owned.
	pub fn as_str(&self) -> &str {
		match self {
			CowArcStr::Borrowed(s) => s,
			CowArcStr::Owned(s) => s,
		}
	}
}

impl<'a> Borrow<str> for CowArcStr<'a> {
	fn borrow(&self) -> &str {
		self.as_str()
	}
}

impl<'a> ToString for CowArcStr<'a> {
	fn to_string(&self) -> String {
		self.as_str().to_owned()
	}
}
