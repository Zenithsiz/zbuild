//! `Cow-Arc str`

// Imports
use std::{
	borrow::{Borrow, Cow},
	sync::Arc,
};

/// A `Cow<'a, str> | Arc<str>`-like type.
///
/// Can either borrow from a `&'a str`, or have shared ownership of an `Arc<str>`
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Debug)]
pub enum CowArcStr<'a> {
	/// Borrowed
	Borrowed(&'a str),

	/// Owned
	Owned(Arc<str>),
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
