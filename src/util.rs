//! Utilities

/// Chains together any number of iterators
pub macro chain {
	($lhs:expr, $rhs:expr $(,)?) => {
		::std::iter::Iterator::chain($lhs, $rhs)
	},

	($lhs:expr, $rhs:expr, $($rest:expr),+ $(,)?) => {
		::std::iter::Iterator::chain($lhs, $crate::util::chain!($rhs, $($rest),*))
	},
}
