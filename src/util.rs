//! Utilities

// Imports
use {std::path::Path, tokio::fs};

/// Chains together any number of `IntoIterator`s
pub macro chain {
	($lhs:expr, $rhs:expr $(,)?) => {
		::std::iter::Iterator::chain($lhs.into_iter(), $rhs.into_iter())
	},

	($lhs:expr, $rhs:expr, $($rest:expr),+ $(,)?) => {
		::std::iter::Iterator::chain($lhs.into_iter(), $crate::util::chain!($rhs, $($rest),*))
	},
}

/// Async `std::fs_try_exists`
pub async fn fs_try_exists(path: impl AsRef<Path> + Send) -> Result<bool, std::io::Error> {
	match fs::metadata(path).await {
		Ok(_) => Ok(true),
		Err(err) if err.kind() == std::io::ErrorKind::NotFound => Ok(false),
		Err(err) => Err(err),
	}
}
