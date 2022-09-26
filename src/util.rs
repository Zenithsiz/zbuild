//! Utilities

// Imports
use {npath::NormPathExt, std::path::Path, tokio::fs};

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

/// Normalizes a string path
pub fn normalize_path(path: &str) -> String {
	let ends_with_sep = path.ends_with(std::path::MAIN_SEPARATOR_STR);

	let mut path = Path::new(&path)
		.normalized()
		.into_os_string()
		.into_string()
		.expect("utf-8 path was no longer utf-8 after normalizing");

	// Note: `npath` doesn't keep `/` at the end, so we have to do it manually
	match ends_with_sep {
		true => {
			path.push_str(std::path::MAIN_SEPARATOR_STR);
			path
		},
		false => path,
	}
}
