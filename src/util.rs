//! Utilities

// Modules
pub mod arc_str;

// Exports
pub use self::arc_str::ArcStr;

// Imports
use {
	futures::Future,
	pin_project::pin_project,
	std::{
		io,
		mem,
		path::{self, Path, PathBuf},
		pin::Pin,
		str::pattern::Pattern,
		task,
		time::{Duration, Instant},
	},
	tokio::fs,
};

/// Chains together any number of `IntoIterator`s
pub macro chain {
	($lhs:expr, $rhs:expr $(,)?) => {
		::std::iter::Iterator::chain($lhs.into_iter(), $rhs.into_iter())
	},

	($lhs:expr, $rhs:expr, $($rest:expr),+ $(,)?) => {
		::std::iter::Iterator::chain($lhs.into_iter(), $crate::util::chain!($rhs, $($rest),+))
	},
}

/// Async `std::fs_try_exists` using [`symlink_metadata`](fs::symlink_metadata).
pub async fn fs_try_exists_symlink(path: impl AsRef<Path> + Send) -> Result<bool, io::Error> {
	match fs::symlink_metadata(path).await {
		Ok(_) => Ok(true),
		Err(err) if err.kind() == io::ErrorKind::NotFound => Ok(false),
		Err(err) => Err(err),
	}
}

/// Measures the duration of a fallible future
pub async fn try_measure_async<F: Future<Output = Result<T, E>>, T, E>(fut: F) -> Result<(Duration, T), E> {
	/// Wrapper future for measuring the future
	#[pin_project]
	struct Wrapper<F> {
		/// Future
		#[pin]
		fut: F,

		/// Start time
		start: Option<Instant>,
	}

	impl<F: Future<Output = Result<T, E>>, T, E> Future for Wrapper<F> {
		type Output = Result<(Duration, T), E>;

		fn poll(self: Pin<&mut Self>, cx: &mut task::Context<'_>) -> task::Poll<Self::Output> {
			let mut this = self.project();
			let start = this.start.get_or_insert_with(Instant::now);

			this.fut
				.as_mut()
				.poll(cx)
				.map(|res| res.map(|value| (start.elapsed(), value)))
		}
	}


	Wrapper { fut, start: None }.await
}

/// Normalizes a string path
pub fn normalize_path(path: &str) -> String {
	let ends_with_sep = path.ends_with(path::MAIN_SEPARATOR_STR);

	let mut path_cmpts = vec![];
	for cmpt in Path::new(path).components() {
		match cmpt {
			// For "normal" components, just push them
			path::Component::Prefix(_) | path::Component::RootDir | path::Component::Normal(_) => path_cmpts.push(cmpt),

			// Ignore current directory components unless this is the very first one
			path::Component::CurDir =>
				if path_cmpts.is_empty() {
					path_cmpts.push(path::Component::CurDir);
				},

			// For each parent directory component, pop the last component unless it's
			// also a parent directory
			path::Component::ParentDir => match path_cmpts.last() {
				Some(path::Component::ParentDir) | None => path_cmpts.push(path::Component::ParentDir),
				Some(_) => _ = path_cmpts.pop(),
			},
		}
	}

	let mut path = path_cmpts
		.into_iter()
		.collect::<PathBuf>()
		.into_os_string()
		.into_string()
		.expect("utf-8 path was no longer utf-8 after normalizing");

	// If the path is empty, add a `.` to symbolize the current directory
	if path.is_empty() {
		path.push('.');
	}

	// If we had a `/` at the end, add it back since `normalize` removes it
	if ends_with_sep {
		path.push_str(path::MAIN_SEPARATOR_STR);
	}

	path
}

/// In-place replaces matching parts of a string.
#[expect(
	clippy::needless_pass_by_value,
	reason = "It's more ergonomic to pass patterns by value"
)]
pub fn string_replace_in_place_with<P>(s: &mut String, pat: P, replace_with: &str)
where
	P: Pattern + Clone,
{
	let mut cur_idx = 0;
	let mut matches = s.match_indices(pat.clone());

	// Find all matches, replacing the range as we go.
	#[expect(
		clippy::string_slice,
		reason = "The index will always be valid, as it's the end of the string returned by `match_indices`, which \
		          must return substrings of the string"
	)]
	while let Some((pos, part)) = matches.next() {
		// Replace the range
		mem::drop(matches);
		s.replace_range(cur_idx + pos..cur_idx + pos + part.len(), replace_with);

		// After replacing `...ABC` with `...DEF`, put ourselves as the end of
		// the string we just replaced, to avoid recursively replacing the same
		// pattern over and over.
		cur_idx += pos + replace_with.len();
		matches = s[cur_idx..].match_indices(pat.clone());
	}
}

#[cfg(test)]
mod tests {
	#[test]
	fn normalize_path() {
		let tests = [
			("a/", "a/"),
			("a/../", "./"),
			("a/../b", "b"),
			("a/./b", "a/b"),
			("a/./b/", "a/b/"),
			("../b", "../b"),
			("a/b/../../c", "c"),
		];

		for (orig, norm) in tests {
			assert_eq!(super::normalize_path(orig), norm, "Case {orig:?} failed");
		}
	}
}
