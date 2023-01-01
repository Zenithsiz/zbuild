//! Utilities

// Imports
use {
	futures::Future,
	npath::NormPathExt,
	pin_project::pin_project,
	std::{
		path::Path,
		pin::Pin,
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

/// Measures the duration of a fallible future
#[expect(clippy::future_not_send)] // It is send if `F: Send` (TODO: Check if this is true)
pub async fn try_measure_async<F: Future<Output = Result<T, E>>, T, E>(fut: F) -> Result<(Duration, T), E> {
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
