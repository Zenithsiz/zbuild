//! Filesystem abstraction

use {
	core::clone::Share,
	std::{
		collections::{HashMap, hash_map},
		fs,
		io,
		path::{Path, PathBuf},
		sync::{Arc, nonpoison::Mutex},
		time::SystemTime,
	},
};

/// Filesystem abstraction
pub trait Fs: Share + Send + Sync + 'static {
	/// Metadata type
	type Metadata: FsMetadata;

	/// Reads `path` to a string
	fn read_to_string(&self, path: impl AsRef<Path>) -> Result<String, io::Error>;

	/// Gets the metadata of `path`, without following symlinks
	fn symlink_metadata(&self, path: impl AsRef<Path>) -> Result<Self::Metadata, io::Error>;

	/// Returns if `path` exists, without following symlinks
	fn try_exists_symlink(&self, path: impl AsRef<Path>) -> Result<bool, io::Error> {
		match self.symlink_metadata(path) {
			Ok(_) => Ok(true),
			Err(err) if err.kind() == io::ErrorKind::NotFound => Ok(false),
			Err(err) => Err(err),
		}
	}

	/// Writes `contents` to `path`
	fn write(&self, path: impl AsRef<Path>, contents: &[u8]) -> Result<(), io::Error>;

	/// Canonicalizes a path
	fn canonicalize(&self, path: impl AsRef<Path>) -> Result<PathBuf, io::Error>;

	/// Gets the current directory
	fn current_dir(&self) -> Result<PathBuf, io::Error>;

	/// Sets the current directory
	fn set_current_dir(&self, path: impl AsRef<Path>) -> Result<(), io::Error>;

	/// Creates the directory `path`, along with all parents
	fn create_dir_all(&self, path: impl AsRef<Path>) -> Result<(), io::Error>;

	/// Touches the file `path`.
	///
	/// If it exists, updates it's modified date, otherwise
	/// creates it as empty
	fn touch(&self, path: impl AsRef<Path>) -> Result<(), io::Error>;
}

/// Metadata
pub trait FsMetadata {
	/// Returns the modified time of this metadata
	fn modified(&self) -> Result<SystemTime, io::Error>;
}

/// Os filesystem
///
/// Implements [`Fs`] by delegating to the actual os filesystem
#[derive(Clone, Copy, Debug)]
pub struct OsFs {}

impl OsFs {
	#[expect(clippy::new_without_default, reason = "We want to be explicit")]
	#[must_use]
	pub const fn new() -> Self {
		Self {}
	}
}

impl Share for OsFs {}

impl Fs for OsFs {
	type Metadata = fs::Metadata;

	fn read_to_string(&self, path: impl AsRef<Path>) -> Result<String, io::Error> {
		std::fs::read_to_string(path)
	}

	fn symlink_metadata(&self, path: impl AsRef<Path>) -> Result<Self::Metadata, io::Error> {
		std::fs::symlink_metadata(path)
	}

	fn write(&self, path: impl AsRef<Path>, contents: &[u8]) -> Result<(), io::Error> {
		std::fs::write(path, contents)
	}

	fn canonicalize(&self, path: impl AsRef<Path>) -> Result<PathBuf, io::Error> {
		std::fs::canonicalize(path)
	}

	fn current_dir(&self) -> Result<PathBuf, io::Error> {
		std::env::current_dir()
	}

	fn set_current_dir(&self, path: impl AsRef<Path>) -> Result<(), io::Error> {
		std::env::set_current_dir(path)
	}

	fn create_dir_all(&self, path: impl AsRef<Path>) -> Result<(), io::Error> {
		std::fs::create_dir_all(path)
	}

	fn touch(&self, path: impl AsRef<Path>) -> Result<(), io::Error> {
		_ = std::fs::OpenOptions::new()
			.create(true)
			.truncate(false)
			.append(true)
			.open(path)?;

		Ok(())
	}
}

impl FsMetadata for fs::Metadata {
	fn modified(&self) -> Result<SystemTime, io::Error> {
		self.modified()
	}
}

/// In-memory filesystem
#[derive(Clone, Debug)]
pub struct MemFs {
	inner: Arc<Mutex<MemFsInner>>,
}

impl MemFs {
	#[expect(clippy::new_without_default, reason = "We want to be explicit")]
	#[must_use]
	pub fn new() -> Self {
		Self {
			inner: Arc::new(Mutex::new(MemFsInner {
				cur_dir: PathBuf::new(),
				files:   HashMap::new(),
			})),
		}
	}
}

#[derive(Debug)]
struct MemFsInner {
	cur_dir: PathBuf,
	files:   HashMap<PathBuf, MemFsFile>,
}

#[derive(Debug)]
struct MemFsFile {
	modified: SystemTime,
	contents: Vec<u8>,
}

impl Share for MemFs {}

impl Fs for MemFs {
	type Metadata = MemFsMetadata;

	fn read_to_string(&self, path: impl AsRef<Path>) -> Result<String, io::Error> {
		let inner = self.inner.lock();
		let path = inner.cur_dir.join(path);
		let Some(file) = inner.files.get(&path) else {
			return Err(io::Error::new(
				io::ErrorKind::NotFound,
				format!("Unknown path {path:?}"),
			));
		};

		let s = String::from_utf8(file.contents.clone()).map_err(io::Error::other)?;
		Ok(s)
	}

	fn symlink_metadata(&self, path: impl AsRef<Path>) -> Result<Self::Metadata, io::Error> {
		let inner = self.inner.lock();
		let path = inner.cur_dir.join(path);
		let Some(file) = inner.files.get(&path) else {
			return Err(io::Error::new(
				io::ErrorKind::NotFound,
				format!("Unknown path {path:?}"),
			));
		};

		Ok(MemFsMetadata {
			modified: file.modified,
		})
	}

	fn write(&self, path: impl AsRef<Path>, contents: &[u8]) -> Result<(), io::Error> {
		let mut inner = self.inner.lock();
		let path = inner.cur_dir.join(path);
		_ = inner.files.insert(path, MemFsFile {
			modified: SystemTime::now(),
			contents: contents.to_vec(),
		});
		Ok(())
	}

	fn canonicalize(&self, path: impl AsRef<Path>) -> Result<PathBuf, io::Error> {
		Ok(path.as_ref().to_path_buf())
	}

	fn current_dir(&self) -> Result<PathBuf, io::Error> {
		let inner = self.inner.lock();
		Ok(inner.cur_dir.clone())
	}

	fn set_current_dir(&self, path: impl AsRef<Path>) -> Result<(), io::Error> {
		let mut inner = self.inner.lock();
		inner.cur_dir = path.as_ref().to_path_buf();
		Ok(())
	}

	fn create_dir_all(&self, path: impl AsRef<Path>) -> Result<(), io::Error> {
		let mut inner = self.inner.lock();
		let path = inner.cur_dir.join(path);
		for path in path.ancestors().collect::<Vec<_>>().into_iter().rev() {
			if path.is_empty() {
				continue;
			}

			let hash_map::Entry::Vacant(entry) = inner.files.entry(path.to_path_buf()) else {
				continue;
			};

			_ = entry.insert(MemFsFile {
				modified: SystemTime::now(),
				contents: vec![],
			});
		}

		Ok(())
	}

	fn touch(&self, path: impl AsRef<Path>) -> Result<(), io::Error> {
		let mut inner = self.inner.lock();
		let path = inner.cur_dir.join(path);
		match inner.files.entry(path) {
			hash_map::Entry::Occupied(entry) => entry.into_mut().modified = SystemTime::now(),
			hash_map::Entry::Vacant(entry) =>
				_ = entry.insert(MemFsFile {
					modified: SystemTime::now(),
					contents: vec![],
				}),
		}

		Ok(())
	}
}

#[derive(Clone, Copy, Debug)]
pub struct MemFsMetadata {
	modified: SystemTime,
}

impl FsMetadata for MemFsMetadata {
	fn modified(&self) -> Result<SystemTime, io::Error> {
		Ok(self.modified)
	}
}
