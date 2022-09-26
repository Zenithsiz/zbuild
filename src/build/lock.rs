//! Build locks

// Imports
use {
	std::sync::Arc,
	tokio::sync::{OwnedRwLockReadGuard, OwnedRwLockWriteGuard, RwLock},
};

/// Build lock
#[derive(Clone, Debug)]
pub struct BuildLock {
	/// Lock
	lock: Arc<RwLock<()>>,
}

impl BuildLock {
	/// Creates a new build lock
	pub fn new() -> Self {
		Self {
			lock: Arc::new(RwLock::new(())),
		}
	}

	/// Locks the build lock for building
	pub async fn lock_build(&self) -> BuildLockBuildGuard {
		BuildLockBuildGuard {
			lock: Arc::clone(&self.lock).write_owned().await,
		}
	}

	/// Locks the build lock as a dependency
	pub async fn lock_dep(&self) -> BuildLockDepGuard {
		BuildLockDepGuard {
			_lock: Arc::clone(&self.lock).read_owned().await,
		}
	}
}

/// Build lock build guard
#[derive(Debug)]
pub struct BuildLockBuildGuard {
	/// Lock
	lock: OwnedRwLockWriteGuard<()>,
}

impl BuildLockBuildGuard {
	/// Downgrades this build lock into a dependency lock
	pub fn into_dep(self) -> BuildLockDepGuard {
		BuildLockDepGuard {
			_lock: self.lock.downgrade(),
		}
	}
}

/// Build lock dependency guard
#[derive(Debug)]
pub struct BuildLockDepGuard {
	/// Lock
	_lock: OwnedRwLockReadGuard<()>,
}
