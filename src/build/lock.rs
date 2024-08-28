//! Build locks

// Imports
use {
	std::{
		assert_matches::assert_matches,
		mem,
		sync::{
			atomic::{self, AtomicBool},
			Arc,
		},
		time::SystemTime,
	},
	tokio::sync::{OwnedRwLockReadGuard, OwnedRwLockWriteGuard, RwLock},
};

/// Build result
#[derive(Clone, Copy, Debug)]
pub struct BuildResult {
	/// Build time
	pub build_time: SystemTime,

	/// If built from a rule
	pub built: bool,
}

/// Build state
#[derive(Debug)]
pub struct BuildState {
	/// Result
	res: Option<Result<BuildResult, ()>>,

	/// Whether someone has reserved a lock upgrade
	upgrade_reserved: AtomicBool,
}

/// Build lock
#[derive(Clone, Debug)]
pub struct BuildLock {
	/// State
	state: Arc<RwLock<BuildState>>,
}

impl BuildLock {
	/// Creates a new build lock
	pub fn new() -> Self {
		Self {
			state: Arc::new(RwLock::new(BuildState {
				res:              None,
				upgrade_reserved: AtomicBool::new(false),
			})),
		}
	}

	/// Locks the build lock for building
	pub async fn lock_build(&self) -> BuildLockBuildGuard {
		BuildLockBuildGuard {
			state: Arc::clone(&self.state).write_owned().await,
		}
	}

	/// Locks the build lock as a dependency
	pub async fn lock_dep(&self) -> BuildLockDepGuard {
		BuildLockDepGuard {
			state: Arc::clone(&self.state).read_owned().await,
		}
	}

	/// Retrieves this lock's result
	pub async fn res(&self) -> Option<Result<BuildResult, ()>> {
		self.state.read().await.res
	}
}

/// Build lock build guard
#[derive(Debug)]
pub struct BuildLockBuildGuard {
	/// State
	state: OwnedRwLockWriteGuard<BuildState>,
}

impl BuildLockBuildGuard {
	/// Retrieves a target's result
	///
	/// Waits for any builders to finish
	pub fn _res(&self) -> Option<Result<BuildResult, ()>> {
		self.state.res
	}

	/// Downgrades this build lock into a dependency lock
	pub fn into_dep(self) -> BuildLockDepGuard {
		BuildLockDepGuard {
			state: self.state.downgrade(),
		}
	}

	/// Resets this build.
	pub fn reset(&mut self) {
		self.state.res = None;
	}

	/// Finishes a build
	pub fn finish(&mut self, res: BuildResult) {
		assert_matches!(self.state.res, None, "Build was already finished");
		self.state.res = Some(Ok(res));
	}

	/// Finishes a build as failed
	pub fn finish_failed(&mut self) {
		assert_matches!(self.state.res, None, "Build was already finished");
		self.state.res = Some(Err(()));
	}
}

/// Build lock dependency guard
#[derive(Debug)]
pub struct BuildLockDepGuard {
	/// State
	state: OwnedRwLockReadGuard<BuildState>,
}

impl BuildLockDepGuard {
	/// Retrieves a target's result
	///
	/// Waits for any builders to finish
	pub fn res(&self) -> Option<Result<BuildResult, ()>> {
		self.state.res
	}

	/// Tries to upgrade this dependency lock to a build lock.
	///
	/// Returns `Err` if someone is already upgrading the lock.
	pub async fn try_upgrade_into_build(self) -> Result<BuildLockBuildGuard, Self> {
		// Try to reserve atomically, and if someone already reserved the upgrade, return `Err`
		if self.state.upgrade_reserved.swap(true, atomic::Ordering::AcqRel) {
			return Err(self);
		}

		// Otherwise, unlock and lock for building
		// TODO: Check if there could be any consequences to performing an unlock first?
		let state = Arc::clone(OwnedRwLockReadGuard::rwlock(&self.state));
		mem::drop(self);
		let mut state = state.write_owned().await;

		// Then unset the reserve flag so we can get locked again in the future.
		*state.upgrade_reserved.get_mut() = false;

		Ok(BuildLockBuildGuard { state })
	}
}
