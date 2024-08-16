//! Build locks

// Imports
use {
	std::{assert_matches::assert_matches, sync::Arc, time::SystemTime},
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
#[derive(Clone, Debug)]
pub struct BuildState {
	/// Result
	res: Option<Result<BuildResult, ()>>,
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
			state: Arc::new(RwLock::new(BuildState { res: None })),
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

	/// Retrieves this lock's result by consuming the lock
	pub fn into_res(self) -> Option<Result<BuildResult, ()>> {
		// TODO: Not panic here
		Arc::try_unwrap(self.state)
			.expect("Leftover references when unwrapping build lock")
			.into_inner()
			.res
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
	pub fn res(&self) -> Option<Result<BuildResult, ()>> {
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
}
