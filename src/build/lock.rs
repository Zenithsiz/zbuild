//! Build locks

// Imports
use {
	crate::AppError,
	std::{sync::Arc, time::SystemTime},
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
	/// Result, if built
	res: Option<Result<BuildResult, Arc<AppError>>>,
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

	/// Retrieves the result of this state.
	///
	/// Waits for any builders to finish
	pub async fn res(&self) -> Option<Result<BuildResult, AppError>> {
		self.state
			.read()
			.await
			.res
			.clone()
			.map(|res| res.map_err(AppError::Shared))
	}
}

/// Build lock build guard
#[derive(Debug)]
pub struct BuildLockBuildGuard {
	/// State
	state: OwnedRwLockWriteGuard<BuildState>,
}

impl BuildLockBuildGuard {
	/// Returns the result of the build
	pub fn res(&self) -> Option<Result<BuildResult, AppError>> {
		self.state.res.clone().map(|res| res.map_err(AppError::Shared))
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
	pub fn finish(&mut self, res: Result<BuildResult, AppError>) -> Result<BuildResult, AppError> {
		let res = res.map_err(Arc::new);
		self.state.res = Some(res.clone());
		res.map_err(AppError::Shared)
	}
}

/// Build lock dependency guard
#[derive(Debug)]
pub struct BuildLockDepGuard {
	/// State
	state: OwnedRwLockReadGuard<BuildState>,
}

impl BuildLockDepGuard {
	/// Returns the result of the build
	pub fn res(&self) -> Option<Result<BuildResult, AppError>> {
		self.state.res.clone().map(|res| res.map_err(AppError::Shared))
	}
}
