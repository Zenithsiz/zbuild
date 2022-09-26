//! Build locks

// Imports
use {
	crate::{rules::Target, AppError},
	std::{collections::HashMap, sync::Arc, time::SystemTime},
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
	/// Targets' result
	targets_res: HashMap<Target<String>, Result<BuildResult, Arc<AppError>>>,
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
				targets_res: HashMap::new(),
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

	/// Retrieves all targets' result
	///
	/// Waits for any builders to finish
	pub async fn all_res(&self) -> Vec<(Target<String>, Result<BuildResult, AppError>)> {
		self.state
			.read()
			.await
			.targets_res
			.iter()
			.map(|(target, res)| (target.clone(), res.clone().map_err(AppError::Shared)))
			.collect()
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
	pub fn res(&self, target: &Target<String>) -> Option<Result<BuildResult, AppError>> {
		self.state
			.targets_res
			.get(target)
			.cloned()
			.map(|res| res.map_err(AppError::Shared))
	}

	/// Downgrades this build lock into a dependency lock
	pub fn into_dep(self) -> BuildLockDepGuard {
		BuildLockDepGuard {
			state: self.state.downgrade(),
		}
	}

	/// Resets this build.
	pub fn reset(&mut self, target: &Target<String>) {
		self.state.targets_res.remove(target);
	}

	/// Finishes a build
	pub fn finish(
		&mut self,
		target: &Target<String>,
		res: Result<BuildResult, AppError>,
	) -> Result<BuildResult, AppError> {
		let res = res.map_err(Arc::new);
		self.state.targets_res.insert(target.clone(), res.clone());
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
	/// Retrieves a target's result
	///
	/// Waits for any builders to finish
	pub fn res(&self, target: &Target<String>) -> Option<Result<BuildResult, AppError>> {
		self.state
			.targets_res
			.get(target)
			.cloned()
			.map(|res| res.map_err(AppError::Shared))
	}
}
