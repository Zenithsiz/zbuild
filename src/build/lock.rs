//! Build locks

// Imports
use {
	crate::{rules::Target, util::CowStr, AppError},
	std::{collections::HashMap, sync::Arc, time::SystemTime},
	tokio::sync::{OwnedRwLockReadGuard, OwnedRwLockWriteGuard, RwLock},
};

/// Build result
#[derive(Clone, Copy, Debug)]
pub struct BuildResult {
	/// Build time
	pub build_time: SystemTime,

	/// If this task was the one who built it
	// TODO: Rename to something here
	pub built_here: bool,

	/// If built from a rule
	pub built: bool,
}

/// Build state
#[derive(Clone, Debug)]
pub struct BuildState<'s> {
	/// Targets' result
	targets_res: HashMap<Target<'s, CowStr<'s>>, Result<BuildResult, Arc<AppError>>>,
}

/// Build lock
#[derive(Clone, Debug)]
pub struct BuildLock<'s> {
	/// State
	state: Arc<RwLock<BuildState<'s>>>,
}

impl<'s> BuildLock<'s> {
	/// Creates a new build lock
	pub fn new() -> Self {
		Self {
			state: Arc::new(RwLock::new(BuildState {
				targets_res: HashMap::new(),
			})),
		}
	}

	/// Locks the build lock for building
	pub async fn lock_build(&self) -> BuildLockBuildGuard<'s> {
		BuildLockBuildGuard {
			state: Arc::clone(&self.state).write_owned().await,
		}
	}

	/// Locks the build lock as a dependency
	pub async fn lock_dep(&self) -> BuildLockDepGuard<'s> {
		BuildLockDepGuard {
			state: Arc::clone(&self.state).read_owned().await,
		}
	}

	/// Retrieves all targets' result by consuming the lock
	pub fn into_res(self) -> Vec<(Target<'s, CowStr<'s>>, Result<BuildResult, AppError>)> {
		// TODO: Not panic here
		Arc::try_unwrap(self.state)
			.expect("Leftover references when unwrapping build lock")
			.into_inner()
			.targets_res
			.into_iter()
			.map(|(target, build)| (target, build.map_err(AppError::Shared)))
			.collect()
	}
}

/// Build lock build guard
#[derive(Debug)]
pub struct BuildLockBuildGuard<'s> {
	/// State
	state: OwnedRwLockWriteGuard<BuildState<'s>>,
}

impl<'s> BuildLockBuildGuard<'s> {
	/// Retrieves a target's result
	///
	/// Waits for any builders to finish
	pub fn res(&self, target: &Target<'s, CowStr<'s>>) -> Option<Result<BuildResult, AppError>> {
		self.state
			.targets_res
			.get(target)
			.cloned()
			.map(|res| res.map_err(AppError::Shared))
	}

	/// Downgrades this build lock into a dependency lock
	pub fn into_dep(self) -> BuildLockDepGuard<'s> {
		BuildLockDepGuard {
			state: self.state.downgrade(),
		}
	}

	/// Resets this build.
	pub fn reset(&mut self, target: &Target<'s, CowStr<'s>>) {
		self.state.targets_res.remove(target);
	}

	/// Finishes a build
	pub fn finish(
		&mut self,
		target: &Target<'s, CowStr<'s>>,
		res: Result<BuildResult, AppError>,
	) -> Result<BuildResult, AppError> {
		let res = res.map_err(Arc::new);
		self.state.targets_res.insert(target.clone(), res.clone());
		res.map_err(AppError::Shared)
	}
}

/// Build lock dependency guard
#[derive(Debug)]
pub struct BuildLockDepGuard<'s> {
	/// State
	state: OwnedRwLockReadGuard<BuildState<'s>>,
}

impl<'s> BuildLockDepGuard<'s> {
	/// Retrieves a target's result
	///
	/// Waits for any builders to finish
	pub fn res(&self, target: &Target<'s, CowStr<'s>>) -> Option<Result<BuildResult, AppError>> {
		self.state
			.targets_res
			.get(target)
			.cloned()
			.map(|res| res.map_err(AppError::Shared))
	}
}
