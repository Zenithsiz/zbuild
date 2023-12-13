//! Build locks

// Imports
use {
	crate::{rules::Target, util::CowStr},
	std::{assert_matches::assert_matches, collections::HashMap, sync::Arc, time::SystemTime},
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
pub struct BuildState<'s> {
	/// Targets' result
	targets_res: HashMap<Target<'s, CowStr<'s>>, Result<BuildResult, ()>>,
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
	pub fn into_res(self) -> Vec<(Target<'s, CowStr<'s>>, Result<BuildResult, ()>)> {
		// TODO: Not panic here
		Arc::try_unwrap(self.state)
			.expect("Leftover references when unwrapping build lock")
			.into_inner()
			.targets_res
			.into_iter()
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
	pub fn res(&self, target: &Target<'s, CowStr<'s>>) -> Option<Result<BuildResult, ()>> {
		self.state.targets_res.get(target).copied()
	}

	/// Downgrades this build lock into a dependency lock
	pub fn into_dep(self) -> BuildLockDepGuard<'s> {
		BuildLockDepGuard {
			state: self.state.downgrade(),
		}
	}

	/// Resets this build.
	pub fn reset(&mut self, target: &Target<'s, CowStr<'s>>) {
		// Note: We don't care about the previous build result
		let _: Option<Result<BuildResult, _>> = self.state.targets_res.remove(target);
	}

	/// Finishes a build
	pub fn finish(&mut self, target: Target<'s, CowStr<'s>>, res: BuildResult) {
		let prev_res = self.state.targets_res.insert(target, Ok(res));
		assert_matches!(prev_res, None, "Build was already finished");
	}

	/// Finishes a build as failed
	pub fn finish_failed(&mut self, target: Target<'s, CowStr<'s>>) {
		let prev_res = self.state.targets_res.insert(target, Err(()));
		assert_matches!(prev_res, None, "Build was already finished");
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
	pub fn res(&self, target: &Target<'s, CowStr<'s>>) -> Option<Result<BuildResult, ()>> {
		self.state.targets_res.get(target).copied()
	}
}
