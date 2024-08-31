//! Build reason

// Imports
use {
	crate::{rules::Target, util::ArcStr, AppError},
	std::{ops::Try, sync::Arc},
};

/// Build reason inner
#[derive(Clone, Debug)]
pub struct BuildReasonInner {
	/// Target
	target: Target<ArcStr>,

	/// Previous reason
	prev: BuildReason,
}

/// Build reason
#[derive(Clone, Debug)]
pub struct BuildReason(Option<Arc<BuildReasonInner>>);

impl BuildReason {
	/// Creates an empty build reason
	pub const fn empty() -> Self {
		Self(None)
	}

	/// Adds a target to this build reason
	pub fn with_target(&self, target: Target<ArcStr>) -> Self {
		Self(Some(Arc::new(BuildReasonInner {
			target,
			prev: self.clone(),
		})))
	}

	/// Returns this reason's target, if any
	pub fn target(&self) -> Option<&Target<ArcStr>> {
		self.0.as_ref().map(|reason| &reason.target)
	}

	/// Iterates over all reasons
	pub fn for_each<F, R>(&self, mut f: F) -> R
	where
		F: FnMut(&Target<ArcStr>) -> R,
		R: Try<Output = ()>,
	{
		let mut reason = &self.0;
		while let Some(inner) = reason {
			f(&inner.target)?;
			reason = &inner.prev.0;
		}

		R::from_output(())
	}

	/// Collects all reasons
	pub fn collect_all(&self) -> Vec<Target<ArcStr>> {
		let mut targets = vec![];

		let mut reason = &self.0;
		while let Some(inner) = reason {
			targets.push(inner.target.clone());
			reason = &inner.prev.0;
		}

		targets
	}

	/// Checks for recursive targets.
	///
	/// Returns `Err` if `target` was already found somewhere in this build reason tree,
	/// otherwise returns `Ok`.
	pub fn check_recursively(&self, target: &Target<ArcStr>) -> Result<(), AppError> {
		self.for_each(|parent_target| match target == parent_target {
			true => Err(AppError::FoundRecursiveRule {
				target:         target.to_string(),
				parent_targets: self.collect_all().iter().map(Target::to_string).collect(),
			}),
			false => Ok(()),
		})
	}
}
