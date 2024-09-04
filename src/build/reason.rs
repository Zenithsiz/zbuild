//! Build reason

// Imports
use {
	crate::{rules::Target, util::ArcStr, AppError},
	std::{ops::Try, sync::Arc},
};

/// Inner type for [`BuildReason`].
///
/// We don't perform the recursion here, and instead
/// perform it on [`BuildReason`] in order to allow creating
/// empty reasons. If this type's `prev` field were a `Option<BuildReason>`,
/// with [`BuildReason`] containing an `Arc<Inner>`, then we would not be
/// able to create a reason without a target.
///
/// This is important for us, since the very top-level isn't building targets
/// because of other targets, but instead because of the user requesting them.
#[derive(Clone, Debug)]
struct Inner {
	/// Target
	target: Target<ArcStr>,

	/// Previous reason
	prev: BuildReason,
}

/// Build reason.
///
/// Accompanied by a target, it represents the reason by which
/// that target is being built. However, it does not include
/// the target itself.
///
/// It's a recursive type that is part of a tree, indicating
/// the reason behind why other targets are being built.
///
/// # Examples
/// For example, given targets `A`, `B`, `C` and `D`, where `A`
/// depends on `B` and `C`, while `C` depends on `D`, the following tree
/// will be created:
///
/// ```no_compile
/// ┌─┐    ┌─┐
/// │A│ <- │B│
/// └─┘    └─┘
///  ^
///  |
/// ┌─┐    ┌─┐
/// │C│ <- │D│
/// └─┘    └─┘
/// ```
///
/// However, do note that the whole tree doesn't exist at the same time.
/// When `B` finishes building, it's build reason will be dropped. Since
/// we use `Arc`s to store the parents of each reason, they will be dropped
/// when all of their children are dropped.
///
/// We're also not doubly-linked, meaning only children have access to their
/// parents, and not the other way around. And reasons also are now aware of
/// their siblings.
///
/// # Usage
/// This is only used for checking recursively-built targets. i.e. targets
/// that require themselves to be built to be built themselves. These targets
/// would result in infinite recursion, so we stop them early as soon as we detect them.
#[derive(Clone, Debug)]
pub struct BuildReason(Option<Arc<Inner>>);

impl BuildReason {
	/// Creates an empty build reason.
	///
	/// This should be used for top-level targets that are being built either because
	/// the user requested, or because their dependencies have changed on-disk while
	/// watching.
	pub const fn empty() -> Self {
		Self(None)
	}

	/// Adds a target to this build reason.
	///
	/// This creates a child reason, with this reason as the parent, indicating
	/// that `target` was the reason why it's being built.
	pub fn with_target(&self, target: Target<ArcStr>) -> Self {
		Self(Some(Arc::new(Inner {
			target,
			prev: self.clone(),
		})))
	}

	/// Returns this reason's target, if any
	pub fn target(&self) -> Option<&Target<ArcStr>> {
		self.0.as_ref().map(|reason| &reason.target)
	}

	/// Iterates over all reasons.
	///
	/// Iterates from the most recent reason, i.e. the parent,
	/// to the oldest reason.
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

	/// Collects all reasons.
	///
	/// The order is from most recent (i.e. the parent) to the oldest reason.
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
