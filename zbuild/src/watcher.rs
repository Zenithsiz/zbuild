//! Target watcher

// TODO: Should we react when rule outputs are changed, or only on leaf-dependencies

// TODO: Output dependencies aren't considered here, if they didn't exist when the file was first built.

// TODO: Rule targets are called multiple times when rebuilding, somehow prevent that?

// TODO: This needs to use the filesystem trait to access it...

// Imports
use {
	crate::{AppError, Builder, Fs, rules::Target, util::ArcStr},
	app_error::Context,
	core::clone::Share,
	dashmap::{DashMap, DashSet},
	futures::{StreamExt, stream::FuturesUnordered},
	notify_debouncer_full::{DebouncedEvent, Debouncer},
	std::{io, path::PathBuf, sync::Arc, time::Duration},
	tokio::sync::Mutex,
};

/// A reverse dependency
#[derive(Clone, Debug)]
struct RevDep {
	/// Target of the dependency
	target: Target<ArcStr>,

	/// All parent targets
	parents: Arc<DashSet<Target<ArcStr>>>,
}

/// Target watcher
#[derive(Debug)]
pub struct Watcher {
	inner:          Mutex<Option<Debouncer<notify::RecommendedWatcher, notify_debouncer_full::RecommendedCache>>>,
	rev_deps:       DashMap<PathBuf, RevDep>,
	builder:        Arc<Builder>,
	ignore_missing: bool,
}

impl Watcher {
	/// Creates a new watcher
	pub async fn new<FS: Fs>(
		fs: FS,
		debouncer_timeout: Duration,
		builder: Arc<Builder>,
		ignore_missing: bool,
	) -> Result<Arc<Self>, AppError> {
		let this = Self {
			inner: Mutex::new(None),
			rev_deps: DashMap::new(),
			builder,
			ignore_missing,
		};
		let this = Arc::new(this);

		let tokio = tokio::runtime::Handle::current();
		let fs_watcher = {
			let this = this.share();
			notify_debouncer_full::new_debouncer(debouncer_timeout, None, move |fs_events| match fs_events {
				Ok(fs_events) =>
					for event in fs_events {
						tokio.block_on(this.on_fs_event(&fs, &event));
					},
				Err(errs) =>
					for err in &errs {
						tracing::warn!(err=?AppError::from(err), "Error while watching");
					},
			})
			.context("Unable to create file watcher")?
		};
		*this.inner.lock().await = Some(fs_watcher);

		Ok(this)
	}

	/// Notifies that a target dependency was built
	pub async fn on_target_dep_built<FS: Fs>(&self, fs: &FS, target: &Target<ArcStr>, dep: &Target<ArcStr>) {
		// Ignore static dependencies
		// TODO: If the event is a removal event, we might care about a removal, or should
		//       we enforce to the user that static items really should live for as long as
		//       zbuild lives for?
		if let Target::File { is_static, .. } = *dep &&
			is_static
		{
			return;
		}

		// Ignore non-file targets and canonicalize file ones
		let dep_path = match &dep {
			Target::File { file, .. } => match fs.canonicalize(&**file) {
				Ok(path) => path,
				Err(err) => {
					tracing::warn!(?file, ?err, "Unable to canonicalize");
					return;
				},
			},
			Target::Rule { .. } => return,
		};

		// Watch the path's parent
		// Note: We do this since we may not receive events if the file is deleted,
		//       but by watching the parent directory, we ensure we get it.
		// TODO: Is this enough? What if the parent directory also gets deleted?
		//       should we watch directories until the root?
		tracing::trace!(?dep_path, "Starting to watch path");
		match &mut *self.inner.lock().await {
			Some(watcher) =>
				if let Err(err) = watcher.watch(
					dep_path.parent().unwrap_or(&dep_path),
					notify::RecursiveMode::NonRecursive,
				) {
					tracing::warn!(?dep_path, ?err, "Unable to watch path");
				},
			None => tracing::warn!("Filesystem watcher is missing"),
		}

		// Note: We don't care if we add a duplicate target
		let _: bool = self
			.rev_deps
			.entry(dep_path)
			.or_insert_with(|| RevDep {
				target:  dep.clone(),
				parents: Arc::new(DashSet::new()),
			})
			.parents
			.insert(target.clone());
	}

	async fn on_fs_event<FS: Fs>(&self, fs: &FS, event: &DebouncedEvent) {
		#[expect(
			clippy::match_same_arms,
			reason = "It reads and is more easily modify-able like this"
		)]
		let allow = match event.event.kind {
			notify::EventKind::Any => true,
			notify::EventKind::Access(kind) => match kind {
				notify::event::AccessKind::Any => true,
				notify::event::AccessKind::Read => false,
				notify::event::AccessKind::Open(_) => false,
				notify::event::AccessKind::Close(mode) => match mode {
					notify::event::AccessMode::Any => true,
					notify::event::AccessMode::Execute => false,
					notify::event::AccessMode::Read => false,
					notify::event::AccessMode::Write => true,
					notify::event::AccessMode::Other => true,
				},
				notify::event::AccessKind::Other => true,
			},
			notify::EventKind::Create(_) => true,
			notify::EventKind::Modify(kind) => match kind {
				notify::event::ModifyKind::Any => true,
				notify::event::ModifyKind::Data(_) => false,
				notify::event::ModifyKind::Metadata(_) => true,
				notify::event::ModifyKind::Name(_) => true,
				notify::event::ModifyKind::Other => true,
			},
			notify::EventKind::Remove(_) => true,
			notify::EventKind::Other => true,
		};

		if !allow {
			return;
		}

		for path in &event.paths {
			// Canonicalize the path
			let path = match fs.canonicalize(path) {
				Ok(path) => path,
				Err(err) => {
					// TODO: Warn on all occasions once this code path isn't hit
					//       by random files that aren't actually dependencies.
					if err.kind() != io::ErrorKind::NotFound {
						tracing::warn!(?path, ?err, "Unable to canonicalize");
					}
					return;
				},
			};

			// Then get the reverse dependencies
			let rev_dep = match self.rev_deps.get(&path) {
				Some(rev_dep) => rev_dep.clone(),
				None => return,
			};
			tracing::debug!(?rev_dep.target, "Changed");
			tracing::trace!(?rev_dep, "Reverse dependencies");

			// Note: We clone the parents so we don't hold onto the rev dep lock for too long
			let dep_parents = rev_dep
				.parents
				.iter()
				.map(|target| (*target).clone())
				.collect::<Vec<_>>();


			// Reset the dependency and all parents' builds
			futures::join!(
				async move {
					self.builder
						.reset_build(&rev_dep.target)
						.await
						.expect("Unable to reset existing build");
				},
				dep_parents
					.iter()
					.map(move |target| async move {
						self.builder
							.reset_build(target)
							.await
							.expect("Unable to reset existing build");
					})
					.collect::<FuturesUnordered<_>>()
					.collect::<()>()
			);

			// Then rebuild them
			// Note: We do this separately to ensure that when we have the following scenario:
			//       A ----> B
			//        \- C -/
			//       We don't first rebuild B fully, then C gets rebuilt, and B gets rebuilt *again*,
			//       unnecessarily. By resetting B and C, building B first will build C, then C won't
			//       get rebuilt.
			dep_parents
				.iter()
				.map(|target| async {
					let _: Result<(), _> = crate::build_target(fs, &self.builder, target, self.ignore_missing).await;
				})
				.collect::<FuturesUnordered<_>>()
				.collect::<()>()
				.await;

			tracing::trace!(?path, "Rebuilt all reverse dependencies");
		}
	}
}
