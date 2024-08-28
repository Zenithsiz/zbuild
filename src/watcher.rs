//! Target watcher

// TODO: Should we react when rule outputs are changed, or only on leaf-dependencies

// TODO: Output dependencies aren't considered here, if they didn't exist when the file was first built.

// TODO: Rule targets are called multiple times when rebuilding, somehow prevent that?

// Imports
use {
	crate::{build, rules::Target, util::ArcStr, AppError, Builder, Rules},
	anyhow::Context,
	dashmap::{DashMap, DashSet},
	futures::{stream::FuturesUnordered, StreamExt},
	notify::Watcher as _,
	notify_debouncer_full::Debouncer,
	std::{
		io,
		path::{Path, PathBuf},
		sync::Arc,
		time::Duration,
	},
	tokio::sync::mpsc,
	tokio_stream::wrappers::ReceiverStream,
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
pub struct Watcher {
	/// Watcher
	watcher: Debouncer<notify::RecommendedWatcher, notify_debouncer_full::FileIdMap>,

	/// Reverse dependencies
	rev_deps: DashMap<PathBuf, RevDep>,

	/// File event stream
	fs_event_stream: ReceiverStream<notify_debouncer_full::DebouncedEvent>,

	/// Builder event receiver
	builder_event_rx: async_broadcast::Receiver<build::Event>,
}

impl Watcher {
	/// Creates a new watcher
	pub fn new(
		builder_event_rx: async_broadcast::Receiver<build::Event>,
		debouncer_timeout: Duration,
	) -> Result<Self, AppError> {
		// Create the watcher
		let (fs_event_tx, fs_event_rx) = mpsc::channel(16);
		let watcher = notify_debouncer_full::new_debouncer(debouncer_timeout, None, move |fs_events| match fs_events {
			Ok(fs_events) =>
				for fs_event in fs_events {
					tracing::trace!(?fs_event, "Watcher fs event");

					#[expect(
						let_underscore_drop,
						clippy::let_underscore_must_use,
						reason = "We don't care if it succeeded or not"
					)]
					let _: Result<(), _> = fs_event_tx.blocking_send(fs_event);
				},
			Err(errs) =>
				for err in errs {
					tracing::warn!(err=?anyhow::Error::from(err), "Error while watching");
				},
		})
		.context("Unable to create file watcher")
		.map_err(AppError::Other)?;

		Ok(Self {
			watcher,
			rev_deps: DashMap::new(),
			fs_event_stream: ReceiverStream::new(fs_event_rx),
			builder_event_rx,
		})
	}

	/// Watches over all files and rebuilds any changed files
	#[expect(clippy::too_many_lines, reason = "TODO: Refactor")]
	pub async fn watch_rebuild(mut self, builder: &Arc<Builder>, rules: &Arc<Rules>, ignore_missing: bool) {
		let rev_deps = &self.rev_deps;
		futures::future::join(
			async move {
				self.builder_event_rx
					.map(move |event| {
						tracing::trace!(?event, "Watcher build event");
						match event {
							build::Event::TargetDepBuilt { target, dep } => {
								// Ignore static dependencies
								// TODO: If the event is a removal event, we might care about a removal, or should
								//       we enforce to the user that static items really should live for as long as
								//       zbuild lives for?
								if let Target::File { is_static, .. } = dep &&
									is_static
								{
									return;
								}

								// Ignore non-file targets and canonicalize file ones
								let dep_path = match &dep {
									Target::File { file, .. } => match Path::new(&**file).canonicalize() {
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
								if let Err(err) = self.watcher.watcher().watch(
									dep_path.parent().unwrap_or(&dep_path),
									notify::RecursiveMode::NonRecursive,
								) {
									tracing::warn!(?dep_path, ?err, "Unable to watch path");
								}

								// Note: We don't care if we add a duplicate target
								let _: bool = rev_deps
									.entry(dep_path)
									.or_insert_with(|| RevDep {
										target:  dep,
										parents: Arc::new(DashSet::new()),
									})
									.parents
									.insert(target);
							},
						}
					})
					.collect::<()>()
					.await;
				tracing::trace!("Watcher task exited");
			},
			self.fs_event_stream
				.filter(|event| {
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

					async move { allow }
				})
				.flat_map(|event| futures::stream::iter(event.event.paths))
				.then(move |path| async move {
					// Canonicalize the path
					let path = match path.canonicalize() {
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
					let rev_dep = match rev_deps.get(&path) {
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
							builder
								.reset_build(&rev_dep.target, rules)
								.await
								.expect("Unable to reset existing build");
						},
						dep_parents
							.iter()
							.map(move |target| async move {
								builder
									.reset_build(target, rules)
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
							#[expect(clippy::let_underscore_must_use, reason = "We don't care if the build succeeds")]
							let _: Result<(), _> = crate::build_target(builder, target, rules, ignore_missing).await;
						})
						.collect::<FuturesUnordered<_>>()
						.collect::<()>()
						.await;

					tracing::trace!(?path, "Rebuilt all reverse dependencies");
				})
				.collect::<()>(),
		)
		.await;
	}
}
