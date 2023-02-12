//! Target watcher

// TODO: Should we react when rule outputs are changed, or only on leaf-dependencies

// TODO: Output dependencies aren't considered here, if they didn't exist when the file was first built.

// Imports
use {
	crate::{build, rules::Target, util::CowStr, AppError, Builder, Rules},
	anyhow::Context,
	dashmap::{DashMap, DashSet},
	futures::{stream::FuturesUnordered, StreamExt},
	notify_debouncer_mini::Debouncer,
	std::{
		path::{Path, PathBuf},
		sync::Arc,
		time::Duration,
	},
	tokio_stream::wrappers::ReceiverStream,
};

/// A reverse dependency
#[derive(Clone, Debug)]
struct RevDep<'s> {
	/// Target of the dependency
	target: Target<'s, CowStr<'s>>,

	/// All parent targets
	parents: Arc<DashSet<Target<'s, CowStr<'s>>>>,
}

/// Target watcher
pub struct Watcher<'s> {
	/// Watcher
	watcher: Debouncer<notify::RecommendedWatcher>,

	/// Reverse dependencies
	rev_deps: DashMap<PathBuf, RevDep<'s>>,

	/// File event stream
	fs_event_stream: ReceiverStream<notify_debouncer_mini::DebouncedEvent>,

	/// Builder event receiver
	builder_event_rx: async_broadcast::Receiver<build::Event<'s>>,
}

impl<'s> Watcher<'s> {
	/// Creates a new watcher
	pub fn new(
		builder_event_rx: async_broadcast::Receiver<build::Event<'s>>,
		watch_debouncer_timeout_ms: f64,
	) -> Result<Self, AppError> {
		// Create the watcher
		let (fs_event_tx, fs_event_rx) = tokio::sync::mpsc::channel(16);
		let watcher = notify_debouncer_mini::new_debouncer(
			Duration::from_secs_f64(watch_debouncer_timeout_ms / 1000.0),
			None,
			move |fs_events| match fs_events {
				Ok(fs_events) =>
					for fs_event in fs_events {
						tracing::trace!(?fs_event, "Watcher fs event");
						#[expect(let_underscore_drop)] // We don't care if it succeeded or not
						let _ = fs_event_tx.blocking_send(fs_event);
					},
				Err(errs) =>
					for err in errs {
						tracing::warn!("Error while watching: {:?}", anyhow::Error::from(err));
					},
			},
		)
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
	pub async fn watch_rebuild(mut self, builder: &Builder<'s>, rules: &Rules<'s>, ignore_missing: bool) {
		let rev_deps = &self.rev_deps;
		futures::join!(
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
								if let Target::File { is_static, .. } = dep && is_static {
									return;
								}

								// Ignore non-file targets and canonicalize file ones
								let dep_path = match &dep {
									Target::File { file, .. } => match Path::new(&**file).canonicalize() {
										Ok(path) => path,
										Err(err) => {
											tracing::warn!("Unable to canonicalize {file:?}: {err:?}");
											return;
										},
									},
									Target::Rule { .. } => return,
								};

								// Watch the path
								tracing::debug!(?dep_path, "Starting to watch path");
								if let Err(err) = self
									.watcher
									.watcher()
									.watch(&dep_path, notify::RecursiveMode::NonRecursive)
								{
									tracing::warn!("Unable to watch path {dep_path:?}: {err:?}")
								}

								rev_deps
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
				.then(async move |event| {
					// Canonicalize the path
					let path = match event.path.canonicalize() {
						Ok(path) => path,
						Err(err) => {
							tracing::warn!("Unable to canonicalize {:?}: {err:?}", event.path);
							return;
						},
					};

					// Then get the reverse dependencies
					let rev_dep = match rev_deps.get(&path) {
						Some(rev_dep) => rev_dep.clone(),
						None => return,
					};
					tracing::debug!("Changed: {:?}", rev_dep.target);
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
								.expect("Unable to reset existing build")
						},
						dep_parents
							.iter()
							.map(async move |target| builder
								.reset_build(target, rules)
								.await
								.expect("Unable to reset existing build"))
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
						.map(|target| crate::build_target(builder, target, rules, ignore_missing))
						.collect::<FuturesUnordered<_>>()
						.collect::<()>()
						.await;

					tracing::trace!(?path, "Rebuilt all reverse dependencies");
				})
				.collect::<()>()
		);
	}
}
