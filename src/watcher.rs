//! Target watcher

// TODO: Should we react when rule outputs are changed, or only on leaf-dependencies

// TODO: Output dependencies aren't considered here, if they didn't exist when the file was first built.

use std::time::SystemTime;

// Imports
use {
	crate::{build, rules::Target, AppError, Builder, Rules},
	anyhow::Context,
	dashmap::{DashMap, DashSet},
	futures::{stream::FuturesUnordered, StreamExt, TryStreamExt},
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
struct RevDep {
	/// Target of the dependency
	target: Target<String>,

	/// All parent targets
	parents: Arc<DashSet<Target<String>>>,
}

/// Target watcher
pub struct Watcher {
	/// Watcher
	watcher: Debouncer<notify::RecommendedWatcher>,

	/// Reverse dependencies
	rev_deps: Arc<DashMap<PathBuf, RevDep>>,

	/// File event stream
	fs_event_stream: ReceiverStream<notify_debouncer_mini::DebouncedEvent>,
}

impl Watcher {
	/// Creates a new watcher
	pub fn new(builder_event_rx: async_broadcast::Receiver<build::Event>) -> Result<Self, AppError> {
		// Create the watcher
		let (fs_event_tx, fs_event_rx) = tokio::sync::mpsc::channel(16);
		let watcher =
			notify_debouncer_mini::new_debouncer(Duration::from_secs(1), None, move |fs_events| match fs_events {
				Ok(fs_events) =>
					for fs_event in fs_events {
						tracing::trace!(?fs_event, "Watcher fs event");
						#[allow(let_underscore_drop)] // We don't care if it succeeded or not
						let _ = fs_event_tx.blocking_send(fs_event);
					},
				Err(errs) =>
					for err in errs {
						tracing::warn!("Error while watching: {:?}", anyhow::Error::from(err));
					},
			})
			.context("Unable to create file watcher")
			.map_err(AppError::Other)?;

		// Then create the task to register all dependencies
		// TODO: Not do this?
		let rev_deps = Arc::new(DashMap::new());
		tokio::task::spawn({
			let rev_deps = Arc::clone(&rev_deps);
			async move {
				builder_event_rx
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
									Target::File { file, .. } => match Path::new(file).canonicalize() {
										Ok(path) => path,
										Err(err) => {
											tracing::warn!("Unable to canonicalize {file:?}: {err:?}");
											return;
										},
									},
									Target::Rule { .. } => return,
								};

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
			}
		});

		Ok(Self {
			watcher,
			rev_deps,
			fs_event_stream: ReceiverStream::new(fs_event_rx),
		})
	}

	/// Watches over all files and rebuilds any changed files
	pub async fn watch_rebuild(
		mut self,
		builder: &Builder,
		rules: &Rules,
		ignore_missing: bool,
	) -> Result<(), AppError> {
		// Watch each target we have the reverse dependencies of
		for entry in self.rev_deps.iter() {
			let path = entry.key();
			self.watcher
				.watcher()
				.watch(path, notify::RecursiveMode::NonRecursive)
				.with_context(|| format!("Unable to watch path {path:?}"))
				.map_err(AppError::Other)?;
		}

		let rev_deps = &self.rev_deps;
		self.fs_event_stream
			.then(async move |event| {
				// Canonicalize the path
				let path = match event.path.canonicalize() {
					Ok(path) => path,
					Err(err) => {
						tracing::warn!("Unable to canonicalize {:?}: {err:?}", event.path);
						return Ok(());
					},
				};

				// Then get the reverse dependencies
				let rev_dep = match rev_deps.get(&path) {
					Some(rev_dep) => rev_dep.clone(),
					None => return Ok(()),
				};
				tracing::info!("Changed: {:?}", rev_dep.target);
				tracing::trace!(?rev_dep, "Reverse dependencies");

				// Note: We clone the parents so we don't hold onto the rev dep lock for too long
				let dep_parents = rev_dep
					.parents
					.iter()
					.map(|target| (*target).clone())
					.collect::<Vec<_>>();


				// Reset the dependency and all parents' builds
				futures::try_join!(
					builder.reset_build(&rev_dep.target, rules),
					dep_parents
						.iter()
						.map(async move |target| builder.reset_build(target, rules).await)
						.collect::<FuturesUnordered<_>>()
						.try_collect::<()>()
				)?;

				// Then rebuild them
				// Note: We do this separately to ensure that when we have the following scenario:
				//       A ----> B
				//        \- C -/
				//       We don't first rebuild B fully, then C gets rebuilt, and B gets rebuilt *again*,
				//       unnecessarily. By resetting B and C, building B first will build C, then C won't
				//       get rebuilt.
				// TODO: Not duplicate this from `main`
				let build_start_time = SystemTime::now();
				dep_parents
					.iter()
					.map(async move |target| {
						tracing::info!("Rechecking: {target:?}");
						let res = builder
							.build(target, rules, ignore_missing)
							.await
							.map_err(AppError::build_target(target));
						(target, res)
					})
					.collect::<FuturesUnordered<_>>()
					.for_each(async move |(target, res)| match res {
						Ok((build_res, _)) => {
							let build_duration = build_res
								.build_time
								.duration_since(build_start_time)
								.expect("Build time was negative");
							tracing::info!("Built {target} in {build_duration:.2?}");
						},
						Err(err) => tracing::error!("Unable to build {target}: {err}"),
					})
					.await;

				tracing::trace!("Rebuilt all: {path:?}");

				Ok::<_, AppError>(())
			})
			.try_collect::<()>()
			.await?;

		Ok(())
	}
}
