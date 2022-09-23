//! Target watcher

use std::time::Duration;

use crate::Rules;

use {
	crate::{build, rules::Target, AppError, Builder},
	anyhow::Context,
	dashmap::{DashMap, DashSet},
	futures::{stream::FuturesUnordered, StreamExt, TryStreamExt},
	std::{
		path::{Path, PathBuf},
		sync::Arc,
	},
	tokio_stream::wrappers::ReceiverStream,
};

/// A reverse dependency
#[derive(Clone, Debug)]
struct RevDep {
	/// Target of the dependency
	target: Target<String>,

	/// All parent targets
	parents: DashSet<Target<String>>,
}

/// Target watcher
#[derive(Debug)]
pub struct Watcher {
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
		let mut watcher =
			notify_debouncer_mini::new_debouncer(Duration::from_secs(1), None, move |fs_events| match fs_events {
				Ok(fs_events) =>
					for fs_event in fs_events {
						tracing::trace!(?fs_event, "Watcher fs event");
						let _ = fs_event_tx.blocking_send(fs_event);
					},
				Err(errs) =>
					for err in errs {
						tracing::warn!("Error while watching: {:?}", anyhow::Error::from(err))
					},
			})
			.context("Unable to create file watcher")?;

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
								// Ignore non-file targets and canonicalize file ones
								let dep_path = match &dep {
									Target::File { file } => match Path::new(file).canonicalize() {
										Ok(path) => path,
										Err(err) => {
											tracing::warn!("Unable to canonicalize {file:?}: {err:?}");
											return;
										},
									},
									Target::Rule { .. } => return,
								};

								rev_deps
									.entry(dep_path.clone())
									.or_insert_with(|| RevDep {
										target:  dep,
										parents: DashSet::new(),
									})
									.parents
									.insert(target);
								let _ = watcher.watcher().watch(&dep_path, notify::RecursiveMode::NonRecursive);
							},
						}
					})
					.collect::<()>()
					.await;
				tracing::trace!("Watcher task exited");
			}
		});

		Ok(Self {
			rev_deps,
			fs_event_stream: ReceiverStream::new(fs_event_rx),
		})
	}

	/// Watches over all files and rebuilds any changed files
	pub async fn watch_rebuild(self, builder: &Builder, rules: &Rules) -> Result<(), AppError> {
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

				tracing::info!("Changed: {path:?}");

				// Then get the reverse dependencies
				let rev_dep = match rev_deps.get(&path) {
					Some(rev_dep) => rev_dep.clone(),
					None => return Ok(()),
				};

				// Mark the dependency as outdated
				builder.mark_outdated(&rev_dep.target).await?;

				// Then rebuild all parents
				rev_dep
					.parents
					.iter()
					.map(|target| (*target).clone())
					.collect::<Vec<_>>()
					.into_iter()
					.map(async move |target| {
						tracing::info!("Rechecking: {target:?}");
						builder.mark_outdated(&target).await?;
						builder.build(&target, rules).await.res()?;
						Ok::<_, AppError>(())
					})
					.collect::<FuturesUnordered<_>>()
					.try_collect::<()>()
					.await?;

				tracing::trace!("Rebuilt all: {path:?}");

				Ok::<_, AppError>(())
			})
			.try_collect::<()>()
			.await?;

		Ok(())
	}
}
