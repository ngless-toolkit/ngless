//! Lock files for serialising expensive, cacheable side effects across concurrent NGLess
//! processes — chiefly reference downloads, FASTA splitting and mapper-index creation. This
//! mirrors `NGLess/Utils/LockFile.hs`.
//!
//! The model: a lock is an empty-ish file created **atomically** with `O_CREAT | O_EXCL`
//! (`create_new`), so exactly one process wins the race. The winner holds a [`LockGuard`]; when it
//! is dropped (normally or while unwinding) the lock file is removed, mirroring Haskell's
//! `ResourceT` `register (removeFileIfExists ...)`. A loser consults [`WhenExistsStrategy`]: do
//! nothing, fail, or sleep-and-retry. A lock file older than `max_age` is assumed stale (its owner
//! died) and is removed; with `mtime_update` the holder bumps the file's mtime every 10 minutes so
//! a long-running job is not mistaken for stale.

use std::fs;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::{Duration, SystemTime};

use crate::errors::{NgError, NgErrorType, NgResult};

/// Parameters for [`with_lock_file`]/[`acquire_lock`], mirroring Haskell's `LockParameters`.
pub struct LockParameters {
    /// The lock file to create.
    pub lock_fname: PathBuf,
    /// A lock file older than this is treated as stale and removed.
    pub max_age: Duration,
    /// What to do when the lock is already held by someone else.
    pub when_exists: WhenExistsStrategy,
    /// Spawn a background thread that touches the lock file every 10 minutes so it is not
    /// mistaken for stale during a long-running critical section.
    pub mtime_update: bool,
}

/// What to do when the lock file already exists and is current, mirroring `WhenExistsStrategy`.
pub enum WhenExistsStrategy {
    /// Give up quietly: [`acquire_lock`] returns `Ok(None)`.
    IfLockedNothing,
    /// Fail with this error.
    IfLockedThrow(NgError),
    /// Sleep `time_between` and retry, up to `nr_retries` times.
    IfLockedRetry {
        nr_retries: u32,
        time_between: Duration,
    },
}

/// An RAII handle to a held lock. Dropping it removes the lock file and stops the mtime-update
/// thread (mirrors the `ReleaseKey` registered in `acquireLock`).
pub struct LockGuard {
    path: PathBuf,
    stop: Option<Arc<AtomicBool>>,
}

impl Drop for LockGuard {
    fn drop(&mut self) {
        if let Some(stop) = &self.stop {
            stop.store(true, Ordering::SeqCst);
        }
        remove_file_if_exists(&self.path);
    }
}

/// Run `act` with `params`'s lock held, mirroring `withLockFile`. If the lock cannot be acquired
/// (only possible with [`WhenExistsStrategy::IfLockedNothing`]) this is a system error, exactly as
/// in Haskell (`throwSystemError "Could not acquire required lock file."`).
pub fn with_lock_file<T>(
    params: LockParameters,
    act: impl FnOnce() -> NgResult<T>,
) -> NgResult<T> {
    match acquire_lock(params)? {
        Some(_guard) => act(),
        None => Err(NgError::new(
            NgErrorType::SystemError,
            "Could not acquire required lock file.",
        )),
    }
}

/// Atomically create the lock file, mirroring `acquireLock`. Returns `Ok(Some(guard))` on success,
/// `Ok(None)` when the lock is held and the strategy is [`WhenExistsStrategy::IfLockedNothing`], or
/// an error on a retry-exhausted/throw strategy.
pub fn acquire_lock(mut params: LockParameters) -> NgResult<Option<LockGuard>> {
    loop {
        match open_lock_file(&params.lock_fname)? {
            Some(()) => {
                let stop = if params.mtime_update {
                    Some(spawn_mtime_updater(params.lock_fname.clone()))
                } else {
                    None
                };
                return Ok(Some(LockGuard {
                    path: params.lock_fname,
                    stop,
                }));
            }
            None => match file_age(&params.lock_fname) {
                // It vanished between the failed create and the age check: retry immediately.
                None => continue,
                Some(age) if age > params.max_age => {
                    remove_file_if_exists(&params.lock_fname);
                    continue;
                }
                Some(_) => match params.when_exists {
                    WhenExistsStrategy::IfLockedNothing => return Ok(None),
                    WhenExistsStrategy::IfLockedThrow(err) => return Err(err),
                    WhenExistsStrategy::IfLockedRetry {
                        nr_retries,
                        time_between,
                    } => {
                        if nr_retries == 0 {
                            return Err(NgError::new(
                                NgErrorType::SystemError,
                                format!(
                                    "Could not obtain lock {} even after waiting for its release.",
                                    params.lock_fname.display()
                                ),
                            ));
                        }
                        std::thread::sleep(time_between);
                        params.when_exists = WhenExistsStrategy::IfLockedRetry {
                            nr_retries: nr_retries - 1,
                            time_between,
                        };
                        continue;
                    }
                },
            },
        }
    }
}

/// Atomically create `path` (mirrors `openLockFile`, which opens with `O_CREAT | O_EXCL`). Returns
/// `Ok(Some(()))` if we created it, `Ok(None)` if it already existed, and propagates any other IO
/// error. On success a small diagnostic line is written, mirroring Haskell's `hPutStrLn`.
fn open_lock_file(path: &Path) -> NgResult<Option<()>> {
    match fs::OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(path)
    {
        Ok(mut f) => {
            use std::io::Write;
            let _ = writeln!(
                f,
                "Lock file created for PID {} on hostname {} at time {}",
                std::process::id(),
                hostname(),
                now_string()
            );
            Ok(Some(()))
        }
        Err(e) if e.kind() == std::io::ErrorKind::AlreadyExists => Ok(None),
        Err(e) => Err(NgError::new(
            NgErrorType::SystemError,
            format!("Could not create lock file {}: {e}", path.display()),
        )),
    }
}

/// Spawn a thread that touches `path` every 10 minutes until told to stop, mirroring the
/// `mtimeUpdate` `updateloop` in `acquireLock`. The returned flag stops it when set (on guard drop).
fn spawn_mtime_updater(path: PathBuf) -> Arc<AtomicBool> {
    let stop = Arc::new(AtomicBool::new(false));
    let stop_thread = Arc::clone(&stop);
    std::thread::spawn(move || {
        // Poll the stop flag frequently but only touch every 10 minutes, so guard drop is prompt.
        let interval = Duration::from_secs(10 * 60);
        let mut elapsed = Duration::ZERO;
        let tick = Duration::from_millis(200);
        while !stop_thread.load(Ordering::SeqCst) {
            std::thread::sleep(tick);
            elapsed += tick;
            if elapsed >= interval {
                elapsed = Duration::ZERO;
                touch_file(&path);
            }
        }
    });
    stop
}

/// Update `path`'s modification time to now (mirrors `touchFile`). Best-effort.
fn touch_file(path: &Path) {
    // Rewriting the file's mtime without a dedicated syscall crate: open for append and write zero
    // bytes does not bump mtime, so re-set it via `set_modified` if available, else rewrite content.
    if let Ok(f) = fs::OpenOptions::new().write(true).open(path) {
        let _ = f.set_modified(SystemTime::now());
    }
}

/// Age of `path` (now − mtime), or `None` if it does not exist (mirrors `fileAge`).
fn file_age(path: &Path) -> Option<Duration> {
    let mtime = fs::metadata(path).ok()?.modified().ok()?;
    SystemTime::now().duration_since(mtime).ok().or(Some(Duration::ZERO))
}

/// Remove `path`, ignoring a "does not exist" error (mirrors `removeFileIfExists`).
pub fn remove_file_if_exists(path: &Path) {
    match fs::remove_file(path) {
        Ok(()) => {}
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
        Err(_) => {}
    }
}

/// Best-effort hostname for the diagnostic line; not part of any tested output.
fn hostname() -> String {
    std::env::var("HOSTNAME")
        .ok()
        .filter(|h| !h.is_empty())
        .or_else(|| {
            fs::read_to_string("/proc/sys/kernel/hostname")
                .ok()
                .map(|s| s.trim().to_string())
                .filter(|h| !h.is_empty())
        })
        .unwrap_or_else(|| "unknown".to_string())
}

/// A coarse timestamp for the diagnostic line (Unix seconds); not part of any tested output.
fn now_string() -> String {
    match SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
        Ok(d) => format!("{} (unix seconds)", d.as_secs()),
        Err(_) => "unknown".to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tmp_lock(name: &str) -> PathBuf {
        let mut p = std::env::temp_dir();
        p.push(format!("ngless-lockfile-test-{}-{}", std::process::id(), name));
        remove_file_if_exists(&p);
        p
    }

    #[test]
    fn acquire_release_round_trip() {
        let path = tmp_lock("round-trip");
        let params = LockParameters {
            lock_fname: path.clone(),
            max_age: Duration::from_secs(300),
            when_exists: WhenExistsStrategy::IfLockedNothing,
            mtime_update: false,
        };
        {
            let guard = acquire_lock(params).unwrap();
            assert!(guard.is_some(), "should acquire a fresh lock");
            assert!(path.exists(), "lock file present while held");
        }
        assert!(!path.exists(), "lock file removed on guard drop");
    }

    #[test]
    fn second_acquire_sees_held_lock() {
        let path = tmp_lock("held");
        let mk = |strategy| LockParameters {
            lock_fname: path.clone(),
            max_age: Duration::from_secs(300),
            when_exists: strategy,
            mtime_update: false,
        };
        let _held = acquire_lock(mk(WhenExistsStrategy::IfLockedNothing))
            .unwrap()
            .expect("first acquire wins");
        // A second attempt with IfLockedNothing gives None (it is current and not stale).
        let second = acquire_lock(mk(WhenExistsStrategy::IfLockedNothing)).unwrap();
        assert!(second.is_none(), "second acquire backs off");
        // IfLockedThrow surfaces the error.
        let err = acquire_lock(mk(WhenExistsStrategy::IfLockedThrow(NgError::new(
            NgErrorType::SystemError,
            "boom",
        ))));
        assert!(err.is_err());
    }

    #[test]
    fn stale_lock_is_reclaimed() {
        let path = tmp_lock("stale");
        // Pre-create a lock file and backdate its mtime well past max_age.
        fs::write(&path, b"old").unwrap();
        let old = SystemTime::now() - Duration::from_secs(10_000);
        fs::OpenOptions::new()
            .write(true)
            .open(&path)
            .unwrap()
            .set_modified(old)
            .unwrap();
        let params = LockParameters {
            lock_fname: path.clone(),
            max_age: Duration::from_secs(300),
            when_exists: WhenExistsStrategy::IfLockedNothing,
            mtime_update: false,
        };
        let guard = acquire_lock(params).unwrap();
        assert!(guard.is_some(), "a stale lock is removed and re-acquired");
        drop(guard);
        assert!(!path.exists());
    }

    #[test]
    fn with_lock_file_runs_action() {
        let path = tmp_lock("with");
        let params = LockParameters {
            lock_fname: path.clone(),
            max_age: Duration::from_secs(300),
            when_exists: WhenExistsStrategy::IfLockedNothing,
            mtime_update: false,
        };
        let out = with_lock_file(params, || Ok(42)).unwrap();
        assert_eq!(out, 42);
        assert!(!path.exists(), "lock released after the action");
    }
}
