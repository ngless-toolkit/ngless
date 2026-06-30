//! Removal of lock files when NGLess is killed by a signal (Ctrl+C / SIGTERM).
//!
//! Lock files ([`crate::lockfile::LockGuard`]) are normally removed by their `Drop` impl — on
//! success, on error, and while unwinding from a panic. A Unix signal, however, terminates the
//! process *without* unwinding the stack, so no destructor runs and the lock file is left behind.
//! This is exactly the "Ctrl+C leaves lock files around" symptom.
//!
//! The Haskell original does not have this problem: GHC's runtime turns SIGINT into an asynchronous
//! exception delivered to the main thread, which unwinds through every `ResourceT`/`bracket` and
//! runs the registered cleanup. To reproduce that behaviour we keep a global registry of paths that
//! must be removed if the process is killed, and arrange for a dedicated thread to remove them on
//! SIGINT/SIGTERM before re-raising the signal (so the exit status still reflects the interruption).
//!
//! Mechanism (Unix): a *self-pipe*. A tiny, async-signal-safe handler writes the signal number to a
//! pipe; a dedicated thread reads it and performs the actual removal with ordinary code (the
//! registry `Mutex` and recursive directory removal are *not* async-signal-safe, so they cannot run
//! inside the handler itself). We deliberately do **not** block the signals process-wide: blocking
//! would be inherited across `execve` by child tools (samtools, bwa, …), leaving them unable to
//! react to Ctrl+C. A caught disposition installed with `sigaction` is instead reset to the default
//! in the child by `execve`, so child processes keep their normal Ctrl+C behaviour.
//!
//! Each artifact is registered when created and unregistered when its owner's `Drop` removes it on
//! the normal path, so the registry only ever holds paths whose owner is still alive.

use std::path::{Path, PathBuf};
use std::sync::{Mutex, OnceLock};

struct Entry {
    id: u64,
    path: PathBuf,
}

#[derive(Default)]
struct Registry {
    next_id: u64,
    entries: Vec<Entry>,
}

fn registry() -> &'static Mutex<Registry> {
    static REGISTRY: OnceLock<Mutex<Registry>> = OnceLock::new();
    REGISTRY.get_or_init(|| Mutex::new(Registry::default()))
}

/// Register `path` for removal if the process is killed by a signal. Returns an id to pass to
/// [`unregister`] once the owner has removed the path itself.
pub fn register(path: &Path) -> u64 {
    let mut reg = registry().lock().unwrap();
    let id = reg.next_id;
    reg.next_id += 1;
    reg.entries.push(Entry {
        id,
        path: path.to_path_buf(),
    });
    id
}

/// Drop the registration created by [`register`]. Called from the owner's normal `Drop` path, where
/// the artifact is removed conventionally and no signal-time cleanup is needed. Order is irrelevant
/// (see [`run_cleanup`]), so this uses an O(1) `swap_remove`.
pub fn unregister(id: u64) {
    let mut reg = registry().lock().unwrap();
    if let Some(pos) = reg.entries.iter().position(|e| e.id == id) {
        reg.entries.swap_remove(pos);
    }
}

/// Remove every still-registered path. Called from the signal-cleanup thread (and unit tests).
/// "Does-not-exist" errors are ignored.
fn run_cleanup() {
    let entries = std::mem::take(&mut registry().lock().unwrap().entries);
    for e in entries {
        let _ = std::fs::remove_file(&e.path);
    }
}

/// Install signal-driven cleanup. Idempotent; safe to call more than once. On non-Unix platforms
/// this is a no-op and artifacts rely on `Drop` alone (as before).
pub fn install() {
    #[cfg(unix)]
    unix::install();
}

#[cfg(unix)]
mod unix {
    use super::run_cleanup;
    use std::os::unix::io::RawFd;
    use std::sync::atomic::{AtomicBool, AtomicI32, Ordering};

    /// Write end of the self-pipe, read by the waiter thread. `-1` until [`install`] runs.
    static WRITE_FD: AtomicI32 = AtomicI32::new(-1);

    /// Async-signal-safe handler: write the signal number to the self-pipe and return. `write` is on
    /// the list of async-signal-safe functions; a short or failed write is ignored, the worst case
    /// being a missed cleanup rather than undefined behaviour.
    extern "C" fn handler(sig: libc::c_int) {
        let fd = WRITE_FD.load(Ordering::SeqCst);
        if fd >= 0 {
            let byte = sig as u8;
            unsafe {
                libc::write(fd, &byte as *const u8 as *const libc::c_void, 1);
            }
        }
    }

    pub fn install() {
        static INSTALLED: AtomicBool = AtomicBool::new(false);
        if INSTALLED.swap(true, Ordering::SeqCst) {
            return;
        }
        let mut fds = [0 as libc::c_int; 2];
        if unsafe { libc::pipe(fds.as_mut_ptr()) } != 0 {
            // Without the pipe we silently fall back to Drop-only cleanup.
            return;
        }
        let (read_fd, write_fd) = (fds[0], fds[1]);
        // Keep both ends out of child processes (samtools, bwa, …).
        unsafe {
            libc::fcntl(read_fd, libc::F_SETFD, libc::FD_CLOEXEC);
            libc::fcntl(write_fd, libc::F_SETFD, libc::FD_CLOEXEC);
        }
        WRITE_FD.store(write_fd, Ordering::SeqCst);

        for &sig in &[libc::SIGINT, libc::SIGTERM] {
            let mut action: libc::sigaction = unsafe { std::mem::zeroed() };
            action.sa_sigaction = handler as libc::sighandler_t;
            action.sa_flags = libc::SA_RESTART;
            unsafe {
                libc::sigemptyset(&mut action.sa_mask);
                libc::sigaction(sig, &action, std::ptr::null_mut());
            }
        }

        std::thread::Builder::new()
            .name("ngless-signal-cleanup".into())
            .spawn(move || waiter(read_fd))
            .expect("spawning the signal-cleanup thread");
    }

    /// Block on the self-pipe; when a signal arrives, remove the registered artifacts, then restore
    /// the default disposition and re-raise so the process terminates as if by the signal (e.g. exit
    /// status 130 for Ctrl+C).
    fn waiter(read_fd: RawFd) {
        loop {
            let mut buf = [0u8; 1];
            let n = unsafe { libc::read(read_fd, buf.as_mut_ptr() as *mut libc::c_void, 1) };
            if n != 1 {
                // EINTR or a spurious wakeup: the write end is held for the whole run, so there is
                // never an EOF to busy-loop on. Re-block on read.
                continue;
            }
            let sig = buf[0] as libc::c_int;
            run_cleanup();
            unsafe {
                let mut action: libc::sigaction = std::mem::zeroed();
                action.sa_sigaction = libc::SIG_DFL;
                libc::sigemptyset(&mut action.sa_mask);
                libc::sigaction(sig, &action, std::ptr::null_mut());
                libc::raise(sig);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn run_cleanup_removes_registered_and_respects_unregister() {
        let dir = std::env::temp_dir().join(format!("ngless_cleanup_test_{}", std::process::id()));
        let _ = std::fs::create_dir_all(&dir);
        let kept = dir.join("kept.lock");
        let removed = dir.join("removed.lock");
        std::fs::write(&kept, b"x").unwrap();
        std::fs::write(&removed, b"x").unwrap();

        let kept_id = register(&kept);
        register(&removed);
        // The owner of `kept` removes it on its normal Drop path and unregisters.
        unregister(kept_id);
        std::fs::remove_file(&kept).unwrap();

        run_cleanup();
        assert!(
            !removed.exists(),
            "registered path removed by signal cleanup"
        );
        assert!(!kept.exists());
        let _ = std::fs::remove_dir_all(&dir);
    }
}
