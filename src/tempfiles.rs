//! Lifecycle management for temporary files and directories, mirroring
//! `NGLess/FileManagement.hs`.
//!
//! Every temporary path NGLess creates during a run is allocated through a single
//! [`TempFiles`] registry. This gives three guarantees the ad-hoc `temp_dir.join(...)`
//! approach did not:
//!
//! 1. **Exclusive creation.** Files are created with `O_CREAT | O_EXCL` (`create_new`)
//!    and directories with `fs::create_dir`, both of which fail if the name already
//!    exists. On a collision the registry bumps its counter and retries, so a name is
//!    atomically reserved (mirroring `openTempFileWithDefaultPermissions`) rather than
//!    silently clobbering a pre-existing file.
//! 2. **Legal names.** Basenames are capped at 240 characters (mirroring
//!    `checkFilenameLength`) so a long descriptive prefix cannot push the name past the
//!    filesystem's 255-byte limit.
//! 3. **Cleanup at exit.** Every allocated path is tracked and removed when the registry
//!    is dropped — on success, on error, and while unwinding from a panic — unless the
//!    user passed `--keep-temporary-files`. This is the RAII analogue of Haskell's
//!    `ResourceT` registration, in the same spirit as [`crate::lockfile::LockGuard`].

use std::fs;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Mutex;

use crate::errors::{NgError, NgErrorType, NgResult};
use crate::lockfile::remove_file_if_exists;

/// Owns the configured temporary directory and tracks everything created under it so it
/// can all be removed when the run ends. Allocation methods take `&self` (interior
/// mutability) to match the call style of the interpreter, which holds a single registry.
pub struct TempFiles {
    dir: PathBuf,
    keep: bool,
    /// Bumped per allocation to vary the candidate name. Uniqueness does not rely on it:
    /// the `O_EXCL` open (and exclusive `create_dir`) retries on any collision, so two
    /// registries sharing a temp dir still never clobber each other's files.
    counter: AtomicU64,
    /// Files and directories created, in allocation order.
    created: Mutex<Vec<PathBuf>>,
}

impl TempFiles {
    /// Create a registry rooted at `dir`. When `keep` is true the tracked paths are *not*
    /// removed at drop (mirrors `nConfKeepTemporaryFiles`). The directory is created if it
    /// does not exist (mirrors `createDirectoryIfMissing True`).
    pub fn new(dir: &Path, keep: bool) -> TempFiles {
        let _ = fs::create_dir_all(dir);
        TempFiles {
            dir: dir.to_path_buf(),
            keep,
            counter: AtomicU64::new(0),
            created: Mutex::new(Vec::new()),
        }
    }

    /// The configured temporary directory (for callers that must pass it to external
    /// tools, e.g. the `TMPDIR` family in `module_env`).
    pub fn dir(&self) -> &Path {
        &self.dir
    }

    /// Whether `path` was allocated through this registry (mirrors testing membership in
    /// `ngleTemporaryFilesCreated`). Used by `write` to decide whether a file may be *moved*
    /// (renamed) to the output rather than copied.
    pub fn was_created(&self, path: &Path) -> bool {
        self.created.lock().unwrap().iter().any(|p| p == path)
    }

    /// Reserve a fresh temporary file and register it for cleanup (mirrors
    /// `openNGLTempFile'`). The file is created atomically with `O_CREAT | O_EXCL`; on a
    /// name collision the counter is bumped and the open retried. The returned file is
    /// left empty — callers later truncate it via `StreamWriter::create`/`File::create`
    /// or hand the path to an external tool, exactly as the Haskell handle is closed and
    /// its path reused.
    pub fn new_file(&self, prefix: &str, ext: &str) -> NgResult<PathBuf> {
        loop {
            let n = self.counter.fetch_add(1, Ordering::Relaxed);
            let suffix = format!("{}.{n}.{ext}", std::process::id());
            let base = check_filename_length(prefix, &suffix);
            let path = self.dir.join(format!("{base}{suffix}"));
            let mut opts = fs::OpenOptions::new();
            opts.write(true).create_new(true);
            #[cfg(unix)]
            {
                use std::os::unix::fs::OpenOptionsExt;
                opts.mode(0o600);
            }
            match opts.open(&path) {
                Ok(_) => {
                    crate::output::trace(0, &format!("Created temporary file {}", path.display()));
                    self.created.lock().unwrap().push(path.clone());
                    return Ok(path);
                }
                Err(e) if e.kind() == std::io::ErrorKind::AlreadyExists => continue,
                Err(e) => {
                    return Err(NgError::new(
                        NgErrorType::SystemError,
                        format!("Could not create temporary file {}: {e}", path.display()),
                    ))
                }
            }
        }
    }

    /// Reserve a fresh temporary directory and register it for (recursive) cleanup
    /// (mirrors `createTempDir`). `fs::create_dir` is itself exclusive — it fails if the
    /// name exists — so the same bump-and-retry loop applies.
    pub fn new_dir(&self, prefix: &str) -> NgResult<PathBuf> {
        loop {
            let n = self.counter.fetch_add(1, Ordering::Relaxed);
            let suffix = format!("{}.{n}", std::process::id());
            let base = check_filename_length(prefix, &suffix);
            let path = self.dir.join(format!("{base}{suffix}"));
            match fs::create_dir(&path) {
                Ok(()) => {
                    crate::output::trace(
                        0,
                        &format!("Created temporary directory {}", path.display()),
                    );
                    self.created.lock().unwrap().push(path.clone());
                    return Ok(path);
                }
                Err(e) if e.kind() == std::io::ErrorKind::AlreadyExists => continue,
                Err(e) => {
                    return Err(NgError::new(
                        NgErrorType::SystemError,
                        format!(
                            "Could not create temporary directory {}: {e}",
                            path.display()
                        ),
                    ))
                }
            }
        }
    }

    /// Remove every tracked path, ignoring "does not exist" (a path may already be gone if
    /// it was moved to an output or never written). No-op when `keep` is set. Idempotent:
    /// the tracking list is drained, so a second call (e.g. an explicit call before drop)
    /// does nothing.
    fn cleanup(&self) {
        if self.keep {
            return;
        }
        let paths = std::mem::take(&mut *self.created.lock().unwrap());
        // Remove in reverse allocation order so a directory created before files placed
        // inside it is removed last.
        for path in paths.into_iter().rev() {
            if path.is_dir() {
                remove_dir_all_if_exists(&path);
            } else {
                remove_file_if_exists(&path);
            }
        }
    }
}

impl Drop for TempFiles {
    fn drop(&mut self) {
        self.cleanup();
    }
}

/// Recursively remove `path`, ignoring a "does not exist" error (the directory analogue of
/// [`remove_file_if_exists`]).
fn remove_dir_all_if_exists(path: &Path) {
    match fs::remove_dir_all(path) {
        Ok(()) => {}
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
        Err(_) => {}
    }
}

/// Shorten a basename so that `base + suffix` stays within the 240-character budget
/// (mirrors `checkFilenameLength`): keep the first third and last third of the combined
/// length, joined by `...`. NGLess prefixes are short, so this is a safety net rather than
/// a routine transformation.
fn check_filename_length(base: &str, suffix: &str) -> String {
    let base_chars: Vec<char> = base.chars().collect();
    let len = base_chars.len() + suffix.chars().count();
    if len <= 240 {
        return base.to_string();
    }
    let head: String = base_chars.iter().take(len / 3).collect();
    let tail: String = base_chars.iter().skip(2 * len / 3).collect();
    format!("{head}...{tail}")
}

#[cfg(test)]
mod tests {
    use super::*;

    fn scratch_dir(tag: &str) -> PathBuf {
        let d = std::env::temp_dir().join(format!(
            "ngless_tempfiles_test_{}_{tag}_{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        fs::create_dir_all(&d).unwrap();
        d
    }

    #[test]
    fn creates_and_cleans_up_files() {
        let dir = scratch_dir("cleanup");
        let p1;
        let p2;
        {
            let tf = TempFiles::new(&dir, false);
            p1 = tf.new_file("a_", "fq").unwrap();
            p2 = tf.new_file("b_", "sam").unwrap();
            assert!(p1.exists() && p2.exists());
            // Writing through the reserved file (as StreamWriter would) is fine.
            fs::write(&p1, b"data").unwrap();
        }
        assert!(!p1.exists(), "temp file should be removed at drop");
        assert!(!p2.exists(), "temp file should be removed at drop");
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn keep_preserves_files() {
        let dir = scratch_dir("keep");
        let p;
        {
            let tf = TempFiles::new(&dir, true);
            p = tf.new_file("k_", "fq").unwrap();
        }
        assert!(p.exists(), "temp file should survive when keep=true");
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn creates_and_cleans_up_dirs() {
        let dir = scratch_dir("dirs");
        let d;
        {
            let tf = TempFiles::new(&dir, false);
            d = tf.new_dir("work_").unwrap();
            assert!(d.is_dir());
            // A file inside the temp dir must not block recursive removal.
            fs::write(d.join("inner.txt"), b"x").unwrap();
        }
        assert!(
            !d.exists(),
            "temp dir should be recursively removed at drop"
        );
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn exclusive_creation_skips_existing_name() {
        let dir = scratch_dir("exclusive");
        let tf = TempFiles::new(&dir, false);
        // Pre-create the name the next allocation would choose (counter starts at 0).
        let taken = dir.join(format!("z_{}.0.fq", std::process::id()));
        fs::write(&taken, b"pre-existing").unwrap();
        let p = tf.new_file("z_", "fq").unwrap();
        assert_ne!(p, taken, "must not reuse an existing name");
        // The pre-existing file is untouched (not clobbered).
        assert_eq!(fs::read(&taken).unwrap(), b"pre-existing");
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn short_names_unchanged() {
        assert_eq!(check_filename_length("counts.", "1234.0.tsv"), "counts.");
    }

    #[test]
    fn long_names_shortened() {
        let base: String = std::iter::repeat('x').take(300).collect();
        let out = check_filename_length(&base, ".fq");
        let len = base.len() + ".fq".len();
        let expected = format!("{}...{}", &base[..len / 3], &base[2 * len / 3..]);
        assert_eq!(out, expected);
        assert!(out.len() < base.len());
    }
}
