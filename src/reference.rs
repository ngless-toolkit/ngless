//! Packaged reference-database resolution and auto-download, mirroring
//! `NGLess/ReferenceDatabases.hs` and the download helpers in `NGLess/Utils/Network.hs`.
//!
//! A packaged reference (e.g. `map(reads, reference="sacCer3")`) resolves to a directory laid out
//! as
//!
//! ```text
//! References/<name>/Sequence/BWAIndex/reference.fa.gz
//! References/<name>/Annotation/annotation.gtf.gz       (optional)
//! ```
//!
//! `ensure_data_present` first looks for an already-installed copy (user data dir, then global),
//! exactly as `findDataFiles` does. If none is found and the name is a builtin reference, it is
//! downloaded from the configured base URL and unpacked, mirroring `installData` →
//! `downloadExpandTar`.

use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::time::Duration;

use crate::errors::{NgError, NgErrorType, NgResult};
use crate::lockfile::{with_lock_file, LockParameters, WhenExistsStrategy};

/// A builtin reference: the canonical (short) name, an alias, and a pack version. Mirrors the
/// `builtinReferences` list in `ReferenceDatabases.hs`. These references were obtained from
/// Ensembl (see `build-scripts/create-standard-packs.py`).
struct BuiltinReference {
    /// Canonical name, e.g. `sacCer3`. Used to build the download URL.
    name: &'static str,
    /// Long alias, e.g. `Saccharomyces_cerevisiae_R64-1-1`.
    alias: &'static str,
}

const BUILTIN_REFERENCES: &[BuiltinReference] = &[
    bref("bosTau4", "Bos_taurus_UMD3.1"),
    bref("ce10", "Caenorhabditis_elegans_WBcel235"),
    bref("canFam3", "Canis_familiaris_CanFam3.1"),
    bref("dm5", "Drosophila_melanogaster_BDGP5"),
    bref("dm6", "Drosophila_melanogaster_BDGP6"),
    bref("gg4", "Gallus_gallus_Galgal4"),
    bref("gg5", "Gallus_gallus_5.0"),
    bref("hg19", "Homo_sapiens_GRCh37.p13"),
    bref("hg38.p7", "Homo_sapiens_GRCh38.p7"),
    bref("hg38.p10", "Homo_sapiens_GRCh38.p10"),
    bref("mm10.p2", "Mus_musculus_GRCm38.p2"),
    bref("mm10.p5", "Mus_musculus_GRCm38.p5"),
    bref("rn5", "Rattus_norvegicus_Rnor_5.0"),
    bref("rn6", "Rattus_norvegicus_Rnor_6.0"),
    bref("sacCer3", "Saccharomyces_cerevisiae_R64-1-1"),
    bref("susScr11", "Sus_scrofa.Sscrofa11.1"),
];

const fn bref(name: &'static str, alias: &'static str) -> BuiltinReference {
    BuiltinReference { name, alias }
}

/// The default download base URL (mirrors `defaultBaseURL` in `Configuration.hs`). Overridable via
/// the `NGLESS_DOWNLOAD_BASE_URL` environment variable (the Haskell build reads the `download-url`
/// config key; the Rust build has no config file yet, so an env var stands in).
const DEFAULT_BASE_URL: &str = "https://ngless-resources.big-data-biology.org/";

fn download_base_url() -> String {
    std::env::var("NGLESS_DOWNLOAD_BASE_URL").unwrap_or_else(|_| DEFAULT_BASE_URL.to_string())
}

/// Is `name` a builtin reference (resolvable without importing a module)? Mirrors
/// `isBuiltinReference`.
pub fn is_builtin_reference(name: &str) -> bool {
    find_builtin(name).is_some()
}

/// Find a builtin reference by name or alias (mirrors `findReference builtinReferences`).
fn find_builtin(name: &str) -> Option<&'static BuiltinReference> {
    BUILTIN_REFERENCES
        .iter()
        .find(|r| r.name == name || r.alias == name)
}

/// The relative path of the reference FASTA within a reference pack (mirrors `referencePath`).
const REFERENCE_PATH: &str = "Sequence/BWAIndex/reference.fa.gz";

/// Make sure the named reference is present, downloading it if necessary, and return the path of
/// its FASTA file (mirrors `ensureDataPresent` followed by `buildFaFilePath`).
///
/// `ngl_version` is the script's `(major, minor)` language version, used to build the
/// version-namespaced download URL (mirrors the `versionDirectory` logic in `installData`).
///
/// The reference directory is named by the *user-typed* name, so `sacCer3` and its alias
/// `Saccharomyces_cerevisiae_R64-1-1` resolve to (and download into) separate directories — exactly
/// as in Haskell, where `installData`'s `destdir` uses the passed-in name while the URL uses the
/// canonical `refName`.
pub fn ensure_data_present(refname: &str, ngl_version: (i64, i64)) -> NgResult<String> {
    if let Some(refdir) = find_data_files(refname) {
        return Ok(build_fa_path(&refdir));
    }
    let refdir = install_data(refname, ngl_version)?;
    Ok(build_fa_path(&refdir))
}

fn build_fa_path(refdir: &Path) -> String {
    refdir.join(REFERENCE_PATH).to_string_lossy().into_owned()
}

/// Look for an already-installed reference, user data directory first then global (mirrors
/// `findDataFiles`'s `User <|> Root`). A directory counts as present only when the FASTA exists.
fn find_data_files(refname: &str) -> Option<PathBuf> {
    for base in data_directories() {
        let refdir = Path::new(&base).join("References").join(refname);
        if refdir.join(REFERENCE_PATH).is_file() {
            return Some(refdir);
        }
    }
    None
}

/// Download and install a reference, returning its directory (mirrors `installData`).
fn install_data(refname: &str, ngl_version: (i64, i64)) -> NgResult<PathBuf> {
    let bref = find_builtin(refname).ok_or_else(|| {
        NgError::script(format!(
            "Could not find reference '{refname}'. It is not builtin nor in one of the loaded \
             modules."
        ))
    })?;

    // versionDirectory: `show majV ++ "." ++ show minV` for versions >= 0.9 (always true for the
    // >= 1.5 scripts this build supports).
    let (maj, min) = ngl_version;
    let base = download_base_url();
    let base = base.trim_end_matches('/');
    // The URL uses the canonical reference name; the install directory uses the user-typed name.
    let url = format!("{base}/References/{maj}.{min}/{}.tar.gz", bref.name);

    let basedir = install_basedir()?;
    let destdir = basedir.join("References").join(refname);

    // Guard the download+unpack with a lock so two concurrent NGLess processes installing the same
    // reference do not race on `destdir`. This goes beyond the Haskell builtin `installData` path
    // (which is unlocked — only URL-typed module references lock, via `downloadIfUrl`), closing a
    // known concurrency gap noted in the migration plan. The lock parameters mirror
    // `downloadIfUrl`'s. We recheck `find_data_files` under the lock: another process may have
    // finished the install while we waited.
    let lock_fname = {
        let mut p = destdir.as_os_str().to_os_string();
        p.push(".download.lock");
        PathBuf::from(p)
    };
    if let Some(parent) = lock_fname.parent() {
        let _ = fs::create_dir_all(parent);
    }
    let params = LockParameters {
        lock_fname,
        max_age: Duration::from_secs(300),
        when_exists: WhenExistsStrategy::IfLockedRetry {
            nr_retries: 37 * 60,
            time_between: Duration::from_secs(60),
        },
        mtime_update: true,
    };
    with_lock_file(params, || {
        if find_data_files(refname).is_some() {
            return Ok(());
        }
        crate::output::info(0, &format!("Starting download from {url}"));
        download_expand_tar(&url, &destdir)?;
        crate::output::info(0, "Reference download completed!");
        Ok(())
    })?;
    Ok(destdir)
}

/// Choose where to install a reference: the global data directory if it is creatable and writable,
/// otherwise the user data directory (mirrors `installData Nothing`'s `canInstallGlobal` check).
fn install_basedir() -> NgResult<PathBuf> {
    if let Some(global) = global_data_directory() {
        if fs::create_dir_all(&global).is_ok() && is_writable(&global) {
            return Ok(global);
        }
    }
    user_data_directory().ok_or_else(|| {
        NgError::new(
            NgErrorType::SystemError,
            "Could not determine a data directory to install the reference into ($HOME is unset).",
        )
    })
}

/// Probe whether `dir` is writable by attempting to create (and immediately remove) a marker file.
fn is_writable(dir: &Path) -> bool {
    let probe = dir.join(".ngless-write-probe");
    match fs::File::create(&probe) {
        Ok(_) => {
            let _ = fs::remove_file(&probe);
            true
        }
        Err(_) => false,
    }
}

/// Download a `.tar.gz` from `url` and expand it into `destdir` (mirrors `downloadExpandTar`):
/// download to `<destdir>.tar.gz`, gunzip+untar into `destdir`, then remove the tarball.
fn download_expand_tar(url: &str, destdir: &Path) -> NgResult<()> {
    fs::create_dir_all(destdir).map_err(|e| {
        NgError::new(
            NgErrorType::SystemError,
            format!("Could not create directory {}: {e}", destdir.display()),
        )
    })?;
    let tar_name = {
        let mut p = destdir.as_os_str().to_os_string();
        p.push(".tar.gz");
        PathBuf::from(p)
    };

    download_file(url, &tar_name)?;

    let f = fs::File::open(&tar_name).map_err(|e| {
        NgError::new(
            NgErrorType::SystemError,
            format!("Could not open downloaded file {}: {e}", tar_name.display()),
        )
    })?;
    let gz = flate2::read::GzDecoder::new(f);
    let mut archive = tar::Archive::new(gz);
    archive.unpack(destdir).map_err(|e| {
        NgError::new(
            NgErrorType::DataError,
            format!(
                "Could not unpack reference archive downloaded from {url} into {}: {e}",
                destdir.display()
            ),
        )
    })?;
    let _ = fs::remove_file(&tar_name);
    Ok(())
}

/// Download `url` to `dest` over HTTP(S) (mirrors `downloadFile`). Sends the same `User-Agent`
/// header as the Haskell build (`NGLess/<version>`).
fn download_file(url: &str, dest: &Path) -> NgResult<()> {
    let user_agent = format!("NGLess/{}", crate::version::VERSION_STR);
    let resp = ureq::get(url)
        .set("User-Agent", &user_agent)
        .call()
        .map_err(|e| match e {
            ureq::Error::Status(code, _) => NgError::new(
                NgErrorType::SystemError,
                format!("Could not connect to {url} (got error code: {code})"),
            ),
            other => NgError::new(
                NgErrorType::SystemError,
                format!("Could not download {url}: {other}"),
            ),
        })?;

    // Stream the body to `dest`, writing to a temporary sibling first so a partial download never
    // leaves a corrupt file in place (mirrors `sinkFileCautious`).
    let tmp = {
        let mut p = dest.as_os_str().to_os_string();
        p.push(".download-tmp");
        PathBuf::from(p)
    };
    let mut out = fs::File::create(&tmp).map_err(|e| {
        NgError::new(
            NgErrorType::SystemError,
            format!("Could not create {}: {e}", tmp.display()),
        )
    })?;
    let mut reader = resp.into_reader();
    std::io::copy(&mut reader, &mut out).map_err(|e| {
        let _ = fs::remove_file(&tmp);
        NgError::new(
            NgErrorType::SystemError,
            format!("Error while downloading {url}: {e}"),
        )
    })?;
    out.flush().ok();
    drop(out);
    fs::rename(&tmp, dest).map_err(|e| {
        let _ = fs::remove_file(&tmp);
        NgError::new(
            NgErrorType::SystemError,
            format!("Could not finalise download into {}: {e}", dest.display()),
        )
    })?;
    Ok(())
}

/// The user data directory, `$HOME/.local/share/ngless/data` (mirrors `nConfUserDataDirectory`).
fn user_data_directory() -> Option<PathBuf> {
    std::env::var("HOME")
        .ok()
        .map(|home| PathBuf::from(format!("{home}/.local/share/ngless/data")))
}

/// The global data directory, `<binary-dir>/../share/ngless/data` (mirrors
/// `nConfGlobalDataDirectory`).
fn global_data_directory() -> Option<PathBuf> {
    let exe = std::env::current_exe().ok()?;
    let bindir = exe.parent()?;
    Some(bindir.join("../share/ngless/data"))
}

/// The NGLess data directories, in search order (user then global), mirroring `findDataFiles`'s
/// `User <|> Root`.
pub fn data_directories() -> Vec<String> {
    let mut dirs = Vec::new();
    if let Some(user) = user_data_directory() {
        dirs.push(user.to_string_lossy().into_owned());
    }
    if let Some(global) = global_data_directory() {
        dirs.push(global.to_string_lossy().into_owned());
    }
    dirs
}
