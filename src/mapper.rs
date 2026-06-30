//! Thin wrapper around the external `bwa` binary, mirroring `NGLess/StandardModules/Mappers/Bwa.hs`
//! and the index/mapping parts of `NGLess/Interpretation/Map.hs`.
//!
//! NGLess shells out to `bwa` to build a reference index and to map reads (`bwa mem`); this module
//! does the same. The binary is taken from `$NGLESS_BWA_BIN` or `bwa` on `PATH` (the Haskell build
//! bundles a pinned binary; here we use whatever is installed). The bwa version is queried at
//! runtime and embedded in the index file names so that indices built with one bwa do not get
//! reused by another (mirrors `indexPrefix`, which uses the compile-time `bwaVersion`).

use std::path::Path;
use std::process::{Command, Stdio};
use std::sync::OnceLock;

use crate::errors::{NgError, NgErrorType, NgResult};

/// The bwa binary to invoke.
pub fn bwa_bin() -> String {
    std::env::var("NGLESS_BWA_BIN").unwrap_or_else(|_| "bwa".to_string())
}

/// The bwa version string used in index names (e.g. `0.7.19`), cached after the first query.
///
/// `bwa` with no arguments prints its usage to stderr, including a `Version: 0.7.19-r1273` line;
/// we take the token after `Version:` up to the first `-` (matching the bundled `bwaVersion`
/// constant, which omits the `-r...` suffix).
pub fn bwa_version() -> NgResult<&'static str> {
    static VERSION: OnceLock<String> = OnceLock::new();
    if let Some(v) = VERSION.get() {
        return Ok(v.as_str());
    }
    let out = Command::new(bwa_bin()).output().map_err(|e| {
        NgError::new(
            NgErrorType::SystemError,
            format!("Could not run bwa (to determine its version): {e}"),
        )
    })?;
    // The version banner is printed to stderr.
    let banner = String::from_utf8_lossy(&out.stderr);
    let version = banner
        .lines()
        .find_map(|l| l.trim().strip_prefix("Version:"))
        .map(|v| v.trim().split('-').next().unwrap_or("").trim().to_string())
        .filter(|v| !v.is_empty())
        .ok_or_else(|| {
            NgError::new(
                NgErrorType::SystemError,
                "Could not determine bwa version from its output.".to_string(),
            )
        })?;
    let _ = VERSION.set(version);
    Ok(VERSION.get().unwrap().as_str())
}

/// The index prefix for `fafile` (mirrors `indexPrefix`): `<basename>-bwa-<version><ext>`, e.g.
/// `ref.fna` with bwa 0.7.19 becomes `ref-bwa-0.7.19.fna`.
pub fn index_prefix(fafile: &str) -> NgResult<String> {
    let version = bwa_version()?;
    Ok(split_extension(fafile, &format!("-bwa-{version}")))
}

/// Insert `insert` between the base name and the extension of `path` (mirrors Haskell's
/// `splitExtension` + concatenation). `ref.fna` + `-bwa-X` -> `ref-bwa-X.fna`.
fn split_extension(path: &str, insert: &str) -> String {
    // Match on the final '.' that is part of the file name (not a directory separator).
    match path.rfind('.') {
        Some(dot) if dot > path.rfind('/').map(|s| s + 1).unwrap_or(0) => {
            format!("{}{}{}", &path[..dot], insert, &path[dot..])
        }
        _ => format!("{path}{insert}"),
    }
}

/// The five files a bwa index is made of (mirrors `indexRequiredFormats`).
const INDEX_FORMATS: [&str; 5] = [".amb", ".ann", ".bwt", ".pac", ".sa"];

/// Whether a complete bwa index already exists for `fafile` (mirrors `hasValidIndex`).
pub fn has_valid_index(fafile: &str) -> NgResult<bool> {
    let prefix = index_prefix(fafile)?;
    Ok(INDEX_FORMATS
        .iter()
        .all(|ext| Path::new(&format!("{prefix}{ext}")).exists()))
}

/// Build a bwa index for `fafile` (mirrors `createIndex`): `bwa index [-b <blocksize>] -p <prefix>
/// <fafile>`. The custom block size (1/10th of the file, for files >= 100MB) mirrors
/// `customBlockSize`.
pub fn create_index(fafile: &str) -> NgResult<()> {
    let prefix = index_prefix(fafile)?;
    let mut args: Vec<String> = vec!["index".to_string()];
    if let Ok(meta) = std::fs::metadata(fafile) {
        let size = meta.len();
        if size >= 100 * 1000 * 1000 {
            args.push("-b".to_string());
            args.push((size / 10).to_string());
        }
    }
    args.push("-p".to_string());
    args.push(prefix);
    args.push(fafile.to_string());
    let refs: Vec<&str> = args.iter().map(|s| s.as_str()).collect();
    run(&refs, "index")?;
    Ok(())
}

/// Map an interleaved FASTQ stream against `ref_index` with `bwa mem`, writing the SAM output to
/// `out_sam` (mirrors `callMapper`). `-K 100000000` fixes the chunk size so the output is
/// independent of the thread count; `-p` reads interleaved paired reads from the input.
pub fn call_mapper(
    ref_index: &str,
    rs: &crate::fastq::ReadSet,
    extra_args: &[String],
    out_sam: &Path,
) -> NgResult<()> {
    let prefix = index_prefix(ref_index)?;
    let threads = crate::parallel::external_tool_threads();
    let out_file = std::fs::File::create(out_sam).map_err(|e| {
        NgError::new(
            NgErrorType::SystemError,
            format!("Could not create mapping output {}: {e}", out_sam.display()),
        )
    })?;

    let mut cmd = Command::new(bwa_bin());
    cmd.arg("mem")
        .arg("-t")
        .arg(threads.to_string())
        .arg("-K")
        .arg("100000000")
        .arg("-p")
        .args(extra_args)
        .arg(&prefix)
        .arg("-")
        .stdin(Stdio::piped())
        .stdout(Stdio::from(out_file))
        .stderr(Stdio::piped());
    let mut child = cmd.spawn().map_err(|e| {
        NgError::new(
            NgErrorType::SystemError,
            format!("Could not start bwa mem: {e}"),
        )
    })?;
    // Stream the interleaved reads on stdin (bounded memory). stdout is redirected straight to
    // the output file, so a blocking write here cannot deadlock against the SAM output.
    {
        let mut stdin = child.stdin.take().ok_or_else(|| {
            NgError::new(
                NgErrorType::SystemError,
                "bwa mem: could not open stdin".to_string(),
            )
        })?;
        crate::interpret::write_interleaved(rs, &mut stdin)?;
        // `stdin` is dropped here, closing the pipe.
    }
    let out = child.wait_with_output().map_err(|e| {
        NgError::new(
            NgErrorType::SystemError,
            format!("bwa mem: failed waiting for the process: {e}"),
        )
    })?;
    if !out.status.success() {
        return Err(NgError::new(
            NgErrorType::SystemError,
            format!(
                "Failed bwa mem (exit {:?}).\nError message was:\n{}",
                out.status.code(),
                String::from_utf8_lossy(&out.stderr)
            ),
        ));
    }
    Ok(())
}

/// Run bwa with `args`, returning its captured output, or an error including stderr on failure.
fn run(args: &[&str], what: &str) -> NgResult<std::process::Output> {
    let out = Command::new(bwa_bin()).args(args).output().map_err(|e| {
        NgError::new(
            NgErrorType::SystemError,
            format!("Could not run bwa ({what}): {e}"),
        )
    })?;
    if !out.status.success() {
        return Err(NgError::new(
            NgErrorType::SystemError,
            format!(
                "Failed bwa {what} (exit {:?}).\nError message was:\n{}",
                out.status.code(),
                String::from_utf8_lossy(&out.stderr)
            ),
        ));
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn split_extension_inserts_before_ext() {
        assert_eq!(
            split_extension("ref.fna", "-bwa-0.7.19"),
            "ref-bwa-0.7.19.fna"
        );
        assert_eq!(
            split_extension("d/ref.splits_1m.0.fna", "-bwa-0.7.19"),
            "d/ref.splits_1m.0-bwa-0.7.19.fna"
        );
        // No extension: appended at the end.
        assert_eq!(split_extension("ref", "-bwa-0.7.19"), "ref-bwa-0.7.19");
        // A dot only in a directory component is not an extension.
        assert_eq!(split_extension("a.b/ref", "-X"), "a.b/ref-X");
    }
}
