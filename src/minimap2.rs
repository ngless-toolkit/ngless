//! Thin wrapper around the external `minimap2` binary, mirroring
//! `NGLess/StandardModules/Mappers/Minimap2.hs` and the minimap2 arm of
//! `NGLess/Interpretation/Map.hs`.
//!
//! Like the bwa wrapper (`crate::mapper`), NGLess shells out to `minimap2` to build a reference
//! index and to map reads. The binary is taken from `$NGLESS_MINIMAP2_BIN` or `minimap2` on
//! `PATH`. The version is queried at runtime and embedded in the index file name so that indices
//! built with one minimap2 do not get reused by another (mirrors `indexName`, which uses the
//! compile-time `minimap2Version`; the Rust build queries the actual binary, matching how
//! `crate::mapper` handles the bwa version).
//!
//! Two things differ from the bwa path and are faithful to the Haskell module:
//!   * mapping is done against the raw FASTA (`refIndex` in `callMapper`), not against the
//!     pre-built `.mm2.idx`; the index existence check only gates index *creation*.
//!   * minimap2's SAM output is *sorted* (header lines kept in order, then the alignment lines
//!     sorted bytewise-ascending) before being written, mirroring `sortSam`.

use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio};
use std::sync::OnceLock;

use crate::errors::{NgError, NgErrorType, NgResult};

/// The minimap2 binary to invoke.
pub fn minimap2_bin() -> String {
    std::env::var("NGLESS_MINIMAP2_BIN").unwrap_or_else(|_| "minimap2".to_string())
}

/// The minimap2 version string used in index names (e.g. `2.31`), cached after the first query.
///
/// `minimap2 --version` prints a single line like `2.31-r1302`; we take the token before the
/// first `-` (matching the bundled `minimap2Version` constant, which omits the `-r...` suffix).
pub fn minimap2_version() -> NgResult<&'static str> {
    static VERSION: OnceLock<String> = OnceLock::new();
    if let Some(v) = VERSION.get() {
        return Ok(v.as_str());
    }
    let out = Command::new(minimap2_bin())
        .arg("--version")
        .output()
        .map_err(|e| {
            NgError::new(
                NgErrorType::SystemError,
                format!("Could not run minimap2 (to determine its version): {e}"),
            )
        })?;
    let banner = String::from_utf8_lossy(&out.stdout);
    let version = banner
        .lines()
        .next()
        .map(|v| v.trim().split('-').next().unwrap_or("").trim().to_string())
        .filter(|v| !v.is_empty())
        .ok_or_else(|| {
            NgError::new(
                NgErrorType::SystemError,
                "Could not determine minimap2 version from its output.".to_string(),
            )
        })?;
    let _ = VERSION.set(version);
    Ok(VERSION.get().unwrap().as_str())
}

/// The index file for `fafile` (mirrors `indexName`): `<base>-minimap2-<version><ext>.mm2.idx`,
/// e.g. `ref.fna` with minimap2 2.31 becomes `ref-minimap2-2.31.fna.mm2.idx`.
pub fn index_name(fafile: &str) -> NgResult<String> {
    let version = minimap2_version()?;
    Ok(format!(
        "{}.mm2.idx",
        split_extension(fafile, &format!("-minimap2-{version}"))
    ))
}

/// Insert `insert` between the base name and the extension of `path` (mirrors Haskell's
/// `splitExtension` + concatenation). `ref.fna` + `-minimap2-X` -> `ref-minimap2-X.fna`.
fn split_extension(path: &str, insert: &str) -> String {
    match path.rfind('.') {
        Some(dot) if dot > path.rfind('/').map(|s| s + 1).unwrap_or(0) => {
            format!("{}{}{}", &path[..dot], insert, &path[dot..])
        }
        _ => format!("{path}{insert}"),
    }
}

/// Whether a minimap2 index already exists for `fafile` (mirrors `hasValidIndex`).
pub fn has_valid_index(fafile: &str) -> NgResult<bool> {
    Ok(Path::new(&index_name(fafile)?).exists())
}

/// Build a minimap2 index for `fafile` (mirrors `createIndex`): `minimap2 <fafile> -d <index>`.
pub fn create_index(fafile: &str) -> NgResult<()> {
    let index = index_name(fafile)?;
    let out = Command::new(minimap2_bin())
        .arg(fafile)
        .arg("-d")
        .arg(&index)
        .output()
        .map_err(|e| {
            NgError::new(
                NgErrorType::SystemError,
                format!("Could not run minimap2 (index): {e}"),
            )
        })?;
    if !out.status.success() {
        return Err(NgError::new(
            NgErrorType::SystemError,
            format!(
                "Failed minimap2 index (exit {:?}).\nError message was:\n{}",
                out.status.code(),
                String::from_utf8_lossy(&out.stderr)
            ),
        ));
    }
    Ok(())
}

/// Map an interleaved FASTQ stream against `ref_fasta` with `minimap2 -a`, writing the *sorted*
/// SAM output to `out_sam` (mirrors `callMapper` + `sortSam`). `-a` requests SAM output; the
/// reference is the raw FASTA (not the `.mm2.idx`), matching the Haskell module.
pub fn call_mapper(
    ref_fasta: &str,
    interleaved: &[u8],
    extra_args: &[String],
    out_sam: &Path,
) -> NgResult<()> {
    let threads = std::thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(1);

    let mut cmd = Command::new(minimap2_bin());
    cmd.arg("-t")
        .arg(threads.to_string())
        .arg("-a")
        .args(extra_args)
        .arg(ref_fasta)
        .arg("-")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    let mut child = cmd.spawn().map_err(|e| {
        NgError::new(
            NgErrorType::SystemError,
            format!("Could not start minimap2: {e}"),
        )
    })?;
    // Feed the interleaved reads on stdin from a separate thread so that a large SAM output on
    // stdout cannot fill its pipe and deadlock against our blocking write.
    {
        let mut stdin = child.stdin.take().ok_or_else(|| {
            NgError::new(
                NgErrorType::SystemError,
                "minimap2: could not open stdin".to_string(),
            )
        })?;
        let reads = interleaved.to_vec();
        std::thread::spawn(move || {
            let _ = stdin.write_all(&reads);
            // `stdin` is dropped here, closing the pipe.
        });
    }
    let out = child.wait_with_output().map_err(|e| {
        NgError::new(
            NgErrorType::SystemError,
            format!("minimap2: failed waiting for the process: {e}"),
        )
    })?;
    if !out.status.success() {
        return Err(NgError::new(
            NgErrorType::SystemError,
            format!(
                "Failed minimap2 (exit {:?}).\nError message was:\n{}",
                out.status.code(),
                String::from_utf8_lossy(&out.stderr)
            ),
        ));
    }

    let sorted = sort_sam(&out.stdout);
    std::fs::write(out_sam, &sorted).map_err(|e| {
        NgError::new(
            NgErrorType::SystemError,
            format!("Could not write mapping output {}: {e}", out_sam.display()),
        )
    })?;
    Ok(())
}

/// Sort a SAM stream the way `sortSam` does: keep the leading `@`-header lines in their original
/// order, then sort the remaining (alignment) lines bytewise-ascending. Returns the reassembled
/// SAM with a trailing newline after every line.
fn sort_sam(sam: &[u8]) -> Vec<u8> {
    // Split into lines without the trailing '\n' (mirrors `linesC`); a final line with no
    // newline is kept, an empty trailing fragment (from a terminating '\n') is dropped.
    let mut lines: Vec<&[u8]> = sam.split(|&b| b == b'\n').collect();
    if matches!(lines.last(), Some(l) if l.is_empty()) {
        lines.pop();
    }
    let split = lines
        .iter()
        .position(|l| l.first() != Some(&b'@'))
        .unwrap_or(lines.len());
    let (headers, body) = lines.split_at(split);
    let mut body: Vec<&[u8]> = body.to_vec();
    body.sort_unstable();

    let mut out = Vec::with_capacity(sam.len() + 1);
    for line in headers.iter().chain(body.iter()) {
        out.extend_from_slice(line);
        out.push(b'\n');
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn split_extension_inserts_before_ext() {
        assert_eq!(
            split_extension("ref.fna", "-minimap2-2.31"),
            "ref-minimap2-2.31.fna"
        );
        assert_eq!(
            split_extension("ref", "-minimap2-2.31"),
            "ref-minimap2-2.31"
        );
        assert_eq!(split_extension("a.b/ref", "-X"), "a.b/ref-X");
    }

    #[test]
    fn sort_sam_keeps_headers_then_sorts_body() {
        let input = b"@HD\tVN:1.6\n@SQ\tSN:ref\nr2\t0\tref\nr1\t0\tref\nr3\t0\tref\n";
        let out = sort_sam(input);
        assert_eq!(
            out,
            b"@HD\tVN:1.6\n@SQ\tSN:ref\nr1\t0\tref\nr2\t0\tref\nr3\t0\tref\n".to_vec()
        );
    }

    #[test]
    fn sort_sam_handles_missing_trailing_newline() {
        let input = b"@HD\tx\nb\ta\na\tb";
        let out = sort_sam(input);
        assert_eq!(out, b"@HD\tx\na\tb\nb\ta\n".to_vec());
    }
}
