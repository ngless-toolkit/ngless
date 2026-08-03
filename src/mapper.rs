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
///
/// `total_reads`, when known, is the number of reads being mapped; it drives the terminal progress
/// bar (mirrors the `interleaveFQs'` count fed to `progressFQ`). `None` suppresses the bar.
pub fn call_mapper(
    ref_index: &str,
    rs: &crate::fastq::ReadSet,
    extra_args: &[String],
    out_sam: &Path,
    total_reads: Option<u64>,
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
    // Drain stderr in a background thread *while* we feed stdin. bwa mem is chatty on stderr (one
    // `[M::mem_pestat]` block per chunk), so if we only collected it after writing all of stdin
    // (as `wait_with_output` would), a long run could fill the 64K stderr pipe buffer; bwa would
    // then block in write(2) and stop reading stdin while we block writing to it -- a deadlock.
    let stderr_thread = child.stderr.take().map(|mut e| {
        std::thread::spawn(move || {
            let mut buf = Vec::new();
            let _ = std::io::Read::read_to_end(&mut e, &mut buf);
            buf
        })
    });
    // Stream the interleaved reads on stdin (bounded memory). stdout is redirected straight to
    // the output file, so a blocking write here cannot deadlock against the SAM output.
    let write_result = {
        let mut stdin = child.stdin.take().ok_or_else(|| {
            NgError::new(
                NgErrorType::SystemError,
                "bwa mem: could not open stdin".to_string(),
            )
        })?;
        crate::interpret::write_interleaved_progress(rs, &mut stdin, total_reads)
        // `stdin` is dropped here, closing the pipe.
    };
    let status = child.wait().map_err(|e| {
        NgError::new(
            NgErrorType::SystemError,
            format!("bwa mem: failed waiting for the process: {e}"),
        )
    })?;
    let stderr = stderr_thread
        .and_then(|t| t.join().ok())
        .unwrap_or_default();
    if !status.success() {
        // A non-zero exit takes precedence over a write error: if bwa died early, our write failed
        // with a meaningless "broken pipe" and bwa's own message is what the user needs.
        return Err(NgError::new(
            NgErrorType::SystemError,
            format!(
                "Failed bwa mem (exit {:?}).\nError message was:\n{}",
                status.code(),
                String::from_utf8_lossy(&stderr)
            ),
        ));
    }
    write_result
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

    /// A mapper that writes far more than a pipe buffer's worth of stderr *before* consuming its
    /// stdin must not deadlock against our stdin writer (bwa mem does exactly this: it emits an
    /// `[M::mem_pestat]` block per chunk). Regression test for the hang reported with paired-end
    /// samples on real data.
    #[test]
    fn call_mapper_does_not_deadlock_on_chatty_stderr() {
        use std::io::Write as _;

        let dir = std::env::temp_dir().join(format!("ngless-mapper-test-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();

        // A fake bwa: prints a version banner when called with no arguments and, for `mem`, floods
        // stderr (~190KB, well past the 64K pipe buffer) before reading a single byte of stdin.
        let fake = dir.join("fake-bwa.sh");
        std::fs::write(
            &fake,
            "#!/bin/sh\n\
             if [ \"$1\" != mem ]; then echo 'Version: 9.9.9-r1' >&2; exit 1; fi\n\
             i=0\n\
             while [ $i -lt 2000 ]; do\n\
             echo '[M::mem_pestat] skip orientation FF as there are not enough pairs.........' >&2\n\
             i=$((i+1))\n\
             done\n\
             cat > /dev/null\n\
             echo '@HD\tVN:1.0'\n",
        )
        .unwrap();
        std::fs::set_permissions(
            &fake,
            std::os::unix::fs::PermissionsExt::from_mode(0o755u32),
        )
        .unwrap();
        std::env::set_var("NGLESS_BWA_BIN", &fake);

        // More than a pipe buffer of reads, so that we too block if the child stops reading.
        let fq = dir.join("reads.fq");
        {
            let mut f = std::io::BufWriter::new(std::fs::File::create(&fq).unwrap());
            for i in 0..5000 {
                writeln!(
                    f,
                    "@read{i}\nACGTACGTACGTACGTACGTACGT\n+\nIIIIIIIIIIIIIIIIIIIIIIII"
                )
                .unwrap();
            }
        }
        let rs = crate::fastq::ReadSet {
            pairs: vec![],
            singletons: vec![crate::fastq::FastQFilePath {
                encoding: crate::fastq::FastQEncoding::Sanger,
                path: fq,
            }],
        };
        let out_sam = dir.join("out.sam");

        // Run in a thread so a regression fails the test instead of hanging it forever.
        let (tx, rx) = std::sync::mpsc::channel();
        std::thread::spawn(move || {
            let r = call_mapper("ref.fna", &rs, &[], &out_sam, None);
            let _ = tx.send(r);
        });
        match rx.recv_timeout(std::time::Duration::from_secs(60)) {
            Ok(r) => r.expect("call_mapper failed"),
            Err(_) => panic!("call_mapper deadlocked against the mapper's stderr"),
        }
        let _ = std::fs::remove_dir_all(&dir);
    }

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
