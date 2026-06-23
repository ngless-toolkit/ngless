//! Thin wrappers around the external `samtools` binary, mirroring `NGLess/Utils/Samtools.hs`
//! and the execution parts of `NGLess/StandardModules/Samtools.hs`.
//!
//! NGLess shells out to `samtools` for BAM<->SAM conversion, sorting and region selection; this
//! module does the same. The binary is taken from `$NGLESS_SAMTOOLS_BIN` or `samtools` on
//! `PATH` (the Haskell build bundles a pinned binary; here we use whatever is installed).

use std::path::Path;
use std::process::Command;

use crate::errors::{NgError, NgErrorType, NgResult};

/// The samtools binary to invoke.
pub fn samtools_bin() -> String {
    std::env::var("NGLESS_SAMTOOLS_BIN").unwrap_or_else(|_| "samtools".to_string())
}

fn run(args: &[&str], what: &str) -> NgResult<std::process::Output> {
    let out = Command::new(samtools_bin())
        .args(args)
        .output()
        .map_err(|e| {
            NgError::new(
                NgErrorType::SystemError,
                format!("Could not run samtools ({what}): {e}"),
            )
        })?;
    if !out.status.success() {
        return Err(NgError::new(
            NgErrorType::SystemError,
            format!(
                "Failed samtools {what} (exit {:?}).\nError message was:\n{}",
                out.status.code(),
                String::from_utf8_lossy(&out.stderr)
            ),
        ));
    }
    Ok(out)
}

/// Read a BAM file as SAM text including the header (mirrors `samBamConduit` for `.bam`:
/// `samtools view -h`).
pub fn bam_to_sam_text(bamfile: &str) -> NgResult<String> {
    let out = run(&["view", "-h", bamfile], "view")?;
    String::from_utf8(out.stdout).map_err(|e| {
        NgError::new(
            NgErrorType::DataError,
            format!("samtools view produced non-UTF8 output: {e}"),
        )
    })
}

/// Convert a SAM file to BAM, writing to `out` (mirrors `convertSamToBam`:
/// `samtools view -bS -o out in`).
pub fn convert_sam_to_bam(samfile: &Path, out: &Path) -> NgResult<()> {
    run(
        &[
            "view",
            "-bS",
            "-o",
            &out.to_string_lossy(),
            &samfile.to_string_lossy(),
        ],
        "view (SAM->BAM)",
    )?;
    Ok(())
}

/// Convert a BAM file to SAM, writing to `out` (mirrors `convertBamToSam`:
/// `samtools view -h -o out in`).
pub fn convert_bam_to_sam(bamfile: &Path, out: &Path) -> NgResult<()> {
    run(
        &[
            "view",
            "-h",
            "-o",
            &out.to_string_lossy(),
            &bamfile.to_string_lossy(),
        ],
        "view (BAM->SAM)",
    )?;
    Ok(())
}

/// `samtools_sort`: sort `input` into `out` in the given output format (`"sam"`/`"bam"`),
/// optionally by read name (mirrors `executeSort`). `temp_prefix` is a path prefix samtools may
/// use for its intermediate files.
pub fn sort(
    input: &Path,
    out: &Path,
    oformat: &str,
    by_name: bool,
    temp_prefix: &Path,
) -> NgResult<()> {
    let mut args: Vec<String> = vec!["sort".to_string()];
    if by_name {
        args.push("-n".to_string());
    }
    args.push("-O".to_string());
    args.push(oformat.to_string());
    args.push("-T".to_string());
    args.push(temp_prefix.to_string_lossy().to_string());
    args.push("-o".to_string());
    args.push(out.to_string_lossy().to_string());
    args.push(input.to_string_lossy().to_string());
    let refs: Vec<&str> = args.iter().map(|s| s.as_str()).collect();
    run(&refs, "sort")?;
    Ok(())
}

/// `samtools_view` with a BED region file: keep only records overlapping `bed_file` (mirrors
/// `executeView`: `samtools view -h -O <fmt> -L <bed> in`), writing to `out`.
pub fn view_bed(input: &Path, bed_file: &str, out: &Path, oformat: &str) -> NgResult<()> {
    run(
        &[
            "view",
            "-h",
            "-O",
            oformat,
            "-L",
            bed_file,
            "-o",
            &out.to_string_lossy(),
            &input.to_string_lossy(),
        ],
        "view (BED)",
    )?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn have_samtools() -> bool {
        Command::new(samtools_bin())
            .arg("--version")
            .output()
            .map(|o| o.status.success())
            .unwrap_or(false)
    }

    #[test]
    fn sam_bam_round_trip() {
        if !have_samtools() {
            eprintln!("skipping: samtools not available");
            return;
        }
        let dir = std::env::temp_dir();
        let n = std::process::id();
        let sam = dir.join(format!("ngless_st_{n}.sam"));
        let bam = dir.join(format!("ngless_st_{n}.bam"));
        let back = dir.join(format!("ngless_st_{n}.back.sam"));
        std::fs::write(
            &sam,
            "@HD\tVN:1.0\tSO:unsorted\n@SQ\tSN:g1\tLN:100\n\
             r1\t0\tg1\t1\t60\t4M\t*\t0\t0\tACGT\tIIII\tNM:i:0\n",
        )
        .unwrap();
        convert_sam_to_bam(&sam, &bam).unwrap();
        // The BAM is readable back as SAM text and the alignment survives.
        let text = bam_to_sam_text(&bam.to_string_lossy()).unwrap();
        assert!(text.contains("r1\t0\tg1\t1\t60\t4M"));
        convert_bam_to_sam(&bam, &back).unwrap();
        assert!(std::fs::read_to_string(&back)
            .unwrap()
            .contains("r1\t0\tg1"));
        for p in [&sam, &bam, &back] {
            let _ = std::fs::remove_file(p);
        }
    }
}
