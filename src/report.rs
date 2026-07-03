//! End-of-run HTML report writer (replaces Haskell's `Output.hs::writeOutputJS` +
//! `setupHtmlViewer`).
//!
//! The old Haskell report was a directory of five files (three AngularJS pages plus two CSS
//! files) whose scripts, chart library (`d3`/`nvd3`), CSS framework (Bootstrap) and jQuery were
//! all loaded *from CDNs*. That made the report useless without an internet connection — a real
//! problem given NGLess routinely runs on compute-cluster nodes with no outbound network.
//!
//! This reimplementation writes a **single self-contained `index.html`**: the run data is embedded
//! inline as a JSON literal, the styling is inline CSS, the interactivity is dependency-free
//! vanilla JavaScript, and the per-position quality chart is hand-rendered as inline SVG. The
//! script listing is syntax-highlighted client-side by a small NGLess tokenizer in the template
//! (mirroring `tokens.rs`), so the Rust side only embeds the raw script text. The page makes *zero*
//! external requests, so it works fully offline. The HTML/JS/CSS shell lives in
//! `report/template.html` (embedded via `include_str!`) with a single `{{DATA}}` placeholder.
//!
//! Alongside `index.html` the directory also gets the raw `script.ngl` and two machine-readable
//! TSV files (`fq.tsv`, `mappings.tsv`), matching the non-transposed `writeOutputTSV` output of
//! the Haskell build.

use std::path::Path;

use serde::Serialize;

use crate::errors::{NgError, NgErrorType, NgResult};
use crate::interpret::{FqInfo, MapInfo};
use crate::output::OutputRecord;
use crate::values::show_double;

/// The self-contained HTML/CSS/JS shell, with a single `{{DATA}}` placeholder where the run's JSON
/// data literal is spliced in (mirrors the file-embedding Haskell did with `embedDir "Html"`).
const TEMPLATE: &str = include_str!("report/template.html");

// ---- JSON payload shapes (the object the embedded JavaScript consumes) ----

#[derive(Serialize)]
struct ReportData<'a> {
    #[serde(rename = "scriptName")]
    script_name: &'a str,
    time: String,
    script: Vec<ScriptLine<'a>>,
    output: Vec<LogLine<'a>>,
    #[serde(rename = "fqStats")]
    fq_stats: Vec<FqOut<'a>>,
    #[serde(rename = "mapStats")]
    map_stats: Vec<MapOut<'a>>,
}

#[derive(Serialize)]
struct ScriptLine<'a> {
    lno: usize,
    text: &'a str,
    /// This line produced FASTQ QC statistics (mirrors `has_QCInfo`).
    qc: bool,
    /// This line produced mapping statistics (mirrors `has_StatsInfo`).
    stats: bool,
}

#[derive(Serialize)]
struct LogLine<'a> {
    lno: usize,
    otype: &'a str,
    time: &'a str,
    message: &'a str,
}

#[derive(Serialize)]
struct FqOut<'a> {
    file: &'a str,
    lno: usize,
    encoding: &'a str,
    #[serde(rename = "gcContent")]
    gc_content: f64,
    #[serde(rename = "nonATCG")]
    non_atcg: f64,
    #[serde(rename = "numSeqs")]
    num_seqs: i64,
    #[serde(rename = "numBasepairs")]
    num_basepairs: i64,
    #[serde(rename = "minLen")]
    min_len: i64,
    #[serde(rename = "maxLen")]
    max_len: i64,
    #[serde(rename = "perBaseQ")]
    per_base_q: Vec<BPos>,
}

#[derive(Serialize)]
struct BPos {
    mean: i64,
    median: i64,
    #[serde(rename = "lowerQuartile")]
    lower_quartile: i64,
    #[serde(rename = "upperQuartile")]
    upper_quartile: i64,
}

#[derive(Serialize)]
struct MapOut<'a> {
    lno: usize,
    reference: &'a str,
    #[serde(rename = "inputFile")]
    input_file: &'a str,
    total: i64,
    aligned: i64,
    unique: i64,
}

/// Write the run report directory: the self-contained `index.html`, the raw `script.ngl`, and the
/// `fq.tsv`/`mappings.tsv` machine-readable statistics (mirrors the block in Haskell's
/// `modeExec DefaultMode` that runs `setupHtmlViewer` + `writeOutputJS` + `writeOutputTSV`).
///
/// If `index.html` already exists in `report_dir` the whole write is skipped, mirroring
/// `setupHtmlViewer`'s existence guard (which avoids clobbering a report a previous run wrote to a
/// shared, explicitly-named `-o` directory).
pub(crate) fn write_report(
    report_dir: &Path,
    script_name: &str,
    script_text: &str,
    output: &[OutputRecord],
    fq_stats: &[FqInfo],
    map_stats: &[MapInfo],
) -> NgResult<()> {
    let index = report_dir.join("index.html");
    if index.exists() {
        return Ok(());
    }
    std::fs::create_dir_all(report_dir).map_err(|e| io_err(report_dir, e))?;

    // Lines that carry QC / mapping statistics, so the script listing can link them (ports
    // `wrapScript`: a line is tagged if its number is in the fq-stats' `scriptLno` / the map-stats'
    // `mi_lno`).
    let qc_lnos: std::collections::HashSet<usize> = fq_stats.iter().map(|f| f.lno).collect();
    let stats_lnos: std::collections::HashSet<usize> = map_stats.iter().map(|m| m.lno).collect();

    let script: Vec<ScriptLine> = script_text
        .lines()
        .enumerate()
        .map(|(i, text)| {
            let lno = i + 1;
            ScriptLine {
                lno,
                text,
                qc: qc_lnos.contains(&lno),
                stats: stats_lnos.contains(&lno),
            }
        })
        .collect();

    let data = ReportData {
        script_name,
        time: crate::output::finished_timestamp(),
        script,
        output: output
            .iter()
            .map(|o| LogLine {
                lno: o.lno,
                otype: o.otype.name(),
                time: &o.time,
                message: &o.message,
            })
            .collect(),
        fq_stats: fq_stats
            .iter()
            .map(|f| FqOut {
                file: &f.file,
                lno: f.lno,
                encoding: &f.encoding,
                gc_content: f.gc_content,
                non_atcg: f.non_atcg,
                num_seqs: f.n_seq,
                num_basepairs: f.n_basepairs,
                min_len: f.min_len,
                max_len: f.max_len,
                per_base_q: f
                    .qual_percentiles
                    .iter()
                    .map(|&(mean, median, lq, uq)| BPos {
                        mean,
                        median,
                        lower_quartile: lq,
                        upper_quartile: uq,
                    })
                    .collect(),
            })
            .collect(),
        map_stats: map_stats
            .iter()
            .map(|m| MapOut {
                lno: m.lno,
                reference: &m.reference,
                input_file: &m.input_file,
                total: m.total,
                aligned: m.aligned,
                unique: m.unique,
            })
            .collect(),
    };

    let json = serde_json::to_string(&data).map_err(|e| {
        NgError::new(
            NgErrorType::SystemError,
            format!("Could not build report: {e}"),
        )
    })?;
    // Escape `<` so a `</script>` sequence in the script text or a log message cannot terminate the
    // inline `<script>` block early. `<` is valid inside the JS/JSON literal and `<` only ever
    // appears inside JSON string values, so a blanket replacement is safe.
    let json = json.replace('<', "\\u003c");
    let html = TEMPLATE.replace("{{DATA}}", &json);

    write_file(&index, html.as_bytes())?;
    write_file(&report_dir.join("script.ngl"), script_text.as_bytes())?;
    write_file(
        &report_dir.join("fq.tsv"),
        format_fq_tsv(fq_stats).as_bytes(),
    )?;
    write_file(
        &report_dir.join("mappings.tsv"),
        format_map_tsv(map_stats).as_bytes(),
    )?;
    Ok(())
}

/// Non-transposed TSV: a header row of the (alphabetically sorted) statistic keys, then one value
/// row per file. Ports the else-branch of `formatTSV`/`encodeFQStats` in `Output.hs`.
fn format_fq_tsv(fq_stats: &[FqInfo]) -> String {
    // Keys in the alphabetical order Haskell's `sort` over `(key, value)` produces.
    const HEADER: &str =
        "encoding\tfile\tgcContent\tmaxSeqLen\tminSeqLen\tnonATCGFraction\tnumBasepairs\tnumSeqs";
    if fq_stats.is_empty() {
        return String::new();
    }
    let mut out = String::from(HEADER);
    out.push('\n');
    for f in fq_stats {
        let cols = [
            f.encoding.clone(),
            f.file.clone(),
            show_double(f.gc_content),
            f.max_len.to_string(),
            f.min_len.to_string(),
            show_double(f.non_atcg),
            f.n_basepairs.to_string(),
            f.n_seq.to_string(),
        ];
        out.push_str(&cols.join("\t"));
        out.push('\n');
    }
    out
}

/// Non-transposed mapping-stats TSV (ports `encodeMapStats`, keys alphabetically sorted).
fn format_map_tsv(map_stats: &[MapInfo]) -> String {
    const HEADER: &str = "aligned\tinputFile\tlineNumber\treference\ttotal\tunique";
    if map_stats.is_empty() {
        return String::new();
    }
    let mut out = String::from(HEADER);
    out.push('\n');
    for m in map_stats {
        let cols = [
            m.aligned.to_string(),
            m.input_file.clone(),
            m.lno.to_string(),
            m.reference.clone(),
            m.total.to_string(),
            m.unique.to_string(),
        ];
        out.push_str(&cols.join("\t"));
        out.push('\n');
    }
    out
}

/// Atomic file write (temp sibling + rename), consistent with the rest of NGLess's output writes
/// so an interrupted run never leaves a half-written report file.
fn write_file(path: &Path, bytes: &[u8]) -> NgResult<()> {
    let p = path.to_str().ok_or_else(|| {
        NgError::new(
            NgErrorType::SystemError,
            format!("Report path is not valid UTF-8: {}", path.display()),
        )
    })?;
    crate::compression::write_bytes_atomic(p, bytes)
}

fn io_err(path: &Path, e: std::io::Error) -> NgError {
    NgError::new(
        NgErrorType::SystemError,
        format!("Could not write report file {}: {e}", path.display()),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fq(file: &str, lno: usize) -> FqInfo {
        FqInfo {
            file: file.to_string(),
            encoding: "Sanger (33 offset)".to_string(),
            gc_content: 0.5,
            non_atcg: 0.0,
            n_seq: 100,
            n_basepairs: 10_000,
            min_len: 50,
            max_len: 100,
            lno,
            qual_percentiles: vec![(30, 32, 28, 35), (31, 33, 29, 36)],
        }
    }

    fn map_info(lno: usize) -> MapInfo {
        MapInfo {
            lno,
            input_file: "reads.fq".to_string(),
            reference: "ref".to_string(),
            total: 100,
            aligned: 90,
            unique: 80,
        }
    }

    #[test]
    fn fq_tsv_header_and_rows() {
        let tsv = format_fq_tsv(&[fq("a.fq", 3)]);
        let mut lines = tsv.lines();
        assert_eq!(
            lines.next().unwrap(),
            "encoding\tfile\tgcContent\tmaxSeqLen\tminSeqLen\tnonATCGFraction\tnumBasepairs\tnumSeqs"
        );
        let row = lines.next().unwrap();
        assert!(row.starts_with("Sanger (33 offset)\ta.fq\t"));
        assert!(row.ends_with("\t10000\t100"));
        assert_eq!(format_fq_tsv(&[]), "");
    }

    #[test]
    fn map_tsv_header_and_rows() {
        let tsv = format_map_tsv(&[map_info(5)]);
        let mut lines = tsv.lines();
        assert_eq!(
            lines.next().unwrap(),
            "aligned\tinputFile\tlineNumber\treference\ttotal\tunique"
        );
        assert_eq!(lines.next().unwrap(), "90\treads.fq\t5\tref\t100\t80");
    }

    #[test]
    fn writes_self_contained_report_with_escaped_data() {
        let dir = std::env::temp_dir().join(format!("ngless-report-test-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        // A `</script>` in the source must not be able to break out of the inline script block.
        let script = "ngless \"1.6\"\nwrite(x, ofile=\"</script>\")\n";
        write_report(
            &dir,
            "test.ngl",
            script,
            &[],
            &[fq("a.fq", 2)],
            &[map_info(2)],
        )
        .unwrap();

        let html = std::fs::read_to_string(dir.join("index.html")).unwrap();
        assert!(!html.contains("{{DATA}}"));
        assert!(html.contains("const REPORT_DATA = {"));
        // The literal closing tag must have been neutralised inside the data literal.
        assert!(html.contains("\\u003c/script>"));
        assert!(dir.join("script.ngl").exists());
        assert!(dir.join("fq.tsv").exists());
        assert!(dir.join("mappings.tsv").exists());
        // A second call is a no-op (existence guard).
        std::fs::write(dir.join("index.html"), "SENTINEL").unwrap();
        write_report(&dir, "test.ngl", script, &[], &[], &[]).unwrap();
        assert_eq!(
            std::fs::read_to_string(dir.join("index.html")).unwrap(),
            "SENTINEL"
        );

        std::fs::remove_dir_all(&dir).unwrap();
    }
}
