//! FASTQ data layer and read trimming, mirroring `NGLess/Data/FastQ.hs`,
//! `NGLess/Interpretation/Substrim.hs`, and `compatibleHeader` from
//! `NGLess/Interpretation/FastQ.hs`.
//!
//! This is the pure computational core of preprocessing: the `ShortRead` record, FASTQ
//! encode/decode, and the `substrim`/`endstrim`/`smoothtrim` trimming algorithms. The
//! streaming pipeline (compressed I/O, parallel decode, the `preprocess` block interpreter)
//! is a later milestone.

use std::io::BufRead;
use std::path::PathBuf;

use crate::errors::{NgError, NgErrorType, NgResult};

/// A single read, with qualities already decoded to integer values.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ShortRead {
    pub header: String,
    pub sequence: String,
    pub qualities: Vec<i8>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FastQEncoding {
    Sanger,
    Solexa,
}

/// A FASTQ file on disk together with its quality encoding (mirrors `FastQFilePath`).
///
/// NGLess read sets are file-backed, not in-memory: `fastq` references the original input
/// file, `preprocess` streams it to a fresh temp file, and `write` copies/recompresses the
/// current file. Keeping the data on disk is what makes `write` of an un-preprocessed read
/// set byte-identical to its input (preserving the original `+`-comment lines and encoding).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FastQFilePath {
    pub encoding: FastQEncoding,
    pub path: PathBuf,
}

/// A set of FASTQ files (mirrors `ReadSet`). Single-end read sets have an empty `pairs` and
/// one or more `singletons`; paired-end support arrives in a later milestone.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ReadSet {
    pub pairs: Vec<(FastQFilePath, FastQFilePath)>,
    pub singletons: Vec<FastQFilePath>,
}

impl FastQEncoding {
    pub fn offset(self) -> i32 {
        match self {
            FastQEncoding::Sanger => 33,
            FastQEncoding::Solexa => 64,
        }
    }

    /// Human-readable name, matching `encodingName` (used in the qcstats TSV).
    pub fn name(self) -> &'static str {
        match self {
            FastQEncoding::Sanger => "Sanger (33 offset)",
            FastQEncoding::Solexa => "Solexa (64 offset)",
        }
    }
}

/// Per-file FASTQ statistics, mirroring the relevant fields of `FQStatistics` plus the derived
/// `gcFraction`/`nonATCGFrac`/`nBasepairs`.
#[derive(Clone, Debug, PartialEq)]
pub struct FastQStats {
    pub n_seq: i64,
    pub min_len: i64,
    pub max_len: i64,
    /// Base counts: (A, C, G, T, other), each case-insensitive.
    pub bp: (i64, i64, i64, i64, i64),
}

impl FastQStats {
    pub fn num_basepairs(&self) -> i64 {
        let (a, c, g, t, o) = self.bp;
        a + c + g + t + o
    }

    /// GC fraction over A/C/G/T only (mirrors `gcFraction`).
    pub fn gc_fraction(&self) -> f64 {
        let (a, c, g, t, _) = self.bp;
        (c + g) as f64 / (a + c + g + t) as f64
    }

    /// Fraction of non-ATCG bases over all bases (mirrors `nonATCGFrac`).
    pub fn non_atcg_fraction(&self) -> f64 {
        let (a, c, g, t, o) = self.bp;
        o as f64 / (a + c + t + g + o) as f64
    }
}

/// Compute FASTQ statistics from decoded reads (mirrors `fqStatsC`). For an empty input the
/// sequence-length range is `(maxBound, 0)` as in the Haskell code.
pub fn stats_from_reads(reads: &[ShortRead]) -> FastQStats {
    let (mut a, mut c, mut g, mut t, mut o) = (0i64, 0i64, 0i64, 0i64, 0i64);
    let mut min_len = i64::MAX;
    let mut max_len = 0i64;
    for r in reads {
        let len = r.sequence.len() as i64;
        min_len = min_len.min(len);
        max_len = max_len.max(len);
        for &b in r.sequence.as_bytes() {
            match b.to_ascii_lowercase() {
                b'a' => a += 1,
                b'c' => c += 1,
                b'g' => g += 1,
                b't' => t += 1,
                _ => o += 1,
            }
        }
    }
    FastQStats {
        n_seq: reads.len() as i64,
        min_len,
        max_len,
        bp: (a, c, g, t, o),
    }
}

impl ShortRead {
    pub fn new(header: &str, sequence: &str, qualities: Vec<i8>) -> Self {
        ShortRead {
            header: header.into(),
            sequence: sequence.into(),
            qualities,
        }
    }

    pub fn len(&self) -> usize {
        self.sequence.len()
    }

    pub fn is_empty(&self) -> bool {
        self.sequence.is_empty()
    }

    /// Slice `n` bases starting at `s` (mirrors `srSlice`).
    pub fn slice(&self, s: usize, n: usize) -> ShortRead {
        ShortRead {
            header: self.header.clone(),
            sequence: self.sequence[s..s + n].to_string(),
            qualities: self.qualities[s..s + n].to_vec(),
        }
    }

    /// Mean quality value (mirrors the `avg_quality` method).
    pub fn avg_quality(&self) -> f64 {
        let sum: i64 = self.qualities.iter().map(|&q| q as i64).sum();
        sum as f64 / self.qualities.len() as f64
    }

    /// Fraction of bases with quality at least `minq` (mirrors `fraction_at_least`).
    pub fn fraction_at_least(&self, minq: i64) -> f64 {
        let n = self.qualities.iter().filter(|&&q| q as i64 >= minq).count();
        n as f64 / self.qualities.len() as f64
    }

    /// Set the quality to 0 at every `N`/`n` base (mirrors `n_to_zero_quality`).
    pub fn n_to_zero_quality(&self) -> ShortRead {
        let bytes = self.sequence.as_bytes();
        let qualities = self
            .qualities
            .iter()
            .enumerate()
            .map(|(i, &q)| {
                if bytes[i] == b'N' || bytes[i] == b'n' {
                    0
                } else {
                    q
                }
            })
            .collect();
        ShortRead {
            header: self.header.clone(),
            sequence: self.sequence.clone(),
            qualities,
        }
    }
}

/// Encode a read as a 4-line FASTQ record (mirrors `fqEncode`).
pub fn fq_encode(enc: FastQEncoding, sr: &ShortRead) -> String {
    let offset = enc.offset();
    let quals: String = sr
        .qualities
        .iter()
        .map(|&q| (q as i32 + offset) as u8 as char)
        .collect();
    format!("{}\n{}\n+\n{}\n", sr.header, sr.sequence, quals)
}

/// Decode FASTQ text into reads (mirrors `fqDecodeVector`/`fqDecodeC`).
pub fn fq_decode(enc: FastQEncoding, text: &str) -> NgResult<Vec<ShortRead>> {
    let offset = enc.offset();
    let lines: Vec<&str> = text.lines().collect();
    if lines.len() % 4 != 0 {
        return Err(NgError::new(
            crate::errors::NgErrorType::DataError,
            "Number of input lines in FastQ file is not a multiple of 4",
        ));
    }
    let mut out = Vec::with_capacity(lines.len() / 4);
    for chunk in lines.chunks(4) {
        let rid = chunk[0];
        let rseq = chunk[1];
        let rqs = chunk[3];
        if rseq.len() != rqs.len() {
            return Err(NgError::new(
                crate::errors::NgErrorType::DataError,
                "Length of quality line is not the same as sequence",
            ));
        }
        let qualities = rqs.bytes().map(|b| (b as i32 - offset) as i8).collect();
        out.push(ShortRead::new(rid, rseq, qualities));
    }
    Ok(out)
}

/// Read one line, returning `None` at clean EOF. Strips a trailing `\n` then `\r`, matching
/// both `str::lines()` and Haskell's `lineWindowsTerminated` (so `\r\n` inputs decode the same
/// as `\n`). Reading into a `String` requires valid UTF-8, as the whole-file path did.
fn read_norm_line<R: BufRead>(reader: &mut R) -> NgResult<Option<String>> {
    let mut s = String::new();
    let n = reader.read_line(&mut s).map_err(|e| {
        NgError::new(
            NgErrorType::DataError,
            format!("Could not read FastQ input: {e}"),
        )
    })?;
    if n == 0 {
        return Ok(None);
    }
    if s.ends_with('\n') {
        s.pop();
        if s.ends_with('\r') {
            s.pop();
        }
    }
    Ok(Some(s))
}

/// A lazy, bounded-memory iterator over the FASTQ records of a reader (mirrors a streaming
/// `fqDecodeC`). Reads four lines per record; the `+` line is ignored, as in [`fq_decode`].
/// When fully consumed it reproduces `fq_decode`'s validation: a clean EOF at a record
/// boundary ends the iterator, 1–3 leftover lines yield the "not a multiple of 4" error, and a
/// sequence/quality length mismatch yields the same error as the whole-file decoder.
pub struct FastqReader<R: BufRead> {
    reader: R,
    offset: i32,
}

pub fn fastq_records<R: BufRead>(reader: R, enc: FastQEncoding) -> FastqReader<R> {
    FastqReader {
        reader,
        offset: enc.offset(),
    }
}

impl<R: BufRead> Iterator for FastqReader<R> {
    type Item = NgResult<ShortRead>;

    fn next(&mut self) -> Option<NgResult<ShortRead>> {
        let not_mult4 = || {
            NgError::new(
                NgErrorType::DataError,
                "Number of input lines in FastQ file is not a multiple of 4",
            )
        };
        // Line 0 (header): clean EOF here ends the stream.
        let header = match read_norm_line(&mut self.reader) {
            Ok(Some(l)) => l,
            Ok(None) => return None,
            Err(e) => return Some(Err(e)),
        };
        // Lines 1..=3: EOF before completing the record is a non-multiple-of-4 error.
        let mut next_line = || match read_norm_line(&mut self.reader) {
            Ok(Some(l)) => Ok(l),
            Ok(None) => Err(not_mult4()),
            Err(e) => Err(e),
        };
        let rseq = match next_line() {
            Ok(l) => l,
            Err(e) => return Some(Err(e)),
        };
        let _plus = match next_line() {
            Ok(l) => l,
            Err(e) => return Some(Err(e)),
        };
        let rqs = match next_line() {
            Ok(l) => l,
            Err(e) => return Some(Err(e)),
        };
        if rseq.len() != rqs.len() {
            return Some(Err(NgError::new(
                NgErrorType::DataError,
                "Length of quality line is not the same as sequence",
            )));
        }
        let qualities = rqs
            .bytes()
            .map(|b| (b as i32 - self.offset) as i8)
            .collect();
        Some(Ok(ShortRead::new(&header, &rseq, qualities)))
    }
}

/// Incremental version of [`stats_from_reads`]: fold reads one at a time so QC statistics can
/// be collected during a single streaming pass. Uses only the sequence (never qualities), so it
/// is independent of the quality encoding — which is what lets the QC pass be decoupled from
/// encoding detection. `finish` on an empty accumulator yields `(min_len, max_len) =
/// (i64::MAX, 0)`, exactly as `stats_from_reads(&[])`.
#[derive(Clone, Debug)]
pub struct FastQStatsAcc {
    n_seq: i64,
    min_len: i64,
    max_len: i64,
    a: i64,
    c: i64,
    g: i64,
    t: i64,
    o: i64,
}

impl Default for FastQStatsAcc {
    fn default() -> Self {
        Self::new()
    }
}

impl FastQStatsAcc {
    pub fn new() -> Self {
        FastQStatsAcc {
            n_seq: 0,
            min_len: i64::MAX,
            max_len: 0,
            a: 0,
            c: 0,
            g: 0,
            t: 0,
            o: 0,
        }
    }

    /// Number of reads folded so far (used by `preprocess` to decide the result shape).
    pub fn n_seq(&self) -> i64 {
        self.n_seq
    }

    pub fn update(&mut self, sr: &ShortRead) {
        let len = sr.sequence.len() as i64;
        self.min_len = self.min_len.min(len);
        self.max_len = self.max_len.max(len);
        self.n_seq += 1;
        for &b in sr.sequence.as_bytes() {
            match b.to_ascii_lowercase() {
                b'a' => self.a += 1,
                b'c' => self.c += 1,
                b'g' => self.g += 1,
                b't' => self.t += 1,
                _ => self.o += 1,
            }
        }
    }

    /// Fold another accumulator into this one. Every field is a commutative/associative
    /// reduction (sum, min, max), so merging per-block accumulators in input order yields a
    /// result identical to a single serial fold — which keeps `preprocess`'s QC stats
    /// byte-identical when the work is parallelised.
    pub fn merge(&mut self, other: &FastQStatsAcc) {
        self.n_seq += other.n_seq;
        self.min_len = self.min_len.min(other.min_len);
        self.max_len = self.max_len.max(other.max_len);
        self.a += other.a;
        self.c += other.c;
        self.g += other.g;
        self.t += other.t;
        self.o += other.o;
    }

    pub fn finish(&self) -> FastQStats {
        FastQStats {
            n_seq: self.n_seq,
            min_len: self.min_len,
            max_len: self.max_len,
            bp: (self.a, self.c, self.g, self.t, self.o),
        }
    }
}

/// Streaming version of [`detect_encoding`]: scan only as far as needed to decide. Mirrors
/// `detect_encoding`'s asymmetry with `fq_decode` — a trailing partial (`<4`-line) record is
/// silently ignored rather than an error.
pub fn detect_encoding_stream<R: BufRead>(mut reader: R) -> NgResult<FastQEncoding> {
    let mut minv: u8 = 255;
    let mut maxv: u8 = 0;
    loop {
        // A record needs all four lines; a short tail just ends the scan (no error).
        if read_norm_line(&mut reader)?.is_none() {
            break;
        }
        if read_norm_line(&mut reader)?.is_none() {
            break;
        }
        if read_norm_line(&mut reader)?.is_none() {
            break;
        }
        let qs = match read_norm_line(&mut reader)? {
            Some(l) => l,
            None => break,
        };
        let qs = qs.as_bytes();
        if qs.is_empty() {
            continue;
        }
        minv = minv.min(*qs.iter().min().unwrap());
        maxv = maxv.max(*qs.iter().max().unwrap());
        if minv < 33 {
            return Err(NgError::new(
                NgErrorType::DataError,
                format!("No known encodings with chars < 33 (Yours was {minv})."),
            ));
        }
        if minv < 58 {
            return Ok(FastQEncoding::Sanger);
        }
        if maxv >= 33 + 45 {
            return Ok(FastQEncoding::Solexa);
        }
    }
    Ok(FastQEncoding::Sanger)
}

/// Guess the FASTQ quality encoding from the quality lines, mirroring `encodingFor`
/// (Interpretation/FastQ.hs). Scans 4-line records tracking the min/max quality byte:
/// any byte `< 33` is an error; a min byte `< 58` means Sanger; a max byte `>= 78` means
/// Solexa; otherwise the heuristic is inconclusive and defaults to Sanger.
pub fn detect_encoding(text: &str) -> NgResult<FastQEncoding> {
    let lines: Vec<&str> = text.lines().collect();
    let mut minv: u8 = 255;
    let mut maxv: u8 = 0;
    for chunk in lines.chunks(4) {
        if chunk.len() < 4 {
            break;
        }
        let qs = chunk[3].as_bytes();
        if qs.is_empty() {
            continue;
        }
        minv = minv.min(*qs.iter().min().unwrap());
        maxv = maxv.max(*qs.iter().max().unwrap());
        if minv < 33 {
            return Err(NgError::new(
                crate::errors::NgErrorType::DataError,
                format!("No known encodings with chars < 33 (Yours was {minv})."),
            ));
        }
        if minv < 58 {
            return Ok(FastQEncoding::Sanger);
        }
        if maxv >= 33 + 45 {
            return Ok(FastQEncoding::Solexa);
        }
    }
    // Empty or inconclusive: default to Sanger (33 offset).
    Ok(FastQEncoding::Sanger)
}

/// Whether two paired-end headers refer to the same fragment (mirrors `compatibleHeader`).
pub fn compatible_header(h1: &str, h2: &str) -> bool {
    if h1 == h2 {
        return true;
    }
    if h1.len() != h2.len() {
        return false;
    }
    match (h1.strip_suffix("/1"), h2.strip_suffix("/2")) {
        (Some(a), Some(b)) => a == b,
        _ => false,
    }
}

// --- trimming -------------------------------------------------------------

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EndstrimEnds {
    Both,
    Three,
    Five,
}

/// Index and size of the longest run of consecutive bases at or above `cutoff`
/// (mirrors `subtrimPos`).
pub fn subtrim_pos(quality: &[i8], cutoff: i8) -> (usize, usize) {
    // S4 i s n_i n_s
    let (mut i, mut s, mut n_i, mut n_s) = (0usize, 0usize, 0usize, 0usize);
    for &q in quality {
        if q < cutoff {
            n_i = n_s + n_i + 1;
            n_s = 0;
        } else if n_s + 1 > s {
            i = n_i;
            s = n_s + 1;
            n_s += 1;
        } else {
            n_s += 1;
        }
    }
    (i, s)
}

/// Trim low-quality bases from the ends (mirrors `endstrimPos`).
pub fn endstrim_pos(method: EndstrimEnds, quality: &[i8], cutoff: i8) -> (usize, usize) {
    let do5 = matches!(method, EndstrimEnds::Five | EndstrimEnds::Both);
    let do3 = matches!(method, EndstrimEnds::Three | EndstrimEnds::Both);
    let len = quality.len();
    let start = if do5 {
        quality.iter().position(|&q| q >= cutoff).unwrap_or(len)
    } else {
        0
    };
    let sliced = &quality[start..];
    let trimmed = if do3 {
        let mut n = sliced.len();
        while n > 0 && sliced[n - 1] < cutoff {
            n -= 1;
        }
        n
    } else {
        sliced.len()
    };
    (start, trimmed)
}

/// Sliding-window smoothing then `subtrim_pos` (mirrors `smoothtrimPos`).
pub fn smoothtrim_pos(window: usize, quality: &[i8], cutoff: i8) -> (usize, usize) {
    if quality.is_empty() {
        return (0, 0);
    }
    let q_head = quality[0];
    let q_last = quality[quality.len() - 1];

    // Pad both ends by repeating the edge values so each position can be averaged over the
    // full window. With an even window the left pad is one smaller than the right.
    let pad = window - 1;
    let left_pad = pad / 2;
    let right_pad = pad - left_pad;

    let mut quals: Vec<i8> = Vec::with_capacity(left_pad + quality.len() + right_pad);
    quals.resize(left_pad, q_head);
    quals.extend_from_slice(quality);
    quals.resize(quals.len() + right_pad, q_last);

    let smoothed: Vec<i8> = (0..quality.len())
        .map(|start| {
            let sum: i64 = quals[start..start + window].iter().map(|&q| q as i64).sum();
            round_half_even(sum as f64 / window as f64) as i8
        })
        .collect();
    subtrim_pos(&smoothed, cutoff)
}

/// Round half to even (Haskell's `round`), unlike Rust's round-half-away-from-zero.
fn round_half_even(x: f64) -> i64 {
    let floor = x.floor();
    let diff = x - floor;
    let floor_i = floor as i64;
    if diff < 0.5 {
        floor_i
    } else if diff > 0.5 {
        floor_i + 1
    } else if floor_i.rem_euclid(2) == 0 {
        floor_i
    } else {
        floor_i + 1
    }
}

pub fn substrim(cutoff: i32, sr: &ShortRead) -> ShortRead {
    let (s, n) = subtrim_pos(&sr.qualities, cutoff as i8);
    sr.slice(s, n)
}

pub fn endstrim(ends: EndstrimEnds, cutoff: i32, sr: &ShortRead) -> ShortRead {
    let (s, n) = endstrim_pos(ends, &sr.qualities, cutoff as i8);
    sr.slice(s, n)
}

pub fn smoothtrim(window: i32, cutoff: i32, sr: &ShortRead) -> ShortRead {
    let (s, n) = smoothtrim_pos(window as usize, &sr.qualities, cutoff as i8);
    sr.slice(s, n)
}

/// Stream reads from `reader`, keeping at most `max_copies` reads per distinct sequence and
/// writing each kept read (FASTQ-encoded with `enc`) to `out` in input order. Mirrors
/// `Interpretation/Unique.performUnique`/`filterUniqueUpTo`: Haskell splits the input into
/// hash-bucketed temp files purely to bound memory and dedups each bucket independently, but
/// since all copies of one sequence land in the same bucket the *set* of kept reads is the
/// same as this single-pass dedup (the bucket split only reorders the output, which is not
/// observable). Returns the number of reads written.
pub fn unique_reads<R: BufRead, W: std::io::Write>(
    reader: R,
    out: &mut W,
    enc: FastQEncoding,
    max_copies: usize,
) -> NgResult<usize> {
    let mut seen: std::collections::HashMap<String, usize> = std::collections::HashMap::new();
    let mut written = 0;
    for r in fastq_records(reader, enc) {
        let sr = r?;
        let count = seen.entry(sr.sequence.clone()).or_insert(0);
        if *count < max_copies {
            *count += 1;
            out.write_all(fq_encode(enc, &sr).as_bytes()).map_err(|e| {
                NgError::new(
                    NgErrorType::SystemError,
                    format!("Error writing unique reads: {e}"),
                )
            })?;
            written += 1;
        }
    }
    Ok(written)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn reads3() -> Vec<ShortRead> {
        vec![
            ShortRead::new("x", "acttg", vec![35, 16, 34, 34, 24]),
            ShortRead::new("y", "catgt", vec![33, 17, 25, 37, 18]),
            ShortRead::new("z", "ccggg", vec![32, 16, 20, 32, 17]),
        ]
    }

    fn encode_recover(enc: FastQEncoding) -> NgResult<Vec<ShortRead>> {
        let text: String = reads3().iter().map(|r| fq_encode(enc, r)).collect();
        fq_decode(enc, &text)
    }

    #[test]
    fn unique_reads_dedup() {
        // Build an input with sequence "acttg" appearing 3 times (headers a/b/c), "catgt"
        // twice (d/e) and "ccggg" once (f).
        let enc = FastQEncoding::Sanger;
        let mk = |h: &str, s: &str| ShortRead::new(h, s, vec![35; s.len()]);
        let input: Vec<ShortRead> = vec![
            mk("a", "acttg"),
            mk("b", "acttg"),
            mk("d", "catgt"),
            mk("c", "acttg"),
            mk("e", "catgt"),
            mk("f", "ccggg"),
        ];
        let text: String = input.iter().map(|r| fq_encode(enc, r)).collect();

        let run = |max_copies: usize| -> Vec<ShortRead> {
            let mut out: Vec<u8> = Vec::new();
            let written = unique_reads(
                std::io::Cursor::new(text.clone()),
                &mut out,
                enc,
                max_copies,
            )
            .unwrap();
            let recovered = fq_decode(enc, std::str::from_utf8(&out).unwrap()).unwrap();
            assert_eq!(written, recovered.len());
            recovered
        };

        // max_copies=1: one of each distinct sequence, first occurrence in input order.
        let one: Vec<String> = run(1).iter().map(|r| r.header.clone()).collect();
        assert_eq!(one, vec!["a", "d", "f"]);

        // max_copies=2: up to two per sequence, in input order.
        let two: Vec<String> = run(2).iter().map(|r| r.header.clone()).collect();
        assert_eq!(two, vec!["a", "b", "d", "e", "f"]);

        // max_copies large enough keeps everything unchanged.
        let all: Vec<String> = run(10).iter().map(|r| r.header.clone()).collect();
        assert_eq!(all, vec!["a", "b", "d", "c", "e", "f"]);
    }

    #[test]
    fn parse_encode_sanger() {
        assert_eq!(encode_recover(FastQEncoding::Sanger).unwrap(), reads3());
    }

    #[test]
    fn parse_encode_solexa() {
        assert_eq!(encode_recover(FastQEncoding::Solexa).unwrap(), reads3());
    }

    #[test]
    fn detect_encoding_cases() {
        // A min byte < 58 ('#' = 35) => Sanger.
        assert_eq!(
            detect_encoding("@r\nAC\n+\n#I\n").unwrap(),
            FastQEncoding::Sanger
        );
        // min >= 58 and a max byte >= 78 ('W' = 87) => Solexa (e.g. the low_quality sample).
        assert_eq!(
            detect_encoding("@r\nGTCAGGACAAT\n+\n<=>?@ABCAWA\n").unwrap(),
            FastQEncoding::Solexa
        );
        // Inconclusive (all bytes in [58, 78)) => default Sanger.
        assert_eq!(
            detect_encoding("@r\nAC\n+\nKK\n").unwrap(),
            FastQEncoding::Sanger
        );
        // A byte < 33 (space = 32) is an error.
        assert!(detect_encoding("@r\nAC\n+\n A\n").is_err());
    }

    #[test]
    fn compatible_header_cases() {
        assert!(compatible_header(
            "@SRR4052021.40730 4073/1",
            "@SRR4052021.40730 4073/2"
        ));
        assert!(compatible_header(
            "@SRR4052021.40730 4073",
            "@SRR4052021.40730 4073"
        ));
        assert!(!compatible_header(
            "@SRR4052021.40730 4073",
            "@SRR4052021.40730 4074"
        ));
        assert!(!compatible_header(
            "@SRR4052021.40730 4073 xa",
            "@SRR4052021.40730 4073 xb"
        ));
    }

    #[test]
    fn substrim_cases() {
        assert_eq!(
            subtrim_pos(&[10, 11, 12, 123, 122, 111, 10, 11, 0], 20),
            (3, 3)
        );
        assert_eq!(subtrim_pos(&[], 20), (0, 0));
    }

    #[test]
    fn endstrim_cases() {
        assert_eq!(
            endstrim_pos(EndstrimEnds::Both, &[1, 2, 2, 2, 2, 1, 2, 2], 10).1,
            0
        );
        assert_eq!(endstrim_pos(EndstrimEnds::Both, &[9, 9, 9, 9], 10).1, 0);
        assert_eq!(
            endstrim_pos(EndstrimEnds::Both, &[10, 10, 10, 10], 10),
            (0, 4)
        );
        assert_eq!(endstrim_pos(EndstrimEnds::Both, &[9, 9, 10, 9], 10), (2, 1));
        assert_eq!(
            endstrim_pos(EndstrimEnds::Three, &[9, 9, 10, 9], 10),
            (0, 3)
        );
        assert_eq!(endstrim_pos(EndstrimEnds::Five, &[9, 9, 10, 9], 10), (2, 2));
        assert_eq!(
            endstrim_pos(EndstrimEnds::Both, &[9, 9, 10, 9, 9, 10, 9], 10),
            (2, 4)
        );
    }

    #[test]
    fn smoothtrim_cases() {
        assert_eq!(smoothtrim_pos(4, &[], 20), (0, 0));
        assert_eq!(
            smoothtrim_pos(4, &[10, 11, 12, 123, 122, 111, 10, 11, 0], 20),
            (1, 6)
        );
        assert_eq!(
            smoothtrim_pos(4, &[32, 32, 14, 32, 32, 14, 14, 2, 14, 14, 32, 32], 20),
            (0, 5)
        );
        assert_eq!(smoothtrim_pos(4, &[32, 32, 32, 1, 32, 32, 32], 20), (0, 7));
        assert_eq!(
            smoothtrim_pos(4, &[24, 2, 24, 24, 24, 24, 2, 24, 24, 24, 24, 2, 24], 20),
            (3, 1)
        );
        assert_eq!(
            smoothtrim_pos(10, &[24, 2, 24, 24, 24, 24, 2, 24, 24, 24, 24, 2, 24], 20),
            (0, 13)
        );
    }

    #[test]
    fn stats_from_reads_cases() {
        // Two reads, case-insensitive base counting, one N (non-ATCG).
        let reads = vec![
            ShortRead::new("a", "ACGT", vec![30, 30, 30, 30]),
            ShortRead::new("b", "acgN", vec![20, 20, 20, 20]),
        ];
        let st = stats_from_reads(&reads);
        assert_eq!(st.n_seq, 2);
        assert_eq!(st.min_len, 4);
        assert_eq!(st.max_len, 4);
        assert_eq!(st.bp, (2, 2, 2, 1, 1)); // A, C, G, T, other
        assert_eq!(st.num_basepairs(), 8);
        // GC over ATCG only: (C+G)/(A+C+G+T) = 4/7.
        assert_eq!(st.gc_fraction(), 4.0 / 7.0);
        // non-ATCG over all: 1/8.
        assert_eq!(st.non_atcg_fraction(), 1.0 / 8.0);
    }

    #[test]
    fn read_quality_methods() {
        // avg_quality: mean of the quality values.
        let sr = ShortRead::new("r", "ACGT", vec![10, 20, 30, 40]);
        assert_eq!(sr.avg_quality(), 25.0);
        // fraction_at_least: fraction of bases with quality >= minq.
        assert_eq!(sr.fraction_at_least(20), 0.75);
        assert_eq!(sr.fraction_at_least(100), 0.0);
        // n_to_zero_quality: zero the quality at N/n bases, leave others.
        let withn = ShortRead::new("r", "AnGN", vec![10, 20, 30, 40]);
        let z = withn.n_to_zero_quality();
        assert_eq!(z.qualities, vec![10, 0, 30, 0]);
        assert_eq!(z.sequence, "AnGN");
    }

    #[test]
    fn fastq_records_matches_fq_decode() {
        let enc = FastQEncoding::Sanger;
        let text: String = reads3().iter().map(|r| fq_encode(enc, r)).collect();
        let streamed: NgResult<Vec<ShortRead>> = fastq_records(text.as_bytes(), enc).collect();
        assert_eq!(streamed.unwrap(), fq_decode(enc, &text).unwrap());
    }

    #[test]
    fn fastq_records_missing_final_newline() {
        // Same record, with and without a trailing newline, decode identically (matches lines()).
        let enc = FastQEncoding::Sanger;
        let with_nl = "@r\nACGT\n+\nIIII\n";
        let without_nl = "@r\nACGT\n+\nIIII";
        let a: Vec<ShortRead> = fastq_records(with_nl.as_bytes(), enc)
            .collect::<NgResult<_>>()
            .unwrap();
        let b: Vec<ShortRead> = fastq_records(without_nl.as_bytes(), enc)
            .collect::<NgResult<_>>()
            .unwrap();
        assert_eq!(a, b);
        assert_eq!(a.len(), 1);
    }

    #[test]
    fn fastq_records_error_cases() {
        let enc = FastQEncoding::Sanger;
        // 1-3 leftover lines => not-a-multiple-of-4 (verbatim message).
        let partial: NgResult<Vec<ShortRead>> =
            fastq_records("@r\nACGT\n+\n".as_bytes(), enc).collect();
        let err = partial.unwrap_err();
        assert!(err
            .to_string()
            .contains("Number of input lines in FastQ file is not a multiple of 4"));
        // seq/qual length mismatch (verbatim message).
        let mismatch: NgResult<Vec<ShortRead>> =
            fastq_records("@r\nACGT\n+\nII\n".as_bytes(), enc).collect();
        assert!(mismatch
            .unwrap_err()
            .to_string()
            .contains("Length of quality line is not the same as sequence"));
    }

    #[test]
    fn fastq_records_crlf() {
        // \r\n line endings decode the same as \n (the qualities must not include \r).
        let enc = FastQEncoding::Sanger;
        let lf: Vec<ShortRead> = fastq_records("@r\nACGT\n+\nIIII\n".as_bytes(), enc)
            .collect::<NgResult<_>>()
            .unwrap();
        let crlf: Vec<ShortRead> = fastq_records("@r\r\nACGT\r\n+\r\nIIII\r\n".as_bytes(), enc)
            .collect::<NgResult<_>>()
            .unwrap();
        assert_eq!(lf, crlf);
    }

    #[test]
    fn stats_acc_matches_stats_from_reads() {
        let reads = reads3();
        let mut acc = FastQStatsAcc::new();
        for r in &reads {
            acc.update(r);
        }
        assert_eq!(acc.finish(), stats_from_reads(&reads));
        // Empty => same as stats_from_reads(&[]) (min_len = i64::MAX, max_len = 0).
        assert_eq!(FastQStatsAcc::new().finish(), stats_from_reads(&[]));
    }

    #[test]
    fn detect_encoding_stream_matches_whole_file() {
        for text in [
            "@r\nAC\n+\n#I\n",
            "@r\nGTCAGGACAAT\n+\n<=>?@ABCAWA\n",
            "@r\nAC\n+\nKK\n",
        ] {
            assert_eq!(
                detect_encoding_stream(text.as_bytes()).unwrap(),
                detect_encoding(text).unwrap()
            );
        }
        // < 33 is an error in both.
        assert!(detect_encoding_stream("@r\nAC\n+\n A\n".as_bytes()).is_err());
        // Trailing partial record is silently ignored (no error), like detect_encoding.
        assert_eq!(
            detect_encoding_stream("@r\nAC\n+\n#I\n@partial\nAC\n".as_bytes()).unwrap(),
            FastQEncoding::Sanger
        );
    }

    #[test]
    fn substrim_on_read() {
        // qualities below: keep the middle run [123,122,111] -> slice (3,3)
        let sr = ShortRead::new("r", "AAATTTCCC", vec![10, 11, 12, 123, 122, 111, 10, 11, 0]);
        let trimmed = substrim(20, &sr);
        assert_eq!(trimmed.sequence, "TTT");
        assert_eq!(trimmed.qualities, vec![123, 122, 111]);
    }
}
