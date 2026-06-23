//! FASTQ data layer and read trimming, mirroring `NGLess/Data/FastQ.hs`,
//! `NGLess/Interpretation/Substrim.hs`, and `compatibleHeader` from
//! `NGLess/Interpretation/FastQ.hs`.
//!
//! This is the pure computational core of preprocessing: the `ShortRead` record, FASTQ
//! encode/decode, and the `substrim`/`endstrim`/`smoothtrim` trimming algorithms. The
//! streaming pipeline (compressed I/O, parallel decode, the `preprocess` block interpreter)
//! is a later milestone.

use std::path::PathBuf;

use crate::errors::{NgError, NgResult};

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
    fn substrim_on_read() {
        // qualities below: keep the middle run [123,122,111] -> slice (3,3)
        let sr = ShortRead::new("r", "AAATTTCCC", vec![10, 11, 12, 123, 122, 111, 10, 11, 0]);
        let trimmed = substrim(20, &sr);
        assert_eq!(trimmed.sequence, "TTT");
        assert_eq!(trimmed.qualities, vec![123, 122, 111]);
    }
}
