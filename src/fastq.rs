//! FASTQ data layer and read trimming, mirroring `NGLess/Data/FastQ.hs`,
//! `NGLess/Interpretation/Substrim.hs`, and `compatibleHeader` from
//! `NGLess/Interpretation/FastQ.hs`.
//!
//! This is the pure computational core of preprocessing: the `ShortRead` record, FASTQ
//! encode/decode, and the `substrim`/`endstrim`/`smoothtrim` trimming algorithms. The
//! streaming pipeline (compressed I/O, parallel decode, the `preprocess` block interpreter)
//! is a later milestone.

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

impl FastQEncoding {
    pub fn offset(self) -> i32 {
        match self {
            FastQEncoding::Sanger => 33,
            FastQEncoding::Solexa => 64,
        }
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
    fn substrim_on_read() {
        // qualities below: keep the middle run [123,122,111] -> slice (3,3)
        let sr = ShortRead::new("r", "AAATTTCCC", vec![10, 11, 12, 123, 122, 111, 10, 11, 0]);
        let trimmed = substrim(20, &sr);
        assert_eq!(trimmed.sequence, "TTT");
        assert_eq!(trimmed.qualities, vec![123, 122, 111]);
    }
}
