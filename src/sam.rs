//! Minimal SAM data layer, mirroring the parts of `NGLess/Data/Sam.hs` needed so far.
//!
//! Only what `as_reads` requires is parsed: the query name, flag, sequence and qualities of each
//! alignment line, plus header lines kept verbatim. The remaining mandatory/optional fields and
//! the `select`/CIGAR machinery arrive with later milestones.

use crate::errors::{NgError, NgErrorType, NgResult};

/// One line of a SAM file: either a header (`@...`, kept verbatim) or an alignment record.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SamRecord {
    Header(String),
    Line(SamLine),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SamLine {
    pub qname: String,
    pub flag: i64,
    pub seq: String,
    pub qual: String,
}

impl SamLine {
    /// Whether the record carries sequence data (mirrors `hasSequence`: `seq /= "*"`).
    pub fn has_sequence(&self) -> bool {
        self.seq != "*"
    }

    /// SAM flag bit 0x40 (mirrors `isFirstInPair`).
    pub fn is_first_in_pair(&self) -> bool {
        self.flag & 0x40 != 0
    }

    /// SAM flag bit 0x80 (mirrors `isSecondInPair`).
    pub fn is_second_in_pair(&self) -> bool {
        self.flag & 0x80 != 0
    }
}

/// Parse SAM text into records (mirrors `readSamLine` applied line-by-line). Header lines start
/// with `@`; alignment lines must have the 11 mandatory tab-separated fields.
pub fn parse_sam(text: &str) -> NgResult<Vec<SamRecord>> {
    let mut out = Vec::new();
    for line in text.lines() {
        if line.is_empty() {
            continue;
        }
        if line.starts_with('@') {
            out.push(SamRecord::Header(line.to_string()));
            continue;
        }
        let fields: Vec<&str> = line.split('\t').collect();
        if fields.len() < 11 {
            return Err(NgError::new(
                NgErrorType::DataError,
                format!("Could not parse sam line {line:?}"),
            ));
        }
        let flag = fields[1].parse::<i64>().map_err(|_| {
            NgError::new(
                NgErrorType::DataError,
                format!("Could not parse sam line {line:?}"),
            )
        })?;
        out.push(SamRecord::Line(SamLine {
            qname: fields[0].to_string(),
            flag,
            seq: fields[9].to_string(),
            qual: fields[10].to_string(),
        }));
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_and_flags() {
        let sam = "@HD\tVN:1.0\n\
                   r1\t77\t*\t0\t0\t*\t*\t0\t0\tACGT\tIIII\tAS:i:0\n\
                   r1\t141\t*\t0\t0\t*\t*\t0\t0\tTTTT\tHHHH\n";
        let recs = parse_sam(sam).unwrap();
        assert_eq!(recs.len(), 3);
        assert!(matches!(recs[0], SamRecord::Header(_)));
        match (&recs[1], &recs[2]) {
            (SamRecord::Line(a), SamRecord::Line(b)) => {
                assert_eq!(a.qname, "r1");
                assert!(a.is_first_in_pair() && !a.is_second_in_pair());
                assert!(b.is_second_in_pair() && !b.is_first_in_pair());
                assert!(a.has_sequence());
            }
            _ => panic!("expected two alignment lines"),
        }
    }

    #[test]
    fn star_sequence_has_none() {
        let sam = "r1\t4\t*\t0\t0\t*\t*\t0\t0\t*\t*\n";
        let recs = parse_sam(sam).unwrap();
        match &recs[0] {
            SamRecord::Line(l) => assert!(!l.has_sequence()),
            _ => panic!(),
        }
    }

    #[test]
    fn too_few_fields_errors() {
        assert!(parse_sam("r1\t0\t*\n").is_err());
    }
}
