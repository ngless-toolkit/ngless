//! SAM data layer, mirroring `NGLess/Data/Sam.hs`.
//!
//! Parses the 11 mandatory SAM fields plus the optional/extra field block, re-encodes records
//! exactly as `encodeSamLine`, and provides the flag predicates, CIGAR match-size/identity
//! helpers and the `fixCigar` rewrite used by `select`/`as_reads`.

use std::io::BufRead;

use crate::errors::{NgError, NgErrorType, NgResult};

/// One line of a SAM file: either a header (`@...`, kept verbatim) or an alignment record.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SamRecord {
    Header(String),
    Line(SamLine),
}

/// An alignment record, holding all twelve fields (`samExtra` is everything after the qualities).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SamLine {
    pub qname: String,
    pub flag: i64,
    pub rname: String,
    pub pos: i64,
    pub mapq: i64,
    pub cigar: String,
    pub rnext: String,
    pub pnext: i64,
    pub tlen: i64,
    pub seq: String,
    pub qual: String,
    pub extra: String,
}

impl SamLine {
    /// Whether the record carries sequence data (mirrors `hasSequence`: `seq /= "*"`).
    pub fn has_sequence(&self) -> bool {
        self.seq != "*"
    }

    /// Length of the stored sequence (mirrors `samLength`).
    pub fn sam_length(&self) -> i64 {
        self.seq.len() as i64
    }

    /// `isAligned`: flag bit 0x4 (unmapped) is *not* set.
    pub fn is_aligned(&self) -> bool {
        self.flag & 0x4 == 0
    }

    /// `isNegative`: flag bit 0x10 (reverse strand) is set.
    pub fn is_negative(&self) -> bool {
        self.flag & 0x10 != 0
    }

    /// `isPositive`: not [`is_negative`](Self::is_negative).
    pub fn is_positive(&self) -> bool {
        !self.is_negative()
    }

    /// SAM flag bit 0x40 (mirrors `isFirstInPair`).
    pub fn is_first_in_pair(&self) -> bool {
        self.flag & 0x40 != 0
    }

    /// SAM flag bit 0x80 (mirrors `isSecondInPair`).
    pub fn is_second_in_pair(&self) -> bool {
        self.flag & 0x80 != 0
    }

    /// Look up an integer optional tag (mirrors `samIntTag`): a tab-separated field of the form
    /// `XX:i:<int>`. Returns `None` when absent or not an integer tag.
    pub fn int_tag(&self, tname: &str) -> Option<i64> {
        for field in self.extra.split('\t') {
            let b = field.as_bytes();
            // `B.take 2 match == tname` and the type letter at offset 3 is 'i'.
            if b.len() >= 4 && &field[..2] == tname && b[3] == b'i' {
                if let Some((n, _)) = read_int(&field[5..]) {
                    return Some(n);
                }
            }
        }
        None
    }

    /// `matchSize`: number of read bases consumed by the CIGAR (`M`/`=`/`X`, plus `I` when
    /// `include_i`). Soft/hard clips never count here. Mirrors `matchSize includeI`.
    pub fn match_size(&self, include_i: bool) -> NgResult<i64> {
        match_size_cigar(include_i, false, &self.cigar)
    }

    /// `matchIdentity`: `(matchSize - NM) / matchSize`. Errors if the `NM` tag is missing.
    pub fn match_identity(&self, include_i: bool) -> NgResult<f64> {
        let errors = self.int_tag("NM").ok_or_else(|| {
            NgError::new(
                NgErrorType::DataError,
                format!(
                    "Could not get NM tag for samline {}, extra tags were: {}",
                    self.qname, self.extra
                ),
            )
        })?;
        let len = self.match_size(include_i)?;
        Ok((len - errors) as f64 / len as f64)
    }
}

/// CIGAR match size (mirrors `matchSize'`): sum of run lengths for the operations that consume
/// read bases. `M`/`=`/`X` always count; `S` counts when `include_soft`; `I` counts when
/// `include_i`. An empty CIGAR is 0; a malformed one is a `DataError`.
pub fn match_size_cigar(include_i: bool, include_soft: bool, cigar: &str) -> NgResult<i64> {
    let mut rest = cigar;
    let mut total = 0i64;
    while !rest.is_empty() {
        let (n, after) = read_int(rest).ok_or_else(|| {
            NgError::new(
                NgErrorType::DataError,
                format!("could not parse cigar '{cigar}'"),
            )
        })?;
        let code = after.as_bytes().first().copied().ok_or_else(|| {
            NgError::new(
                NgErrorType::DataError,
                format!("could not parse cigar '{cigar}'"),
            )
        })?;
        let add = match code {
            b'M' | b'=' | b'X' => n,
            b'S' if include_soft => n,
            b'I' if include_i => n,
            _ => 0,
        };
        total += add;
        rest = &after[1..];
    }
    Ok(total)
}

/// `fixCigar prev n`: return a CIGAR representing a sequence of length `n`. If `prev` already
/// matches (counting `M`/`=`/`X`/`S`/`I`), it is returned unchanged; otherwise hard clips (`H`)
/// are rewritten to soft clips (`S`). Errors if neither matches.
pub fn fix_cigar(prev: &str, n: i64) -> NgResult<String> {
    if match_size_cigar(true, true, prev)? == n {
        return Ok(prev.to_string());
    }
    let prev2: String = prev
        .chars()
        .map(|c| if c == 'H' { 'S' } else { c })
        .collect();
    if match_size_cigar(true, true, &prev2)? == n {
        return Ok(prev2);
    }
    Err(NgError::new(
        NgErrorType::DataError,
        format!("Cannot fix CIGAR string \"{prev}\" to represent a sequence of length {n}"),
    ))
}

/// Re-encode an alignment record exactly as `encodeSamLine`: the twelve fields joined by tabs.
pub fn encode_sam_line(l: &SamLine) -> String {
    format!(
        "{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}",
        l.qname,
        l.flag,
        l.rname,
        l.pos,
        l.mapq,
        l.cigar,
        l.rnext,
        l.pnext,
        l.tlen,
        l.seq,
        l.qual,
        l.extra
    )
}

/// Whether a raw line is a SAM header line (mirrors `isSamHeaderString`).
pub fn is_header_line(line: &str) -> bool {
    line.starts_with('@')
}

/// Parse one alignment line, mirroring the `samP` `SimpleParser` exactly — including its quirk
/// that the qualities use the "optional tab" parser, so an 11-column line (no extra fields)
/// leaves `qual` empty and stores the quality string in `extra`.
pub fn parse_sam_line(line: &str) -> NgResult<SamLine> {
    fn err(line: &str) -> NgError {
        NgError::new(
            NgErrorType::DataError,
            format!("Could not parse sam line {line:?}"),
        )
    }
    let r = line;
    let (qname, r) = take_tab(r).ok_or_else(|| err(line))?;
    let (flag, r) = read_int_tab(r).ok_or_else(|| err(line))?;
    let (rname, r) = take_tab(r).ok_or_else(|| err(line))?;
    let (pos, r) = read_int_tab(r).ok_or_else(|| err(line))?;
    let (mapq, r) = read_int_tab(r).ok_or_else(|| err(line))?;
    let (cigar, r) = take_tab(r).ok_or_else(|| err(line))?;
    let (rnext, r) = take_tab(r).ok_or_else(|| err(line))?;
    let (pnext, r) = read_int_tab(r).ok_or_else(|| err(line))?;
    let (tlen, r) = read_int_tab(r).ok_or_else(|| err(line))?;
    let (seq, r) = take_tab(r).ok_or_else(|| err(line))?;
    let (qual, extra) = take_tab_opt(r);
    Ok(SamLine {
        qname: qname.to_string(),
        flag,
        rname: rname.to_string(),
        pos,
        mapq,
        cigar: cigar.to_string(),
        rnext: rnext.to_string(),
        pnext,
        tlen,
        seq: seq.to_string(),
        qual: qual.to_string(),
        extra: extra.to_string(),
    })
}

/// Parse SAM text into records (mirrors `readSamLine` applied line-by-line).
pub fn parse_sam(text: &str) -> NgResult<Vec<SamRecord>> {
    let mut out = Vec::new();
    for line in text.lines() {
        if line.is_empty() {
            continue;
        }
        if is_header_line(line) {
            out.push(SamRecord::Header(line.to_string()));
        } else {
            out.push(SamRecord::Line(parse_sam_line(line)?));
        }
    }
    Ok(out)
}

/// `tabDelim`: split at the first tab, requiring one to be present.
fn take_tab(input: &str) -> Option<(&str, &str)> {
    input.find('\t').map(|ix| (&input[..ix], &input[ix + 1..]))
}

/// `tabDelimOpts`: split at the first tab if present, else `("", input)`.
fn take_tab_opt(input: &str) -> (&str, &str) {
    match input.find('\t') {
        Some(ix) => (&input[..ix], &input[ix + 1..]),
        None => ("", input),
    }
}

/// `readIntTab`: read an integer then drop the following byte (the field separator).
fn read_int_tab(input: &str) -> Option<(i64, &str)> {
    let (n, rest) = read_int(input)?;
    Some((n, rest.get(1..).unwrap_or("")))
}

/// Read a leading (optionally signed) integer, mirroring `B8.readInt`. Returns the value and the
/// remaining input starting at the first non-digit.
fn read_int(input: &str) -> Option<(i64, &str)> {
    let bytes = input.as_bytes();
    let mut i = 0;
    let neg = match bytes.first() {
        Some(b'-') => {
            i = 1;
            true
        }
        Some(b'+') => {
            i = 1;
            false
        }
        _ => false,
    };
    let start = i;
    let mut val: i64 = 0;
    while i < bytes.len() && bytes[i].is_ascii_digit() {
        val = val * 10 + (bytes[i] - b'0') as i64;
        i += 1;
    }
    if i == start {
        return None;
    }
    Some((if neg { -val } else { val }, &input[i..]))
}

/// A streaming reader over the *text* of a SAM/BAM file: transparently decompresses gzip
/// (via `compression::open_read`) and decodes BAM by piping `samtools view -h` (mirrors
/// `samBamConduit`). Unlike `read_sam_text`, the whole file is never held in memory.
pub struct SamReader {
    inner: Box<dyn BufRead>,
    /// The `samtools` child for BAM input; kept alive so its stdout stays open and `wait()`-ed
    /// on EOF to surface a non-zero exit.
    child: Option<std::process::Child>,
    buf: String,
}

impl SamReader {
    /// Read the next line (newline stripped). `None` at EOF, at which point the BAM child (if any)
    /// is waited on and a non-zero exit is reported as an error.
    fn next_line(&mut self) -> NgResult<Option<String>> {
        self.buf.clear();
        let n = self.inner.read_line(&mut self.buf).map_err(|e| {
            NgError::new(
                NgErrorType::DataError,
                format!("Could not read SAM input: {e}"),
            )
        })?;
        if n == 0 {
            self.finish()?;
            return Ok(None);
        }
        if self.buf.ends_with('\n') {
            self.buf.pop();
            if self.buf.ends_with('\r') {
                self.buf.pop();
            }
        }
        Ok(Some(std::mem::take(&mut self.buf)))
    }

    /// Wait for the BAM `samtools` child (if any) and error on a non-zero exit.
    fn finish(&mut self) -> NgResult<()> {
        if let Some(mut child) = self.child.take() {
            let mut stderr = String::new();
            if let Some(mut e) = child.stderr.take() {
                use std::io::Read;
                let _ = e.read_to_string(&mut stderr);
            }
            let status = child.wait().map_err(|e| {
                NgError::new(
                    NgErrorType::SystemError,
                    format!("samtools view failed: {e}"),
                )
            })?;
            if !status.success() {
                return Err(NgError::new(
                    NgErrorType::SystemError,
                    format!(
                        "Failed samtools view (exit {:?}).\nError message was:\n{stderr}",
                        status.code()
                    ),
                ));
            }
        }
        Ok(())
    }
}

/// Open a SAM/BAM file for streaming line-by-line (BAM decoded via samtools, gzip transparent).
pub fn open_sam(path: &str) -> NgResult<SamReader> {
    if path.ends_with(".bam") {
        let mut child = crate::samtools::bam_to_sam_child(path)?;
        let stdout = child.stdout.take().ok_or_else(|| {
            NgError::new(
                NgErrorType::SystemError,
                "samtools view: no stdout pipe".to_string(),
            )
        })?;
        Ok(SamReader {
            inner: Box::new(std::io::BufReader::new(stdout)),
            child: Some(child),
            buf: String::new(),
        })
    } else {
        Ok(SamReader {
            inner: crate::compression::open_read(path)?,
            child: None,
            buf: String::new(),
        })
    }
}

/// One grouping unit of a SAM file: a header line (verbatim) or a group of alignment records
/// (with their original raw lines) that share a read name. Mirrors `readSamGroupsAsConduit`'s
/// output: header lines are each their own item; consecutive alignment lines with the same read
/// name (and, when not `paired`, the same first-in-pair bit) form one data group.
pub enum SamItem {
    Header(String),
    Data(Vec<(SamLine, String)>),
}

/// Lazy iterator over `SamItem`s, the streaming analogue of the old whole-file `group_sam`.
pub struct SamGroups {
    reader: SamReader,
    paired: bool,
    cur: Vec<(SamLine, String)>,
    cur_key: Option<(String, bool)>,
    /// A line already read but not yet consumed (held back to start the next group/header).
    pending: Option<String>,
    done: bool,
}

/// Open `path` and group its records the way `select` does (see `SamItem`).
pub fn group_sam_stream(path: &str, paired: bool) -> NgResult<SamGroups> {
    Ok(SamGroups {
        reader: open_sam(path)?,
        paired,
        cur: Vec::new(),
        cur_key: None,
        pending: None,
        done: false,
    })
}

impl Iterator for SamGroups {
    type Item = NgResult<SamItem>;

    fn next(&mut self) -> Option<NgResult<SamItem>> {
        if self.done {
            return None;
        }
        loop {
            let line = match self.pending.take() {
                Some(l) => l,
                None => match self.reader.next_line() {
                    Ok(Some(l)) => l,
                    Ok(None) => {
                        self.done = true;
                        if !self.cur.is_empty() {
                            self.cur_key = None;
                            return Some(Ok(SamItem::Data(std::mem::take(&mut self.cur))));
                        }
                        return None;
                    }
                    Err(e) => {
                        self.done = true;
                        return Some(Err(e));
                    }
                },
            };
            if line.is_empty() {
                continue;
            }
            if is_header_line(&line) {
                if !self.cur.is_empty() {
                    self.pending = Some(line);
                    self.cur_key = None;
                    return Some(Ok(SamItem::Data(std::mem::take(&mut self.cur))));
                }
                return Some(Ok(SamItem::Header(line)));
            }
            let sl = match parse_sam_line(&line) {
                Ok(s) => s,
                Err(e) => {
                    self.done = true;
                    return Some(Err(e));
                }
            };
            let key = (
                sl.qname.clone(),
                if self.paired {
                    false
                } else {
                    sl.is_first_in_pair()
                },
            );
            if self.cur_key.as_ref() == Some(&key) {
                self.cur.push((sl, line));
            } else if !self.cur.is_empty() {
                self.pending = Some(line);
                self.cur_key = None;
                return Some(Ok(SamItem::Data(std::mem::take(&mut self.cur))));
            } else {
                self.cur_key = Some(key);
                self.cur.push((sl, line));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_and_flags() {
        let sam = "@HD\tVN:1.0\n\
                   r1\t77\t*\t0\t0\t*\t*\t0\t0\tACGT\tIIII\tAS:i:0\n\
                   r1\t141\t*\t0\t0\t*\t*\t0\t0\tTTTT\tHHHH\tNM:i:2\n";
        let recs = parse_sam(sam).unwrap();
        assert_eq!(recs.len(), 3);
        assert!(matches!(recs[0], SamRecord::Header(_)));
        match (&recs[1], &recs[2]) {
            (SamRecord::Line(a), SamRecord::Line(b)) => {
                assert_eq!(a.qname, "r1");
                assert!(a.is_first_in_pair() && !a.is_second_in_pair());
                assert!(b.is_second_in_pair() && !b.is_first_in_pair());
                assert!(a.has_sequence());
                assert_eq!(a.qual, "IIII");
                assert_eq!(a.extra, "AS:i:0");
                assert_eq!(b.int_tag("NM"), Some(2));
            }
            _ => panic!("expected two alignment lines"),
        }
    }

    #[test]
    fn encode_round_trips_with_extra() {
        let line = "ER9\t73\tg1\t1504\t0\t44M23S\t=\t1504\t0\tACGT\tHHHH\tNM:i:0\tMD:Z:44";
        let l = parse_sam_line(line).unwrap();
        assert_eq!(encode_sam_line(&l), line);
    }

    #[test]
    fn eleven_field_line_quirk() {
        // No extra fields: the qualities land in `extra`, matching the Haskell parser.
        let line = "r\t0\t*\t0\t0\t*\t*\t0\t0\tACGT\tIIII";
        let l = parse_sam_line(line).unwrap();
        assert_eq!(l.qual, "");
        assert_eq!(l.extra, "IIII");
    }

    #[test]
    fn star_sequence_has_none() {
        let sam = "r1\t4\t*\t0\t0\t*\t*\t0\t0\t*\t*\tNM:i:0\n";
        let recs = parse_sam(sam).unwrap();
        match &recs[0] {
            SamRecord::Line(l) => assert!(!l.has_sequence()),
            _ => panic!(),
        }
    }

    #[test]
    fn match_size_and_identity() {
        let line = "r\t0\tg\t1\t0\t44M23S\t=\t1\t0\tACGT\tHHHH\tNM:i:0";
        let l = parse_sam_line(line).unwrap();
        assert_eq!(l.match_size(false).unwrap(), 44);
        assert_eq!(l.match_size_cigar_for_test(true, true), 67);
        assert_eq!(l.match_identity(false).unwrap(), 1.0);
    }

    // Mirrors Tests/Select.hs `case_matchSize1..3`, `case_match_identity_soft` and
    // `case_isAligned_raw`: match sizes for real alignments with deletions, soft and hard clips
    // and an insertion, plus the soft-clip identity and the aligned-flag check.
    #[test]
    fn match_size_and_identity_real_alignments() {
        // 26M3D9M3D6M6D8M2D21M: only the M runs consume read bases -> 26+9+6+8+21 = 70.
        let complex = "SRR070372.3\t16\tV\t7198336\t21\t26M3D9M3D6M6D8M2D21M\t*\t0\t0\tCCCTTATGCAGGTCTTAACACAATTCTTGTATGTTCCATCGTTCTCCAGAATGAATATCAATGATACCAA\t014<<BBBBDDFFFDDDDFHHFFD?@??DBBBB5555::?=BBBBDDF@BBFHHHHHHHFFFFFD@@@@@\tNM:i:14\tMD:Z:26^TTT9^TTC6^TTTTTT8^AA21\tAS:i:3\tXS:i:0";
        let complex = parse_sam_line(complex).unwrap();
        assert_eq!(complex.match_size(true).unwrap(), 26 + 9 + 6 + 8 + 21);
        assert!(complex.is_aligned());

        // 69M16S: soft clips do not count -> 69.
        let simple = "simulated:1:1:38:663#0\t0\tRef1\t1018\t3\t69M16S\t=\t1018\t0\tTTCGAGAAGATGGGTATCGTGGGAAATAACGGAACGGGGAAGTCTACCTTCATCAAGATGCTGCTGGGCTTGGTGAAACCCGACA\tIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII\tNM:i:5\tMD:Z:17T5T14A2A2G24\tAS:i:44\tXS:i:40";
        let simple = parse_sam_line(simple).unwrap();
        assert_eq!(simple.match_size(true).unwrap(), 69);

        // 5M1I40M54H: the insertion counts (include_i), hard clips do not -> 5+1+40 = 46.
        let refinsert = "SRR6028238.2619770\t417\tX\t1319005\t0\t5M1I40M54H\t=\t2019245\t700291\tTTTTCCGCTGAATATGCCCAAAGTGCAACAACGACGACCGCCGCCA\t@DDDEDDDDBDDDEEECDDDDDCAACCCCBBDDDBBBBBB<@BBDB\tNM:i:1\tMD:Z:45\tMC:Z:51M50H\tAS:i:40";
        let refinsert = parse_sam_line(refinsert).unwrap();
        assert_eq!(refinsert.match_size(true).unwrap(), 46);

        // 40M with NM:i:1 -> identity (40-1)/40 = 0.975.
        let soft = "IRIS:7:3:1046:1723#0\t4\t*\t0\t0\t40M\t*\t0\t0\tAAAAAAAAAAAAAAAAAAAATTTAAA\taaaaaaaaaaaaaaaaaa`abbba`^\tAS:i:0\tXS:i:0\tNM:i:1";
        let soft = parse_sam_line(soft).unwrap();
        assert_eq!(soft.match_identity(true).unwrap(), 0.975);
    }

    #[test]
    fn fix_cigar_rewrites_hard_to_soft() {
        // 19H48M consumes 48 read bases; to represent length 67 the H must become S.
        assert_eq!(fix_cigar("19H48M", 67).unwrap(), "19S48M");
        // Already correct: returned unchanged.
        assert_eq!(fix_cigar("44M23S", 67).unwrap(), "44M23S");
        assert!(fix_cigar("10M", 99).is_err());
    }

    #[test]
    fn too_few_fields_errors() {
        assert!(parse_sam("r1\t0\t*\n").is_err());
    }

    /// Reference (whole-file) grouping: the same logic the streaming `SamGroups` replaces. Used to
    /// pin the streaming iterator to its in-memory analogue.
    fn group_sam_whole(text: &str, paired: bool) -> Vec<SamItem> {
        let mut items = Vec::new();
        let mut cur: Vec<(SamLine, String)> = Vec::new();
        let mut cur_key: Option<(String, bool)> = None;
        for line in text.lines() {
            if line.is_empty() {
                continue;
            }
            if is_header_line(line) {
                if !cur.is_empty() {
                    items.push(SamItem::Data(std::mem::take(&mut cur)));
                }
                cur_key = None;
                items.push(SamItem::Header(line.to_string()));
                continue;
            }
            let sl = parse_sam_line(line).unwrap();
            let key = (
                sl.qname.clone(),
                if paired { false } else { sl.is_first_in_pair() },
            );
            if cur_key.as_ref() == Some(&key) {
                cur.push((sl, line.to_string()));
            } else {
                if !cur.is_empty() {
                    items.push(SamItem::Data(std::mem::take(&mut cur)));
                }
                cur_key = Some(key);
                cur.push((sl, line.to_string()));
            }
        }
        if !cur.is_empty() {
            items.push(SamItem::Data(cur));
        }
        items
    }

    fn item_shape(it: &SamItem) -> String {
        match it {
            SamItem::Header(h) => format!("H:{h}"),
            SamItem::Data(g) => {
                let raws: Vec<&str> = g.iter().map(|(_, raw)| raw.as_str()).collect();
                format!("D:{}", raws.join("|"))
            }
        }
    }

    #[test]
    fn stream_grouping_matches_whole_file() {
        // Headers, a singleton, an unpaired mate pair, and a 3-record same-name group.
        let sam = "@HD\tVN:1.0\n\
                   @SQ\tSN:g1\tLN:100\n\
                   r1\t0\tg1\t1\t60\t4M\t*\t0\t0\tACGT\tIIII\tNM:i:0\n\
                   r2\t77\tg1\t1\t60\t4M\t*\t0\t0\tACGT\tIIII\tNM:i:0\n\
                   r2\t141\tg1\t1\t60\t4M\t*\t0\t0\tTTTT\tHHHH\tNM:i:0\n\
                   r3\t0\tg1\t1\t60\t4M\t*\t0\t0\tAAAA\tIIII\tNM:i:0\n\
                   r3\t0\tg1\t5\t60\t4M\t*\t0\t0\tCCCC\tIIII\tNM:i:0\n";
        let dir = std::env::temp_dir();
        for paired in [true, false] {
            let path = dir.join(format!("ngless_sam_stream_test_{paired}.sam"));
            std::fs::write(&path, sam).unwrap();
            let want: Vec<String> = group_sam_whole(sam, paired)
                .iter()
                .map(item_shape)
                .collect();
            let got: Vec<String> = group_sam_stream(&path.to_string_lossy(), paired)
                .unwrap()
                .map(|it| item_shape(&it.unwrap()))
                .collect();
            assert_eq!(got, want, "paired={paired}");
            std::fs::remove_file(&path).ok();
        }
    }

    impl SamLine {
        fn match_size_cigar_for_test(&self, include_i: bool, include_soft: bool) -> i64 {
            match_size_cigar(include_i, include_soft, &self.cigar).unwrap()
        }
    }
}
