//! GFF/GTF parsing, mirroring `NGLess/Data/GFF.hs`.
//!
//! Only the pieces used by `count()` are ported: the line structure (sequence id, type,
//! start/end, strand and the attribute list) and the attribute parser, which has to cope with
//! both GFF3 (`key=value`) and GTF (`key "value"`) syntaxes — chosen *per attribute* by whether
//! the field contains an `=` — and to comma-split a value into several `(key, value)` pairs
//! (this is what makes `GOs=GO:1,GO:2` expand to one feature per GO id).

use crate::errors::{NgError, NgErrorType, NgResult};

/// GFF strand column (`gffStrand`). `Unstranded` is `.`, `Unknown` is `?`.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum GffStrand {
    Pos,
    Neg,
    Unknown,
    Unstranded,
}

/// A parsed GFF/GTF line. Score and phase are not used by counting and are dropped.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GffLine {
    pub seq_id: String,
    pub source: String,
    pub gtype: String,
    pub start: i64,
    pub end: i64,
    pub strand: GffStrand,
    pub attrs: Vec<(String, String)>,
}

impl GffLine {
    /// Collect the values of all attributes whose key is `name` (mirrors `filterSubFeatures`).
    pub fn attr_values(&self, name: &str) -> Vec<String> {
        self.attrs
            .iter()
            .filter(|(k, _)| k == name)
            .map(|(_, v)| v.clone())
            .collect()
    }
}

fn data_error(msg: String) -> NgError {
    NgError::new(NgErrorType::DataError, msg)
}

/// Parse a single strand character (mirrors `parseStrand`).
fn parse_strand(c: char) -> NgResult<GffStrand> {
    match c {
        '.' => Ok(GffStrand::Unstranded),
        '+' => Ok(GffStrand::Pos),
        '-' => Ok(GffStrand::Neg),
        '?' => Ok(GffStrand::Unknown),
        u => Err(data_error(format!(
            "Parsing GFF line: unhandled value for strand ({u})"
        ))),
    }
}

/// Parse a GFF/GTF data line (mirrors `readGffLine`): exactly nine TAB-separated columns.
pub fn read_gff_line(line: &str) -> NgResult<GffLine> {
    let cols: Vec<&str> = line.split('\t').collect();
    if cols.len() != 9 {
        return Err(data_error(format!("unexpected line in GFF: {line:?}")));
    }
    let start = cols[3]
        .parse::<i64>()
        .map_err(|_| data_error(format!("Could not parse GFF line (reading start): {line:?}")))?;
    let end = cols[4]
        .parse::<i64>()
        .map_err(|_| data_error(format!("Could not parse GFF line (reading end): {line:?}")))?;
    let strand = match cols[6].chars().next() {
        Some(c) => parse_strand(c)?,
        None => return Err(data_error("Could not parse GFF line (empty strand field)".into())),
    };
    Ok(GffLine {
        seq_id: cols[0].to_string(),
        source: cols[1].to_string(),
        gtype: cols[2].to_string(),
        start,
        end,
        strand,
        attrs: parse_gff_attributes(cols[8]),
    })
}

/// Trim ASCII spaces from both ends (mirrors `_trimString`, which only strips `' '`).
fn trim_spaces(s: &str) -> &str {
    s.trim_matches(' ')
}

/// Parse the GFF attribute column (mirrors `_parseGffAttributes`).
pub fn parse_gff_attributes(col: &str) -> Vec<(String, String)> {
    let mut col = trim_spaces(col);
    // removeLastDel: drop a single trailing ';'
    if let Some(stripped) = col.strip_suffix(';') {
        col = stripped;
    }
    let mut out = Vec::new();
    for field in col.split(';') {
        let field = trim_spaces(field);
        if field.is_empty() {
            // An empty field still yields one ("", "") pair in Haskell only if present; a stray
            // separator with no content produces a key with empty value. We mirror by skipping
            // truly empty fragments, which only arise from leading/trailing separators.
            continue;
        }
        // checkAttrTag: '=' if the field contains one, otherwise ' '.
        let tag = if field.contains('=') { '=' } else { ' ' };
        let (key, rest) = match field.find(tag) {
            Some(i) => (&field[..i], &field[i..]),
            None => (field, ""),
        };
        // `B.tail` drops the tag char; quotes are stripped from the value.
        let value: String = rest.chars().skip(1).filter(|&c| c != '"').collect();
        for v in value.split(',') {
            out.push((key.to_string(), v.to_string()));
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn gtf_quoted_attributes() {
        let attrs = parse_gff_attributes("gene_id \"WBGene00000510\"; gene_name \"abc\";");
        assert_eq!(
            attrs,
            vec![
                ("gene_id".to_string(), "WBGene00000510".to_string()),
                ("gene_name".to_string(), "abc".to_string()),
            ]
        );
    }

    #[test]
    fn gff3_comma_split_attributes() {
        let attrs = parse_gff_attributes("ID=AC062_1558;GOs=GO:0005840,GO:0071704,GO:1901576");
        assert_eq!(
            attrs,
            vec![
                ("ID".to_string(), "AC062_1558".to_string()),
                ("GOs".to_string(), "GO:0005840".to_string()),
                ("GOs".to_string(), "GO:0071704".to_string()),
                ("GOs".to_string(), "GO:1901576".to_string()),
            ]
        );
    }

    #[test]
    fn parse_line_fields() {
        let l =
            read_gff_line("CP013830\trefseq\teggnog\t1000\t1377\t.\t-\t.\tID=AC062_1558").unwrap();
        assert_eq!(l.seq_id, "CP013830");
        assert_eq!(l.gtype, "eggnog");
        assert_eq!(l.start, 1000);
        assert_eq!(l.end, 1377);
        assert_eq!(l.strand, GffStrand::Neg);
        assert_eq!(l.attr_values("ID"), vec!["AC062_1558".to_string()]);
    }

    // Mirrors Tests.hs `case_trim_attrs_1..5`: `_trimString` strips only leading/trailing spaces.
    #[test]
    fn trim_string_strips_outer_spaces() {
        assert_eq!(trim_spaces(" x = 10"), "x = 10");
        assert_eq!(trim_spaces(" x = 10 "), "x = 10");
        assert_eq!(trim_spaces("x = 10 "), "x = 10");
        assert_eq!(trim_spaces("x = 10"), "x = 10");
        assert_eq!(trim_spaces("   X    "), "X");
    }

    // Mirrors Tests.hs `case_parse_gff_atributes_normal_*` / `*_trail_del*`: GFF3 `key=value`
    // attributes, with an optional single trailing `;` (and trailing space) dropped.
    #[test]
    fn gff3_attributes_trailing_delimiter() {
        let expected = vec![
            ("ID".to_string(), "chrI".to_string()),
            ("dbxref".to_string(), "NCBI:NC_001133".to_string()),
            ("Name".to_string(), "chrI".to_string()),
        ];
        assert_eq!(
            parse_gff_attributes("ID=chrI;dbxref=NCBI:NC_001133;Name=chrI"),
            expected
        );
        let expected2 = vec![
            ("gene_id".to_string(), "chrI".to_string()),
            ("dbxref".to_string(), "NCBI:NC_001133".to_string()),
            ("Name".to_string(), "chrI".to_string()),
        ];
        assert_eq!(
            parse_gff_attributes("gene_id=chrI;dbxref=NCBI:NC_001133;Name=chrI"),
            expected2
        );
        assert_eq!(
            parse_gff_attributes("gene_id=chrI;dbxref=NCBI:NC_001133;Name=chrI;"),
            expected2
        );
        assert_eq!(
            parse_gff_attributes("gene_id=chrI;dbxref=NCBI:NC_001133;Name=chrI; "),
            expected2
        );
    }

    // Mirrors Tests.hs `case_parse_gff_line`: a full GTF exon line with quoted attributes and a
    // negative strand.
    #[test]
    fn gtf_full_line() {
        let line = "chrI\tunknown\texon\t4124\t4358\t.\t-\t.\tgene_id \"Y74C9A.3\"; transcript_id \"NM_058260\"; gene_name \"Y74C9A.3\"; p_id \"P23728\"; tss_id \"TSS14501\";";
        let l = read_gff_line(line).unwrap();
        assert_eq!(l.seq_id, "chrI");
        assert_eq!(l.gtype, "exon");
        assert_eq!(l.start, 4124);
        assert_eq!(l.end, 4358);
        assert_eq!(l.strand, GffStrand::Neg);
        assert_eq!(
            parse_gff_attributes("gene_id \"Y74C9A.3\"; transcript_id \"NM_058260\"; gene_name \"Y74C9A.3\"; p_id \"P23728\"; tss_id \"TSS14501\";"),
            vec![
                ("gene_id".to_string(), "Y74C9A.3".to_string()),
                ("transcript_id".to_string(), "NM_058260".to_string()),
                ("gene_name".to_string(), "Y74C9A.3".to_string()),
                ("p_id".to_string(), "P23728".to_string()),
                ("tss_id".to_string(), "TSS14501".to_string()),
            ]
        );
    }
}
