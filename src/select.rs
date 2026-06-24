//! Pure `select`/filter logic, mirroring `NGLess/Interpretation/Select.hs`.
//!
//! This module holds everything that does not need interpreter state: parsing the
//! `keep_if`/`drop_if` conditions, the per-group keep/drop tests, the `.filter(...)` group
//! options, and the "sequence reinjection" used when a kept mate has had its sequence dropped by
//! the aligner. The interpreter glue (grouping the file, running the block, materialising the
//! result) lives in [`crate::interpret`].

use crate::errors::{NgError, NgErrorType, NgResult};
use crate::sam::{fix_cigar, SamLine};

/// A single `keep_if`/`drop_if` condition (mirrors `SelectCondition`).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum SelectCondition {
    Mapped,
    Unmapped,
    Unique,
}

/// The combined condition for a `select` call (mirrors `MatchCondition`).
#[derive(Clone, Debug)]
pub enum MatchCondition {
    KeepIf(Vec<SelectCondition>),
    DropIf(Vec<SelectCondition>),
}

impl SelectCondition {
    fn parse(s: &str) -> NgResult<SelectCondition> {
        match s {
            "mapped" => Ok(SelectCondition::Mapped),
            "unmapped" => Ok(SelectCondition::Unmapped),
            "unique" => Ok(SelectCondition::Unique),
            other => Err(NgError::should_not_occur(format!(
                "Check failed.  Should not have seen this condition: '{other}'"
            ))),
        }
    }
}

/// Build the [`MatchCondition`] from the `keep_if`/`drop_if` symbol lists (mirrors
/// `_parseConditions`). Using both is an error.
pub fn parse_conditions(keep_if: &[String], drop_if: &[String]) -> NgResult<MatchCondition> {
    let keep: Vec<SelectCondition> = keep_if
        .iter()
        .map(|s| SelectCondition::parse(s))
        .collect::<NgResult<_>>()?;
    let drop: Vec<SelectCondition> = drop_if
        .iter()
        .map(|s| SelectCondition::parse(s))
        .collect::<NgResult<_>>()?;
    match (keep.is_empty(), drop.is_empty()) {
        (_, true) => Ok(MatchCondition::KeepIf(keep)),
        (true, false) => Ok(MatchCondition::DropIf(drop)),
        (false, false) => Err(NgError::new(
            NgErrorType::ScriptError,
            "To select, you cannot use both keep_if and drop_if",
        )),
    }
}

/// `isGroupUnique`: a group maps uniquely if it is empty, a singleton, or a properly-paired
/// pair to the same reference.
pub fn is_group_unique(g: &[SamLine]) -> bool {
    match g {
        [] | [_] => true,
        [f, s] => {
            (f.is_first_in_pair() != s.is_first_in_pair())
                && (!(f.is_aligned() && s.is_aligned()) || f.rname == s.rname)
        }
        _ => false,
    }
}

/// Apply the parsed `keep_if`/`drop_if` conditions to one group of `(record, raw-line)` pairs
/// (mirrors `matchConditions'`). Order is preserved.
pub fn apply_conditions<T>(cond: &MatchCondition, group: Vec<(SamLine, T)>) -> Vec<(SamLine, T)> {
    match cond {
        MatchCondition::KeepIf(cs) => cs.iter().fold(group, |g, c| keep1(*c, g)),
        MatchCondition::DropIf(cs) => cs.iter().fold(group, |g, c| drop1(*c, g)),
    }
}

fn keep1<T>(c: SelectCondition, g: Vec<(SamLine, T)>) -> Vec<(SamLine, T)> {
    match c {
        SelectCondition::Mapped => g.into_iter().filter(|(s, _)| s.is_aligned()).collect(),
        SelectCondition::Unmapped => {
            if g.iter().any(|(s, _)| s.is_aligned()) {
                Vec::new()
            } else {
                g
            }
        }
        SelectCondition::Unique => {
            if is_group_unique(&samlines(&g)) {
                g
            } else {
                Vec::new()
            }
        }
    }
}

fn drop1<T>(c: SelectCondition, g: Vec<(SamLine, T)>) -> Vec<(SamLine, T)> {
    match c {
        SelectCondition::Unmapped => g.into_iter().filter(|(s, _)| s.is_aligned()).collect(),
        SelectCondition::Mapped => {
            if g.iter().any(|(s, _)| s.is_aligned()) {
                Vec::new()
            } else {
                g
            }
        }
        SelectCondition::Unique => {
            if is_group_unique(&samlines(&g)) {
                Vec::new()
            } else {
                g
            }
        }
    }
}

fn samlines<T>(g: &[(SamLine, T)]) -> Vec<SamLine> {
    g.iter().map(|(s, _)| s.clone()).collect()
}

/// `mUnique`: keep the group only if it maps uniquely, otherwise drop it entirely.
pub fn m_unique(group: Vec<SamLine>) -> Vec<SamLine> {
    if is_group_unique(&group) {
        group
    } else {
        Vec::new()
    }
}

/// `pe_filter` (`filterPE`): keep only aligned, properly-mated pairs. For each aligned
/// positive-strand record, find the first aligned negative-strand record on the same reference and
/// emit the pair; unmatched records are dropped.
pub fn filter_pe(group: Vec<SamLine>) -> Vec<SamLine> {
    let aligned: Vec<SamLine> = group.into_iter().filter(|s| s.is_aligned()).collect();
    let mut out = Vec::new();
    for sl in &aligned {
        if !sl.is_positive() {
            continue;
        }
        if let Some(sl2) = aligned
            .iter()
            .find(|o| o.is_negative() && o.rname == sl.rname)
        {
            out.push(sl.clone());
            out.push(sl2.clone());
        }
    }
    out
}

/// `allbest` (`mBesthit`): within each mate bucket, keep only the records with the best
/// (minimum-NM) match. The `use_newer` flag controls how match sizes are computed, but since the
/// per-record max match size cancels out of the comparison, only the NM distance matters.
pub fn m_besthit(use_newer: bool, group: Vec<SamLine>) -> Vec<SamLine> {
    if group.len() <= 1 {
        return group;
    }
    let (g1, g2, gs) = split3(&group);
    let mut out = m_besthit_bucket(use_newer, g1);
    out.extend(m_besthit_bucket(use_newer, g2));
    out.extend(m_besthit_bucket(use_newer, gs));
    out
}

fn m_besthit_bucket(use_newer: bool, group: Vec<SamLine>) -> Vec<SamLine> {
    if group.len() <= 1 {
        return group;
    }
    // Records that have both an NM tag and a computable match size; others are dropped (unless
    // none qualify, in which case the bucket is returned unchanged).
    let extracted: Vec<(i64, SamLine)> = group
        .iter()
        .filter_map(|s| {
            let dist = s.int_tag("NM")?;
            let _size = s.match_size(use_newer).ok()?;
            Some((dist, s.clone()))
        })
        .collect();
    if extracted.is_empty() {
        return group;
    }
    let min_dist = extracted.iter().map(|(d, _)| *d).min().unwrap();
    extracted
        .into_iter()
        .filter(|(d, _)| *d <= min_dist)
        .map(|(_, s)| s)
        .collect()
}

/// What to do with a record that fails the `.filter(...)` test (mirrors `FilterAction`).
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum FilterAction {
    Drop,
    Unmatch,
}

/// The options of a `.filter(...)` method call (mirrors `SelectGroupOptions`).
#[derive(Clone, Copy)]
pub struct FilterOptions {
    /// Minimum match identity as a fraction (`min_identity_pc / 100`); negative disables.
    pub min_id: f64,
    /// Minimum match size; `-1` disables.
    pub min_match_size: i64,
    /// Maximum trim; `-1` disables.
    pub max_trim: i64,
    pub reverse: bool,
    pub action: FilterAction,
    /// `__version11_or_higher`: count insertions in match sizes.
    pub use_newer: bool,
}

/// Apply a `.filter(...)` to a group of records (mirrors `applySelect`).
pub fn apply_filter(opts: &FilterOptions, group: Vec<SamLine>) -> Vec<SamLine> {
    if opts.max_trim == -1 {
        apply_filter_no_trim(opts, group)
    } else {
        // First apply the identity/size tests, then the per-mate trim test.
        let no_trim_opts = FilterOptions {
            max_trim: -1,
            ..*opts
        };
        let filtered = apply_filter_no_trim(&no_trim_opts, group);
        let (g1, g2, gs) = split3(&filtered);
        let s1 = seq_size(&g1);
        let s2 = seq_size(&g2);
        let ss = seq_size(&gs);
        let ok_trim = |seqlen: i64, s: &SamLine| -> bool {
            seqlen - s.match_size(opts.use_newer).unwrap_or(0) <= opts.max_trim
        };
        match opts.action {
            FilterAction::Drop => {
                let mut out = Vec::new();
                out.extend(g1.into_iter().filter(|s| ok_trim(s1, s)));
                out.extend(g2.into_iter().filter(|s| ok_trim(s2, s)));
                out.extend(gs.into_iter().filter(|s| ok_trim(ss, s)));
                out
            }
            FilterAction::Unmatch => {
                let mut out = Vec::new();
                out.extend(g1.into_iter().map(|s| unmatch_unless(ok_trim(s1, &s), s)));
                out.extend(g2.into_iter().map(|s| unmatch_unless(ok_trim(s2, &s), s)));
                out.extend(gs.into_iter().map(|s| unmatch_unless(ok_trim(ss, &s), s)));
                out
            }
        }
    }
}

fn apply_filter_no_trim(opts: &FilterOptions, group: Vec<SamLine>) -> Vec<SamLine> {
    let raw_test = |s: &SamLine| -> bool {
        let ok_id =
            opts.min_id < 0.0 || s.match_identity(opts.use_newer).unwrap_or(0.0) >= opts.min_id;
        let ok_size = opts.min_match_size == -1
            || s.match_size(opts.use_newer).unwrap_or(0) >= opts.min_match_size;
        ok_id && ok_size
    };
    let pass = |s: &SamLine| -> bool {
        if opts.reverse {
            !raw_test(s)
        } else {
            raw_test(s)
        }
    };
    match opts.action {
        FilterAction::Drop => group.into_iter().filter(&pass).collect(),
        FilterAction::Unmatch => group
            .into_iter()
            .map(|s| unmatch_unless(pass(&s), s))
            .collect(),
    }
}

fn unmatch_unless(cond: bool, s: SamLine) -> SamLine {
    if cond {
        s
    } else {
        unmatch(s)
    }
}

/// `unmatch`: mark a record unmapped — set the unmapped bit, clear the proper-pair bit, and blank
/// the reference name and CIGAR.
fn unmatch(mut s: SamLine) -> SamLine {
    s.flag = (s.flag | 0x4) & !0x2;
    s.rname = "*".to_string();
    s.cigar = "*".to_string();
    s
}

fn seq_size(g: &[SamLine]) -> i64 {
    g.iter().map(|s| s.seq.len() as i64).max().unwrap_or(0)
}

/// Split a group into (first-in-pair, second-in-pair, other), preserving order within each part
/// (mirrors `splitSamlines3`).
pub fn split3(lines: &[SamLine]) -> (Vec<SamLine>, Vec<SamLine>, Vec<SamLine>) {
    let mut g1 = Vec::new();
    let mut g2 = Vec::new();
    let mut gs = Vec::new();
    for s in lines {
        if s.is_first_in_pair() {
            g1.push(s.clone());
        } else if s.is_second_in_pair() {
            g2.push(s.clone());
        } else {
            gs.push(s.clone());
        }
    }
    (g1, g2, gs)
}

/// Whether a filtered group needs sequence reinjection: some mate-bucket is non-empty but has no
/// record carrying sequence (mirrors `needsReinject`).
pub fn needs_reinject(filtered: &[SamLine]) -> bool {
    let (f1, f2, f0) = split3(filtered);
    needs_reinject1(&f1) || needs_reinject1(&f2) || needs_reinject1(&f0)
}

fn needs_reinject1(xs: &[SamLine]) -> bool {
    !xs.is_empty() && !xs.iter().any(|s| s.has_sequence())
}

/// Reinject sequence/quality (and fix the CIGAR) into a filtered group whose kept records have
/// lost their sequence, taking it from the matching original records (mirrors
/// `reinjectSequences`).
pub fn reinject_sequences(original: &[SamLine], filtered: &[SamLine]) -> NgResult<Vec<SamLine>> {
    let (o1, o2, os) = split3(original);
    let (f1, f2, fs) = split3(filtered);
    let mut out = reinject_one(&o1, f1)?;
    out.extend(reinject_one(&o2, f2)?);
    out.extend(reinject_one(&os, fs)?);
    Ok(out)
}

fn reinject_one(original: &[SamLine], filtered: Vec<SamLine>) -> NgResult<Vec<SamLine>> {
    // No reinjection needed when the group is empty or already carries sequence.
    if filtered.is_empty() || filtered.iter().any(|s| s.has_sequence()) {
        return Ok(filtered);
    }
    let s = filtered[0].clone();
    let rs = &filtered[1..];
    // Try each original record that has sequence; take the first whose CIGAR can be fixed.
    for s_prime in original.iter().filter(|x| x.has_sequence()) {
        if let Ok(cigar) = fix_cigar(&s.cigar, s_prime.sam_length()) {
            let mut newfirst = s.clone();
            newfirst.seq = s_prime.seq.clone();
            newfirst.qual = s_prime.qual.clone();
            newfirst.cigar = cigar;
            let mut result = vec![newfirst];
            result.extend_from_slice(rs);
            return Ok(result);
        }
    }
    // `asum' [] = return f`: no candidate worked, leave the group unchanged.
    Ok(filtered)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::sam::parse_sam_line;

    fn line(s: &str) -> SamLine {
        parse_sam_line(s).unwrap()
    }

    #[test]
    fn keep_mapped_filters_unaligned() {
        let g = vec![
            (
                line("a\t0\tg\t1\t0\t10M\t=\t1\t0\tAAAAAAAAAA\tIIIIIIIIII\tNM:i:0"),
                (),
            ),
            (
                line("a\t4\t*\t0\t0\t*\t*\t0\t0\tAAAAAAAAAA\tIIIIIIIIII\tNM:i:0"),
                (),
            ),
        ];
        let cond = parse_conditions(&["mapped".into()], &[]).unwrap();
        let out = apply_conditions(&cond, g);
        assert_eq!(out.len(), 1);
        assert!(out[0].0.is_aligned());
    }

    #[test]
    fn filter_by_identity_and_size() {
        let opts = FilterOptions {
            min_id: 0.97,
            min_match_size: -1,
            max_trim: -1,
            reverse: false,
            action: FilterAction::Drop,
            use_newer: false,
        };
        let keep = line("a\t0\tg\t1\t0\t124M\t=\t1\t0\tA\tI\tNM:i:1");
        let dropped = line("a\t0\t*\t0\t0\t*\t*\t0\t0\tA\tI\tNM:i:0"); // cigar * -> id 0
        let out = apply_filter(&opts, vec![keep.clone(), dropped]);
        assert_eq!(out, vec![keep]);
    }

    #[test]
    fn reinject_fixes_hard_clip() {
        // original: first-in-pair has the sequence (44M23S, 67bp); kept line lost it (19H48M).
        let seq = "A".repeat(67);
        let qual = "H".repeat(67);
        let orig = vec![
            line(&format!(
                "a\t73\tg\t1\t0\t44M23S\t=\t1\t0\t{seq}\t{qual}\tNM:i:0"
            )),
            line("a\t73\tg2\t11\t0\t19H48M\t=\t11\t0\t*\t*\tNM:i:1"),
        ];
        let filtered = vec![orig[1].clone()];
        let out = reinject_sequences(&orig, &filtered).unwrap();
        assert_eq!(out.len(), 1);
        assert_eq!(out[0].cigar, "19S48M");
        assert!(out[0].has_sequence());
    }
}
