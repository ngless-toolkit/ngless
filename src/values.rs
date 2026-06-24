//! Runtime values and pure evaluation helpers, mirroring the relevant parts of
//! `NGLess/Language.hs` (`NGLessObject`, `evalBinary`) and `NGLess/Interpret.hs`
//! (`_evalUnary`, `_evalIndex`).
//!
//! Only the subset needed by the current interpreter is present: scalars, symbols, filenames,
//! void and lists. The sequence/mapping/counts values arrive with their subsystems in later
//! milestones.

use crate::ast::BOp;
use crate::errors::{NgError, NgResult};
use crate::fastq::{ReadSet, ShortRead};
use crate::sam::SamLine;

#[derive(Clone, Debug, PartialEq)]
pub enum NGLessObject {
    String(String),
    Bool(bool),
    Integer(i64),
    Double(f64),
    Symbol(String),
    Filename(String),
    Void,
    List(Vec<NGLessObject>),
    Read(ShortRead),
    /// A file-backed read set (see [`ReadSet`]). `name` is the user-facing name (the original
    /// file path or a `group`/`paired` name); the data lives in the referenced files on disk.
    ReadSet {
        name: String,
        readset: ReadSet,
    },
    /// A counts/statistics table backed by a TSV file on disk (mirrors `NGOCounts (File ...)`),
    /// e.g. the result of `qcstats`. `write` copies the file to the output.
    Counts(std::path::PathBuf),
    /// A mapped read set backed by a SAM/BAM file on disk (mirrors `NGOMappedReadSet`), e.g. the
    /// result of `samfile`. `name` is the user-facing group name.
    MappedReadSet {
        name: String,
        path: std::path::PathBuf,
    },
    /// One group of SAM alignment records sharing a read name (mirrors `NGOMappedRead`). This is
    /// the value bound to the block variable of `select(...) using |mr|:`.
    MappedRead(Vec<SamLine>),
}

impl NGLessObject {
    fn type_name(&self) -> &'static str {
        match self {
            NGLessObject::String(_) => "string",
            NGLessObject::Bool(_) => "bool",
            NGLessObject::Integer(_) => "integer",
            NGLessObject::Double(_) => "double",
            NGLessObject::Symbol(_) => "symbol",
            NGLessObject::Filename(_) => "filename",
            NGLessObject::Void => "void",
            NGLessObject::List(_) => "list",
            NGLessObject::Read(_) => "read",
            NGLessObject::ReadSet { .. } => "readset",
            NGLessObject::Counts(_) => "counts",
            NGLessObject::MappedReadSet { .. } => "mappedreadset",
            NGLessObject::MappedRead(_) => "mappedread",
        }
    }
}

/// Format a double exactly the way Haskell's `show` does (`showFloat`/`formatRealFloat
/// FFGeneric`): a fixed-point layout for exponents in `0..=7`, scientific notation otherwise,
/// always with a fractional part (e.g. `1.0`, `36.0`, `3.896103896103896e-2`). The shortest
/// round-tripping digits come from Rust's `{:e}` formatting, which (like Haskell's
/// `floatToDigits`) yields the unique minimal digit sequence.
pub fn show_double(x: f64) -> String {
    if x.is_nan() {
        return "NaN".to_string();
    }
    if x.is_infinite() {
        return if x < 0.0 { "-Infinity" } else { "Infinity" }.to_string();
    }
    if x == 0.0 {
        return if x.is_sign_negative() { "-0.0" } else { "0.0" }.to_string();
    }
    if x < 0.0 {
        return format!("-{}", show_double(-x));
    }
    // `{:e}` gives `d[.ddd]e±X`, i.e. value = mantissa × 10^X with a single leading digit.
    let s = format!("{x:e}");
    let (mantissa, exp_str) = s.split_once('e').expect("{:e} always contains 'e'");
    let exp_x: i32 = exp_str.parse().expect("valid exponent");
    let ds: Vec<char> = mantissa.chars().filter(|c| *c != '.').collect();
    // floatToDigits exponent: value = 0.d1d2… × 10^e, so e = X + 1.
    let e = exp_x + 1;
    if !(0..=7).contains(&e) {
        // Scientific: d.rest e(e-1), with `.0` when there is no remaining digit.
        let d = ds[0];
        let rest: String = ds[1..].iter().collect();
        let rest = if rest.is_empty() {
            "0".to_string()
        } else {
            rest
        };
        format!("{d}.{rest}e{}", e - 1)
    } else if e <= 0 {
        let zeros = "0".repeat((-e) as usize);
        let digits: String = ds.iter().collect();
        format!("0.{zeros}{digits}")
    } else {
        let e = e as usize;
        let mut ds = ds;
        while ds.len() < e {
            ds.push('0');
        }
        let before: String = ds[..e].iter().collect();
        let after: String = ds[e..].iter().collect();
        let after = if after.is_empty() {
            "0".to_string()
        } else {
            after
        };
        format!("{before}.{after}")
    }
}

fn as_double(v: &NGLessObject) -> NgResult<f64> {
    match v {
        NGLessObject::Double(d) => Ok(*d),
        NGLessObject::Integer(i) => Ok(*i as f64),
        other => Err(NgError::script(format!(
            "Expected numeric value, got: {}",
            other.type_name()
        ))),
    }
}

/// Join two paths the way `System.FilePath.</>` does (a simplified version).
fn path_append(a: &str, b: &str) -> String {
    if b.starts_with('/') {
        b.to_string()
    } else if a.is_empty() || a.ends_with('/') {
        format!("{a}{b}")
    } else {
        format!("{a}/{b}")
    }
}

/// Evaluate a binary operator, mirroring `evalBinary` in Language.hs.
pub fn eval_binary(op: BOp, a: &NGLessObject, b: &NGLessObject) -> NgResult<NGLessObject> {
    use NGLessObject::*;
    match op {
        BOp::Add => match (a, b) {
            (Integer(x), Integer(y)) => Ok(Integer(x + y)),
            (String(x), String(y)) => Ok(String(format!("{x}{y}"))),
            _ => Ok(Double(as_double(a)? + as_double(b)?)),
        },
        BOp::Mul => match (a, b) {
            (Integer(x), Integer(y)) => Ok(Integer(x * y)),
            _ => Ok(Double(as_double(a)? * as_double(b)?)),
        },
        BOp::PathAppend => match (a, b) {
            (String(x), String(y)) => Ok(String(path_append(x, y))),
            _ => Err(NgError::should_not_occur("Operator </>: invalid arguments")),
        },
        BOp::EQ => match (a, b) {
            (String(x), String(y)) => Ok(Bool(x == y)),
            _ => Ok(Bool(cmp(op, as_double(a)?, as_double(b)?))),
        },
        BOp::NEQ => match (a, b) {
            (String(x), String(y)) => Ok(Bool(x != y)),
            _ => Ok(Bool(cmp(op, as_double(a)?, as_double(b)?))),
        },
        BOp::LT | BOp::GT | BOp::LTE | BOp::GTE => Ok(Bool(cmp(op, as_double(a)?, as_double(b)?))),
    }
}

fn cmp(op: BOp, a: f64, b: f64) -> bool {
    match op {
        BOp::LT => a < b,
        BOp::GT => a > b,
        BOp::LTE => a <= b,
        BOp::GTE => a >= b,
        BOp::EQ => a == b,
        BOp::NEQ => a != b,
        _ => unreachable!("cmp called with non-comparison operator"),
    }
}

/// Evaluate a unary operator, mirroring `_evalUnary`.
pub fn eval_unary(op: crate::ast::UOp, v: &NGLessObject) -> NgResult<NGLessObject> {
    use crate::ast::UOp;
    use NGLessObject::*;
    match (op, v) {
        (UOp::Minus, Integer(n)) => Ok(Integer(-n)),
        (UOp::Len, List(elems)) => Ok(Integer(elems.len() as i64)),
        (UOp::Len, Read(r)) => Ok(Integer(r.len() as i64)),
        (UOp::Not, Bool(b)) => Ok(Bool(!b)),
        _ => Err(NgError::script(format!(
            "invalid unary operation ({op:?}) on value of type {}",
            v.type_name()
        ))),
    }
}

/// Evaluate an index expression, mirroring `_evalIndex`. `indices` is the already-evaluated
/// index (one element for `[i]`, two for `[a:b]`).
pub fn eval_index(v: &NGLessObject, indices: &[Option<NGLessObject>]) -> NgResult<NGLessObject> {
    use NGLessObject::*;
    match (v, indices) {
        (List(elems), [Some(Integer(ix))]) => {
            let ix = *ix;
            if ix < 0 || ix as usize >= elems.len() {
                Err(NgError::script(format!(
                    "Accessing element {ix} in list of size {}.",
                    elems.len()
                )))
            } else {
                Ok(elems[ix as usize].clone())
            }
        }
        // Read slicing, mirroring the IndexTwo cases of `_evalIndex`.
        (Read(r), [Some(Integer(s)), None]) => read_slice(r, *s, r.len() as i64),
        (Read(r), [None, Some(Integer(e))]) => read_slice(r, 0, *e),
        (Read(r), [Some(Integer(s)), Some(Integer(e))]) => read_slice(r, *s, *e),
        _ => Err(NgError::script("_evalIndex: invalid operation")),
    }
}

/// Slice a read for `read[s:e]` (half-open), with bounds checking.
fn read_slice(r: &ShortRead, s: i64, e: i64) -> NgResult<NGLessObject> {
    if s < 0 || e < s || e as usize > r.len() {
        return Err(NgError::script(format!(
            "Invalid slice [{s}:{e}] of read of length {}.",
            r.len()
        )));
    }
    Ok(NGLessObject::Read(r.slice(s as usize, (e - s) as usize)))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{BOp, UOp};
    use NGLessObject::*;

    #[test]
    fn add_integers_and_strings() {
        assert_eq!(
            eval_binary(BOp::Add, &Integer(40), &Integer(2)).unwrap(),
            Integer(42)
        );
        assert_eq!(
            eval_binary(BOp::Add, &String("a".into()), &String("b".into())).unwrap(),
            String("ab".into())
        );
    }

    #[test]
    fn comparisons() {
        assert_eq!(
            eval_binary(BOp::LT, &Integer(1), &Integer(2)).unwrap(),
            Bool(true)
        );
        assert_eq!(
            eval_binary(BOp::EQ, &String("x".into()), &String("x".into())).unwrap(),
            Bool(true)
        );
        assert_eq!(
            eval_binary(BOp::GTE, &Double(2.0), &Integer(2)).unwrap(),
            Bool(true)
        );
    }

    #[test]
    fn unary_ops() {
        assert_eq!(eval_unary(UOp::Minus, &Integer(5)).unwrap(), Integer(-5));
        assert_eq!(eval_unary(UOp::Not, &Bool(false)).unwrap(), Bool(true));
        assert_eq!(
            eval_unary(UOp::Len, &List(vec![Integer(1), Integer(2)])).unwrap(),
            Integer(2)
        );
    }

    #[test]
    fn index_list() {
        let l = List(vec![Integer(10), Integer(20)]);
        assert_eq!(eval_index(&l, &[Some(Integer(1))]).unwrap(), Integer(20));
        assert!(eval_index(&l, &[Some(Integer(5))]).is_err());
    }

    #[test]
    fn double_formatting() {
        // Matches Haskell's `show` for Double, including its scientific-notation threshold.
        assert_eq!(show_double(1.0), "1.0");
        assert_eq!(show_double(1.5), "1.5");
        assert_eq!(show_double(36.0), "36.0");
        assert_eq!(show_double(0.0), "0.0");
        assert_eq!(show_double(29.0 / 74.0), "0.3918918918918919");
        // Magnitude < 0.1 switches to scientific notation, like Haskell.
        assert_eq!(show_double(3.0 / 77.0), "3.896103896103896e-2");
        assert_eq!(show_double(0.005), "5.0e-3");
        assert_eq!(show_double(7.0e7), "7.0e7");
        assert_eq!(show_double(7_000_000.0), "7000000.0");
        assert_eq!(show_double(-1.5), "-1.5");
    }
}
