//! Interpreter for the currently-supported subset of NGLess, mirroring `NGLess/Interpret.hs`.
//!
//! Implemented: pure expression evaluation (constants, variables, operators, indexing, lists),
//! conditionals and assignment; the simple builtins `print`, `println`, `read_int`,
//! `read_double`, `__assert`, the `to_string`/`avg_quality`/`fraction_at_least`/
//! `n_to_zero_quality` methods; and a file-backed FASTQ path — `fastq` (reference a file with
//! its detected encoding), `preprocess(...) using |read|:` blocks (with
//! `substrim`/`endstrim`/`smoothtrim`, read slicing, `len`, `discard`/`continue`), and `write`
//! (copy the read set's current file to the output).
//!
//! Read sets are file-backed (see [`crate::fastq::ReadSet`]): `fastq` keeps the original file,
//! `preprocess` streams it to a fresh temp file, and `write` copies the current file. This is
//! what makes `write` of an un-preprocessed read set byte-identical to its input.
//!
//! Compressed I/O is transparent for gzip (see [`crate::compression`]); bzip2/zstd are not
//! handled yet. Other simplifications vs. the Haskell runtime, to be lifted in later
//! milestones: files are read whole rather than streamed (no FileOrStream/bounded queues), and
//! only single-end read sets are supported (no paired ends).

use std::collections::HashMap;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};

use crate::ast::*;
use crate::errors::{NgError, NgErrorType, NgResult};
use crate::fastq::{self, FastQFilePath, ReadSet, ShortRead};
use crate::values::{eval_binary, eval_index, eval_unary, show_double, NGLessObject};

/// Interpret a script body (already type-checked and validated). `temp_dir` is where
/// intermediate FASTQ files (e.g. from `preprocess`) are written.
pub fn interpret(body: &[(usize, Expression)], temp_dir: &Path) -> NgResult<()> {
    let mut interp = Interpreter {
        env: HashMap::new(),
        temp_dir: temp_dir.to_path_buf(),
    };
    for (_lno, e) in body {
        interp.interpret_top(e)?;
    }
    Ok(())
}

struct Interpreter {
    env: HashMap<String, NGLessObject>,
    temp_dir: PathBuf,
}

/// The outcome of running a (block) statement, mirroring `BlockStatus`.
#[derive(Clone, Copy, PartialEq, Eq)]
enum BlockStatus {
    Ok,
    Discarded,
    Continued,
}

impl Interpreter {
    fn interpret_top(&mut self, e: &Expression) -> NgResult<()> {
        match e {
            Expression::Assignment(Variable(var), val) => {
                let v = self.interpret_top_value(val)?;
                self.env.insert(var.clone(), v);
                Ok(())
            }
            Expression::FunctionCall(f, expr, args, block) => {
                self.interpret_function(f, expr, args, block.as_ref())?;
                Ok(())
            }
            Expression::Condition(c, t_branch, f_branch) => {
                let cond = self.interpret_expr(c)?;
                match cond {
                    NGLessObject::Bool(true) => self.interpret_top(t_branch),
                    NGLessObject::Bool(false) => self.interpret_top(f_branch),
                    other => Err(NgError::script(format!(
                        "interpreting if condition: expected a boolean, got {other:?}"
                    ))),
                }
            }
            Expression::Sequence(es) => {
                for e in es {
                    self.interpret_top(e)?;
                }
                Ok(())
            }
            other => Err(NgError::should_not_occur(format!(
                "Unexpected top level statement: {other:?}"
            ))),
        }
    }

    /// Evaluate an expression that may be a (possibly side-effecting) function call.
    fn interpret_top_value(&mut self, e: &Expression) -> NgResult<NGLessObject> {
        match e {
            Expression::FunctionCall(f, expr, args, block) => {
                self.interpret_function(f, expr, args, block.as_ref())
            }
            Expression::ListExpression(es) => {
                let mut out = Vec::with_capacity(es.len());
                for e in es {
                    out.push(self.interpret_top_value(e)?);
                }
                Ok(NGLessObject::List(out))
            }
            _ => self.interpret_expr(e),
        }
    }

    /// Evaluate a pure expression. `overlay` is an optional read-write block variable that
    /// shadows the (read-only) global environment, used while interpreting a block body.
    fn eval(
        &self,
        e: &Expression,
        overlay: Option<(&str, &NGLessObject)>,
    ) -> NgResult<NGLessObject> {
        match e {
            Expression::Lookup(_, Variable(v)) => {
                if let Some((name, value)) = overlay {
                    if name == v {
                        return Ok(value.clone());
                    }
                }
                self.env.get(v).cloned().ok_or_else(|| {
                    NgError::should_not_occur(format!("Variable lookup error. Variable: {v}"))
                })
            }
            Expression::BuiltinConstant(Variable(v)) => match v.as_str() {
                "STDIN" => Ok(NGLessObject::String("/dev/stdin".into())),
                "STDOUT" => Ok(NGLessObject::String("/dev/stdout".into())),
                "__VOID" => Ok(NGLessObject::Void),
                other => Err(NgError::should_not_occur(format!(
                    "Unknown builtin constant '{other}': it should not have been accepted."
                ))),
            },
            Expression::ConstStr(s) => Ok(NGLessObject::String(s.clone())),
            Expression::ConstBool(b) => Ok(NGLessObject::Bool(*b)),
            Expression::ConstSymbol(s) => Ok(NGLessObject::Symbol(s.clone())),
            Expression::ConstInt(n) => Ok(NGLessObject::Integer(*n)),
            Expression::ConstDouble(n) => Ok(NGLessObject::Double(*n)),
            Expression::UnaryOp(op, v) => {
                let v = self.eval(v, overlay)?;
                eval_unary(*op, &v)
            }
            Expression::BinaryOp(op, a, b) => {
                let a = self.eval(a, overlay)?;
                let b = self.eval(b, overlay)?;
                eval_binary(*op, &a, &b)
            }
            Expression::IndexExpression(expr, ix) => {
                let v = self.eval(expr, overlay)?;
                let indices = self.interpret_index(ix, overlay)?;
                eval_index(&v, &indices)
            }
            Expression::ListExpression(es) => {
                let mut out = Vec::with_capacity(es.len());
                for e in es {
                    out.push(self.eval(e, overlay)?);
                }
                Ok(NGLessObject::List(out))
            }
            Expression::MethodCall(met, self_e, arg, args) => {
                let self_v = self.eval(self_e, overlay)?;
                let arg_v = match arg {
                    Some(a) => Some(self.eval(a, overlay)?),
                    None => None,
                };
                let mut argvs = Vec::new();
                for (Variable(v), e) in args {
                    argvs.push((v.clone(), self.eval(e, overlay)?));
                }
                execute_method(met, &self_v, arg_v.as_ref(), &argvs)
            }
            other => Err(NgError::should_not_occur(format!(
                "Expected an expression, received {other:?} (in eval)"
            ))),
        }
    }

    fn interpret_expr(&self, e: &Expression) -> NgResult<NGLessObject> {
        self.eval(e, None)
    }

    fn interpret_index(
        &self,
        ix: &Index,
        overlay: Option<(&str, &NGLessObject)>,
    ) -> NgResult<Vec<Option<NGLessObject>>> {
        match ix {
            Index::One(a) => Ok(vec![Some(self.eval(a, overlay)?)]),
            Index::Two(a, b) => {
                let a = match a {
                    Some(a) => Some(self.eval(a, overlay)?),
                    None => None,
                };
                let b = match b {
                    Some(b) => Some(self.eval(b, overlay)?),
                    None => None,
                };
                Ok(vec![a, b])
            }
        }
    }

    fn interpret_function(
        &mut self,
        f: &FuncName,
        expr: &Expression,
        args: &[(Variable, Expression)],
        block: Option<&Block>,
    ) -> NgResult<NGLessObject> {
        if f.0 == "preprocess" {
            return match block {
                Some(b) => {
                    let readset = self.interpret_expr(expr)?;
                    self.execute_preprocess(&readset, b)
                }
                None => Err(NgError::script("preprocess requires a block")),
            };
        }
        if block.is_some() {
            return Err(NgError::script(format!(
                "Function '{}' with a block is not implemented in this build yet.",
                f.0
            )));
        }
        let expr_v = self.interpret_top_value(expr)?;
        let mut argvs = Vec::new();
        for (Variable(v), e) in args {
            argvs.push((v.clone(), self.interpret_expr(e)?));
        }
        // Functions that touch the filesystem need interpreter state (the temp dir);
        // the rest are pure and handled by the free `execute_function`.
        match f.0.as_str() {
            "fastq" => self.execute_fastq(&expr_v),
            "write" => execute_write(&expr_v, &argvs),
            _ => execute_function(f, &expr_v, &argvs),
        }
    }

    /// Allocate a fresh temp file path under the configured temp directory. The counter is
    /// process-global so paths stay unique across interpreter instances sharing a temp dir.
    fn new_temp_path(&self, prefix: &str, ext: &str) -> PathBuf {
        static COUNTER: AtomicU64 = AtomicU64::new(0);
        let n = COUNTER.fetch_add(1, Ordering::Relaxed);
        self.temp_dir
            .join(format!("{prefix}{}.{n}.{ext}", std::process::id()))
    }

    // --- fastq / preprocess --------------------------------------------------

    /// `fastq(fname)`: reference the input file with its detected (or given) encoding. The file
    /// is *not* rewritten, so a later `write` reproduces it byte-for-byte (mirrors
    /// `executeFastq` for the non-interleaved case).
    fn execute_fastq(&self, expr: &NGLessObject) -> NgResult<NGLessObject> {
        let path = as_string(expr, "fastq")?;
        let text = read_fastq_text(&path)?;
        let encoding = fastq::detect_encoding(&text)?;
        Ok(NGLessObject::ReadSet {
            name: path.clone(),
            readset: ReadSet {
                pairs: Vec::new(),
                singletons: vec![FastQFilePath {
                    encoding,
                    path: PathBuf::from(path),
                }],
            },
        })
    }

    /// `preprocess(rs) using |read|: ...`: stream each input file through the block, writing the
    /// surviving reads to a fresh temp file (mirrors `executePreprocess`). Output encoding is the
    /// common input encoding, or Sanger if the inputs disagree.
    fn execute_preprocess(&self, readset: &NGLessObject, block: &Block) -> NgResult<NGLessObject> {
        let (name, rs) = match readset {
            NGLessObject::ReadSet { name, readset } => (name.clone(), readset.clone()),
            other => {
                return Err(NgError::should_not_occur(format!(
                    "preprocess expected a read set, got {other:?}"
                )))
            }
        };
        if !rs.pairs.is_empty() {
            return Err(NgError::script(
                "preprocess of paired-end read sets is not implemented in this build yet.",
            ));
        }
        let outenc = output_encoding(&rs.singletons);
        let var = &block.variable.0;
        let mut out = String::new();
        for fqf in &rs.singletons {
            let text = read_fastq_text(&fqf.path.to_string_lossy())?;
            let reads = fastq::fq_decode(fqf.encoding, &text)?;
            for r in reads {
                let (status, value) =
                    self.interpret_block_stmt(var, NGLessObject::Read(r), &block.body)?;
                match status {
                    BlockStatus::Discarded => {}
                    BlockStatus::Ok | BlockStatus::Continued => match value {
                        NGLessObject::Read(sr) => out.push_str(&fastq::fq_encode(outenc, &sr)),
                        other => {
                            return Err(NgError::should_not_occur(format!(
                                "preprocess block produced a non-read value: {other:?}"
                            )))
                        }
                    },
                }
            }
        }
        let outfile = self.new_temp_path("preprocessed.singles.", "fq");
        std::fs::write(&outfile, out).map_err(|e| {
            NgError::new(
                NgErrorType::SystemError,
                format!("Could not write temp file {}: {e}", outfile.display()),
            )
        })?;
        Ok(NGLessObject::ReadSet {
            name,
            readset: ReadSet {
                pairs: Vec::new(),
                singletons: vec![FastQFilePath {
                    encoding: outenc,
                    path: outfile,
                }],
            },
        })
    }

    /// Interpret one block statement, mirroring `interpretBlock1`/`interpretBlock`. The block
    /// variable `name` currently holds `value`; returns the updated value and the status.
    fn interpret_block_stmt(
        &self,
        name: &str,
        value: NGLessObject,
        e: &Expression,
    ) -> NgResult<(BlockStatus, NGLessObject)> {
        match e {
            Expression::Assignment(Variable(n), val) => {
                let v = self.eval_block_expr(val, name, &value)?;
                if n != name {
                    return Err(NgError::should_not_occur(format!(
                        "only assignments to the block variable are possible [assigning to '{n}']"
                    )));
                }
                Ok((BlockStatus::Ok, v))
            }
            Expression::Discard => Ok((BlockStatus::Discarded, value)),
            Expression::Continue => Ok((BlockStatus::Continued, value)),
            Expression::Condition(c, t_branch, f_branch) => {
                let cond = match self.eval_block_expr(c, name, &value)? {
                    NGLessObject::Bool(b) => b,
                    _ => {
                        return Err(NgError::should_not_occur(
                            "Wrong type for condition (interpret_block_stmt)",
                        ))
                    }
                };
                self.interpret_block_stmt(name, value, if cond { t_branch } else { f_branch })
            }
            Expression::Sequence(es) => {
                let mut value = value;
                for e in es {
                    let (status, v) = self.interpret_block_stmt(name, value, e)?;
                    value = v;
                    if status != BlockStatus::Ok {
                        return Ok((status, value));
                    }
                }
                Ok((BlockStatus::Ok, value))
            }
            other => Err(NgError::should_not_occur(format!(
                "interpret_block_stmt: unexpected {other:?}"
            ))),
        }
    }

    /// Evaluate an expression inside a block, mirroring `interpretPreProcessExpr`: the trimming
    /// functions operate on the (read) value, everything else is ordinary evaluation with the
    /// block variable overlaid.
    fn eval_block_expr(
        &self,
        e: &Expression,
        name: &str,
        value: &NGLessObject,
    ) -> NgResult<NGLessObject> {
        let overlay = Some((name, value));
        if let Expression::FunctionCall(FuncName(fname), var, args, _) = e {
            match fname.as_str() {
                "substrim" => {
                    let r = self.eval_read(var, overlay)?;
                    let mq = self.kwarg_int(args, "min_quality", 0, overlay)?;
                    return Ok(NGLessObject::Read(fastq::substrim(mq, &r)));
                }
                "endstrim" => {
                    let r = self.eval_read(var, overlay)?;
                    let mq = self.kwarg_int(args, "min_quality", 0, overlay)?;
                    let ends = self.kwarg_symbol(args, "from_ends", "both", overlay)?;
                    let ends = match ends.as_str() {
                        "both" => fastq::EndstrimEnds::Both,
                        "3" => fastq::EndstrimEnds::Three,
                        "5" => fastq::EndstrimEnds::Five,
                        other => {
                            return Err(NgError::script(format!(
                                "Illegal argument for `from_ends`: {other}"
                            )))
                        }
                    };
                    return Ok(NGLessObject::Read(fastq::endstrim(ends, mq, &r)));
                }
                "smoothtrim" => {
                    let r = self.eval_read(var, overlay)?;
                    let mq = self.kwarg_int(args, "min_quality", 0, overlay)?;
                    let w = self.kwarg_int(args, "window", 0, overlay)?;
                    return Ok(NGLessObject::Read(fastq::smoothtrim(w, mq, &r)));
                }
                _ => {}
            }
        }
        self.eval(e, overlay)
    }

    fn eval_read(
        &self,
        e: &Expression,
        overlay: Option<(&str, &NGLessObject)>,
    ) -> NgResult<ShortRead> {
        match self.eval(e, overlay)? {
            NGLessObject::Read(r) => Ok(r),
            other => Err(NgError::should_not_occur(format!(
                "Expected a read, got {other:?}"
            ))),
        }
    }

    fn kwarg_int(
        &self,
        args: &[(Variable, Expression)],
        name: &str,
        default: i32,
        overlay: Option<(&str, &NGLessObject)>,
    ) -> NgResult<i32> {
        for (Variable(v), e) in args {
            if v == name {
                return match self.eval(e, overlay)? {
                    NGLessObject::Integer(i) => Ok(i as i32),
                    other => Err(NgError::script(format!(
                        "Argument `{name}` should be an integer, got {other:?}"
                    ))),
                };
            }
        }
        Ok(default)
    }

    fn kwarg_symbol(
        &self,
        args: &[(Variable, Expression)],
        name: &str,
        default: &str,
        overlay: Option<(&str, &NGLessObject)>,
    ) -> NgResult<String> {
        for (Variable(v), e) in args {
            if v == name {
                return match self.eval(e, overlay)? {
                    NGLessObject::Symbol(s) => Ok(s),
                    other => Err(NgError::script(format!(
                        "Argument `{name}` should be a symbol, got {other:?}"
                    ))),
                };
            }
        }
        Ok(default.to_string())
    }
}

fn execute_function(
    f: &FuncName,
    expr: &NGLessObject,
    args: &[(String, NGLessObject)],
) -> NgResult<NGLessObject> {
    match f.0.as_str() {
        "print" => execute_print(expr),
        "println" => {
            execute_print(expr)?;
            execute_print(&NGLessObject::String("\n".into()))
        }
        "read_int" => execute_read_int(expr, args),
        "read_double" => execute_read_double(expr, args),
        "__assert" => match expr {
            NGLessObject::Bool(true) => Ok(NGLessObject::Void),
            NGLessObject::Bool(false) => Err(NgError::script("Assert failed")),
            _ => Err(NgError::should_not_occur(
                "Assert did not receive a boolean!",
            )),
        },
        other => Err(NgError::script(format!(
            "Interpretation of function `{other}` is not implemented in this build yet."
        ))),
    }
}

/// Common encoding of a list of files: the shared encoding if they all agree, else Sanger
/// (mirrors the `outenc` computation in `executePreprocess`).
fn output_encoding(files: &[FastQFilePath]) -> fastq::FastQEncoding {
    match files.split_first() {
        None => fastq::FastQEncoding::Sanger,
        Some((first, rest)) if rest.iter().all(|f| f.encoding == first.encoding) => first.encoding,
        _ => fastq::FastQEncoding::Sanger,
    }
}

/// Read a (possibly compressed) FASTQ file into memory.
fn read_fastq_text(path: &str) -> NgResult<String> {
    crate::compression::read_to_string(path)
}

/// `write(rs, ofile=...)`: copy the read set's current file(s) to `ofile`. For an
/// un-preprocessed read set this reproduces the input byte-for-byte (mirrors `executeWrite`
/// of an `NGOReadSet`, which `moveOrCopyCompress`es the underlying FASTQ files).
fn execute_write(expr: &NGLessObject, args: &[(String, NGLessObject)]) -> NgResult<NGLessObject> {
    let ofile = match lookup_arg(args, "ofile") {
        Some(NGLessObject::String(s)) => s.clone(),
        _ => {
            return Err(NgError::script(
                "write: argument `ofile` (a string) is required",
            ))
        }
    };
    match expr {
        NGLessObject::ReadSet { readset, .. } => {
            if !readset.pairs.is_empty() {
                return Err(NgError::script(
                    "write of paired-end read sets is not implemented in this build yet.",
                ));
            }
            match readset.singletons.as_slice() {
                [single] => {
                    copy_fastq(&single.path, &ofile)?;
                    Ok(NGLessObject::String(ofile))
                }
                [] => Err(NgError::script(
                    "write: the read set has no files to write.",
                )),
                _ => Err(NgError::script(
                    "write of a multi-file read set is not implemented in this build yet.",
                )),
            }
        }
        other => Err(NgError::script(format!(
            "write of {} is not implemented in this build yet.",
            type_label(other)
        ))),
    }
}

/// Copy a FASTQ file to `ofile` (mirrors `moveOrCopyCompress`). When the source and
/// destination share a compression format the bytes are copied verbatim; otherwise the source
/// is decompressed and re-compressed to the destination format.
fn copy_fastq(src: &Path, ofile: &str) -> NgResult<()> {
    use crate::compression::{detect, read_bytes, write_bytes};
    let src_str = src.to_string_lossy();
    if detect(&src_str) == detect(ofile) {
        std::fs::copy(src, ofile).map_err(|e| {
            NgError::new(
                NgErrorType::SystemError,
                format!("Could not write {ofile}: {e}"),
            )
        })?;
    } else {
        let data = read_bytes(&src_str)?;
        write_bytes(ofile, &data)?;
    }
    Ok(())
}

fn execute_print(v: &NGLessObject) -> NgResult<NGLessObject> {
    let s = match v {
        NGLessObject::String(s) => s.clone(),
        NGLessObject::Integer(i) => i.to_string(),
        NGLessObject::Double(d) => show_double(*d),
        other => return Err(NgError::script(format!("Cannot print {other:?}"))),
    };
    let stdout = std::io::stdout();
    let mut h = stdout.lock();
    h.write_all(s.as_bytes())
        .map_err(|e| NgError::new(NgErrorType::SystemError, e.to_string()))?;
    Ok(NGLessObject::Void)
}

fn execute_read_int(v: &NGLessObject, args: &[(String, NGLessObject)]) -> NgResult<NGLessObject> {
    match v {
        NGLessObject::String(s) if s.is_empty() => match lookup_arg(args, "on_empty_return") {
            Some(NGLessObject::Integer(i)) => Ok(NGLessObject::Integer(*i)),
            _ => Err(NgError::script(
                "read_int: argument `on_empty_return` (an integer) is required for empty input",
            )),
        },
        NGLessObject::String(s) => {
            s.trim()
                .parse::<i64>()
                .map(NGLessObject::Integer)
                .map_err(|e| {
                    NgError::new(
                        NgErrorType::DataError,
                        format!("Could not parse integer from '{s}'. Error: {e}"),
                    )
                })
        }
        other => Err(NgError::script(format!(
            "Cannot parse this object as integer: {other:?}"
        ))),
    }
}

fn execute_read_double(
    v: &NGLessObject,
    args: &[(String, NGLessObject)],
) -> NgResult<NGLessObject> {
    match v {
        NGLessObject::String(s) if s.is_empty() => match lookup_arg(args, "on_empty_return") {
            Some(NGLessObject::Double(d)) => Ok(NGLessObject::Double(*d)),
            Some(NGLessObject::Integer(i)) => Ok(NGLessObject::Double(*i as f64)),
            _ => Err(NgError::script(
                "read_double: argument `on_empty_return` (a double) is required for empty input",
            )),
        },
        NGLessObject::String(s) => s
            .trim()
            .parse::<f64>()
            .map(NGLessObject::Double)
            .map_err(|e| {
                NgError::new(
                    NgErrorType::DataError,
                    format!("Could not parse double from '{s}'. Error: {e}"),
                )
            }),
        other => Err(NgError::script(format!(
            "Cannot parse this object as double: {other:?}"
        ))),
    }
}

fn execute_method(
    met: &MethodName,
    self_v: &NGLessObject,
    _arg: Option<&NGLessObject>,
    _args: &[(String, NGLessObject)],
) -> NgResult<NGLessObject> {
    match (met.0.as_str(), self_v) {
        ("to_string", NGLessObject::Double(d)) => Ok(NGLessObject::String(show_double(*d))),
        ("to_string", NGLessObject::Integer(i)) => Ok(NGLessObject::String(i.to_string())),
        ("avg_quality", NGLessObject::Read(r)) => Ok(NGLessObject::Double(r.avg_quality())),
        ("n_to_zero_quality", NGLessObject::Read(r)) => {
            Ok(NGLessObject::Read(r.n_to_zero_quality()))
        }
        ("fraction_at_least", NGLessObject::Read(r)) => match _arg {
            Some(NGLessObject::Integer(minq)) => {
                Ok(NGLessObject::Double(r.fraction_at_least(*minq)))
            }
            _ => Err(NgError::script(
                "fraction_at_least requires an integer argument",
            )),
        },
        (other, _) => Err(NgError::script(format!(
            "Method `{other}` is not implemented in this build yet."
        ))),
    }
}

fn lookup_arg<'a>(args: &'a [(String, NGLessObject)], name: &str) -> Option<&'a NGLessObject> {
    args.iter().find(|(k, _)| k == name).map(|(_, v)| v)
}

fn as_string(v: &NGLessObject, who: &str) -> NgResult<String> {
    match v {
        NGLessObject::String(s) => Ok(s.clone()),
        NGLessObject::Filename(s) => Ok(s.clone()),
        other => Err(NgError::script(format!(
            "{who}: expected a string, got {other:?}"
        ))),
    }
}

fn type_label(v: &NGLessObject) -> &'static str {
    match v {
        NGLessObject::String(_) => "a string",
        NGLessObject::ReadSet { .. } => "a read set",
        _ => "this value",
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_ngless;

    fn run(text: &str) -> NgResult<()> {
        let script = parse_ngless("test", true, text).expect("parse failed");
        interpret(&script.body, &std::env::temp_dir())
    }

    #[test]
    fn arithmetic_and_assert_pass() {
        // NGLess operators have no precedence and are right-associative, so the addition must
        // be parenthesised: `(40 + 2) == 42`, not `40 + 2 == 42`.
        run("ngless '1.5'\n__assert((40 + 2) == 42)\n__assert(1 < 2)\n__assert(not false)\n")
            .unwrap();
    }

    #[test]
    fn assert_failure_is_error() {
        assert!(run("ngless '1.5'\n__assert(1 > 2)\n").is_err());
    }

    #[test]
    fn variables_and_read_int() {
        run("ngless '1.5'\nx = read_int('7')\n__assert(x == 7)\n").unwrap();
    }

    #[test]
    fn read_int_on_empty_return() {
        run("ngless '1.5'\nx = read_int('', on_empty_return=3)\n__assert(x == 3)\n").unwrap();
    }

    #[test]
    fn list_indexing() {
        run("ngless '1.5'\nxs = [10, 20, 30]\n__assert(xs[1] == 20)\n__assert(len(xs) == 3)\n")
            .unwrap();
    }

    #[test]
    fn unimplemented_function_errors() {
        // map is not implemented yet.
        assert!(run("ngless '1.5'\nx = fastq('/nonexistent.fq')\ny = map(x)\n").is_err());
    }

    // --- FASTQ path (file-backed) -------------------------------------------

    /// A unique temp path tagged by the caller, so parallel tests never collide.
    fn unique_temp(tag: &str, ext: &str) -> std::path::PathBuf {
        use std::sync::atomic::{AtomicU64, Ordering};
        static N: AtomicU64 = AtomicU64::new(0);
        let n = N.fetch_add(1, Ordering::Relaxed);
        std::env::temp_dir().join(format!(
            "ngless_rust_{tag}_{}_{n}.{ext}",
            std::process::id()
        ))
    }

    fn write_temp_fastq(tag: &str) -> std::path::PathBuf {
        // Three reads; qualities chosen (Sanger) so substrim(min_quality=20) and the length
        // filter give predictable results.
        let p = unique_temp(tag, "fq");
        // 'I' = qual 40, '#' = qual 2 (Sanger offset 33)
        let content = "\
@r1\nACGTACGT\n+\nIIIIIIII\n\
@r2\nAAAATTTT\n+\nIIII####\n\
@r3\nGGGGCCCC\n+\n########\n";
        std::fs::write(&p, content).unwrap();
        p
    }

    #[test]
    fn fastq_preprocess_write_end_to_end() {
        let input = write_temp_fastq("ppin");
        let output = unique_temp("ppout", "fq");
        let script = format!(
            "ngless '1.5'\n\
             input = fastq('{}')\n\
             input = preprocess(input) using |read|:\n\
             \x20\x20\x20\x20read = substrim(read, min_quality=20)\n\
             \x20\x20\x20\x20if len(read) < 4:\n\
             \x20\x20\x20\x20\x20\x20\x20\x20discard\n\
             write(input, ofile='{}')\n",
            input.display(),
            output.display()
        );
        run(&script).unwrap();
        let out = std::fs::read_to_string(&output).unwrap();
        // r1 is all high quality -> kept (length 8). r2 keeps only "AAAA" (length 4) -> kept.
        // r3 is all low quality -> trimmed to length 0 -> discarded by the length filter.
        assert!(out.contains("@r1"));
        assert!(out.contains("@r2\nAAAA\n"));
        assert!(!out.contains("@r3"));
        let _ = std::fs::remove_file(&input);
        let _ = std::fs::remove_file(&output);
    }

    #[test]
    fn read_slicing_in_block() {
        let input = write_temp_fastq("slicein");
        let output = unique_temp("sliceout", "fq");
        let script = format!(
            "ngless '1.5'\n\
             input = fastq('{}')\n\
             input = preprocess(input) using |read|:\n\
             \x20\x20\x20\x20read = read[2:]\n\
             write(input, ofile='{}')\n",
            input.display(),
            output.display()
        );
        run(&script).unwrap();
        let out = std::fs::read_to_string(&output).unwrap();
        // r1 ACGTACGT sliced from index 2 -> GTACGT
        assert!(out.contains("@r1\nGTACGT\n"));
        let _ = std::fs::remove_file(&input);
        let _ = std::fs::remove_file(&output);
    }
}
