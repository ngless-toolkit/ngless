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
//! Read sets are file-backed (see [`crate::fastq::ReadSet`]): `fastq`/`paired` keep the
//! original files, `preprocess` streams them to fresh temp files, and `write` copies the
//! current files (deriving `pair.1`/`pair.2`/`singles` names for paired sets). This is what
//! makes `write` of an un-preprocessed single-end set byte-identical to its input.
//!
//! `qcstats({fastq})` produces the per-file QC statistics TSV (collected as `fastq`/`paired`/
//! `preprocess` run). Compressed I/O is transparent for gzip (see [`crate::compression`]);
//! bzip2/zstd are not handled yet. Other simplifications vs. the Haskell runtime, to be lifted
//! in later milestones: files are read whole rather than streamed (no FileOrStream/bounded
//! queues), and per-position quality percentiles are not collected.

use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};

use crate::ast::*;
use crate::errors::{NgError, NgErrorType, NgResult};
use crate::fastq::{self, FastQEncoding, FastQFilePath, ReadSet, ShortRead};
use crate::sam::{self, encode_sam_line, is_header_line, parse_sam_line, SamLine, SamRecord};
use crate::select::{self, FilterAction, FilterOptions};
use crate::values::{eval_binary, eval_index, eval_unary, show_double, NGLessObject};

/// Interpret a script body (already type-checked and validated). `temp_dir` is where
/// intermediate FASTQ files (e.g. from `preprocess`) are written.
pub fn interpret(body: &[(usize, Expression)], temp_dir: &Path) -> NgResult<()> {
    let mut interp = Interpreter {
        env: HashMap::new(),
        temp_dir: temp_dir.to_path_buf(),
        cur_lno: Cell::new(0),
        fq_stats: RefCell::new(Vec::new()),
    };
    for (lno, e) in body {
        interp.cur_lno.set(*lno);
        interp.interpret_top(e)?;
    }
    Ok(())
}

/// Collected per-file FASTQ statistics, in registration order (mirrors the `savedOutput`
/// accumulator). `qcstats({fastq})` serialises these to a TSV.
struct FqInfo {
    file: String,
    encoding: String,
    gc_content: f64,
    non_atcg: f64,
    n_seq: i64,
    n_basepairs: i64,
    min_len: i64,
    max_len: i64,
}

struct Interpreter {
    env: HashMap<String, NGLessObject>,
    temp_dir: PathBuf,
    /// Line number of the top-level statement currently executing (used for `preproc.lnoN`).
    cur_lno: Cell<usize>,
    fq_stats: RefCell<Vec<FqInfo>>,
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
        if f.0 == "select" {
            let expr_v = self.interpret_top_value(expr)?;
            let mut argvs = Vec::new();
            for (Variable(v), e) in args {
                argvs.push((v.clone(), self.interpret_expr(e)?));
            }
            return match block {
                Some(b) => self.execute_select_block(&expr_v, &argvs, b),
                None => self.execute_select(&expr_v, &argvs),
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
            "paired" => self.execute_paired(&expr_v, &argvs),
            "samfile" => self.execute_samfile(&expr_v, &argvs),
            "as_reads" => self.execute_as_reads(&expr_v),
            "qcstats" => self.execute_qcstats(&expr_v),
            "samtools_sort" => self.execute_samtools_sort(&expr_v, &argvs),
            "samtools_view" => self.execute_samtools_view(&expr_v, &argvs),
            "count" => self.execute_count(&expr_v, &argvs),
            "write" => self.execute_write(&expr_v, &argvs),
            _ => execute_function(f, &expr_v, &argvs),
        }
    }

    /// `as_reads(mapped)`: reconstruct FASTQ reads from the SAM records (mirrors `executeReads`
    /// / `samToFastQ`). Records are grouped by read name; within a group, mates are split into
    /// pair.1/pair.2 by their flag bits and lone reads become singletons.
    fn execute_as_reads(&self, expr: &NGLessObject) -> NgResult<NGLessObject> {
        let (name, path) = match expr {
            NGLessObject::MappedReadSet { name, path } => (name.clone(), path.clone()),
            other => {
                return Err(NgError::should_not_occur(format!(
                    "as_reads expected a mapped read set, got {other:?}"
                )))
            }
        };
        let text = read_sam_text(&path.to_string_lossy())?;
        let records = sam::parse_sam(&text)?;
        let lines: Vec<&SamLine> = records
            .iter()
            .filter_map(|r| match r {
                SamRecord::Line(l) => Some(l),
                SamRecord::Header(_) => None,
            })
            .collect();

        let (mut p1, mut p2, mut s) = (String::new(), String::new(), String::new());
        let (mut has_paired, mut has_single) = (false, false);
        let mut i = 0;
        while i < lines.len() {
            // Group consecutive records with the same read name.
            let mut j = i + 1;
            while j < lines.len() && lines[j].qname == lines[i].qname {
                j += 1;
            }
            match as_fq(&lines[i..j]) {
                FQResult::Single(b) => {
                    s.push_str(&b);
                    has_single = true;
                }
                FQResult::Paired(a, b) => {
                    p1.push_str(&a);
                    p2.push_str(&b);
                    has_paired = true;
                }
                FQResult::None => {}
            }
            i = j;
        }

        let readset = match (has_paired, has_single) {
            (true, true) => {
                let enc = fastq::detect_encoding(&p1)?;
                ReadSet {
                    pairs: vec![(
                        self.write_fq_temp(&p1, enc, "reads_.1.")?,
                        self.write_fq_temp(&p2, enc, "reads_.2.")?,
                    )],
                    singletons: vec![self.write_fq_temp(&s, enc, "reads_.singles.")?],
                }
            }
            (true, false) => {
                let enc = fastq::detect_encoding(&p1)?;
                ReadSet {
                    pairs: vec![(
                        self.write_fq_temp(&p1, enc, "reads_.1.")?,
                        self.write_fq_temp(&p2, enc, "reads_.2.")?,
                    )],
                    singletons: Vec::new(),
                }
            }
            (false, true) => {
                let enc = fastq::detect_encoding(&s)?;
                ReadSet {
                    pairs: Vec::new(),
                    singletons: vec![self.write_fq_temp(&s, enc, "reads_.singles.")?],
                }
            }
            (false, false) => ReadSet {
                pairs: vec![(
                    self.write_fq_temp("", FastQEncoding::Sanger, "reads_.1.")?,
                    self.write_fq_temp("", FastQEncoding::Sanger, "reads_.2.")?,
                )],
                singletons: Vec::new(),
            },
        };
        Ok(NGLessObject::ReadSet { name, readset })
    }

    /// `count(mapped, ...)`: annotate and count mapped reads against sequence names, a GFF file or
    /// a functional map (mirrors `executeCount`). Returns a `Counts` value backed by a TSV file.
    fn execute_count(
        &self,
        expr: &NGLessObject,
        args: &[(String, NGLessObject)],
    ) -> NgResult<NGLessObject> {
        if let NGLessObject::List(es) = expr {
            let counted = es
                .iter()
                .map(|e| self.execute_count(e, args))
                .collect::<NgResult<Vec<_>>>()?;
            return Ok(NGLessObject::List(counted));
        }
        let (name, path) = mapped_read_set(expr, "count")?;
        let opts = parse_count_opts(args)?;

        let text = read_sam_text(&path.to_string_lossy())?;
        let records = sam::parse_sam(&text)?;
        let mut sq_header: Vec<(String, i64)> = Vec::new();
        let mut groups: Vec<Vec<SamLine>> = Vec::new();
        for rec in records {
            match rec {
                SamRecord::Header(h) => {
                    if let Some(sq) = parse_sq_header(&h) {
                        sq_header.push(sq);
                    }
                }
                SamRecord::Line(l) => {
                    if let Some(last) = groups.last_mut() {
                        if last[0].qname == l.qname {
                            last.push(l);
                            continue;
                        }
                    }
                    groups.push(vec![l]);
                }
            }
        }

        let tsv = crate::count::perform_count(&groups, &sq_header, &name, &opts)?;
        let out = self.new_temp_path("counts.", "txt");
        std::fs::write(&out, tsv).map_err(|e| {
            NgError::new(
                NgErrorType::SystemError,
                format!("Could not write counts file: {e}"),
            )
        })?;
        Ok(NGLessObject::Counts(out))
    }

    /// Write FASTQ text to a fresh temp file and reference it with the given encoding.
    fn write_fq_temp(
        &self,
        data: &str,
        encoding: FastQEncoding,
        prefix: &str,
    ) -> NgResult<FastQFilePath> {
        let path = self.new_temp_path(prefix, "fq");
        std::fs::write(&path, data).map_err(|e| {
            NgError::new(
                NgErrorType::SystemError,
                format!("Could not write temp file {}: {e}", path.display()),
            )
        })?;
        Ok(FastQFilePath { encoding, path })
    }

    /// `samfile(fname [, name=, headers=])`: reference a SAM file as a mapped read set (mirrors
    /// `executeSamfile`). With `headers`, the header file is prepended to the alignments by
    /// materialising the concatenation to a temp file.
    fn execute_samfile(
        &self,
        expr: &NGLessObject,
        args: &[(String, NGLessObject)],
    ) -> NgResult<NGLessObject> {
        let fname = as_string(expr, "samfile")?;
        let name = match lookup_arg(args, "name") {
            Some(NGLessObject::String(s)) => s.clone(),
            _ => fname.clone(),
        };
        let headers = match lookup_arg(args, "headers") {
            Some(NGLessObject::String(s)) => s.clone(),
            _ => String::new(),
        };
        if headers.is_empty() {
            return Ok(NGLessObject::MappedReadSet {
                name,
                path: PathBuf::from(fname),
            });
        }
        // Sourcing `headers` then `fname` and re-emitting line-by-line is, for newline-terminated
        // files, the same as concatenating their bytes.
        let mut data = read_sam_text(&headers)?;
        data.push_str(&read_sam_text(&fname)?);
        let path = self.write_sam_temp(&data)?;
        Ok(NGLessObject::MappedReadSet { name, path })
    }

    /// `select(mapped, keep_if=/drop_if=)` (call form, mirrors `executeSelect`): apply the
    /// conditions per read-name group, reinjecting sequence where a kept mate lost it, and write
    /// the surviving lines (with headers preserved) to a fresh SAM temp file.
    fn execute_select(
        &self,
        expr: &NGLessObject,
        args: &[(String, NGLessObject)],
    ) -> NgResult<NGLessObject> {
        let (name, path) = mapped_read_set(expr, "select")?;
        let paired = lookup_bool(args, "paired", true)?;
        let keep_if = lookup_symbol_list(args, "keep_if");
        let drop_if = lookup_symbol_list(args, "drop_if");
        let cond = select::parse_conditions(&keep_if, &drop_if)?;
        let text = read_sam_text(&path.to_string_lossy())?;
        let mut out = String::new();
        for item in group_sam(&text, paired)? {
            match item {
                SamItem::Header(line) => {
                    out.push_str(&line);
                    out.push('\n');
                }
                SamItem::Data(group) => {
                    let filtered = select::apply_conditions(&cond, group.clone());
                    for line in reinject_call(&group, filtered)? {
                        out.push_str(&line);
                        out.push('\n');
                    }
                }
            }
        }
        let outpath = self.write_sam_temp(&out)?;
        Ok(NGLessObject::MappedReadSet {
            name,
            path: outpath,
        })
    }

    /// `select(mapped) using |mr|: ...` (block form, mirrors `executeSelectWBlock`): run the block
    /// on every read-name group, re-encode the surviving (reinjected) records, and write the
    /// result — with leading headers preserved — to a fresh SAM temp file.
    fn execute_select_block(
        &self,
        expr: &NGLessObject,
        args: &[(String, NGLessObject)],
        block: &Block,
    ) -> NgResult<NGLessObject> {
        let (name, path) = mapped_read_set(expr, "select")?;
        let paired = lookup_bool(args, "paired", true)?;
        let var = &block.variable.0;
        let text = read_sam_text(&path.to_string_lossy())?;
        let mut out = String::new();
        for item in group_sam(&text, paired)? {
            match item {
                SamItem::Header(line) => {
                    out.push_str(&line);
                    out.push('\n');
                }
                SamItem::Data(group) => {
                    let group_lines: Vec<SamLine> = group.iter().map(|(s, _)| s.clone()).collect();
                    let (status, value) = self.interpret_block_stmt(
                        var,
                        NGLessObject::MappedRead(group_lines.clone()),
                        &block.body,
                    )?;
                    if status == BlockStatus::Discarded {
                        continue;
                    }
                    let rs = match value {
                        NGLessObject::MappedRead(rs) => rs,
                        other => {
                            return Err(NgError::should_not_occur(format!(
                                "Expected variable {var} to contain a mapped read, got {other:?}"
                            )))
                        }
                    };
                    if rs.is_empty() {
                        continue;
                    }
                    for l in select::reinject_sequences(&group_lines, &rs)? {
                        out.push_str(&encode_sam_line(&l));
                        out.push('\n');
                    }
                }
            }
        }
        let outpath = self.write_sam_temp(&out)?;
        Ok(NGLessObject::MappedReadSet {
            name,
            path: outpath,
        })
    }

    /// Give samtools a file it can read natively. samtools handles `.sam`, `.bam` and gzipped
    /// SAM, but not zstd/bzip2; for those we decompress to a plain SAM temp first.
    fn samtools_input(&self, path: &Path) -> NgResult<PathBuf> {
        let s = path.to_string_lossy().to_string();
        match crate::compression::detect(&s) {
            crate::compression::Compress::Zstd | crate::compression::Compress::Bzip2 => {
                let text = read_sam_text(&s)?;
                self.write_sam_temp(&text)
            }
            _ => Ok(path.to_path_buf()),
        }
    }

    /// Write SAM text to a fresh temp file and return its path.
    fn write_sam_temp(&self, data: &str) -> NgResult<PathBuf> {
        let path = self.new_temp_path("selected_", "sam");
        std::fs::write(&path, data).map_err(|e| {
            NgError::new(
                NgErrorType::SystemError,
                format!("Could not write temp file {}: {e}", path.display()),
            )
        })?;
        Ok(path)
    }

    /// `samtools_sort(mapped, by={coordinate|name})` (from the `samtools` module, mirrors
    /// `executeSort`). The result is materialised as a sorted SAM temp file; `write` later
    /// converts it to BAM if the output filename asks for it.
    fn execute_samtools_sort(
        &self,
        expr: &NGLessObject,
        args: &[(String, NGLessObject)],
    ) -> NgResult<NGLessObject> {
        let (name, path) = mapped_read_set(expr, "samtools_sort")?;
        let by_name = match lookup_symbol(args, "by", "coordinate")?.as_str() {
            "coordinate" => false,
            "name" => true,
            other => {
                return Err(NgError::should_not_occur(format!(
                    "Check failed. No samtool_sort option: {other}"
                )))
            }
        };
        let input = self.samtools_input(&path)?;
        let out = self.new_temp_path("sorted_", "sam");
        let temp_prefix = self.new_temp_path("samtools_sort_temp", "tmp");
        crate::samtools::sort(&input, &out, "sam", by_name, &temp_prefix)?;
        Ok(NGLessObject::MappedReadSet { name, path: out })
    }

    /// `samtools_view(mapped, bed_file=...)` (from the `samtools` module, mirrors `executeView`):
    /// keep only records overlapping the BED regions, materialised as a SAM temp file.
    fn execute_samtools_view(
        &self,
        expr: &NGLessObject,
        args: &[(String, NGLessObject)],
    ) -> NgResult<NGLessObject> {
        let (name, path) = mapped_read_set(expr, "samtools_view")?;
        let bed = match lookup_arg(args, "bed_file") {
            Some(NGLessObject::String(s)) => s.clone(),
            _ => {
                return Err(NgError::script(
                    "samtools_view: `bed_file` (a string) is required",
                ))
            }
        };
        let input = self.samtools_input(&path)?;
        let out = self.new_temp_path("subset_", "sam");
        crate::samtools::view_bed(&input, &bed, &out, "sam")?;
        Ok(NGLessObject::MappedReadSet { name, path: out })
    }

    /// `write(...)`: dispatch on the value type (mirrors `executeWrite`). Read sets and counts are
    /// handled by the free helpers; mapped read sets may need samtools for BAM conversion, which
    /// needs the interpreter's temp dir, hence this lives on the interpreter.
    fn execute_write(
        &self,
        expr: &NGLessObject,
        args: &[(String, NGLessObject)],
    ) -> NgResult<NGLessObject> {
        if let NGLessObject::MappedReadSet { path, .. } = expr {
            let ofile = match lookup_arg(args, "ofile") {
                Some(NGLessObject::String(s)) => s.clone(),
                _ => {
                    return Err(NgError::script(
                        "write: argument `ofile` (a string) is required",
                    ))
                }
            };
            self.write_mapped_read_set(path, &ofile)?;
            return Ok(NGLessObject::String(ofile));
        }
        execute_write(expr, args)
    }

    /// Write a mapped read set to `ofile` (mirrors `executeWrite` of an `NGOMappedReadSet`). The
    /// output format comes from the extension; SAM/BAM conversion goes through samtools.
    fn write_mapped_read_set(&self, path: &Path, ofile: &str) -> NgResult<()> {
        let is_bam = |p: &str| p.ends_with(".bam");
        let sam_ext = [".sam", ".sam.gz", ".sam.bz2", ".sam.zst", ".sam.zstd"];
        let format = if ofile == "/dev/stdout" || sam_ext.iter().any(|e| ofile.ends_with(e)) {
            "sam"
        } else {
            // `.bam`, or (as in Haskell) anything unrecognised, defaults to BAM.
            "bam"
        };
        let src = path.to_string_lossy().to_string();
        // Produce a file in the requested format, converting via samtools when needed.
        let orig: PathBuf = match (format, is_bam(&src)) {
            ("sam", false) => path.to_path_buf(),
            ("sam", true) => {
                let out = self.new_temp_path("converted_", "sam");
                crate::samtools::convert_bam_to_sam(path, &out)?;
                out
            }
            ("bam", true) => path.to_path_buf(),
            (_, _) => {
                let input = self.samtools_input(path)?;
                let out = self.new_temp_path("converted_", "bam");
                crate::samtools::convert_sam_to_bam(&input, &out)?;
                out
            }
        };
        copy_fastq(&orig, ofile)
    }

    /// Decode a FASTQ file, register its statistics under `label`, and return a reference to it
    /// (mirrors `asFQFilePathMayQC` with QC enabled).
    fn load_and_qc(&self, path: &str, label: &str) -> NgResult<FastQFilePath> {
        let text = read_fastq_text(path)?;
        let encoding = fastq::detect_encoding(&text)?;
        let reads = fastq::fq_decode(encoding, &text)?;
        self.register_fq_stats(label, &reads, encoding);
        Ok(FastQFilePath {
            encoding,
            path: PathBuf::from(path),
        })
    }

    /// Compute and record FASTQ statistics for a set of reads (mirrors `outputFQStatistics`).
    fn register_fq_stats(&self, label: &str, reads: &[ShortRead], enc: FastQEncoding) {
        let st = fastq::stats_from_reads(reads);
        self.fq_stats.borrow_mut().push(FqInfo {
            file: label.to_string(),
            encoding: enc.name().to_string(),
            gc_content: st.gc_fraction(),
            non_atcg: st.non_atcg_fraction(),
            n_seq: st.n_seq,
            n_basepairs: st.num_basepairs(),
            min_len: st.min_len,
            max_len: st.max_len,
        });
    }

    /// `qcstats({fastq})`: serialise the collected FASTQ statistics to a TSV temp file and return
    /// it as a counts table (mirrors `executeStats` + `writeOutputTSV` for the fastq case).
    fn execute_qcstats(&self, expr: &NGLessObject) -> NgResult<NGLessObject> {
        let stats_type = match expr {
            NGLessObject::Symbol(s) => s.as_str(),
            other => {
                return Err(NgError::should_not_occur(format!(
                    "qcstats expected a symbol, got {other:?}"
                )))
            }
        };
        match stats_type {
            "fastq" => {
                let tsv = self.format_fq_stats_tsv();
                let path = self.new_temp_path("qcstats.", "tsv");
                std::fs::write(&path, tsv).map_err(|e| {
                    NgError::new(
                        NgErrorType::SystemError,
                        format!("Could not write {}: {e}", path.display()),
                    )
                })?;
                Ok(NGLessObject::Counts(path))
            }
            "mapping" => Err(NgError::script(
                "qcstats({mapping}) is not implemented in this build yet.",
            )),
            other => Err(NgError::script(format!("Unknown stats type: {other}"))),
        }
    }

    /// Serialise collected FASTQ stats as the transposed TSV produced by `writeOutputTSV`.
    fn format_fq_stats_tsv(&self) -> String {
        let stats = self.fq_stats.borrow();
        let mut out = String::from("\tstats\n");
        for (i, info) in stats.iter().enumerate() {
            // Keys in the alphabetical order produced by Haskell's `sort` over (key, value).
            let rows = [
                ("encoding", info.encoding.clone()),
                ("file", info.file.clone()),
                ("gcContent", show_double(info.gc_content)),
                ("maxSeqLen", info.max_len.to_string()),
                ("minSeqLen", info.min_len.to_string()),
                ("nonATCGFraction", show_double(info.non_atcg)),
                ("numBasepairs", info.n_basepairs.to_string()),
                ("numSeqs", info.n_seq.to_string()),
            ];
            for (k, v) in rows {
                out.push_str(&format!("{i}:{k}\t{v}\n"));
            }
        }
        out
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
        let fqf = self.load_and_qc(&path, &path)?;
        Ok(NGLessObject::ReadSet {
            name: path,
            readset: ReadSet {
                pairs: Vec::new(),
                singletons: vec![fqf],
            },
        })
    }

    /// `paired(mate1, second=mate2, singles=mate3)`: reference the mate files (mirrors
    /// `executePaired`). The mates must share an encoding; an empty `singles` file (or one whose
    /// encoding disagrees but is empty) is dropped.
    fn execute_paired(
        &self,
        expr: &NGLessObject,
        args: &[(String, NGLessObject)],
    ) -> NgResult<NGLessObject> {
        let mate1 = as_string(expr, "paired")?;
        let mate2 = match lookup_arg(args, "second") {
            Some(NGLessObject::String(s)) => s.clone(),
            _ => return Err(NgError::script("paired: a second mate file is required")),
        };
        let mate3 = match lookup_arg(args, "singles") {
            Some(NGLessObject::String(s)) => s.clone(),
            _ => String::new(),
        };
        let f1 = self.load_and_qc(&mate1, &mate1)?;
        let f2 = self.load_and_qc(&mate2, &mate2)?;
        if f1.encoding != f2.encoding {
            return Err(NgError::new(
                NgErrorType::DataError,
                format!(
                    "Mates do not seem to have the same quality encoding! ([{mate1}] vs [{mate2}])."
                ),
            ));
        }
        let pair = (f1.clone(), f2);
        let singletons = if mate3.is_empty() {
            Vec::new()
        } else {
            // QC stats are registered for the singles file even if it is later dropped.
            let text = read_fastq_text(&mate3)?;
            let enc3 = fastq::detect_encoding(&text)?;
            let reads3 = fastq::fq_decode(enc3, &text)?;
            self.register_fq_stats(&mate3, &reads3, enc3);
            if f1.encoding != enc3 {
                // Special case seen in the wild: an empty singles file with a default encoding.
                if reads3.is_empty() {
                    Vec::new()
                } else {
                    return Err(NgError::new(NgErrorType::DataError,
                        format!("Mates do not seem to have the same quality encoding! (paired mates vs single [{mate3}]).")));
                }
            } else {
                vec![FastQFilePath {
                    encoding: enc3,
                    path: PathBuf::from(&mate3),
                }]
            }
        };
        Ok(NGLessObject::ReadSet {
            name: mate1,
            readset: ReadSet {
                pairs: vec![pair],
                singletons,
            },
        })
    }

    /// `preprocess(rs) using |read|: ...`: stream the read set through the block (mirrors
    /// `executePreprocess`). Paired reads are processed in lockstep — a pair where both mates
    /// survive stays paired, a pair where one survives becomes a singleton (`keep_singles`,
    /// default true). Output encoding is the common input encoding, or Sanger if they disagree.
    fn execute_preprocess(&self, readset: &NGLessObject, block: &Block) -> NgResult<NGLessObject> {
        let (name, rs) = match readset {
            NGLessObject::ReadSet { name, readset } => (name.clone(), readset.clone()),
            other => {
                return Err(NgError::should_not_occur(format!(
                    "preprocess expected a read set, got {other:?}"
                )))
            }
        };
        let mut inputs: Vec<FastQFilePath> = Vec::new();
        for (a, b) in &rs.pairs {
            inputs.push(a.clone());
            inputs.push(b.clone());
        }
        inputs.extend(rs.singletons.iter().cloned());
        let outenc = output_encoding(&inputs);
        let var = &block.variable.0;
        let keep_singles = true;

        let (mut p1, mut p2, mut s): (Vec<ShortRead>, Vec<ShortRead>, Vec<ShortRead>) =
            (Vec::new(), Vec::new(), Vec::new());

        for (f1, f2) in &rs.pairs {
            let reads1 =
                fastq::fq_decode(f1.encoding, &read_fastq_text(&f1.path.to_string_lossy())?)?;
            let reads2 =
                fastq::fq_decode(f2.encoding, &read_fastq_text(&f2.path.to_string_lossy())?)?;
            for (r1, r2) in reads1.into_iter().zip(reads2.into_iter()) {
                let o1 = self.run_block_on_read(var, r1, block)?;
                let o2 = self.run_block_on_read(var, r2, block)?;
                match (o1, o2) {
                    (Some(a), Some(b)) => {
                        p1.push(a);
                        p2.push(b);
                    }
                    (Some(r), None) | (None, Some(r)) => {
                        if keep_singles {
                            s.push(r);
                        }
                    }
                    (None, None) => {}
                }
            }
        }
        for fqf in &rs.singletons {
            let reads =
                fastq::fq_decode(fqf.encoding, &read_fastq_text(&fqf.path.to_string_lossy())?)?;
            for r in reads {
                if let Some(sr) = self.run_block_on_read(var, r, block)? {
                    s.push(sr);
                }
            }
        }

        // Register QC statistics for all three output slots, as executePreprocess does (before
        // deciding the result shape), labelled with the preprocess statement's line number.
        let lno = self.cur_lno.get();
        self.register_fq_stats(&format!("preproc.lno{lno}.pairs.1"), &p1, outenc);
        self.register_fq_stats(&format!("preproc.lno{lno}.pairs.2"), &p2, outenc);
        self.register_fq_stats(&format!("preproc.lno{lno}.singles"), &s, outenc);

        // Choose the result shape exactly as executePreprocess does, materialising only the
        // temp files the result references.
        let (n_pairs, n_singles) = (p1.len(), s.len());
        let make = |this: &Self, prefix: &str, reads: &[ShortRead]| -> NgResult<FastQFilePath> {
            let mut data = String::new();
            for r in reads {
                data.push_str(&fastq::fq_encode(outenc, r));
            }
            let path = this.new_temp_path(prefix, "fq");
            std::fs::write(&path, data).map_err(|e| {
                NgError::new(
                    NgErrorType::SystemError,
                    format!("Could not write temp file {}: {e}", path.display()),
                )
            })?;
            Ok(FastQFilePath {
                encoding: outenc,
                path,
            })
        };
        let readset = match (n_pairs > 0, n_singles > 0) {
            (true, false) => ReadSet {
                pairs: vec![(
                    make(self, "preprocessed.1.", &p1)?,
                    make(self, "preprocessed.2.", &p2)?,
                )],
                singletons: Vec::new(),
            },
            (false, true) => ReadSet {
                pairs: Vec::new(),
                singletons: vec![make(self, "preprocessed.singles.", &s)?],
            },
            _ if rs.pairs.is_empty() => ReadSet {
                pairs: Vec::new(),
                singletons: vec![make(self, "preprocessed.singles.", &s)?],
            },
            _ => ReadSet {
                pairs: vec![(
                    make(self, "preprocessed.1.", &p1)?,
                    make(self, "preprocessed.2.", &p2)?,
                )],
                singletons: vec![make(self, "preprocessed.singles.", &s)?],
            },
        };
        Ok(NGLessObject::ReadSet { name, readset })
    }

    /// Run the preprocess block on a single read, returning `None` if it was discarded.
    fn run_block_on_read(
        &self,
        var: &str,
        r: ShortRead,
        block: &Block,
    ) -> NgResult<Option<ShortRead>> {
        let (status, value) = self.interpret_block_stmt(var, NGLessObject::Read(r), &block.body)?;
        match status {
            BlockStatus::Discarded => Ok(None),
            BlockStatus::Ok | BlockStatus::Continued => match value {
                NGLessObject::Read(sr) => Ok(Some(sr)),
                other => Err(NgError::should_not_occur(format!(
                    "preprocess block produced a non-read value: {other:?}"
                ))),
            },
        }
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

/// Read a SAM file into memory. BAM is decoded via samtools (mirrors `samBamConduit`); gzip is
/// transparent.
fn read_sam_text(path: &str) -> NgResult<String> {
    if path.ends_with(".bam") {
        return crate::samtools::bam_to_sam_text(path);
    }
    crate::compression::read_to_string(path)
}

/// One grouping unit of a SAM file for `select`: a header line (verbatim) or a group of
/// alignment records (with their original raw lines) that share a read name.
enum SamItem {
    Header(String),
    Data(Vec<(SamLine, String)>),
}

/// Group SAM text the way `select` does (mirrors `readSamGroupsAsConduit`): header lines are
/// each their own item; consecutive alignment lines with the same read name (and, when not
/// `paired`, the same first-in-pair bit) form one data group.
fn group_sam(text: &str, paired: bool) -> NgResult<Vec<SamItem>> {
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
        let sl = parse_sam_line(line)?;
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
    Ok(items)
}

/// Produce the output lines for one filtered group in the `select` *call* form (mirrors
/// `reinjectSequencesIfNeeded`): when the survivors lost their sequence, reinject it and re-encode
/// every line; otherwise emit the original raw lines unchanged.
fn reinject_call(
    orig: &[(SamLine, String)],
    filtered: Vec<(SamLine, String)>,
) -> NgResult<Vec<String>> {
    let filtered_lines: Vec<SamLine> = filtered.iter().map(|(s, _)| s.clone()).collect();
    if select::needs_reinject(&filtered_lines) {
        let orig_lines: Vec<SamLine> = orig.iter().map(|(s, _)| s.clone()).collect();
        let reinjected = select::reinject_sequences(&orig_lines, &filtered_lines)?;
        Ok(reinjected.iter().map(encode_sam_line).collect())
    } else {
        Ok(filtered.into_iter().map(|(_, line)| line).collect())
    }
}

/// The outcome of reconstructing reads for one read-name group (mirrors `FQResult`).
enum FQResult {
    None,
    Single(String),
    Paired(String, String),
}

/// Reconstruct FASTQ text for one read-name group (mirrors `asFQ`).
fn as_fq(group: &[&SamLine]) -> FQResult {
    let with_seq: Vec<&SamLine> = group.iter().copied().filter(|l| l.has_sequence()).collect();
    let tagged = as_fq_collect(&with_seq);
    match tagged.as_slice() {
        [(_, b)] => FQResult::Single(b.clone()),
        [(1, a), (2, b)] => FQResult::Paired(a.clone(), b.clone()),
        [(2, b), (1, a)] => FQResult::Paired(a.clone(), b.clone()),
        _ => FQResult::None,
    }
}

/// Tag each sequence-bearing record as mate 1, mate 2 or a singleton (mirrors `asFQ'`): a lone
/// record is always a singleton (tag 3); otherwise the first first-in-pair and first
/// second-in-pair records are taken.
fn as_fq_collect(lines: &[&SamLine]) -> Vec<(i32, String)> {
    if lines.len() == 1 {
        return vec![(3, as_fq1(lines[0]))];
    }
    let mut out = Vec::new();
    let (mut seen1, mut seen2) = (false, false);
    for l in lines {
        if l.is_first_in_pair() && !seen1 {
            out.push((1, as_fq1(l)));
            seen1 = true;
        } else if l.is_second_in_pair() && !seen2 {
            out.push((2, as_fq1(l)));
            seen2 = true;
        }
    }
    out
}

/// Format one SAM record as a 4-line FASTQ record (mirrors `asFQ1`).
fn as_fq1(l: &SamLine) -> String {
    format!("@{}\n{}\n+\n{}\n", l.qname, l.seq, l.qual)
}

/// `write(rs, ofile=...)`: write the read set's current file(s) to `ofile` (mirrors
/// `executeWrite` of an `NGOReadSet`). A single-end set goes straight to `ofile`; a paired set
/// produces `<base>.pair.1.<ext>` / `.pair.2.<ext>` (and `.singles.<ext>` when singletons
/// remain). For an un-preprocessed single-end set this reproduces the input byte-for-byte.
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
            if readset.pairs.is_empty() {
                let files: Vec<&FastQFilePath> = readset.singletons.iter().collect();
                write_fq_files(&files, &ofile)?;
            } else {
                let f1: Vec<&FastQFilePath> = readset.pairs.iter().map(|(a, _)| a).collect();
                let f2: Vec<&FastQFilePath> = readset.pairs.iter().map(|(_, b)| b).collect();
                write_fq_files(&f1, &format_fq_oname(&ofile, "pair.1")?)?;
                write_fq_files(&f2, &format_fq_oname(&ofile, "pair.2")?)?;
                if !readset.singletons.is_empty() {
                    let f3: Vec<&FastQFilePath> = readset.singletons.iter().collect();
                    write_fq_files(&f3, &format_fq_oname(&ofile, "singles")?)?;
                }
            }
            Ok(NGLessObject::String(ofile))
        }
        NGLessObject::Counts(path) => {
            write_counts(path, &ofile, args)?;
            Ok(NGLessObject::String(ofile))
        }
        other => Err(NgError::script(format!(
            "write of {} is not implemented in this build yet.",
            type_label(other)
        ))),
    }
}

/// Derive a per-mate output name from a base name and an insert (mirrors `_formatFQOname`):
/// `output.fq` + `pair.1` -> `output.pair.1.fq`.
fn format_fq_oname(base: &str, insert: &str) -> NgResult<String> {
    if let Some(stripped) = base.strip_suffix(".subsampled") {
        return format_fq_oname(stripped, &format!("{insert}.subsampled"));
    }
    if base.contains("{index}") {
        return Ok(base.replace("{index}", insert));
    }
    for ext in [".fq", ".fq.gz", ".fq.bz2"] {
        if let Some(stripped) = base.strip_suffix(ext) {
            return Ok(format!("{stripped}.{insert}{ext}"));
        }
    }
    Err(NgError::script(format!(
        "Cannot handle filename {base} (expected extension .fq/.fq.gz/.fq.bz2)."
    )))
}

/// Write a list of FASTQ files to a single output (mirrors `moveOrCopyCompressFQs`): one file
/// is copied/recompressed, several are concatenated (decompressed) then compressed to `ofile`,
/// and an empty list produces an empty output file.
fn write_fq_files(files: &[&FastQFilePath], ofile: &str) -> NgResult<()> {
    use crate::compression::{read_bytes, write_bytes};
    match files {
        [] => write_bytes(ofile, b""),
        [single] => copy_fastq(&single.path, ofile),
        many => {
            let mut data = Vec::new();
            for f in many {
                data.extend(read_bytes(&f.path.to_string_lossy())?);
            }
            write_bytes(ofile, &data)
        }
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

/// Write a counts TSV to `ofile` (mirrors the `NGOCounts` arm of `executeWrite`). Supports
/// `format={tsv|csv}` and a manual `comment=` (each line prefixed `# `). `auto_comments` is not
/// supported yet and is ignored.
fn write_counts(path: &Path, ofile: &str, args: &[(String, NGLessObject)]) -> NgResult<()> {
    let format = match lookup_arg(args, "format") {
        None => "tsv".to_string(),
        Some(NGLessObject::Symbol(s)) => s.clone(),
        Some(other) => {
            return Err(NgError::script(format!(
                "write: `format` should be a symbol, got {other:?}"
            )))
        }
    };
    let comment = match lookup_arg(args, "comment") {
        Some(NGLessObject::String(s)) => Some(s.clone()),
        _ => None,
    };
    // Fast path: an unmodified TSV is copied (and recompressed) verbatim.
    if format == "tsv" && comment.is_none() {
        return copy_fastq(path, ofile);
    }
    let body = crate::compression::read_to_string(&path.to_string_lossy())?;
    let mut out = String::new();
    if let Some(c) = &comment {
        for line in c.split('\n') {
            out.push_str("# ");
            out.push_str(line);
            out.push('\n');
        }
    }
    match format.as_str() {
        "tsv" => out.push_str(&body),
        "csv" => out.push_str(&body.replace('\t', ",")),
        f => {
            return Err(NgError::script(format!(
                "Invalid format in write: {{{f}}}.\n\tWhen writing counts, only accepted values are {{tsv}} (TAB separated values; default) or {{csv}} (COMMA separated values)."
            )))
        }
    }
    crate::compression::write_bytes(ofile, out.as_bytes())
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
        // NGOMappedRead methods (mirrors `executeMappedReadMethod`).
        ("flag", NGLessObject::MappedRead(samlines)) => {
            let flag = match _arg {
                Some(NGLessObject::Symbol(s)) => s.as_str(),
                _ => return Err(NgError::script("flag method requires a symbol argument")),
            };
            let v = match flag {
                "mapped" => samlines.iter().any(|s| s.is_aligned()),
                "unmapped" => !samlines.iter().any(|s| s.is_aligned()),
                other => {
                    return Err(NgError::script(format!(
                        "Flag {other:?} is unknown for method flag"
                    )))
                }
            };
            Ok(NGLessObject::Bool(v))
        }
        ("filter", NGLessObject::MappedRead(samlines)) => {
            let min_id_pc = lookup_int(_args, "min_identity_pc", -1)?;
            let min_match_size = lookup_int(_args, "min_match_size", -1)?;
            let max_trim = lookup_int(_args, "max_trim", -1)?;
            let reverse = lookup_bool(_args, "reverse", false)?;
            let use_newer = lookup_bool(_args, "__version11_or_higher", false)?;
            let action = match lookup_symbol(_args, "action", "drop")?.as_str() {
                "drop" => FilterAction::Drop,
                "unmatch" => FilterAction::Unmatch,
                other => {
                    return Err(NgError::script(format!(
                        "unknown action in filter(): `{other}`.\nAllowed values are:\n\tdrop\n\tunmatch\n\tkeep\n"
                    )))
                }
            };
            let opts = FilterOptions {
                min_id: min_id_pc as f64 / 100.0,
                min_match_size,
                max_trim,
                reverse,
                action,
                use_newer,
            };
            Ok(NGLessObject::MappedRead(select::apply_filter(
                &opts,
                samlines.clone(),
            )))
        }
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

fn lookup_bool(args: &[(String, NGLessObject)], name: &str, default: bool) -> NgResult<bool> {
    match lookup_arg(args, name) {
        None => Ok(default),
        Some(NGLessObject::Bool(b)) => Ok(*b),
        Some(other) => Err(NgError::script(format!(
            "Argument `{name}` should be a boolean, got {other:?}"
        ))),
    }
}

fn lookup_int(args: &[(String, NGLessObject)], name: &str, default: i64) -> NgResult<i64> {
    match lookup_arg(args, name) {
        None => Ok(default),
        Some(NGLessObject::Integer(i)) => Ok(*i),
        Some(other) => Err(NgError::script(format!(
            "Argument `{name}` should be an integer, got {other:?}"
        ))),
    }
}

fn lookup_symbol(args: &[(String, NGLessObject)], name: &str, default: &str) -> NgResult<String> {
    match lookup_arg(args, name) {
        None => Ok(default.to_string()),
        Some(NGLessObject::Symbol(s)) => Ok(s.clone()),
        Some(other) => Err(NgError::script(format!(
            "Argument `{name}` should be a symbol, got {other:?}"
        ))),
    }
}

/// Extract a list-of-symbols keyword argument (e.g. `keep_if=[{mapped}]`), defaulting to empty.
fn lookup_symbol_list(args: &[(String, NGLessObject)], name: &str) -> Vec<String> {
    match lookup_arg(args, name) {
        Some(NGLessObject::List(xs)) => xs
            .iter()
            .filter_map(|x| match x {
                NGLessObject::Symbol(s) => Some(s.clone()),
                _ => None,
            })
            .collect(),
        _ => Vec::new(),
    }
}

/// Extract a string or list-of-strings keyword argument; `None` when absent (mirrors the
/// `features`/`subfeatures` parsing in `parseOptions`).
fn lookup_string_list(
    args: &[(String, NGLessObject)],
    name: &str,
) -> NgResult<Option<Vec<String>>> {
    match lookup_arg(args, name) {
        None => Ok(None),
        Some(NGLessObject::String(s)) => Ok(Some(vec![s.clone()])),
        Some(NGLessObject::List(xs)) => {
            let mut out = Vec::with_capacity(xs.len());
            for x in xs {
                match x {
                    NGLessObject::String(s) => out.push(s.clone()),
                    other => {
                        return Err(NgError::script(format!(
                            "Argument `{name}` should be a list of strings, got element {other:?}"
                        )))
                    }
                }
            }
            Ok(Some(out))
        }
        Some(other) => Err(NgError::script(format!(
            "Argument `{name}` should be a string or list of strings, got {other:?}"
        ))),
    }
}

/// Look up an optional string/filename keyword argument.
fn lookup_opt_string(args: &[(String, NGLessObject)], name: &str) -> Option<String> {
    match lookup_arg(args, name) {
        Some(NGLessObject::String(s)) | Some(NGLessObject::Filename(s)) => Some(s.clone()),
        _ => None,
    }
}

/// Parse an `@SQ` header line into `(sequence name, length)`; `None` for other headers or when
/// `SN:`/`LN:` are missing.
fn parse_sq_header(line: &str) -> Option<(String, i64)> {
    let mut fields = line.split('\t');
    if fields.next() != Some("@SQ") {
        return None;
    }
    let (mut name, mut len) = (None, None);
    for f in fields {
        if let Some(n) = f.strip_prefix("SN:") {
            name = Some(n.to_string());
        } else if let Some(l) = f.strip_prefix("LN:") {
            len = l.parse::<i64>().ok();
        }
    }
    Some((name?, len?))
}

/// Build [`count::CountOpts`] from the keyword arguments (mirrors `parseOptions`).
fn parse_count_opts(args: &[(String, NGLessObject)]) -> NgResult<crate::count::CountOpts> {
    use crate::count::{AnnotationMode, CountOpts, IntersectMode, MMMethod, NMode, StrandMode};

    let features =
        lookup_string_list(args, "features")?.unwrap_or_else(|| vec!["gene".to_string()]);
    let subfeatures = lookup_string_list(args, "subfeatures")?;

    let mm_method = match lookup_symbol(args, "multiple", "dist1")?.as_str() {
        "all1" => MMMethod::CountAll,
        "dist1" => MMMethod::Dist1,
        "1overN" => MMMethod::OneOverN,
        "unique_only" => MMMethod::UniqueOnly,
        other => return Err(NgError::script(format!("Unexpected value for `multiple`: {other}"))),
    };

    let strand_specific = lookup_bool(args, "strand", false)?;
    let sense_default = if strand_specific { "sense" } else { "both" };
    let strand_mode = match lookup_symbol(args, "sense", sense_default)?.as_str() {
        "both" => StrandMode::Both,
        "sense" => StrandMode::Sense,
        "antisense" => StrandMode::Antisense,
        other => return Err(NgError::script(format!("Unexpected value for `sense`: {other}"))),
    };

    let intersect_mode = match lookup_symbol(args, "mode", "union")?.as_str() {
        "union" => IntersectMode::Union,
        "intersection_strict" => IntersectMode::Strict,
        "intersection_non_empty" => IntersectMode::NonEmpty,
        other => return Err(NgError::script(format!("Unexpected value for `mode`: {other}"))),
    };

    let norm_size = lookup_bool(args, "norm", false)?;
    let norm_default = if norm_size { "normed" } else { "raw" };
    let norm_mode = match lookup_symbol(args, "normalization", norm_default)?.as_str() {
        "raw" => NMode::Raw,
        "normed" => NMode::Normed,
        "scaled" => NMode::Scaled,
        "fpkm" => NMode::Fpkm,
        other => {
            return Err(NgError::script(format!(
                "Unexpected value for `normalization`: {other}"
            )))
        }
    };

    let min_count = if lookup_bool(args, "discard_zeros", false)? {
        f64::MIN_POSITIVE
    } else {
        lookup_int(args, "min", 0)? as f64
    };
    let include_minus1 = lookup_bool(args, "include_minus1", true)?;

    let annotation_mode = if features == ["seqname"] {
        AnnotationMode::SeqName
    } else if let Some(m) = lookup_opt_string(args, "functional_map") {
        AnnotationMode::FunctionalMap(m)
    } else if let Some(g) = lookup_opt_string(args, "gff_file") {
        AnnotationMode::Gff(g)
    } else if lookup_opt_string(args, "reference").is_some() {
        return Err(NgError::script(
            "count(): the `reference` argument (automatic annotation download) is not supported in this build yet.".to_string(),
        ));
    } else {
        return Err(NgError::script(
            "For counting, you must use seqname mode, pass a `gff_file`, or pass a `functional_map`.".to_string(),
        ));
    };

    Ok(CountOpts {
        features,
        subfeatures,
        annotation_mode,
        intersect_mode,
        strand_mode,
        min_count,
        mm_method,
        norm_mode,
        include_minus1,
    })
}

/// Destructure a mapped read set value into its name and backing file path.
fn mapped_read_set(v: &NGLessObject, who: &str) -> NgResult<(String, PathBuf)> {
    match v {
        NGLessObject::MappedReadSet { name, path } => Ok((name.clone(), path.clone())),
        other => Err(NgError::should_not_occur(format!(
            "{who} expected a mapped read set, got {other:?}"
        ))),
    }
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

    #[test]
    fn format_fq_oname_cases() {
        assert_eq!(
            format_fq_oname("output.fq", "pair.1").unwrap(),
            "output.pair.1.fq"
        );
        assert_eq!(
            format_fq_oname("o.fq.gz", "singles").unwrap(),
            "o.singles.fq.gz"
        );
        assert_eq!(
            format_fq_oname("x{index}y.fq", "pair.2").unwrap(),
            "xpair.2y.fq"
        );
        assert!(format_fq_oname("output.sam", "pair.1").is_err());
    }

    #[test]
    fn paired_preprocess_write_end_to_end() {
        // Each mate has two reads: r1 stays long enough to survive, r2 is trimmed away in both
        // mates -> the pair is dropped entirely, leaving no singletons.
        let content = "\
@p1\nACGTACGTAC\n+\nIIIIIIIIII\n\
@p2\nAC\n+\nII\n";
        let m1 = unique_temp("m1", "fq");
        let m2 = unique_temp("m2", "fq");
        std::fs::write(&m1, content).unwrap();
        std::fs::write(&m2, content).unwrap();
        let base = unique_temp("pout", "fq");
        let script = format!(
            "ngless '1.5'\n\
             input = paired('{}', '{}')\n\
             input = preprocess(input) using |read|:\n\
             \x20\x20\x20\x20if len(read) < 5:\n\
             \x20\x20\x20\x20\x20\x20\x20\x20discard\n\
             write(input, ofile='{}')\n",
            m1.display(),
            m2.display(),
            base.display()
        );
        run(&script).unwrap();
        let base_s = base.to_string_lossy().to_string();
        let o1 = format_fq_oname(&base_s, "pair.1").unwrap();
        let o2 = format_fq_oname(&base_s, "pair.2").unwrap();
        let singles = format_fq_oname(&base_s, "singles").unwrap();
        let out1 = std::fs::read_to_string(&o1).unwrap();
        let out2 = std::fs::read_to_string(&o2).unwrap();
        assert_eq!(out1, "@p1\nACGTACGTAC\n+\nIIIIIIIIII\n");
        assert_eq!(out2, "@p1\nACGTACGTAC\n+\nIIIIIIIIII\n");
        // No singletons survived, so the singles file is never written.
        assert!(!std::path::Path::new(&singles).exists());
        for p in [&m1, &m2] {
            let _ = std::fs::remove_file(p);
        }
        let _ = std::fs::remove_file(&o1);
        let _ = std::fs::remove_file(&o2);
    }
}
