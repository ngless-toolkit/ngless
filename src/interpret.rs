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
use std::time::Duration;

use crate::ast::*;
use crate::errors::{NgError, NgErrorType, NgResult};
use crate::fastq::{self, FastQEncoding, FastQFilePath, ReadSet, ShortRead};
use crate::lockfile::{with_lock_file, LockParameters, WhenExistsStrategy};
use crate::sam::{self, encode_sam_line, group_sam_stream, SamItem, SamLine, SamRecord};
use crate::select::{self, FilterAction, FilterOptions};
use crate::values::{eval_binary, eval_index, eval_unary, show_double, IndexValue, NGLessObject};

use serde::Deserialize;
use std::collections::BTreeMap;

/// A YAML sample manifest for `load_sample_list` (mirrors `SampleFile` in
/// BuiltinModules/Samples.hs). Each sample maps to a list of single-key entries (`{paired: [..]}`
/// or `{single: ..}`). `BTreeMap` keeps samples in the sorted order the Haskell aeson
/// `KeyMap.toList` produces.
#[derive(Deserialize)]
struct SampleFile {
    basedir: Option<String>,
    samples: BTreeMap<String, Vec<BTreeMap<String, serde_yaml::Value>>>,
}

/// Recognised FASTQ file extensions for directory loading (mirrors `exts`).
fn fastq_directory_exts() -> Vec<String> {
    let mut out = Vec::new();
    for fq in [".fq", ".fastq"] {
        for comp in ["", ".gz", ".bz2", ".xz"] {
            out.push(format!("{fq}{comp}"));
        }
    }
    out
}

/// Pair up FASTQ files in a directory by mate suffix (mirrors `matchUp`). Returns the leftover
/// singletons and the paired files (mate1, mate2, optional singles), preserving input order.
#[allow(clippy::type_complexity)]
fn match_up(fqfiles: &[String]) -> NgResult<(Vec<String>, Vec<(String, String, Option<String>)>)> {
    // Pair suffixes: the cross product of the extensions with the three mate-marker pairs.
    let mut paired_ends: Vec<(String, String)> = Vec::new();
    for ext in fastq_directory_exts() {
        for (s1, s2) in [(".1", ".2"), ("_1", "_2"), ("_F", "_R")] {
            paired_ends.push((format!("{s1}{ext}"), format!("{s2}{ext}")));
        }
    }
    // For a file ending in a mate-1 (or mate-2) suffix, recover the (mate1, mate2) base pair.
    let match1 = |fp: &str| -> Option<(String, String)> {
        for (p1, p2) in &paired_ends {
            if let Some(base) = fp.strip_suffix(p1.as_str()) {
                return Some((fp.to_string(), format!("{base}{p2}")));
            }
            if let Some(base) = fp.strip_suffix(p2.as_str()) {
                return Some((format!("{base}{p1}"), fp.to_string()));
            }
        }
        None
    };
    // `nub` over the matched pairs (both pair.1 and pair.2 map to the same record).
    let mut matched1: Vec<(String, String)> = Vec::new();
    for fp in fqfiles {
        if let Some(m) = match1(fp) {
            if !matched1.contains(&m) {
                matched1.push(m);
            }
        }
    }
    let exists = |f: &str| fqfiles.iter().any(|x| x == f);
    let mut paired = Vec::new();
    for (m1, m2) in matched1 {
        if !exists(&m1) {
            return Err(NgError::new(
                NgErrorType::DataError,
                format!("Cannot find match for file: {m2}"),
            ));
        }
        if !exists(&m2) {
            return Err(NgError::new(
                NgErrorType::DataError,
                format!("Cannot find match for file: {m1}"),
            ));
        }
        let singles = build_single(&m1);
        let singles = if exists(&singles) {
            Some(singles)
        } else {
            None
        };
        paired.push((m1, m2, singles));
    }
    // Files not used by any pair (incl. the matched singles) are singletons.
    let mut used: Vec<&str> = Vec::new();
    for (a, b, c) in &paired {
        used.push(a);
        used.push(b);
        if let Some(c) = c {
            used.push(c);
        }
    }
    let singletons = fqfiles
        .iter()
        .filter(|f| !used.contains(&f.as_str()))
        .cloned()
        .collect();
    Ok((singletons, paired))
}

/// Derive the singles-file name for a paired sample by replacing `pair.1`/`pair.2` with `single`
/// (mirrors `buildSingle`). Returns a marker that never matches when no `pair.N` is present.
fn build_single(m1: &str) -> String {
    if m1.contains("pair.1") {
        m1.replace("pair.1", "single")
    } else if m1.contains("pair.2") {
        m1.replace("pair.2", "single")
    } else {
        "MARKER_FOR_FILE_WHICH_DOES_NOT_EXIST".to_string()
    }
}

/// Deterministically keep a 1/10 sample of FASTQ records (mirrors `drop90`/`performSubsample`):
/// keep the first 4 of every 40 input lines (one record in ten), then take at most 100000 of the
/// kept lines.
fn subsample_text(text: &str) -> String {
    // Mirror conduit's `CB.lines`: split on `\n` and drop a single trailing empty fragment.
    let mut lines: Vec<&str> = text.split('\n').collect();
    if lines.last() == Some(&"") {
        lines.pop();
    }
    let mut out = String::new();
    let mut kept = 0usize;
    for (i, line) in lines.iter().enumerate() {
        if i % 40 < 4 {
            out.push_str(line);
            out.push('\n');
            kept += 1;
            if kept >= 100000 {
                break;
            }
        }
    }
    out
}

/// Tally one read-name group into the `aligned`/`unique` counters (the per-group body of
/// `samStatsC'`).
fn tally_group(group: &[&SamLine], aligned: &mut i64, unique: &mut i64) {
    let is_aligned = group.iter().any(|l| l.is_aligned());
    let same_rname = group.iter().all(|l| l.rname == group[0].rname);
    if is_aligned {
        *aligned += 1;
        if same_rname {
            *unique += 1;
        }
    }
}

/// Streaming `samStatsC'`: compute (total, aligned, unique) read groups directly from a SAM/BAM
/// file without holding it in memory (groups by consecutive read name, mirroring `sam_group_stats`).
fn sam_group_stats_stream(path: &str) -> NgResult<(i64, i64, i64)> {
    let (mut total, mut aligned, mut unique) = (0i64, 0i64, 0i64);
    for item in group_sam_stream(path, true)? {
        if let SamItem::Data(g) = item? {
            total += 1;
            let lines: Vec<&SamLine> = g.iter().map(|(s, _)| s).collect();
            tally_group(&lines, &mut aligned, &mut unique);
        }
    }
    Ok((total, aligned, unique))
}

/// Runtime values of the constants contributed by a standard module (mirrors `modConstants`).
/// Only the `example` module exposes any; the corresponding types live in
/// `modules::module_constants`.
pub fn module_constant_values(name: &str, _version: &str) -> Vec<(String, NGLessObject)> {
    match name {
        "example" => vec![
            ("EXAMPLE_0".to_string(), NGLessObject::Integer(0)),
            ("EXAMPLE_TRUE".to_string(), NGLessObject::Bool(true)),
            (
                "EXAMPLE_HELLO".to_string(),
                NGLessObject::String("Hello".to_string()),
            ),
        ],
        _ => Vec::new(),
    }
}

/// Convert an external-module argument default into a runtime value (mirrors `cargDef`).
fn default_to_object(d: &crate::external_modules::DefaultVal) -> NGLessObject {
    use crate::external_modules::DefaultVal;
    match d {
        DefaultVal::Bool(b) => NGLessObject::Bool(*b),
        DefaultVal::Sym(s) => NGLessObject::Symbol(s.clone()),
        DefaultVal::Int(i) => NGLessObject::Integer(*i),
        DefaultVal::Str(s) => NGLessObject::String(s.clone()),
    }
}

/// The NGLess data directories, in search order (user then global), mirroring `findDataFiles`'s
/// `User <|> Root`. Defined in `crate::reference`; re-exported here for callers that look it up
/// (e.g. external-module resolution in `cli.rs`).
pub use crate::reference::data_directories;

/// `example(input)`: the demo standard module function (mirrors `StandardModules.Example`).
/// Prints diagnostic information (including the Haskell-`show` of its arguments) and returns the
/// input unchanged.
fn execute_example(
    input: &NGLessObject,
    args: &[(String, NGLessObject)],
) -> NgResult<NGLessObject> {
    println!("Called example function 'example'");
    println!("First argument is {}", haskell_show(input));
    let kwargs: Vec<String> = args
        .iter()
        .map(|(k, v)| format!("({:?},{})", k, haskell_show(v)))
        .collect();
    println!("Keyword arguments are [{}]", kwargs.join(","));
    println!("This function does nothing except print this information");
    Ok(input.clone())
}

/// Render an `NGLessObject` in Haskell's derived-`Show` format (as produced by `show` on the
/// Haskell `NGLessObject`). Only the cases reachable from the `example` module are implemented.
fn haskell_show(v: &NGLessObject) -> String {
    match v {
        NGLessObject::String(s) => format!("{s:?}"),
        NGLessObject::Integer(i) => format!("NGOInteger {i}"),
        NGLessObject::Bool(b) => format!("NGOBool {}", if *b { "True" } else { "False" }),
        NGLessObject::Symbol(s) => format!("NGOSymbol {s:?}"),
        NGLessObject::ReadSet { name, readset } => {
            let fqpath = |f: &FastQFilePath| {
                let enc = match f.encoding {
                    FastQEncoding::Sanger => "SangerEncoding",
                    FastQEncoding::Solexa => "SolexaEncoding",
                };
                format!(
                    "FastQFilePath {{fqpathEncoding = {enc}, fqpathFilePath = {:?}}}",
                    f.path.to_string_lossy()
                )
            };
            let pairs: Vec<String> = readset
                .pairs
                .iter()
                .map(|(a, b)| format!("({},{})", fqpath(a), fqpath(b)))
                .collect();
            let singles: Vec<String> = readset.singletons.iter().map(fqpath).collect();
            format!(
                "NGOReadSet {name:?} (ReadSet {{pairedSamples = [{}], singleSamples = [{}]}})",
                pairs.join(","),
                singles.join(",")
            )
        }
        other => format!("{other:?}"),
    }
}

/// `discard_singles(rs)`: drop the singleton reads, keeping the pairs (mirrors
/// `executeDiscardSingles`).
fn execute_discard_singles(expr: &NGLessObject) -> NgResult<NGLessObject> {
    match expr {
        NGLessObject::ReadSet { name, readset } => Ok(NGLessObject::ReadSet {
            name: name.clone(),
            readset: ReadSet {
                pairs: readset.pairs.clone(),
                singletons: Vec::new(),
            },
        }),
        other => Err(NgError::should_not_occur(format!(
            "discard_singles expected a read set, got {other:?}"
        ))),
    }
}

/// `__check_ofile(path, original_lno=N)`: verify the output directory exists and is writable
/// (mirrors `checkOFile` + the `__check_ofile` arm of `executeChecks`). Inserted by
/// [`crate::transform::add_file_checks`] so that a write to a bad directory fails *before* any
/// preceding statement runs.
fn execute_check_ofile(
    expr: &NGLessObject,
    args: &[(String, NGLessObject)],
) -> NgResult<NGLessObject> {
    let oname = match expr {
        NGLessObject::String(s) => s.clone(),
        other => {
            return Err(NgError::should_not_occur(format!(
                "output file check expected a string, got {other:?}"
            )))
        }
    };
    let lno = match lookup_arg(args, "original_lno") {
        Some(NGLessObject::Integer(i)) => *i,
        _ => 0,
    };
    if let Some(err) = check_ofile(&oname) {
        return Err(NgError::new(
            NgErrorType::SystemError,
            format!("{err} (used in line {lno})."),
        ));
    }
    Ok(NGLessObject::Void)
}

/// `__check_ifile(path, original_lno=N)`: verify an input file exists and is readable (mirrors the
/// `__check_ifile` arm of `executeChecks`). Inserted by [`crate::transform::add_file_checks`] for
/// *non-constant* input paths, so a missing file is reported right after the variable it depends on
/// is bound rather than when the file is finally read. The literal path is checked (no search-path
/// expansion), matching Haskell.
fn execute_check_ifile(
    expr: &NGLessObject,
    args: &[(String, NGLessObject)],
) -> NgResult<NGLessObject> {
    // Mirrors `filepathOrTypeError`: the checked argument may be a plain path string, a
    // `Filename`, or a `SequenceSet` (e.g. `map(..., fafile=assemble(...))`), each of which
    // carries a file path to verify.
    let fname = match expr {
        NGLessObject::String(s) | NGLessObject::Filename(s) | NGLessObject::SequenceSet(s) => {
            s.clone()
        }
        other => {
            return Err(NgError::script(format!(
                "Expected a filepath (received {other:?}) in context 'checking input file'"
            )))
        }
    };
    let lno = match lookup_arg(args, "original_lno") {
        Some(NGLessObject::Integer(i)) => *i,
        _ => 0,
    };
    if let Some(err) = crate::suggestion::check_file_readable(&fname) {
        return Err(NgError::new(
            NgErrorType::SystemError,
            format!("{err} (used in line {lno})."),
        ));
    }
    Ok(NGLessObject::Void)
}

/// Check that `oname`'s directory exists and is writable (mirrors `checkOFile` in
/// `BuiltinModules/Checks.hs`). Returns an error message, or `None` when the file can be written.
pub(crate) fn check_ofile(oname: &str) -> Option<String> {
    let dirname = take_directory(oname);
    if !std::path::Path::new(&dirname).is_dir() {
        return Some(format!(
            "File name '{oname}' used as output, but directory {dirname} does not exist."
        ));
    }
    // `writable <$> getPermissions dirname`. `Permissions::readonly()` is true only when *no* write
    // bit is set, so this errs toward "writable" (never a false alarm for a user-owned directory).
    let writable = std::fs::metadata(&dirname)
        .map(|m| !m.permissions().readonly())
        .unwrap_or(false);
    if !writable {
        return Some(format!(
            "write call to file {oname}, but directory {dirname} is not writable."
        ));
    }
    None
}

/// `System.FilePath.takeDirectory`: the directory portion of a path, or "." when there is none.
fn take_directory(path: &str) -> String {
    match path.rfind('/') {
        None => ".".to_string(),
        Some(0) => "/".to_string(),
        Some(i) => path[..i].to_string(),
    }
}

/// Interpret a script body (already type-checked and validated). `temp_dir` is where
/// intermediate FASTQ files (e.g. from `preprocess`) are written.
#[allow(clippy::too_many_arguments)]
pub fn interpret(
    body: &[(usize, Expression)],
    temp_dir: &Path,
    keep_temporary_files: bool,
    script_text: &str,
    search_path: &[String],
    argv: &[String],
    subsample: bool,
    external_modules: Vec<crate::external_modules::ExternalModule>,
    constants: Vec<(String, NGLessObject)>,
    active_mappers: Vec<String>,
    ngl_version: (i64, i64),
) -> NgResult<()> {
    let mut env = HashMap::new();
    for (name, value) in constants {
        env.insert(name, value);
    }
    let mut interp = Interpreter {
        env,
        temp_files: crate::tempfiles::TempFiles::new(temp_dir, keep_temporary_files),
        cur_lno: Cell::new(0),
        fq_stats: RefCell::new(Vec::new()),
        map_stats: RefCell::new(Vec::new()),
        script_text: script_text.to_string(),
        search_path: search_path.to_vec(),
        argv: argv.to_vec(),
        subsample,
        external_modules,
        active_mappers,
        ngl_version,
        held_locks: Vec::new(),
    };
    for (lno, e) in body {
        interp.cur_lno.set(*lno);
        crate::output::trace(*lno, &format!("Interpreting [{lno}]: {e:?}"));
        interp.interpret_top(e)?;
    }
    crate::output::info(0, "Interpretation finished.");
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

/// Collected per-`map()`-call mapping statistics (mirrors `MappingInfo`). `qcstats({mapping})`
/// serialises these. The `input_file`/`reference` fields hold temp/index paths exactly as the
/// Haskell code records them, so this output is not byte-reproducible (no test diffs it).
struct MapInfo {
    lno: usize,
    input_file: String,
    reference: String,
    total: i64,
    aligned: i64,
    unique: i64,
}

struct Interpreter {
    env: HashMap<String, NGLessObject>,
    /// Registry for all temporary files/directories created during the run. Allocating
    /// through it guarantees exclusive (O_EXCL) creation, legal names, and removal of
    /// every temp at the end of the run (unless `--keep-temporary-files`) — the latter
    /// happens when the interpreter, and hence this field, is dropped.
    temp_files: crate::tempfiles::TempFiles,
    /// Line number of the top-level statement currently executing (used for `preproc.lnoN`).
    cur_lno: Cell<usize>,
    fq_stats: RefCell<Vec<FqInfo>>,
    /// Collected mapping statistics, one entry per `map()` call (mirrors the `mapOutput`
    /// accumulator). `qcstats({mapping})` serialises these.
    map_stats: RefCell<Vec<MapInfo>>,
    /// The original (verbatim) script source, used by `write(..., auto_comments=[{script}])`
    /// (mirrors `ngleScriptText`).
    script_text: String,
    /// Reference search path (`--search-path`), used to resolve `<references>` placeholders in
    /// `map(..., fafile=...)` (mirrors `nConfSearchPath`).
    search_path: Vec<String>,
    /// The command-line arguments exposed as the `ARGV` constant (`[script_path, ...extra]`,
    /// mirroring `nConfArgv`/`builtin.argv`).
    argv: Vec<String>,
    /// `--subsample`: keep a deterministic 1/10 of reads on FASTQ load (mirrors
    /// `nConfSubsample`).
    subsample: bool,
    /// Loaded external YAML modules, used to dispatch their command functions.
    external_modules: Vec<crate::external_modules::ExternalModule>,
    /// Mappers that may be requested by `map(mapper=...)` (mirrors `ngleMappersActive`). Always
    /// includes `bwa`; `minimap2` is added when the `minimap2` module is imported.
    active_mappers: Vec<String>,
    /// The script's `(major, minor)` language version, used to build the version-namespaced
    /// reference-download URL (mirrors `ngleVersion` in the `installData` URL construction).
    ngl_version: (i64, i64),
    /// Lock-file guards held by `lock1`/`run_for_all` for the duration of the run (mirrors the
    /// `ReleaseKey`s registered in `executeLock1OrForAll`). Held here so the claimed `.lock` file
    /// stays in place while the entry is processed and is released (removed) when the interpreter is
    /// dropped at the end of the run.
    held_locks: Vec<crate::lockfile::LockGuard>,
}

/// Which external mapper a `map()` call uses (mirrors the `Mapper` dispatch in `Map.hs`). Only
/// bwa and minimap2 are implemented; `soap` is not.
#[derive(Clone, Copy, PartialEq, Eq)]
enum MapperKind {
    Bwa,
    Minimap2,
}

impl MapperKind {
    /// Whether a complete index already exists for `fafile` (dispatches `hasValidIndex`).
    fn has_valid_index(self, fafile: &str) -> NgResult<bool> {
        match self {
            MapperKind::Bwa => crate::mapper::has_valid_index(fafile),
            MapperKind::Minimap2 => crate::minimap2::has_valid_index(fafile),
        }
    }

    /// Build the index for `fafile` (dispatches `createIndex`).
    fn create_index(self, fafile: &str) -> NgResult<()> {
        match self {
            MapperKind::Bwa => crate::mapper::create_index(fafile),
            MapperKind::Minimap2 => crate::minimap2::create_index(fafile),
        }
    }

    /// Map `interleaved` reads against `ref_index`, writing SAM to `out_sam` (dispatches
    /// `callMapper`).
    fn call_mapper(
        self,
        ref_index: &str,
        rs: &ReadSet,
        extra_args: &[String],
        out_sam: &Path,
    ) -> NgResult<()> {
        match self {
            MapperKind::Bwa => crate::mapper::call_mapper(ref_index, rs, extra_args, out_sam),
            MapperKind::Minimap2 => {
                crate::minimap2::call_mapper(ref_index, rs, extra_args, out_sam)
            }
        }
    }
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
                "ARGV" => Ok(NGLessObject::List(
                    self.argv
                        .iter()
                        .cloned()
                        .map(NGLessObject::String)
                        .collect(),
                )),
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
            // A function call in expression position (e.g. `read_int(s)` inside an `__assert`).
            // Only pure functions are valid here; IO functions error clearly via `execute_function`.
            Expression::FunctionCall(f, expr, args, None) => {
                let expr_v = self.eval(expr, overlay)?;
                let mut argvs = Vec::new();
                for (Variable(v), e) in args {
                    argvs.push((v.clone(), self.eval(e, overlay)?));
                }
                execute_function(f, &expr_v, &argvs)
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
    ) -> NgResult<IndexValue> {
        match ix {
            Index::One(a) => Ok(IndexValue::One(self.eval(a, overlay)?)),
            Index::Two(a, b) => {
                let a = match a {
                    Some(a) => Some(self.eval(a, overlay)?),
                    None => None,
                };
                let b = match b {
                    Some(b) => Some(self.eval(b, overlay)?),
                    None => None,
                };
                Ok(IndexValue::Two(a, b))
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
            "fastq" => self.execute_fastq(&expr_v, &argvs),
            "paired" => self.execute_paired(&expr_v, &argvs),
            "group" => self.execute_group(&expr_v, &argvs),
            "load_fastq_directory" | "load_mocat_sample" => {
                self.execute_load_directory(&expr_v, &argvs)
            }
            "load_sample_list" => self.execute_load_sample_list(&expr_v),
            "discard_singles" => execute_discard_singles(&expr_v),
            "unique" => self.execute_unique(&expr_v, &argvs),
            "example" => execute_example(&expr_v, &argvs),
            "samfile" => self.execute_samfile(&expr_v, &argvs),
            "map" => self.execute_map(&expr_v, &argvs),
            "as_reads" => self.execute_as_reads(&expr_v),
            "qcstats" => self.execute_qcstats(&expr_v),
            "samtools_sort" => self.execute_samtools_sort(&expr_v, &argvs),
            "samtools_view" => self.execute_samtools_view(&expr_v, &argvs),
            "count" => self.execute_count(&expr_v, &argvs),
            "countfile" => self.execute_countfile(&expr_v),
            "mapstats" => self.execute_mapstats(&expr_v),
            "__merge_samfiles" => self.execute_merge_sams(&expr_v),
            "write" => self.execute_write(&expr_v, &argvs),
            "lock1" | "run_for_all" | "run_for_all_samples" => self.execute_lock1(&expr_v, &argvs),
            "collect" => self.execute_collect(&expr_v, &argvs),
            "__paste" => execute_paste(&expr_v, &argvs),
            "assemble" => self.execute_assemble(&expr_v, &argvs),
            "orf_find" => self.execute_orf_find(&expr_v, &argvs),
            "__check_ofile" => execute_check_ofile(&expr_v, &argvs),
            "__check_ifile" => execute_check_ifile(&expr_v, &argvs),
            other => {
                if self
                    .external_modules
                    .iter()
                    .any(|m| m.find_command(other).is_some())
                {
                    self.execute_external_command(other, &expr_v, &argvs)
                } else {
                    execute_function(f, &expr_v, &argvs)
                }
            }
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

        // Stream the SAM in read-name groups (mirroring the conduit pipeline) to keep memory
        // bounded — no more than one read group is held at a time. All `@SQ` headers precede the
        // alignment records, so we eagerly consume the leading header items into `sq_header` (which
        // `perform_count` needs up front to build its annotators), stopping at the first data group,
        // then feed the remaining groups lazily.
        let mut stream = group_sam_stream(&path.to_string_lossy(), true)?;
        let mut sq_header: Vec<(String, i64)> = Vec::new();
        let mut first_group: Option<Vec<SamLine>> = None;
        for item in stream.by_ref() {
            match item? {
                SamItem::Header(h) => {
                    if let Some(sq) = parse_sq_header(&h) {
                        sq_header.push(sq);
                    }
                }
                SamItem::Data(g) => {
                    first_group = Some(g.into_iter().map(|(s, _)| s).collect());
                    break;
                }
            }
        }
        // Any header appearing after the first alignment is not valid SAM and is ignored (it cannot
        // retroactively change the already-built annotators).
        let rest = stream.filter_map(|item| match item {
            Ok(SamItem::Header(_)) => None,
            Ok(SamItem::Data(g)) => Some(Ok(g.into_iter().map(|(s, _)| s).collect())),
            Err(e) => Some(Err(e)),
        });
        let groups = first_group.map(Ok).into_iter().chain(rest);

        let tsv = crate::count::perform_count(groups, &sq_header, &name, &opts)?;
        let out = self.new_temp_path("counts.", "txt")?;
        std::fs::write(&out, tsv).map_err(|e| {
            NgError::new(
                NgErrorType::SystemError,
                format!("Could not write counts file: {e}"),
            )
        })?;
        Ok(NGLessObject::Counts(out))
    }

    /// `mapstats(mapped)`: summarize a mapped read set as total / aligned / unique read groups
    /// (mirrors `executeMapStats` + `samStatsC'`). Reads are grouped by name (both mates together);
    /// a group is *aligned* if any read is aligned, and *unique* if it is aligned and all its
    /// records share one reference name.
    fn execute_mapstats(&self, expr: &NGLessObject) -> NgResult<NGLessObject> {
        let (name, path) = mapped_read_set(expr, "mapstats")?;
        let (total, aligned, unique) = sam_group_stats_stream(&path.to_string_lossy())?;

        let tsv = format!("\t{name}\ntotal\t{total}\naligned\t{aligned}\nunique\t{unique}\n");
        let out = self.new_temp_path("sam_stats_", "stats")?;
        std::fs::write(&out, tsv).map_err(|e| {
            NgError::new(
                NgErrorType::SystemError,
                format!("Could not write mapstats file: {e}"),
            )
        })?;
        Ok(NGLessObject::Counts(out))
    }

    /// `countfile(fname)`: reference an existing counts TSV, reordering its rows by tag (the first
    /// column) when they are not already sorted (mirrors `executeCountFile`). Leading comment lines
    /// and the sample-name header line are kept in place; only the data rows are sorted.
    fn execute_countfile(&self, expr: &NGLessObject) -> NgResult<NGLessObject> {
        let fname = as_string(expr, "countfile")?;
        let text = crate::compression::read_to_string(&fname)?;
        if is_tag_ordered(&text) {
            return Ok(NGLessObject::Counts(PathBuf::from(fname)));
        }
        let out = self.new_temp_path("normalized", "tsv")?;
        std::fs::write(&out, normalize_count_file(&text)).map_err(|e| {
            NgError::new(
                NgErrorType::SystemError,
                format!("Could not write normalized counts file: {e}"),
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
        let path = self.new_temp_path(prefix, "fq")?;
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

    /// `map(reads, fafile=/reference=, mode_all=, mapper=, block_size_megabases=, __extra_args=)`:
    /// map a read set against a reference (mirrors `executeMap`). This build supports the `bwa`
    /// and `minimap2` mappers; the `soap` mapper is not implemented. Packaged `reference=`
    /// databases must already be installed locally (no download).
    fn execute_map(
        &self,
        expr: &NGLessObject,
        args: &[(String, NGLessObject)],
    ) -> NgResult<NGLessObject> {
        // Auto-comprehension: `map` over a list of read sets returns a list (mirrors the
        // `NGOList` arm of `executeMap'`).
        if let NGLessObject::List(es) = expr {
            let mapped = es
                .iter()
                .map(|e| self.execute_map(e, args))
                .collect::<NgResult<Vec<_>>>()?;
            return Ok(NGLessObject::List(mapped));
        }
        let (name, rs) = match expr {
            NGLessObject::ReadSet { name, readset } => (name.clone(), readset.clone()),
            other => {
                return Err(NgError::should_not_occur(format!(
                    "map expects ReadSet, got {other:?}"
                )))
            }
        };

        // getMapper: the requested mapper must be active (bwa always is; minimap2 requires the
        // `minimap2` import). Resolve it to a `MapperKind` for dispatch.
        let mapper = lookup_opt_string(args, "mapper").unwrap_or_else(|| "bwa".to_string());
        if !self.active_mappers.iter().any(|m| m == &mapper) {
            return Err(NgError::script(format!(
                "Requested mapper '{mapper}' is not active."
            )));
        }
        let mapper = match mapper.as_str() {
            "bwa" => MapperKind::Bwa,
            "minimap2" => MapperKind::Minimap2,
            other => {
                return Err(NgError::script(format!(
                    "map(): mapper '{other}' is not supported in this build yet (only 'bwa' and \
                     'minimap2' are)."
                )))
            }
        };

        // lookupReference: exactly one of `reference`/`fafile` (mirrors `lookupReference`).
        let reference = lookup_opt_string(args, "reference");
        let fafile = lookup_opt_string(args, "fafile");
        let fafile = match (reference, fafile) {
            (None, None) => {
                return Err(NgError::script("Either reference or fafile must be passed"))
            }
            (Some(_), Some(_)) => {
                return Err(NgError::script(
                    "Reference and fafile cannot be used simmultaneously",
                ))
            }
            // A packaged `reference=` is resolved to its FASTA in the data directories; the FASTA
            // then flows through the same indexing/mapping path as `fafile=`.
            (Some(refname), None) => self.resolve_reference(&refname)?,
            (None, Some(fa)) => self.expand_path(&fa)?,
        };

        let mode_all = lookup_bool(args, "mode_all", false)?;
        let mut bwa_args = lookup_string_list(args, "__extra_args")?.unwrap_or_default();
        if mode_all {
            bwa_args.push("-a".to_string());
        }
        let block_size = lookup_int(args, "block_size_megabases", 0)?;

        self.perform_map(mapper, &fafile, block_size, &name, &rs, &bwa_args)
    }

    /// Resolve a packaged `reference=` database name to its FASTA file (mirrors
    /// `ensureDataPresent`/`findDataFiles`). The reference directory is named by the user-typed
    /// name (so `sacCer3` and its alias `Saccharomyces_cerevisiae_R64-1-1` resolve to separate,
    /// independently-installed directories) and is searched for in the user data directory first,
    /// then the global one. A missing builtin reference is downloaded from the configured base URL
    /// (mirrors `installData`), using the script's language version for the URL's version directory.
    fn resolve_reference(&self, refname: &str) -> NgResult<String> {
        crate::reference::ensure_data_present(refname, self.ngl_version)
    }

    /// Run an external-module command function (mirrors `executeCommand`): encode the unnamed input
    /// argument and the named arguments onto the command line, run the module's executable with the
    /// module environment, and decode the result.
    fn execute_external_command(
        &self,
        funcname: &str,
        input: &NGLessObject,
        args: &[(String, NGLessObject)],
    ) -> NgResult<NGLessObject> {
        // Locate the module + command (cloned to release the borrow on `self`).
        let (module_dir, cmd) = self
            .external_modules
            .iter()
            .find_map(|m| m.find_command(funcname).map(|c| (m.dir.clone(), c.clone())))
            .ok_or_else(|| {
                NgError::should_not_occur(format!("Call to undefined function {funcname}."))
            })?;

        let mut cmdline: Vec<String> = self.encode_command_arg(&cmd.arg1, Some(input))?;
        for a in &cmd.additional {
            let value = lookup_arg(args, &a.name);
            cmdline.extend(self.encode_command_arg(a, value)?);
        }

        // A non-void return adds a `--name=<tempfile>` output argument.
        let out_file = match cmd.ret.rtype {
            NGLType::Void => None,
            _ => {
                let ext = if cmd.ret.extension.is_empty() {
                    "out"
                } else {
                    &cmd.ret.extension
                };
                let path = self.new_temp_path("eout_", ext)?;
                std::fs::write(&path, b"").ok();
                let p = path.to_string_lossy().into_owned();
                cmdline.push(format!("--{}={p}", cmd.ret.name));
                Some(p)
            }
        };

        let exe = module_dir.join(&cmd.arg0);
        let output = std::process::Command::new(&exe)
            .args(&cmdline)
            .envs(crate::external_modules::module_env(
                &module_dir,
                self.temp_files.dir(),
            ))
            .output()
            .map_err(|e| {
                NgError::new(
                    NgErrorType::SystemError,
                    format!("Error running command for function {funcname}: {e}"),
                )
            })?;
        if !output.status.success() {
            return Err(NgError::new(
                NgErrorType::SystemError,
                format!(
                    "Error running command for function {funcname}\n\texit code = {}\n\tstdout='{}'\n\tstderr='{}'",
                    output.status.code().unwrap_or(-1),
                    String::from_utf8_lossy(&output.stdout),
                    String::from_utf8_lossy(&output.stderr),
                ),
            ));
        }

        let group_name = match input {
            NGLessObject::ReadSet { name, .. } => name.clone(),
            NGLessObject::MappedReadSet { name, .. } => name.clone(),
            _ => String::new(),
        };
        match (out_file, &cmd.ret.rtype) {
            (None, _) => Ok(NGLessObject::Void),
            (Some(p), NGLType::Counts) => Ok(NGLessObject::Counts(PathBuf::from(p))),
            (Some(p), NGLType::MappedReadSet) => Ok(NGLessObject::MappedReadSet {
                name: group_name,
                path: PathBuf::from(p),
            }),
            (Some(_), other) => Err(NgError::should_not_occur(format!(
                "External module return type {other:?} is not implemented in this build yet."
            ))),
        }
    }

    /// Encode one command argument onto the command line (mirrors `encodeArgument`).
    fn encode_command_arg(
        &self,
        arg: &crate::external_modules::CommandArg,
        value: Option<&NGLessObject>,
    ) -> NgResult<Vec<String>> {
        use crate::external_modules::ArgKind;

        // Fall back to the declared default when no value is supplied (mirrors the `Nothing` arm).
        let owned_default = value
            .is_none()
            .then(|| arg.default.as_ref().map(default_to_object));
        let value: Option<&NGLessObject> = match value {
            Some(v) => Some(v),
            None => match &owned_default {
                Some(Some(v)) => Some(v),
                _ => {
                    if arg.required {
                        return Err(NgError::script(format!(
                            "Missing value for required argument {}.",
                            arg.name
                        )));
                    }
                    return Ok(Vec::new());
                }
            },
        };
        let value = value.unwrap();

        match arg.kind {
            ArgKind::Flag => {
                let on = matches!(value, NGLessObject::Bool(true));
                if !on {
                    Ok(Vec::new())
                } else if let Some(flags) = &arg.when_true {
                    Ok(flags.clone())
                } else {
                    Ok(vec![format!("--{}", arg.name)])
                }
            }
            ArgKind::ReadSet => self.encode_readset_files(value),
            _ => {
                let as_str = match arg.kind {
                    ArgKind::Str => {
                        let s = as_string(value, "external module")?;
                        if arg.expand_searchpath {
                            self.expand_path_or_self(&s)
                        } else {
                            s
                        }
                    }
                    ArgKind::Option => match value {
                        NGLessObject::Symbol(s) => s.clone(),
                        other => {
                            return Err(NgError::script(format!(
                                "Expected a symbol in external module, got {other:?}"
                            )))
                        }
                    },
                    ArgKind::Int => match value {
                        NGLessObject::Integer(i) => i.to_string(),
                        other => {
                            return Err(NgError::script(format!(
                                "Expected an integer in external module, got {other:?}"
                            )))
                        }
                    },
                    ArgKind::MappedReadSet => match value {
                        NGLessObject::MappedReadSet { path, .. } => {
                            path.to_string_lossy().into_owned()
                        }
                        other => {
                            return Err(NgError::script(format!(
                                "Expected a mapped read set in external module, got {other:?}"
                            )))
                        }
                    },
                    ArgKind::Counts => match value {
                        NGLessObject::Counts(p) => p.to_string_lossy().into_owned(),
                        other => {
                            return Err(NgError::script(format!(
                                "Expected counts in external module, got {other:?}"
                            )))
                        }
                    },
                    _ => unreachable!("flag/readset handled above"),
                };
                Ok(if arg.name.is_empty() {
                    vec![as_str]
                } else {
                    vec![format!("--{}={as_str}", arg.name)]
                })
            }
        }
    }

    /// Encode a read set as command-line file paths (mirrors `asFilePaths`): the paired mate-1 and
    /// mate-2 files and/or the singletons are each concatenated to a temp file. With only
    /// singletons the result is `[singles]`; with pairs it is `[mate1, mate2]` (plus `[singles]`).
    fn encode_readset_files(&self, value: &NGLessObject) -> NgResult<Vec<String>> {
        let rs = match value {
            NGLessObject::ReadSet { readset, .. } => readset,
            other => {
                return Err(NgError::script(format!(
                    "Expected readset for argument in function call, got {other:?}"
                )))
            }
        };
        let concat = |files: Vec<&FastQFilePath>| -> NgResult<Option<String>> {
            if files.is_empty() {
                return Ok(None);
            }
            let out = self.new_temp_path("module_in_", "fq")?;
            let o = out.to_string_lossy().into_owned();
            write_fq_files(&files, &o)?;
            Ok(Some(o))
        };
        let fq1 = concat(rs.pairs.iter().map(|(a, _)| a).collect())?;
        let fq2 = concat(rs.pairs.iter().map(|(_, b)| b).collect())?;
        let fq3 = concat(rs.singletons.iter().collect())?;
        match (fq1, fq2, fq3) {
            (None, None, Some(f)) => Ok(vec![f]),
            (Some(f1), Some(f2), None) => Ok(vec![f1, f2]),
            (Some(f1), Some(f2), Some(f3)) => Ok(vec![f1, f2, f3]),
            _ => Err(NgError::script(
                "Malformed input argument to external module",
            )),
        }
    }

    /// Expand `<...>` search-path placeholders, falling back to the original string when no
    /// expansion resolves to an existing file (mirrors `fromMaybe str <$> expandPath str`).
    fn expand_path_or_self(&self, s: &str) -> String {
        for candidate in expand_path_candidates(s, &self.search_path) {
            if Path::new(&candidate).is_file() {
                return candidate;
            }
        }
        s.to_string()
    }

    /// Build (lazily) the needed bwa index(es) and map `rs` against them (mirrors `performMap`).
    /// With no block size there is a single index; otherwise the FASTA is split into blocks, each
    /// indexed and mapped, and the partial SAMs are merged (best-only).
    fn perform_map(
        &self,
        mapper: MapperKind,
        fafile: &str,
        block_size: i64,
        name: &str,
        rs: &ReadSet,
        extra_args: &[String],
    ) -> NgResult<NGLessObject> {
        let refs = self.ensure_index_exists(mapper, block_size, fafile)?;
        let (path, reference) = match refs.as_slice() {
            [single] => (
                self.map_to_reference(mapper, single, rs, extra_args)?,
                single.clone(),
            ),
            blocks => {
                let mut partials = Vec::with_capacity(blocks.len());
                for b in blocks {
                    partials.push(self.map_to_reference(mapper, b, rs, extra_args)?);
                }
                (self.merge_sam_files(&partials)?, fafile.to_string())
            }
        };
        // Record mapping statistics for `qcstats({mapping})` (mirrors `outputMappedSetStatistics`).
        self.register_map_stats(&path, &reference)?;
        Ok(NGLessObject::MappedReadSet {
            name: name.to_string(),
            path,
        })
    }

    /// Compute (total, aligned, unique) read groups for a freshly-mapped SAM and record them in the
    /// mapping-stats accumulator (mirrors the `addMapOutput` in `outputMappedSetStatistics`).
    fn register_map_stats(&self, sam: &Path, reference: &str) -> NgResult<()> {
        let (total, aligned, unique) = sam_group_stats_stream(&sam.to_string_lossy())?;
        self.map_stats.borrow_mut().push(MapInfo {
            lno: self.cur_lno.get(),
            input_file: sam.to_string_lossy().into_owned(),
            reference: reference.to_string(),
            total,
            aligned,
            unique,
        });
        Ok(())
    }

    /// Ensure a bwa index exists for `fafile` (or for each of its splits) and return the FASTA
    /// path(s) to map against (mirrors `ensureIndexExists`). Index creation is lock-guarded with a
    /// `<fafile>.ngless-index.lock` file so concurrent NGLess processes mapping against the same
    /// reference do not build the index on top of each other (mirrors the `withLockFile` around
    /// `createIndex`).
    fn ensure_index_exists(
        &self,
        mapper: MapperKind,
        block_size: i64,
        fafile: &str,
    ) -> NgResult<Vec<String>> {
        if block_size <= 0 {
            self.ensure_one_index_exists(mapper, fafile)?;
            Ok(vec![fafile.to_string()])
        } else {
            let blocks = self.ensure_splits_exist(block_size, fafile)?;
            for b in &blocks {
                self.ensure_one_index_exists(mapper, b)?;
            }
            Ok(blocks)
        }
    }

    /// Build the index for a single FASTA under a lock if it is missing, rechecking once the lock is
    /// held (mirrors `ensureIndexExists 0`: the index may have been built by another process while
    /// we waited for the lock).
    fn ensure_one_index_exists(&self, mapper: MapperKind, fafile: &str) -> NgResult<()> {
        if mapper.has_valid_index(fafile)? {
            return Ok(());
        }
        let params = LockParameters {
            lock_fname: PathBuf::from(format!("{fafile}.ngless-index.lock")),
            max_age: Duration::from_secs(36 * 3600),
            when_exists: WhenExistsStrategy::IfLockedRetry {
                nr_retries: 37 * 60,
                time_between: Duration::from_secs(60),
            },
            mtime_update: true,
        };
        with_lock_file(params, || {
            // Recheck under the lock: the index may have been created while we slept.
            if !mapper.has_valid_index(fafile)? {
                mapper.create_index(fafile)?;
            }
            Ok(())
        })
    }

    /// Map `rs` against one indexed reference, returning the SAM temp file (mirrors
    /// `mapToReference`). The reads are interleaved and streamed to the mapper on stdin.
    fn map_to_reference(
        &self,
        mapper: MapperKind,
        ref_index: &str,
        rs: &ReadSet,
        extra_args: &[String],
    ) -> NgResult<PathBuf> {
        let out = self.new_temp_path("mapped_", "sam")?;
        mapper.call_mapper(ref_index, rs, extra_args, &out)?;
        Ok(out)
    }

    /// Split `fafile` into block-sized FASTA chunks if not already done, returning the chunk paths
    /// (mirrors `ensureSplitsExist`). A `.done` receipt file records completion so repeated runs
    /// reuse existing splits.
    fn ensure_splits_exist(&self, block_size: i64, fafile: &str) -> NgResult<Vec<String>> {
        let p = Path::new(fafile);
        let stem = p
            .file_stem()
            .map(|s| s.to_string_lossy().to_string())
            .unwrap_or_default();
        let base_name = format!("{stem}.splits_{block_size}m");
        let ofa_base = match p.parent() {
            Some(dir) if !dir.as_os_str().is_empty() => {
                dir.join(&base_name).to_string_lossy().to_string()
            }
            _ => base_name.clone(),
        };
        let receipt = format!("{ofa_base}.done");
        if Path::new(&receipt).exists() {
            // Reuse: collect the existing `<base>.*.fna` chunks, sorted lexicographically (as
            // Haskell's `sort . namesMatching` does).
            let dir = p.parent().filter(|d| !d.as_os_str().is_empty());
            let prefix = format!("{base_name}.");
            let mut found = Vec::new();
            let read_dir = match dir {
                Some(d) => std::fs::read_dir(d),
                None => std::fs::read_dir("."),
            }
            .map_err(|e| {
                NgError::new(
                    NgErrorType::SystemError,
                    format!("Could not list split directory: {e}"),
                )
            })?;
            for entry in read_dir.flatten() {
                let fname = entry.file_name().to_string_lossy().to_string();
                if fname.starts_with(&prefix) && fname.ends_with(".fna") {
                    let full = match dir {
                        Some(d) => d.join(&fname).to_string_lossy().to_string(),
                        None => fname.clone(),
                    };
                    found.push(full);
                }
            }
            found.sort();
            return Ok(found);
        }
        // Guard the (expensive) split with a lock so concurrent processes splitting the same
        // reference do not stomp on each other (mirrors the `withLockFile` in `splitFASTA`).
        let params = LockParameters {
            lock_fname: PathBuf::from(format!("{fafile}.{block_size}m.split.lock")),
            max_age: Duration::from_secs(36 * 3000),
            when_exists: WhenExistsStrategy::IfLockedRetry {
                nr_retries: 120,
                time_between: Duration::from_secs(60),
            },
            mtime_update: true,
        };
        let splits = with_lock_file(params, || split_fasta(block_size, fafile, &ofa_base))?;
        std::fs::write(
            &receipt,
            format!("FASTA file '{fafile}' split into blocks of {block_size} megabases.\n"),
        )
        .map_err(|e| {
            NgError::new(
                NgErrorType::SystemError,
                format!("Could not write split receipt {receipt}: {e}"),
            )
        })?;
        Ok(splits)
    }

    /// Merge partial SAM files from block mapping, keeping the best alignment(s) per read (mirrors
    /// `mergeSamFiles` + `mergeSAMGroups MSBestOnly`). Headers from every partial are preserved.
    fn merge_sam_files(&self, partials: &[PathBuf]) -> NgResult<PathBuf> {
        let mut headers: Vec<String> = Vec::new();
        let mut per_file: Vec<Vec<Vec<SamLine>>> = Vec::new();
        for p in partials {
            let mut groups = Vec::new();
            for item in group_sam_stream(&p.to_string_lossy(), true)? {
                match item? {
                    SamItem::Header(h) => headers.push(h),
                    SamItem::Data(g) => groups.push(g.into_iter().map(|(s, _)| s).collect()),
                }
            }
            per_file.push(groups);
        }
        let mut out = String::new();
        for h in &headers {
            out.push_str(h);
            out.push('\n');
        }
        let ngroups = per_file.first().map(|g| g.len()).unwrap_or(0);
        if per_file.iter().any(|g| g.len() != ngroups) {
            return Err(NgError::new(
                NgErrorType::DataError,
                "Merging unsynced SAM files (not implemented yet)".to_string(),
            ));
        }
        for gi in 0..ngroups {
            let combined: Vec<SamLine> = per_file.iter().flat_map(|g| g[gi].clone()).collect();
            for l in merge_sam_group(&combined)? {
                out.push_str(&encode_sam_line(&l));
                out.push('\n');
            }
        }
        self.write_sam_temp(&out)
    }

    /// `__merge_samfiles([f1, f2, ...])`: merge several SAM files into one mapped read set
    /// (mirrors `executeMergeSams`). The argument is a list of file paths; merging keeps the
    /// headers from every input and resolves per-read-name groups best-only.
    fn execute_merge_sams(&self, expr: &NGLessObject) -> NgResult<NGLessObject> {
        let files = match expr {
            NGLessObject::List(items) => items
                .iter()
                .map(|o| match o {
                    NGLessObject::String(s) => Ok(PathBuf::from(s)),
                    other => Err(NgError::script(format!(
                        "__merge_samfiles: expected a string, got {other:?}"
                    ))),
                })
                .collect::<NgResult<Vec<_>>>()?,
            other => {
                return Err(NgError::script(format!(
                    "Wrong argument for internal function __merge_samfiles: {other:?}"
                )))
            }
        };
        let path = self.merge_sam_files(&files)?;
        Ok(NGLessObject::MappedReadSet {
            name: "test".to_string(),
            path,
        })
    }

    /// Resolve a FASTA path through the search path, expanding `<references>`-style placeholders
    /// (mirrors `expandPath` + `indexReference`). Errors if no candidate exists on disk.
    fn expand_path(&self, fbase: &str) -> NgResult<String> {
        for candidate in expand_path_candidates(fbase, &self.search_path) {
            if Path::new(&candidate).exists() {
                return Ok(candidate);
            }
        }
        Err(NgError::new(
            NgErrorType::DataError,
            format!("Could not find FASTA file: {fbase}"),
        ))
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
        let outpath = self.new_temp_path("selected_", "sam")?;
        let mut out = sam_temp_writer(&outpath)?;
        for item in group_sam_stream(&path.to_string_lossy(), paired)? {
            match item? {
                SamItem::Header(line) => {
                    writeln!(out, "{line}").map_err(sam_write_err)?;
                }
                SamItem::Data(group) => {
                    let filtered = select::apply_conditions(&cond, group.clone());
                    for line in reinject_call(&group, filtered)? {
                        writeln!(out, "{line}").map_err(sam_write_err)?;
                    }
                }
            }
        }
        out.flush().map_err(sam_write_err)?;
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
        let outpath = self.new_temp_path("selected_", "sam")?;
        let mut out = sam_temp_writer(&outpath)?;
        for item in group_sam_stream(&path.to_string_lossy(), paired)? {
            match item? {
                SamItem::Header(line) => {
                    writeln!(out, "{line}").map_err(sam_write_err)?;
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
                        writeln!(out, "{}", encode_sam_line(&l)).map_err(sam_write_err)?;
                    }
                }
            }
        }
        out.flush().map_err(sam_write_err)?;
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
        let path = self.new_temp_path("selected_", "sam")?;
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
        let out = self.new_temp_path("sorted_", "sam")?;
        let temp_prefix = self.new_temp_path("samtools_sort_temp", "tmp")?;
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
        let out = self.new_temp_path("subset_", "sam")?;
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
        execute_write(expr, args, &self.script_text, self.subsample)
    }

    /// `lock1`/`run_for_all`/`run_for_all_samples` (mirrors `executeLock1OrForAll`): given a list
    /// of strings (or read sets), atomically claim one entry by creating a `.lock` file in a hash
    /// directory under `ngless-locks/`, and return the claimed entry (unsanitized).
    fn execute_lock1(
        &mut self,
        expr: &NGLessObject,
        args: &[(String, NGLessObject)],
    ) -> NgResult<NGLessObject> {
        let entries = match expr {
            NGLessObject::List(items) => items,
            other => {
                return Err(NgError::script(format!(
                    "Wrong argument for lock1 (expected a list, got `{other:?}`)"
                )))
            }
        };
        if entries.is_empty() {
            return Err(NgError::new(
                NgErrorType::DataError,
                "Cannot run on empty list",
            ));
        }
        let names: Vec<String> = entries
            .iter()
            .map(entry_name)
            .collect::<NgResult<Vec<String>>>()?;
        let hash = string_arg(args, "__hash").unwrap_or_default();
        let tag = string_arg(args, "__parallel_tag")
            .or_else(|| string_arg(args, "tag"))
            .unwrap_or_default();
        let prefix = if tag.is_empty() {
            String::new()
        } else {
            format!("{tag}-")
        };
        let lockdir = self.setup_hash_directory(&prefix, "ngless-locks", &hash)?;
        let sane: Vec<String> = names.iter().map(|n| sanitize_path(n)).collect();
        let (chosen, guard) = get_lock(&lockdir, &sane)?;
        // Hold the lock for the rest of the run (mirrors keeping the `ReleaseKey`); it is released
        // when the interpreter is dropped.
        self.held_locks.push(guard);
        Ok(entries[chosen].clone())
    }

    /// `collect()` (mirrors `executeCollect`): append the current sample's counts as a gzipped
    /// partial file under `ngless-partials/`, and — once every needed partial is present — merge
    /// them all into `ofile`.
    fn execute_collect(
        &mut self,
        expr: &NGLessObject,
        args: &[(String, NGLessObject)],
    ) -> NgResult<NGLessObject> {
        let path = match expr {
            NGLessObject::Counts(p) => p.clone(),
            other => {
                return Err(NgError::script(format!(
                    "collect got unexpected argument: {other:?}"
                )))
            }
        };
        let current = match lookup_arg(args, "current") {
            Some(v) => entry_name(v)?,
            None => return Err(NgError::script("current not specified in collect call")),
        };
        let allneeded: Vec<String> = match lookup_arg(args, "allneeded") {
            Some(NGLessObject::List(items)) => {
                items.iter().map(entry_name).collect::<NgResult<Vec<_>>>()?
            }
            _ => {
                return Err(NgError::script(
                    "collect() called without 'allneeded' argument",
                ))
            }
        };
        let ofile = string_arg(args, "ofile")
            .ok_or_else(|| NgError::script("collect arguments: ofile is required"))?;
        let hash = string_arg(args, "__hash").unwrap_or_default();
        let tag = string_arg(args, "__parallel_tag").unwrap_or_default();
        let prefix = if tag.is_empty() {
            String::new()
        } else {
            format!("{tag}-")
        };
        let hashdir = self.setup_hash_directory(&prefix, "ngless-partials", &hash)?;
        let partial_file =
            |entry: &str| format!("{hashdir}/partial.{}.tsv.gz", sanitize_path(entry));

        // Write the current sample's counts as a gzipped partial. The write is atomic (temp +
        // fsync + rename, mirroring Haskell's `moveOrCopy` of a `syncFile`d temp), so a concurrent
        // run never reads a half-written partial.
        let body = crate::compression::read_bytes(&path.to_string_lossy())?;
        crate::compression::write_bytes_atomic(&partial_file(&current), &body)?;

        // Collect once every needed partial is present (checked in reverse, as Haskell does).
        let can_collect = allneeded
            .iter()
            .rev()
            .all(|e| Path::new(&partial_file(e)).exists());
        if can_collect {
            let comment = string_arg(args, "comment");
            let auto_comments: Vec<String> = match lookup_arg(args, "auto_comments") {
                Some(NGLessObject::List(items)) => items
                    .iter()
                    .map(|i| match i {
                        NGLessObject::Symbol(s) => Ok(s.clone()),
                        other => Err(NgError::script(format!(
                            "auto_comments argument in collect() call: expected a symbol, got {other:?}"
                        ))),
                    })
                    .collect::<NgResult<Vec<String>>>()?,
                _ => Vec::new(),
            };
            let comments = build_comment(
                comment.as_deref(),
                &auto_comments,
                &self.script_text,
                Some(&hash),
            )?;
            let inputs: Vec<String> = allneeded.iter().map(|e| partial_file(e)).collect();
            let merged = paste_counts(&comments, false, &allneeded, &inputs)?;
            crate::compression::write_bytes_atomic(&ofile, merged.as_bytes())?;
        } else {
            // Not all partials are present: this is the normal case when the parallel module is
            // driven one sample per ngless invocation. Haskell defers this to a FinishOkHook; the
            // Rust port has no hook system, so we emit the same guidance now (it is purely
            // informational — `collect` still returns successfully). Emitted at `Info` level, so it
            // is suppressed by `--quiet`, matching Haskell. Mirrors the message in `executeCollect`.
            let lno = self.cur_lno.get();
            crate::output::info(
                lno,
                "The collect() call could not be executed as there are partial results missing.\n\
                 When you use the parallel module and the collect() function,\n\
                 you typically need to run ngless *multiple times* (once per sample)!\n\n\n\
                 For more information, see \
                 https://ngless.readthedocs.io/en/latest/stdlib.html#parallel-module",
            );
        }
        Ok(NGLessObject::Void)
    }

    /// `assemble(reads)` (mirrors `executeAssemble`): run megahit over the read set and return the
    /// resulting contigs FASTA as a sequence set. Mates and singletons are concatenated (and
    /// recompressed to gzip, which megahit accepts) into the `-1`/`-2`/`-r` inputs.
    fn execute_assemble(
        &mut self,
        expr: &NGLessObject,
        args: &[(String, NGLessObject)],
    ) -> NgResult<NGLessObject> {
        let rs = match expr {
            NGLessObject::ReadSet { readset, .. } => readset,
            other => {
                return Err(NgError::script(format!(
                    "megahit:assemble first argument should have been readset, got '{other:?}'"
                )))
            }
        };
        // Concatenate (and gzip-recompress) a group of FASTQ files into a single temp input.
        let concat = |files: Vec<&FastQFilePath>| -> NgResult<Option<String>> {
            if files.is_empty() {
                return Ok(None);
            }
            let out = self.new_temp_path("ngless-megahit-input_", "fq.gz")?;
            let o = out.to_string_lossy().into_owned();
            write_fq_files(&files, &o)?;
            Ok(Some(o))
        };
        let mut cmd_args: Vec<String> = Vec::new();
        if !rs.pairs.is_empty() {
            let f1 = concat(rs.pairs.iter().map(|(a, _)| a).collect())?.unwrap();
            let f2 = concat(rs.pairs.iter().map(|(_, b)| b).collect())?.unwrap();
            cmd_args.extend(["-1".to_string(), f1, "-2".to_string(), f2]);
        }
        if let Some(f) = concat(rs.singletons.iter().collect())? {
            cmd_args.extend(["-r".to_string(), f]);
        }
        let extra = lookup_string_list(args, "__extra_megahit_args")?.unwrap_or_default();

        // megahit creates its own `megahit-output` subdirectory, so we allocate the enclosing
        // assembly directory (and a separate scratch tmp-dir) exclusively; both are registered
        // for recursive cleanup at the end of the run.
        let odir = self.temp_files.new_dir("ngless-megahit-assembly_")?;
        let odir = odir.to_string_lossy().into_owned();
        let mhtmp = self.temp_files.new_dir("ngless-megahit-tmpdir_")?;
        let mhtmp = mhtmp.to_string_lossy().into_owned();
        let megahit = std::env::var("NGLESS_MEGAHIT_BIN").unwrap_or_else(|_| "megahit".to_string());
        // ngless defaults to a single thread (`--jobs 1`), and megahit's result is thread-count
        // dependent, so the contigs are only reproducible with the same thread count.
        cmd_args.extend([
            "-o".to_string(),
            format!("{odir}/megahit-output"),
            "--num-cpu-threads".to_string(),
            "1".to_string(),
            "--tmp-dir".to_string(),
            mhtmp,
        ]);
        cmd_args.extend(extra);
        run_subprocess(&megahit, &cmd_args, "megahit")?;
        Ok(NGLessObject::SequenceSet(format!(
            "{odir}/megahit-output/final.contigs.fa"
        )))
    }

    /// `orf_find(seqset, is_metagenome=, prots_out=, ...)` (mirrors `executeORFFind`): run prodigal
    /// gene prediction and return the predicted-genes (nucleotide) FASTA filename.
    fn execute_orf_find(
        &mut self,
        expr: &NGLessObject,
        args: &[(String, NGLessObject)],
    ) -> NgResult<NGLessObject> {
        let fp = match expr {
            NGLessObject::SequenceSet(f) | NGLessObject::Filename(f) | NGLessObject::String(f) => {
                f.clone()
            }
            other => {
                return Err(NgError::script(format!(
                    "orf_find first argument should have been sequenceset, got '{other:?}'"
                )))
            }
        };
        let is_metagenome = lookup_bool(args, "is_metagenome", false)?;
        let coords_out = string_arg(args, "coords_out");
        let aa_out = string_arg(args, "prots_out");
        let include_fragments = lookup_bool(args, "include_fragments", true)?;

        let dnaout = self.new_temp_path("gene_predict_", "fna")?;
        let dnaout = dnaout.to_string_lossy().into_owned();
        let prodigal =
            std::env::var("NGLESS_PRODIGAL_BIN").unwrap_or_else(|_| "prodigal".to_string());
        let mut cmd_args: Vec<String> =
            vec!["-i".to_string(), fp, "-d".to_string(), dnaout.clone()];
        if is_metagenome {
            cmd_args.extend(["-p".to_string(), "meta".to_string()]);
        }
        match &coords_out {
            None => cmd_args.extend(["-o".to_string(), "/dev/null".to_string()]),
            Some(c) => cmd_args.extend([
                "-o".to_string(),
                c.clone(),
                "-f".to_string(),
                "gff".to_string(),
            ]),
        }
        if let Some(ao) = &aa_out {
            cmd_args.extend(["-a".to_string(), ao.clone()]);
        }
        if !include_fragments {
            cmd_args.push("-c".to_string());
        }
        run_subprocess(&prodigal, &cmd_args, "prodigal")?;
        Ok(NGLessObject::Filename(dnaout))
    }

    /// Set up a hash-named action directory (mirrors `setupHashDirectory`): `<basename>/<prefix><8
    /// hash chars>` (plus a `-subsample` suffix under `--subsample`), seeding it with `script.ngl`.
    fn setup_hash_directory(&self, prefix: &str, basename: &str, hash: &str) -> NgResult<String> {
        let short: String = hash.chars().take(8).collect();
        let suffix = if self.subsample { "-subsample" } else { "" };
        let actiondir = format!("{basename}/{prefix}{short}{suffix}");
        std::fs::create_dir_all(&actiondir).map_err(|e| {
            NgError::new(
                NgErrorType::SystemError,
                format!("Could not create directory '{actiondir}': {e}"),
            )
        })?;
        let scriptfile = format!("{actiondir}/script.ngl");
        if !Path::new(&scriptfile).exists() {
            std::fs::write(&scriptfile, &self.script_text).map_err(|e| {
                NgError::new(
                    NgErrorType::SystemError,
                    format!("Could not write '{scriptfile}': {e}"),
                )
            })?;
        }
        Ok(actiondir)
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
                let out = self.new_temp_path("converted_", "sam")?;
                crate::samtools::convert_bam_to_sam(path, &out)?;
                out
            }
            ("bam", true) => path.to_path_buf(),
            (_, _) => {
                let input = self.samtools_input(path)?;
                let out = self.new_temp_path("converted_", "bam")?;
                crate::samtools::convert_sam_to_bam(&input, &out)?;
                out
            }
        };
        copy_fastq(&orig, ofile)
    }

    /// Decode a FASTQ file, register its statistics under `label`, and return a reference to it
    /// (mirrors `asFQFilePathMayQC` with QC enabled). Two bounded passes — encoding detection
    /// (prefix only) then a streaming statistics fold — mirror Haskell's `encodingFor` followed
    /// by `fqStatsC`, so memory stays bounded and the original file is referenced unchanged
    /// (a later `write` reproduces it byte-for-byte). The decode pass still runs to completion,
    /// so the `%4`/length errors fire in the same order (detect, then decode).
    fn load_and_qc(&self, path: &str, label: &str) -> NgResult<FastQFilePath> {
        let encoding = fastq::detect_encoding_stream(crate::compression::open_read(path)?)?;
        let mut acc = fastq::FastQStatsAcc::new();
        for r in fastq::fastq_records(crate::compression::open_read(path)?, encoding) {
            acc.update(&r?);
        }
        self.register_fq_stats_from(label, &acc.finish(), encoding);
        Ok(FastQFilePath {
            encoding,
            path: PathBuf::from(path),
        })
    }

    /// Record an already-computed [`fastq::FastQStats`] under `label`, used by the streaming
    /// QC passes (mirrors `outputFQStatistics` given the folded statistics).
    fn register_fq_stats_from(&self, label: &str, st: &fastq::FastQStats, enc: FastQEncoding) {
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
                let path = self.new_temp_path("qcstats.", "tsv")?;
                std::fs::write(&path, tsv).map_err(|e| {
                    NgError::new(
                        NgErrorType::SystemError,
                        format!("Could not write {}: {e}", path.display()),
                    )
                })?;
                Ok(NGLessObject::Counts(path))
            }
            "mapping" => {
                let tsv = self.format_map_stats_tsv();
                let path = self.new_temp_path("qcstats.", "tsv")?;
                std::fs::write(&path, tsv).map_err(|e| {
                    NgError::new(
                        NgErrorType::SystemError,
                        format!("Could not write {}: {e}", path.display()),
                    )
                })?;
                Ok(NGLessObject::Counts(path))
            }
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

    /// Serialise collected mapping stats as the transposed TSV produced by `writeOutputTSV` for the
    /// mapping case (`encodeMapStats`: keys sorted alphabetically). Empty input yields an empty file.
    fn format_map_stats_tsv(&self) -> String {
        let stats = self.map_stats.borrow();
        if stats.is_empty() {
            return String::new();
        }
        let mut out = String::from("\tstats\n");
        for (i, info) in stats.iter().enumerate() {
            // Keys in the alphabetical order produced by Haskell's `sort` over (key, value).
            let rows = [
                ("aligned", info.aligned.to_string()),
                ("inputFile", info.input_file.clone()),
                ("lineNumber", info.lno.to_string()),
                ("reference", info.reference.clone()),
                ("total", info.total.to_string()),
                ("unique", info.unique.to_string()),
            ];
            for (k, v) in rows {
                out.push_str(&format!("{i}:{k}\t{v}\n"));
            }
        }
        out
    }

    /// Reserve a fresh temp file under the configured temp directory, creating it
    /// exclusively (`O_EXCL`) and registering it for cleanup at the end of the run. See
    /// [`crate::tempfiles::TempFiles::new_file`].
    fn new_temp_path(&self, prefix: &str, ext: &str) -> NgResult<PathBuf> {
        self.temp_files.new_file(prefix, ext)
    }

    // --- fastq / preprocess --------------------------------------------------

    /// `fastq(fname)`: reference the input file with its detected (or given) encoding. The file
    /// is *not* rewritten, so a later `write` reproduces it byte-for-byte (mirrors
    /// `executeFastq` for the non-interleaved case).
    fn execute_fastq(
        &self,
        expr: &NGLessObject,
        args: &[(String, NGLessObject)],
    ) -> NgResult<NGLessObject> {
        let name = as_string(expr, "fastq")?;
        // `fastq(fname, interleaved=True)` splits an interleaved file into a paired read set.
        if lookup_bool(args, "interleaved", false)? {
            let readset = self.uninterleave(&name)?;
            return Ok(NGLessObject::ReadSet { name, readset });
        }
        let path = self.maybe_subsample(&name)?;
        let fqf = self.load_and_qc(&path, &path)?;
        Ok(NGLessObject::ReadSet {
            name,
            readset: ReadSet {
                pairs: Vec::new(),
                singletons: vec![fqf],
            },
        })
    }

    /// Split an interleaved FASTQ file into pair.1/pair.2/singles temp files (mirrors
    /// `uninterleave`). Consecutive records whose headers are compatible (`compatible_header`)
    /// become a mate pair; any record that does not pair with its successor is a singleton. The
    /// result is always one pair entry and one singletons entry, even when a file is empty.
    fn uninterleave(&self, path: &str) -> NgResult<ReadSet> {
        let enc = fastq::detect_encoding_stream(crate::compression::open_read(path)?)?;
        let (f1, f2, f3) = (
            self.new_temp_path("uninterleave_", "fq")?,
            self.new_temp_path("uninterleave_", "fq")?,
            self.new_temp_path("uninterleave_", "fq")?,
        );
        let mut w1 = crate::compression::StreamWriter::create(&f1.to_string_lossy())?;
        let mut w2 = crate::compression::StreamWriter::create(&f2.to_string_lossy())?;
        let mut ws = crate::compression::StreamWriter::create(&f3.to_string_lossy())?;
        // Statistics are computed over the whole input file (the `passthroughSink fqStatsC`).
        let mut acc = fastq::FastQStatsAcc::new();
        let mut it = fastq::fastq_records(crate::compression::open_read(path)?, enc);
        // One-record lookahead (not `Peekable`, so iterator errors propagate cleanly): a record
        // is paired with its successor when their headers are compatible, else it is a singleton
        // and the successor is re-examined as the next current record.
        let mut held: Option<ShortRead> = None;
        loop {
            let cur = match held.take() {
                Some(r) => r,
                None => match it.next() {
                    Some(r) => r?,
                    None => break,
                },
            };
            acc.update(&cur);
            match it.next() {
                Some(nxt) => {
                    let nxt = nxt?;
                    if fastq::compatible_header(&cur.header, &nxt.header) {
                        acc.update(&nxt);
                        w1.write_chunk(fastq::fq_encode(enc, &cur).as_bytes())?;
                        w2.write_chunk(fastq::fq_encode(enc, &nxt).as_bytes())?;
                    } else {
                        ws.write_chunk(fastq::fq_encode(enc, &cur).as_bytes())?;
                        held = Some(nxt);
                    }
                }
                None => {
                    ws.write_chunk(fastq::fq_encode(enc, &cur).as_bytes())?;
                    break;
                }
            }
        }
        w1.finish()?;
        w2.finish()?;
        ws.finish()?;
        self.register_fq_stats_from(path, &acc.finish(), enc);
        Ok(ReadSet {
            pairs: vec![(
                FastQFilePath {
                    encoding: enc,
                    path: f1,
                },
                FastQFilePath {
                    encoding: enc,
                    path: f2,
                },
            )],
            singletons: vec![FastQFilePath {
                encoding: enc,
                path: f3,
            }],
        })
    }

    /// If `--subsample` is active, deterministically keep a 1/10 sample of the FASTQ records and
    /// return the path to a temp file holding them; otherwise return `path` unchanged (mirrors
    /// `optionalSubsample`/`performSubsample`).
    fn maybe_subsample(&self, path: &str) -> NgResult<String> {
        if !self.subsample {
            return Ok(path.to_string());
        }
        let text = read_fastq_text(path)?;
        let sampled = subsample_text(&text);
        let out = self.new_temp_path("subsampled.", "fq")?;
        std::fs::write(&out, sampled).map_err(|e| {
            NgError::new(
                NgErrorType::SystemError,
                format!("Could not write temp file {}: {e}", out.display()),
            )
        })?;
        Ok(out.to_string_lossy().into_owned())
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
        let p1 = self.maybe_subsample(&mate1)?;
        let p2 = self.maybe_subsample(&mate2)?;
        let f1 = self.load_and_qc(&p1, &p1)?;
        let f2 = self.load_and_qc(&p2, &p2)?;
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
            let p3 = self.maybe_subsample(&mate3)?;
            // QC stats are registered for the singles file even if it is later dropped.
            let enc3 = fastq::detect_encoding_stream(crate::compression::open_read(&p3)?)?;
            let mut acc3 = fastq::FastQStatsAcc::new();
            for r in fastq::fastq_records(crate::compression::open_read(&p3)?, enc3) {
                acc3.update(&r?);
            }
            let n_singles3 = acc3.n_seq();
            self.register_fq_stats_from(&p3, &acc3.finish(), enc3);
            if f1.encoding != enc3 {
                // Special case seen in the wild: an empty singles file with a default encoding.
                if n_singles3 == 0 {
                    Vec::new()
                } else {
                    return Err(NgError::new(NgErrorType::DataError,
                        format!("Mates do not seem to have the same quality encoding! (paired mates vs single [{mate3}]).")));
                }
            } else {
                vec![FastQFilePath {
                    encoding: enc3,
                    path: PathBuf::from(&p3),
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

    /// `group([rs...], name=...)`: combine read sets into one named read set (mirrors
    /// `executeGroup`/`groupFiles`). A single member is just renamed; multiple members have their
    /// pair- and singleton-file lists concatenated in order.
    fn execute_group(
        &self,
        expr: &NGLessObject,
        args: &[(String, NGLessObject)],
    ) -> NgResult<NGLessObject> {
        let name = match lookup_arg(args, "name") {
            Some(NGLessObject::String(s)) => s.clone(),
            _ => {
                return Err(NgError::script(
                    "group: argument `name` (a string) is required",
                ))
            }
        };
        let members = match expr {
            NGLessObject::List(items) => items,
            other => {
                return Err(NgError::should_not_occur(format!(
                    "group expected a list of read sets, got {other:?}"
                )))
            }
        };
        let mut pairs = Vec::new();
        let mut singletons = Vec::new();
        for m in members {
            match m {
                NGLessObject::ReadSet { readset, .. } => {
                    pairs.extend(readset.pairs.iter().cloned());
                    singletons.extend(readset.singletons.iter().cloned());
                }
                other => {
                    return Err(NgError::should_not_occur(format!(
                        "In group call, all arguments should have been read sets, got {other:?}"
                    )))
                }
            }
        }
        if pairs.is_empty() && singletons.is_empty() {
            return Err(NgError::new(
                NgErrorType::DataError,
                format!("Attempted to group sample '{name}' but sample is empty (no read files)."),
            ));
        }
        Ok(NGLessObject::ReadSet {
            name,
            readset: ReadSet { pairs, singletons },
        })
    }

    /// `load_fastq_directory(dir)` / `load_mocat_sample(dir)`: discover FASTQ files in a directory,
    /// pairing them by suffix, and group them into a single named read set (mirrors
    /// `executeLoad`/`loadDirectoryFiles` in BuiltinModules/LoadDirectory.hs).
    fn execute_load_directory(
        &self,
        expr: &NGLessObject,
        _args: &[(String, NGLessObject)],
    ) -> NgResult<NGLessObject> {
        let basedir = as_string(expr, "load_fastq_directory")?;
        if !Path::new(&basedir).is_dir() {
            return Err(NgError::new(
                NgErrorType::DataError,
                format!("Attempting to load directory '{basedir}', but directory does not exist."),
            ));
        }
        // Glob `<dir>/*.{fq,fastq}{,.gz,.bz2,.xz}` and sort (mirrors the `exts`/`namesMatching` set).
        let mut fqfiles: Vec<String> = Vec::new();
        for entry in std::fs::read_dir(&basedir).map_err(|e| {
            NgError::new(
                NgErrorType::SystemError,
                format!("Could not read directory {basedir}: {e}"),
            )
        })? {
            let entry = entry.map_err(|e| {
                NgError::new(
                    NgErrorType::SystemError,
                    format!("Error reading directory: {e}"),
                )
            })?;
            let fname = entry.file_name().to_string_lossy().into_owned();
            if fastq_directory_exts()
                .iter()
                .any(|ext| fname.ends_with(ext))
            {
                fqfiles.push(
                    Path::new(&basedir)
                        .join(&fname)
                        .to_string_lossy()
                        .into_owned(),
                );
            }
        }
        fqfiles.sort();

        let (singletons, paired) = match_up(&fqfiles)?;
        let mut members: Vec<NGLessObject> = Vec::new();
        for f in &singletons {
            members.push(self.execute_fastq(&NGLessObject::String(f.clone()), &[])?);
        }
        for p in &paired {
            let mut pargs = vec![("second".to_string(), NGLessObject::String(p.1.clone()))];
            if let Some(singles) = &p.2 {
                pargs.push(("singles".to_string(), NGLessObject::String(singles.clone())));
            }
            members.push(self.execute_paired(&NGLessObject::String(p.0.clone()), &pargs)?);
        }
        self.execute_group(
            &NGLessObject::List(members),
            &[("name".to_string(), NGLessObject::String(basedir))],
        )
    }

    /// `load_sample_list(yaml)`: parse a YAML sample manifest into a list of named read sets
    /// (mirrors `executeLoadSampleList`/`normalize` in BuiltinModules/Samples.hs). All files are
    /// referenced with Sanger encoding, as in the Haskell code.
    fn execute_load_sample_list(&self, expr: &NGLessObject) -> NgResult<NGLessObject> {
        let fname = as_string(expr, "load_sample_list")?;
        let text = std::fs::read_to_string(&fname).map_err(|e| {
            NgError::new(
                NgErrorType::SystemError,
                format!("Could not read sample information file {fname}: {e}"),
            )
        })?;
        let parsed: SampleFile = serde_yaml::from_str(&text).map_err(|e| {
            NgError::new(
                NgErrorType::SystemError,
                format!("Could not parse sample information file {fname}. Error was `{e}`"),
            )
        })?;
        let mut out = Vec::new();
        for (name, entries) in parsed.samples {
            let mut pairs = Vec::new();
            let mut singletons = Vec::new();
            let resolve = |p: &str| -> FastQFilePath {
                let path = match &parsed.basedir {
                    Some(base) if !Path::new(p).is_absolute() => {
                        Path::new(base).join(p).to_string_lossy().into_owned()
                    }
                    _ => p.to_string(),
                };
                FastQFilePath {
                    encoding: FastQEncoding::Sanger,
                    path: PathBuf::from(path),
                }
            };
            let invalid =
                || NgError::new(NgErrorType::DataError, format!("Invalid sample '{name}'"));
            for entry in entries {
                if let Some(v) = entry.get("paired") {
                    let seq = v.as_sequence().ok_or_else(invalid)?;
                    let files: Vec<&str> = seq.iter().filter_map(|e| e.as_str()).collect();
                    if files.len() != 2 {
                        return Err(NgError::new(
                            NgErrorType::DataError,
                            format!("Invalid paired sample '{name}'"),
                        ));
                    }
                    pairs.push((resolve(files[0]), resolve(files[1])));
                } else if let Some(v) = entry.get("single") {
                    // A bare string or a one-element list (mirrors the aeson parser).
                    if let Some(s) = v.as_str() {
                        singletons.push(resolve(s));
                    } else if let Some(seq) = v.as_sequence() {
                        let files: Vec<&str> = seq.iter().filter_map(|e| e.as_str()).collect();
                        if files.len() != 1 {
                            return Err(invalid());
                        }
                        singletons.push(resolve(files[0]));
                    } else {
                        return Err(invalid());
                    }
                } else {
                    return Err(invalid());
                }
            }
            out.push(NGLessObject::ReadSet {
                name,
                readset: ReadSet { pairs, singletons },
            });
        }
        Ok(NGLessObject::List(out))
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

        // Stream through three output writers (pair.1, pair.2, singles), collecting QC stats per
        // output in one pass (mirrors executePreprocess: open all three, stream, then delete the
        // empty ones and pick the result shape from the per-output counts). Temp paths are
        // allocated up front in slot order; `.fq` => uncompressed (StreamWriter::Plain).
        let p1path = self.new_temp_path("preprocessed.1.", "fq")?;
        let p2path = self.new_temp_path("preprocessed.2.", "fq")?;
        let spath = self.new_temp_path("preprocessed.singles.", "fq")?;
        let mut w1 = crate::compression::StreamWriter::create(&p1path.to_string_lossy())?;
        let mut w2 = crate::compression::StreamWriter::create(&p2path.to_string_lossy())?;
        let mut ws = crate::compression::StreamWriter::create(&spath.to_string_lossy())?;
        let (mut a1, mut a2, mut as_) = (
            fastq::FastQStatsAcc::new(),
            fastq::FastQStatsAcc::new(),
            fastq::FastQStatsAcc::new(),
        );

        for (f1, f2) in &rs.pairs {
            let mut it1 = fastq::fastq_records(
                crate::compression::open_read(&f1.path.to_string_lossy())?,
                f1.encoding,
            );
            let mut it2 = fastq::fastq_records(
                crate::compression::open_read(&f2.path.to_string_lossy())?,
                f2.encoding,
            );
            // `zip` semantics: read mate 1 first, and only then mate 2; stop at the shorter mate.
            loop {
                let r1 = match it1.next() {
                    Some(r) => r?,
                    None => break,
                };
                let r2 = match it2.next() {
                    Some(r) => r?,
                    None => break,
                };
                let o1 = self.run_block_on_read(var, r1, block)?;
                let o2 = self.run_block_on_read(var, r2, block)?;
                match (o1, o2) {
                    (Some(a), Some(b)) => {
                        w1.write_chunk(fastq::fq_encode(outenc, &a).as_bytes())?;
                        a1.update(&a);
                        w2.write_chunk(fastq::fq_encode(outenc, &b).as_bytes())?;
                        a2.update(&b);
                    }
                    (Some(r), None) | (None, Some(r)) => {
                        if keep_singles {
                            ws.write_chunk(fastq::fq_encode(outenc, &r).as_bytes())?;
                            as_.update(&r);
                        }
                    }
                    (None, None) => {}
                }
            }
        }
        for fqf in &rs.singletons {
            for r in fastq::fastq_records(
                crate::compression::open_read(&fqf.path.to_string_lossy())?,
                fqf.encoding,
            ) {
                if let Some(sr) = self.run_block_on_read(var, r?, block)? {
                    ws.write_chunk(fastq::fq_encode(outenc, &sr).as_bytes())?;
                    as_.update(&sr);
                }
            }
        }
        w1.finish()?;
        w2.finish()?;
        ws.finish()?;

        // Register QC statistics for all three output slots, as executePreprocess does (before
        // deciding the result shape), labelled with the preprocess statement's line number.
        let lno = self.cur_lno.get();
        self.register_fq_stats_from(&format!("preproc.lno{lno}.pairs.1"), &a1.finish(), outenc);
        self.register_fq_stats_from(&format!("preproc.lno{lno}.pairs.2"), &a2.finish(), outenc);
        self.register_fq_stats_from(&format!("preproc.lno{lno}.singles"), &as_.finish(), outenc);

        // Choose the result shape exactly as executePreprocess does, then delete the temp files
        // the result does not reference (instead of never writing them — unobservable, and what
        // Haskell does).
        let (n_pairs, n_singles) = (a1.n_seq() > 0, as_.n_seq() > 0);
        let f1 = FastQFilePath {
            encoding: outenc,
            path: p1path.clone(),
        };
        let f2 = FastQFilePath {
            encoding: outenc,
            path: p2path.clone(),
        };
        let fs = FastQFilePath {
            encoding: outenc,
            path: spath.clone(),
        };
        // `keep` flags are [pair.1, pair.2, singles].
        let (readset, keep) = match (n_pairs, n_singles) {
            (true, false) => (
                ReadSet {
                    pairs: vec![(f1, f2)],
                    singletons: Vec::new(),
                },
                [true, true, false],
            ),
            (false, true) => (
                ReadSet {
                    pairs: Vec::new(),
                    singletons: vec![fs],
                },
                [false, false, true],
            ),
            _ if rs.pairs.is_empty() => (
                ReadSet {
                    pairs: Vec::new(),
                    singletons: vec![fs],
                },
                [false, false, true],
            ),
            _ => (
                ReadSet {
                    pairs: vec![(f1, f2)],
                    singletons: vec![fs],
                },
                [true, true, true],
            ),
        };
        for (kept, path) in [(keep[0], &p1path), (keep[1], &p2path), (keep[2], &spath)] {
            if !kept {
                let _ = std::fs::remove_file(path);
            }
        }
        Ok(NGLessObject::ReadSet { name, readset })
    }

    /// `unique(reads, max_copies=N)`: drop duplicate reads, keeping at most `max_copies`
    /// copies of each distinct sequence (mirrors `Interpretation/Unique.executeUnique`). Only
    /// single-end read sets are supported (as in Haskell); a list of read sets is mapped
    /// element-wise. Each input file is streamed to a fresh gzipped temp file.
    fn execute_unique(
        &self,
        expr: &NGLessObject,
        args: &[(String, NGLessObject)],
    ) -> NgResult<NGLessObject> {
        if let NGLessObject::List(items) = expr {
            let mut out = Vec::with_capacity(items.len());
            for item in items {
                out.push(self.execute_unique(item, args)?);
            }
            return Ok(NGLessObject::List(out));
        }
        let max_copies = lookup_int(args, "max_copies", 1)?;
        if max_copies < 1 {
            return Err(NgError::script(format!(
                "unique: max_copies must be at least 1, got {max_copies}"
            )));
        }
        let (name, rs) = match expr {
            NGLessObject::ReadSet { name, readset } => (name.clone(), readset.clone()),
            other => {
                return Err(NgError::should_not_occur(format!(
                    "executeUnique: Cannot handle argument {other:?}"
                )))
            }
        };
        if !rs.pairs.is_empty() || rs.singletons.len() != 1 {
            return Err(NgError::should_not_occur(
                "executeUnique: Cannot handle paired-end read set",
            ));
        }
        let input = &rs.singletons[0];
        let dest = self.new_temp_path("unique.", "fq.gz")?;
        let reader = crate::compression::open_read(&input.path.to_string_lossy())?;
        let mut writer = crate::compression::StreamWriter::create(&dest.to_string_lossy())?;
        fastq::unique_reads(reader, &mut writer, input.encoding, max_copies as usize)?;
        writer.finish()?;
        Ok(NGLessObject::ReadSet {
            name,
            readset: ReadSet {
                pairs: Vec::new(),
                singletons: vec![FastQFilePath {
                    encoding: input.encoding,
                    path: dest,
                }],
            },
        })
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
        "readlines" => execute_readlines(expr),
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

/// A buffered writer over a fresh plain-SAM temp file, for streaming `select` output line-by-line.
fn sam_temp_writer(path: &Path) -> NgResult<std::io::BufWriter<std::fs::File>> {
    let f = std::fs::File::create(path).map_err(|e| {
        NgError::new(
            NgErrorType::SystemError,
            format!("Could not write temp file {}: {e}", path.display()),
        )
    })?;
    Ok(std::io::BufWriter::new(f))
}

/// Error wrapper for a failed SAM write.
fn sam_write_err(e: std::io::Error) -> NgError {
    NgError::new(
        NgErrorType::SystemError,
        format!("Could not write SAM output: {e}"),
    )
}

/// Read a SAM file into memory. BAM is decoded via samtools (mirrors `samBamConduit`); gzip is
/// transparent.
fn read_sam_text(path: &str) -> NgResult<String> {
    if path.ends_with(".bam") {
        return crate::samtools::bam_to_sam_text(path);
    }
    crate::compression::read_to_string(path)
}

/// Stream a read set as a single interleaved FASTQ byte stream into `out`, for `bwa mem -p`
/// (mirrors `interleaveFQs`): each pair is emitted record-by-record (mate 1's record, then
/// mate 2's), re-normalised to LF line endings; the singleton files are then appended verbatim
/// (decompressed). Bounded memory — at most a few lines are held at once. Both mates of a pair
/// must have the same number of lines.
pub(crate) fn write_interleaved<W: Write>(rs: &ReadSet, out: &mut W) -> NgResult<()> {
    let werr = |e: std::io::Error| {
        NgError::new(
            NgErrorType::SystemError,
            format!("Could not write interleaved reads: {e}"),
        )
    };
    // Read one line (newline-stripped, `\r\n`-aware) from a boxed reader; `None` at EOF.
    fn read_one(it: &mut dyn std::io::BufRead) -> NgResult<Option<String>> {
        let mut s = String::new();
        let n = it.read_line(&mut s).map_err(|e| {
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
    let take4 = |it: &mut dyn std::io::BufRead| -> NgResult<Vec<String>> {
        let mut v = Vec::with_capacity(4);
        for _ in 0..4 {
            match read_one(it)? {
                Some(l) => v.push(l),
                None => break,
            }
        }
        Ok(v)
    };
    for (f1, f2) in &rs.pairs {
        let mut a = crate::compression::open_read(&f1.path.to_string_lossy())?;
        let mut b = crate::compression::open_read(&f2.path.to_string_lossy())?;
        loop {
            let q1 = take4(&mut *a)?;
            let q2 = take4(&mut *b)?;
            if q1.is_empty() && q2.is_empty() {
                break;
            }
            if q1.len() != q2.len() {
                return Err(NgError::should_not_occur(format!(
                    "interleavePair: mismatched lengths: ({}, {})",
                    q1.len(),
                    q2.len()
                )));
            }
            for l in q1.iter().chain(q2.iter()) {
                out.write_all(l.as_bytes()).map_err(werr)?;
                out.write_all(b"\n").map_err(werr)?;
            }
        }
    }
    for s in &rs.singletons {
        std::io::copy(
            &mut crate::compression::open_read(&s.path.to_string_lossy())?,
            out,
        )
        .map_err(werr)?;
    }
    Ok(())
}

/// Candidate paths for a FASTA reference under the search path (mirrors `expandPath'`). With no
/// `<...>` placeholder the path is used as-is; otherwise, for each search-path entry, the
/// placeholder is resolved (supporting `name=/path` entries) and joined with the remainder.
pub(crate) fn expand_path_candidates(fbase: &str, search: &[String]) -> Vec<String> {
    match find_angle_group(fbase) {
        None => vec![fbase.to_string()],
        Some((start, end, code)) => {
            // The remainder is the path with the `<...>` removed and any leading slashes dropped.
            let mut rest = String::new();
            rest.push_str(&fbase[..start]);
            rest.push_str(&fbase[end..]);
            let fbase2 = rest.trim_start_matches('/').to_string();
            search
                .iter()
                .filter_map(|p| {
                    let base = simplify_search(&code, p)?;
                    Some(Path::new(&base).join(&fbase2).to_string_lossy().to_string())
                })
                .collect()
        }
    }
}

/// Find the first `<...>` placeholder in `fbase`, returning `(start, end, code)` where the
/// bracketed `code` is an (optionally empty) identifier. Returns `None` if there is no such
/// well-formed placeholder (matching the `<(@{%id})?>` regex used in Haskell).
fn find_angle_group(fbase: &str) -> Option<(usize, usize, String)> {
    let bytes = fbase.as_bytes();
    let start = fbase.find('<')?;
    let mut i = start + 1;
    while i < bytes.len() && (bytes[i].is_ascii_alphanumeric() || bytes[i] == b'_') {
        i += 1;
    }
    if i < bytes.len() && bytes[i] == b'>' {
        let code = fbase[start + 1..i].to_string();
        Some((start, i + 1, code))
    } else {
        None
    }
}

/// Resolve one search-path entry for a placeholder named `code` (mirrors `simplify`): a plain
/// directory is used directly; a `name=/path` entry matches only when `name == code`.
fn simplify_search(code: &str, path: &str) -> Option<String> {
    if !path.contains('=') {
        Some(path.to_string())
    } else {
        path.strip_prefix(&format!("{code}=")).map(String::from)
    }
}

/// One FASTA record, kept as its (LF-normalised) text plus the length of its sequence in bases.
struct FaSeq {
    record: String,
    length: i64,
}

/// Parse FASTA text into records, preserving each record's lines (mirrors `faConduit` closely
/// enough for splitting: sequence length is the number of sequence bases).
fn parse_fasta(text: &str) -> Vec<FaSeq> {
    let mut out = Vec::new();
    let mut cur: Option<FaSeq> = None;
    for line in text.lines() {
        if line.starts_with('>') {
            if let Some(s) = cur.take() {
                out.push(s);
            }
            cur = Some(FaSeq {
                record: format!("{line}\n"),
                length: 0,
            });
        } else if let Some(s) = cur.as_mut() {
            s.record.push_str(line);
            s.record.push('\n');
            s.length += line.len() as i64;
        }
    }
    if let Some(s) = cur {
        out.push(s);
    }
    out
}

/// Split a FASTA file into blocks of at most `block_size` megabases, writing `<ofile_base>.<n>.fna`
/// chunks (mirrors `splitFASTA`/`splitWriter`). A sequence longer than the block size is placed
/// alone in its own chunk (NGLess never splits a single sequence).
fn split_fasta(block_size: i64, ifile: &str, ofile_base: &str) -> NgResult<Vec<String>> {
    let text = read_fastq_text(ifile)?;
    let seqs = parse_fasta(&text);
    let max_bps = 1_000_000 * block_size;
    let mut chunks = Vec::new();
    let mut idx = 0;
    let mut n = 0;
    while idx < seqs.len() {
        let f = format!("{ofile_base}.{n}.fna");
        let mut content = String::new();
        content.push_str(&seqs[idx].record);
        let mut sofar = seqs[idx].length;
        let first_fits = seqs[idx].length <= max_bps;
        if !first_fits {
            crate::output::warn(
                0,
                &format!(
                    "While splitting file '{ifile}': a sequence is {} bases long (longer than the \
                     block size). Note that NGLess does not split sequences.",
                    seqs[idx].length
                ),
            );
        }
        idx += 1;
        if first_fits {
            while idx < seqs.len() && seqs[idx].length + sofar <= max_bps {
                content.push_str(&seqs[idx].record);
                sofar += seqs[idx].length;
                idx += 1;
            }
        }
        std::fs::write(&f, content).map_err(|e| {
            NgError::new(
                NgErrorType::SystemError,
                format!("Could not write split file {f}: {e}"),
            )
        })?;
        chunks.push(f);
        n += 1;
    }
    Ok(chunks)
}

/// Merge the alignment records of one read across block-partial files, keeping the best match(es)
/// (mirrors `mergeSAMGroups MSBestOnly` with the post-1.1 behaviour). Records are partitioned into
/// first-in-pair, second-in-pair and singleton, and within each partition only the alignments with
/// the maximal `matchSize - NM` are kept (unaligned reads pass through a single representative).
fn merge_sam_group(combined: &[SamLine]) -> NgResult<Vec<SamLine>> {
    let mut g1 = Vec::new();
    let mut g2 = Vec::new();
    let mut gs = Vec::new();
    // The Haskell `foldl (\... s -> (s:g1, ...)) ([],[],[])` *prepends*, so each partition ends
    // up reversed relative to the input order; mirror that to keep output ordering identical.
    for s in combined.iter().rev() {
        if s.is_first_in_pair() {
            g1.push(s.clone());
        } else if s.is_second_in_pair() {
            g2.push(s.clone());
        } else {
            gs.push(s.clone());
        }
    }
    let mut out = Vec::new();
    for sub in [g1, g2, gs] {
        out.extend(pick_best(sub));
    }
    Ok(out)
}

/// Pick the best-scoring alignments from a partition (mirrors `group`/`pick`): if none are
/// aligned, keep the first record; otherwise keep every aligned record whose `matchSize - NM` is
/// maximal.
fn pick_best(group: Vec<SamLine>) -> Vec<SamLine> {
    if group.is_empty() {
        return group;
    }
    let aligned: Vec<&SamLine> = group.iter().filter(|s| s.is_aligned()).collect();
    if aligned.is_empty() {
        return vec![group[0].clone()];
    }
    let match_value = |s: &SamLine| s.match_size(true).unwrap_or(0) - s.int_tag("NM").unwrap_or(0);
    let best = aligned.iter().map(|s| match_value(s)).max().unwrap();
    group
        .into_iter()
        .filter(|s| s.is_aligned() && match_value(s) == best)
        .collect()
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

/// Name of a parallel-module list entry: the string itself, or a read set's name (mirrors the
/// `readSetOrTypeError`/`stringOrTypeError` handling in `executeLock1OrForAll`/`executeCollect`).
fn entry_name(v: &NGLessObject) -> NgResult<String> {
    match v {
        NGLessObject::String(s) => Ok(s.clone()),
        NGLessObject::ReadSet { name, .. } => Ok(name.clone()),
        other => Err(NgError::should_not_occur(format!(
            "Expected a string or readset, got {other:?}"
        ))),
    }
}

/// Look up a required-string keyword argument.
fn string_arg(args: &[(String, NGLessObject)], name: &str) -> Option<String> {
    match lookup_arg(args, name) {
        Some(NGLessObject::String(s)) => Some(s.clone()),
        _ => None,
    }
}

/// `sanitizePath`: replace `/` and `\` with `_` so an entry name is safe as a lock filename.
fn sanitize_path(s: &str) -> String {
    s.chars()
        .map(|c| if c == '/' || c == '\\' { '_' } else { c })
        .collect()
}

/// Claim a lock in `lockdir` for the first entry that is not finished or failed (mirrors `getLock`,
/// collapsing its not-locked/stale-locked passes into one: each candidate is claimed through the
/// shared `lockfile::acquire_lock`, which atomically creates `<entry>.lock` with `O_CREAT|O_EXCL`
/// and reclaims a stale lock — one older than an hour, the `getLock'` `maxAge` — so a crashed peer
/// does not block forever). A held, fresh lock makes `acquire_lock` return `None`, so we move on to
/// the next entry. Returns the chosen index and its [`LockGuard`], which the caller keeps alive for
/// the run. A non-zero exit / contention message matches the Haskell text.
fn get_lock(lockdir: &str, sane: &[String]) -> NgResult<(usize, crate::lockfile::LockGuard)> {
    use crate::lockfile::{acquire_lock, LockParameters, WhenExistsStrategy};
    use std::collections::HashSet;
    let existing: HashSet<String> = std::fs::read_dir(lockdir)
        .map(|rd| {
            rd.filter_map(|e| e.ok())
                .map(|e| e.file_name().to_string_lossy().to_string())
                .collect()
        })
        .unwrap_or_default();
    for (i, name) in sane.iter().enumerate() {
        if existing.contains(&format!("{name}.finished"))
            || existing.contains(&format!("{name}.failed"))
        {
            continue;
        }
        let params = LockParameters {
            lock_fname: PathBuf::from(format!("{lockdir}/{name}.lock")),
            // One hour: lock files are mtime-refreshed every ten minutes while healthy, so an older
            // one signals a crashed process (mirrors `getLock'`).
            max_age: Duration::from_secs(60 * 60),
            when_exists: WhenExistsStrategy::IfLockedNothing,
            mtime_update: true,
        };
        if let Some(guard) = acquire_lock(params)? {
            // Recheck `.finished` under the lock: a peer may have finished (and released the lock)
            // between our directory scan and the acquire (mirrors the `doesFileExist finishedName`
            // recheck in `getLock'`).
            if Path::new(&format!("{lockdir}/{name}.finished")).exists() {
                drop(guard);
                continue;
            }
            return Ok((i, guard));
        }
    }
    Err(NgError::new(
        NgErrorType::DataError,
        "All jobs are finished or running",
    ))
}

/// `__paste()` (mirrors `executePaste`): merge a set of counts files into `ofile`.
fn execute_paste(expr: &NGLessObject, args: &[(String, NGLessObject)]) -> NgResult<NGLessObject> {
    eprintln!("Calling __paste which is an internal function, exposed for testing only");
    let inputs: Vec<String> = match expr {
        NGLessObject::List(items) => items
            .iter()
            .map(|i| match i {
                NGLessObject::String(s) => Ok(s.clone()),
                other => Err(NgError::script(format!(
                    "__concat argument: expected a string, got {other:?}"
                ))),
            })
            .collect::<NgResult<Vec<_>>>()?,
        other => {
            return Err(NgError::script(format!(
                "Bad call to test function __paste: {other:?}"
            )))
        }
    };
    let ofile = string_arg(args, "ofile")
        .ok_or_else(|| NgError::script("__paste arguments: ofile is required"))?;
    let headers: Vec<String> = match lookup_arg(args, "headers") {
        Some(NGLessObject::List(items)) => items
            .iter()
            .map(|i| match i {
                NGLessObject::String(s) => Ok(s.clone()),
                other => Err(NgError::script(format!(
                    "__paste headers: expected a string, got {other:?}"
                ))),
            })
            .collect::<NgResult<Vec<_>>>()?,
        _ => return Err(NgError::script("__paste arguments: headers is required")),
    };
    let matching_rows = matches!(
        lookup_arg(args, "matching_rows"),
        Some(NGLessObject::Bool(true))
    );
    let merged = paste_counts(&[], matching_rows, &headers, &inputs)?;
    crate::compression::write_bytes(&ofile, merged.as_bytes())?;
    Ok(NGLessObject::Void)
}

/// Merge a set of counts files (mirrors `pasteCounts`). Each input contributes its value columns,
/// keyed by the first (TAB-delimited) field; rows missing from an input are filled with zeros.
/// The output is `# <comment>` lines, then a header line (`\t` + `headers`), then the merged rows
/// in ascending index order. `matching_rows` is accepted for parity but the (index-merging) path
/// is correct in both cases when the inputs are consistent.
fn paste_counts(
    comments: &[String],
    _matching_rows: bool,
    headers: &[String],
    inputs: &[String],
) -> NgResult<String> {
    use std::collections::BTreeSet;
    // For each input file: number of value columns, and a map index -> payload (the bytes from the
    // first TAB onward, i.e. including the leading TAB).
    let mut per_file: Vec<(usize, std::collections::HashMap<String, String>)> =
        Vec::with_capacity(inputs.len());
    let mut all_indices: BTreeSet<String> = BTreeSet::new();
    for input in inputs {
        let content = crate::compression::read_to_string(input)?;
        let mut lines = content.lines();
        let header = lines.next().unwrap_or("");
        let ncols = header.matches('\t').count();
        let mut map = std::collections::HashMap::new();
        for line in lines {
            if line.is_empty() {
                continue;
            }
            let tab = line.find('\t').ok_or_else(|| {
                NgError::new(NgErrorType::DataError, "Line does not have a TAB character")
            })?;
            let (index, payload) = line.split_at(tab);
            all_indices.insert(index.to_string());
            map.insert(index.to_string(), payload.to_string());
        }
        per_file.push((ncols, map));
    }
    let mut out = String::new();
    for c in comments {
        out.push_str("# ");
        out.push_str(c);
        out.push('\n');
    }
    out.push('\t');
    out.push_str(&headers.join("\t"));
    out.push('\n');
    for index in &all_indices {
        out.push_str(index);
        for (ncols, map) in &per_file {
            match map.get(index) {
                Some(p) => out.push_str(p),
                None => {
                    for _ in 0..*ncols {
                        out.push_str("\t0");
                    }
                }
            }
        }
        out.push('\n');
    }
    Ok(out)
}

/// Run an external command, inheriting stderr, and fail with a clear error on a non-zero exit or
/// a missing binary (mirrors `Utils.Process.runProcess`).
fn run_subprocess(bin: &str, args: &[String], label: &str) -> NgResult<()> {
    let status = std::process::Command::new(bin)
        .args(args)
        .stdout(std::process::Stdio::null())
        .status()
        .map_err(|e| {
            NgError::new(
                NgErrorType::SystemError,
                format!("Could not run {label} ('{bin}'): {e}"),
            )
        })?;
    if !status.success() {
        return Err(NgError::new(
            NgErrorType::SystemError,
            format!("{label} ('{bin}') failed with exit status {status}"),
        ));
    }
    Ok(())
}

/// Format one SAM record as a 4-line FASTQ record (mirrors `asFQ1`).
fn as_fq1(l: &SamLine) -> String {
    format!("@{}\n{}\n+\n{}\n", l.qname, l.seq, l.qual)
}

/// `write(rs, ofile=...)`: write the read set's current file(s) to `ofile` (mirrors
/// `executeWrite` of an `NGOReadSet`). A single-end set goes straight to `ofile`; a paired set
/// produces `<base>.pair.1.<ext>` / `.pair.2.<ext>` (and `.singles.<ext>` when singletons
/// remain). For an un-preprocessed single-end set this reproduces the input byte-for-byte.
fn execute_write(
    expr: &NGLessObject,
    args: &[(String, NGLessObject)],
    script_text: &str,
    subsample: bool,
) -> NgResult<NGLessObject> {
    let mut ofile = match lookup_arg(args, "ofile") {
        Some(NGLessObject::String(s)) => s.clone(),
        _ => {
            return Err(NgError::script(
                "write: argument `ofile` (a string) is required",
            ))
        }
    };
    // Under `--subsample`, the output name gains a `.subsampled` marker that `format_fq_oname`
    // moves before the extension (mirrors the `subpostfix` in `parseWriteOptions`).
    if subsample {
        ofile.push_str(".subsampled");
    }
    let format_flags = match lookup_arg(args, "format_flags") {
        Some(NGLessObject::Symbol(s)) => Some(s.clone()),
        _ => None,
    };
    match expr {
        // `format_flags={interleaved}`: stream the interleaved pairs (then singletons) to a single
        // output, which may be STDOUT (`/dev/stdout`). Mirrors the `interleaveFQs` branch of
        // `executeWrite`; no per-mate filename derivation (so no extension check on STDOUT).
        NGLessObject::ReadSet { readset, .. } if format_flags.as_deref() == Some("interleaved") => {
            let mut w = crate::compression::StreamWriter::create(&ofile)?;
            write_interleaved(readset, &mut w)?;
            w.finish()?;
            Ok(NGLessObject::String(ofile))
        }
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
            write_counts(path, &ofile, args, script_text)?;
            Ok(NGLessObject::String(ofile))
        }
        // A sequence set (assemble contigs) or a plain filename (orf_find output) is copied to the
        // destination, recompressing only if the extension demands it (mirrors `moveOrCopyCompress`).
        NGLessObject::SequenceSet(path) | NGLessObject::Filename(path) => {
            copy_fastq(Path::new(path), &ofile)?;
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
    use crate::compression::{open_read, write_bytes, StreamWriter};
    match files {
        [] => write_bytes(ofile, b""),
        // Single file keeps the verbatim `std::fs::copy` byte-copy fast path (load-bearing for
        // write byte-identity of un-preprocessed sets).
        [single] => copy_fastq(&single.path, ofile),
        // Stream each decompressed input through one compressing writer: the encoder sees the
        // identical concatenated byte stream, so the output is content-equivalent to
        // decompress-all-then-`write_bytes`, but bounded in memory.
        many => {
            let mut w = StreamWriter::create(ofile)?;
            for f in many {
                std::io::copy(&mut open_read(&f.path.to_string_lossy())?, &mut w).map_err(|e| {
                    NgError::new(
                        NgErrorType::SystemError,
                        format!("Could not write {ofile}: {e}"),
                    )
                })?;
            }
            w.finish()
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

/// Build the comment lines for `write()` (mirrors `buildComment`): the manual `comment` (if
/// any) first, then each requested auto-comment block in order. The `# ` marker is *not*
/// included here (the caller prepends it, as `commentC "# "` does).
fn build_comment(
    comment: Option<&str>,
    auto: &[String],
    script_text: &str,
    hash: Option<&str>,
) -> NgResult<Vec<String>> {
    let mut out = Vec::new();
    if let Some(c) = comment {
        out.push(c.to_string());
    }
    for a in auto {
        match a.as_str() {
            // `map addInitialIndent . T.lines . ngleScriptText`, preceded by a header line.
            "script" => {
                out.push("Output generated by:".to_string());
                for line in script_text.lines() {
                    out.push(format!("    {line}"));
                }
            }
            // `Output hash: <md5>` — the hash injected as the hidden `__hash` argument by the
            // `add_output_hash` transform.
            "hash" => {
                let h = hash.ok_or_else(|| {
                    NgError::should_not_occur(
                        "auto_comments={hash} but no __hash argument was injected",
                    )
                })?;
                out.push(format!("Output hash: {h}"));
            }
            other => {
                return Err(NgError::script(format!(
                    "write: auto_comments={{{other}}} is not supported in this build yet."
                )))
            }
        }
    }
    Ok(out)
}

/// Write a counts TSV to `ofile` (mirrors the `NGOCounts` arm of `executeWrite`). Supports
/// `format={tsv|csv}`, a manual `comment=`, and `auto_comments=[{script}]` (each comment line
/// prefixed `# `).
fn write_counts(
    path: &Path,
    ofile: &str,
    args: &[(String, NGLessObject)],
    script_text: &str,
) -> NgResult<()> {
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
    let auto_comments: Vec<String> = match lookup_arg(args, "auto_comments") {
        Some(NGLessObject::List(items)) => items
            .iter()
            .map(|i| match i {
                NGLessObject::Symbol(s) => Ok(s.clone()),
                other => Err(NgError::script(format!(
                    "write: auto_comments must be a list of symbols, got {other:?}"
                ))),
            })
            .collect::<NgResult<Vec<String>>>()?,
        None => Vec::new(),
        Some(other) => {
            return Err(NgError::script(format!(
                "write: auto_comments must be a list of symbols, got {other:?}"
            )))
        }
    };
    let hash = match lookup_arg(args, "__hash") {
        Some(NGLessObject::String(s)) => Some(s.clone()),
        _ => None,
    };
    let comments = build_comment(
        comment.as_deref(),
        &auto_comments,
        script_text,
        hash.as_deref(),
    )?;
    // Fast path: an unmodified TSV with no comments is copied (and recompressed) verbatim.
    if format == "tsv" && comments.is_empty() {
        return copy_fastq(path, ofile);
    }
    let body = crate::compression::read_to_string(&path.to_string_lossy())?;
    let mut out = String::new();
    for c in &comments {
        out.push_str("# ");
        out.push_str(c);
        out.push('\n');
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

/// `readlines(fname)`: read a file and return its lines as a list of strings (mirrors
/// `executeReadlines`). Lines are split on `\n`, a trailing `\r` is stripped (Windows line
/// endings), and no trailing empty line is produced when the file ends with a newline — exactly
/// what `linesC` does and what Rust's `str::lines()` provides.
fn execute_readlines(v: &NGLessObject) -> NgResult<NGLessObject> {
    let fname = match v {
        NGLessObject::String(s) => s,
        other => {
            return Err(NgError::should_not_occur(format!(
                "executeReadlines called with argument: {other:?}"
            )))
        }
    };
    let content = std::fs::read_to_string(fname).map_err(|e| {
        NgError::new(
            NgErrorType::DataError,
            format!("Could not read file '{fname}': {e}"),
        )
    })?;
    Ok(NGLessObject::List(
        content
            .lines()
            .map(|l| NGLessObject::String(l.to_string()))
            .collect(),
    ))
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
        ("some_match", NGLessObject::MappedRead(samlines)) => {
            let target = match _arg {
                Some(NGLessObject::String(s)) => s.as_str(),
                _ => {
                    return Err(NgError::script(
                        "some_match method requires a string argument",
                    ))
                }
            };
            Ok(NGLessObject::Bool(
                samlines.iter().any(|s| s.rname == target),
            ))
        }
        ("pe_filter", NGLessObject::MappedRead(samlines)) => Ok(NGLessObject::MappedRead(
            select::filter_pe(samlines.clone()),
        )),
        ("unique", NGLessObject::MappedRead(samlines)) => {
            Ok(NGLessObject::MappedRead(select::m_unique(samlines.clone())))
        }
        ("allbest", NGLessObject::MappedRead(samlines)) => {
            let use_newer = lookup_bool(_args, "__version11_or_higher", false)?;
            Ok(NGLessObject::MappedRead(select::m_besthit(
                use_newer,
                samlines.clone(),
            )))
        }
        ("name", NGLessObject::ReadSet { name, .. }) => Ok(NGLessObject::String(name.clone())),
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
        Some(NGLessObject::String(s))
        | Some(NGLessObject::Filename(s))
        | Some(NGLessObject::SequenceSet(s)) => Some(s.clone()),
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

/// The tag of a counts line: the bytes before the first TAB (mirrors `Interpretation.CountFile.tag`).
fn count_tag(line: &str) -> &str {
    match line.find('\t') {
        Some(i) => &line[..i],
        None => line,
    }
}

/// Whether every consecutive pair of lines is strictly tag-ordered (`checkCountFile`).
fn is_tag_ordered(text: &str) -> bool {
    let mut prev: Option<&str> = None;
    for line in text.lines() {
        if let Some(p) = prev {
            if count_tag(p) >= count_tag(line) {
                return false;
            }
        }
        prev = Some(line);
    }
    true
}

/// Reorder a counts file by tag (`normalizeCountFile`): keep leading comment/blank lines and the
/// following sample-name header line in place, then stable-sort the remaining rows by tag.
fn normalize_count_file(text: &str) -> String {
    let lines: Vec<&str> = text.lines().collect();
    let mut i = 0;
    let mut out: Vec<&str> = Vec::with_capacity(lines.len());
    while i < lines.len() && (lines[i].is_empty() || lines[i].starts_with('#')) {
        out.push(lines[i]);
        i += 1;
    }
    if i < lines.len() {
        out.push(lines[i]);
        i += 1;
    }
    let mut content: Vec<&str> = lines[i..].to_vec();
    content.sort_by(|a, b| count_tag(a).cmp(count_tag(b)));
    out.extend(content);
    let mut s = out.join("\n");
    s.push('\n');
    s
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
        other => {
            return Err(NgError::script(format!(
                "Unexpected value for `multiple`: {other}"
            )))
        }
    };

    let strand_specific = lookup_bool(args, "strand", false)?;
    let sense_default = if strand_specific { "sense" } else { "both" };
    let strand_mode = match lookup_symbol(args, "sense", sense_default)?.as_str() {
        "both" => StrandMode::Both,
        "sense" => StrandMode::Sense,
        "antisense" => StrandMode::Antisense,
        other => {
            return Err(NgError::script(format!(
                "Unexpected value for `sense`: {other}"
            )))
        }
    };

    let intersect_mode = match lookup_symbol(args, "mode", "union")?.as_str() {
        "union" => IntersectMode::Union,
        "intersection_strict" => IntersectMode::Strict,
        "intersection_non_empty" => IntersectMode::NonEmpty,
        other => {
            return Err(NgError::script(format!(
                "Unexpected value for `mode`: {other}"
            )))
        }
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
    use crate::sam::parse_sam_line;

    #[test]
    fn count_file_unordered_is_normalized() {
        let input = "# comment\n\tsample.fq\nB\t112\nA\t1\n-1\t5\n";
        assert!(!is_tag_ordered(input));
        assert_eq!(
            normalize_count_file(input),
            "# comment\n\tsample.fq\n-1\t5\nA\t1\nB\t112\n"
        );
    }

    #[test]
    fn count_file_already_ordered() {
        // No comments; header tag "" sorts before the rest, which are ascending.
        let input = "\tsample.fq\n-1\t5\nA\t1\nB\t2\n";
        assert!(is_tag_ordered(input));
    }

    #[test]
    fn build_comment_manual_then_script() {
        // `auto_comments=[{script}]` expands to a header line plus the script source, each
        // line indented by four spaces; a manual comment (if any) comes first.
        let script = "ngless '1.5'\n\ncount(x)\n";
        let cs = build_comment(Some("hello"), &["script".to_string()], script, None).unwrap();
        assert_eq!(
            cs,
            vec![
                "hello".to_string(),
                "Output generated by:".to_string(),
                "    ngless '1.5'".to_string(),
                "    ".to_string(),
                "    count(x)".to_string(),
            ]
        );
    }

    #[test]
    fn build_comment_unknown_auto_errors() {
        assert!(build_comment(None, &["date".to_string()], "", None).is_err());
    }
    use crate::parser::parse_ngless;

    fn s(v: &[&str]) -> Vec<String> {
        v.iter().map(|x| x.to_string()).collect()
    }

    #[test]
    fn match_up_pairs_with_singles() {
        // A paired sample with a singles file plus an unrelated single-end file.
        let files = s(&["a.pair.1.fq", "a.pair.2.fq", "a.single.fq", "b.fq"]);
        let (singletons, paired) = match_up(&files).unwrap();
        assert_eq!(singletons, s(&["b.fq"]));
        assert_eq!(
            paired,
            vec![(
                "a.pair.1.fq".to_string(),
                "a.pair.2.fq".to_string(),
                Some("a.single.fq".to_string())
            )]
        );
    }

    #[test]
    fn match_up_underscore_suffix_no_singles() {
        // `_1`/`_2` markers, gzip-compressed, with no matching singles file.
        let files = s(&["x_1.fq.gz", "x_2.fq.gz"]);
        let (singletons, paired) = match_up(&files).unwrap();
        assert!(singletons.is_empty());
        assert_eq!(
            paired,
            vec![("x_1.fq.gz".to_string(), "x_2.fq.gz".to_string(), None)]
        );
    }

    #[test]
    fn match_up_dedups_mate2() {
        // Seeing pair.2 before/after pair.1 must not produce a duplicate record.
        let files = s(&["a.pair.2.fq", "a.pair.1.fq"]);
        let (_, paired) = match_up(&files).unwrap();
        assert_eq!(paired.len(), 1);
    }

    // Mirrors Tests/LoadFQDirectory.hs `case_error_p1`/`case_error_p2`: a lone mate (pair.1 with no
    // pair.2, or vice-versa) is an error.
    #[test]
    fn match_up_unpaired_mate_is_error() {
        assert!(match_up(&s(&["sample/sample.pair.1.fq.bz2"])).is_err());
        assert!(match_up(&s(&["sample/sample.pair.2.fq.bz2"])).is_err());
    }

    // Mirrors Tests/LoadFQDirectory.hs `known_cases`: a single uncompressed/compressed read file
    // with no pairing markers is a singleton, not a pair.
    #[test]
    fn match_up_lone_files_are_singletons() {
        for f in [
            "sample/uncompressed.fq",
            "sample/sample.fq.gz",
            "sample/sample.fq.bz2",
        ] {
            let (singletons, paired) = match_up(&s(&[f])).unwrap();
            assert_eq!(singletons, s(&[f]));
            assert!(paired.is_empty());
        }
    }

    // Mirrors Tests/LoadFQDirectory.hs `known_cases`: bz2 pair.1/pair.2/single in a deeper
    // directory group into one paired record with a singles file.
    #[test]
    fn match_up_bz2_paired_with_single() {
        let files = s(&[
            "mocat_sample_bz2_paired/sample/sample.pair.1.fq.bz2",
            "mocat_sample_bz2_paired/sample/sample.pair.2.fq.bz2",
            "mocat_sample_bz2_paired/sample/sample.single.fq.bz2",
        ]);
        let (singletons, paired) = match_up(&files).unwrap();
        assert!(singletons.is_empty());
        assert_eq!(
            paired,
            vec![(
                "mocat_sample_bz2_paired/sample/sample.pair.1.fq.bz2".to_string(),
                "mocat_sample_bz2_paired/sample/sample.pair.2.fq.bz2".to_string(),
                Some("mocat_sample_bz2_paired/sample/sample.single.fq.bz2".to_string())
            )]
        );
    }

    // Mirrors Tests/LoadFQDirectory.hs `known_cases` "mixed": two paired samples (one with a
    // singles file, one without) plus an unrelated single-end file.
    #[test]
    fn match_up_mixed() {
        let files = s(&[
            "sample/sampleC.single.fq.bz2",
            "sample/sampleB.pair.1.fq.bz2",
            "sample/sampleB.pair.2.fq.bz2",
            "sample/sampleA.pair.1.fq.bz2",
            "sample/sampleA.pair.2.fq.bz2",
            "sample/sampleA.single.fq.bz2",
        ]);
        let (singletons, mut paired) = match_up(&files).unwrap();
        assert_eq!(singletons, s(&["sample/sampleC.single.fq.bz2"]));
        paired.sort();
        assert_eq!(
            paired,
            vec![
                (
                    "sample/sampleA.pair.1.fq.bz2".to_string(),
                    "sample/sampleA.pair.2.fq.bz2".to_string(),
                    Some("sample/sampleA.single.fq.bz2".to_string())
                ),
                (
                    "sample/sampleB.pair.1.fq.bz2".to_string(),
                    "sample/sampleB.pair.2.fq.bz2".to_string(),
                    None
                ),
            ]
        );
    }

    #[test]
    fn subsample_keeps_one_in_ten_records() {
        // 20 four-line records => 80 lines. drop90 keeps the first 4 of every 40 lines,
        // i.e. records 0 and 10 => 8 lines.
        let mut text = String::new();
        for i in 0..20 {
            text.push_str(&format!("@r{i}\nACGT\n+\nIIII\n"));
        }
        let out = subsample_text(&text);
        let lines: Vec<&str> = out.lines().collect();
        assert_eq!(lines.len(), 8);
        assert_eq!(lines[0], "@r0");
        assert_eq!(lines[4], "@r10");
    }

    fn run(text: &str) -> NgResult<()> {
        let script = parse_ngless("test", true, text).expect("parse failed");
        interpret(
            &script.body,
            &std::env::temp_dir(),
            false,
            text,
            &[],
            &[],
            false,
            Vec::new(),
            Vec::new(),
            vec!["bwa".to_string()],
            (1, 5),
        )
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
    fn fastq_of_missing_file_errors() {
        // `fastq` of a non-existent file fails before mapping is ever reached.
        assert!(run("ngless '1.5'\nx = fastq('/nonexistent.fq')\ny = map(x)\n").is_err());
    }

    #[test]
    fn expand_path_no_placeholder_is_identity() {
        assert_eq!(expand_path_candidates("ref.fna", &[]), vec!["ref.fna"]);
        assert_eq!(
            expand_path_candidates("dir/ref.fna", &["ignored".to_string()]),
            vec!["dir/ref.fna"]
        );
    }

    #[test]
    fn expand_path_placeholder_joins_search_dirs() {
        let search = vec!["subdir1".to_string(), "subdir2".to_string()];
        assert_eq!(
            expand_path_candidates("<data>/ref.fna", &search),
            vec!["subdir1/ref.fna", "subdir2/ref.fna"]
        );
        // An empty placeholder `<>` works the same way.
        assert_eq!(
            expand_path_candidates("<>/ref.fna", &["d".to_string()]),
            vec!["d/ref.fna"]
        );
    }

    #[test]
    fn expand_path_named_search_entries() {
        // `name=/path` entries only match the matching placeholder name.
        let search = vec!["data=/opt/refs".to_string(), "other=/elsewhere".to_string()];
        assert_eq!(
            expand_path_candidates("<data>/ref.fna", &search),
            vec!["/opt/refs/ref.fna"]
        );
        // A plain entry always matches.
        assert_eq!(
            expand_path_candidates("<data>/ref.fna", &["/plain".to_string()]),
            vec!["/plain/ref.fna"]
        );
    }

    #[test]
    fn parse_fasta_counts_sequence_bases() {
        let fa = ">seq1 desc\nACGT\nACGT\n>seq2\nTTTTTT\n";
        let seqs = parse_fasta(fa);
        assert_eq!(seqs.len(), 2);
        assert_eq!(seqs[0].length, 8);
        assert_eq!(seqs[0].record, ">seq1 desc\nACGT\nACGT\n");
        assert_eq!(seqs[1].length, 6);
    }

    #[test]
    fn pick_best_keeps_maximal_match() {
        // Two alignments of the same mate: a 50-base perfect match beats a 30-base one.
        let good = parse_sam_line("r\t0\tg1\t1\t60\t50M\t*\t0\t0\tACGT\tIIII\tNM:i:0").unwrap();
        let worse = parse_sam_line("r\t0\tg2\t1\t60\t30M20S\t*\t0\t0\tACGT\tIIII\tNM:i:0").unwrap();
        let merged = merge_sam_group(&[good.clone(), worse]).unwrap();
        assert_eq!(merged.len(), 1);
        assert_eq!(merged[0].rname, "g1");
    }

    #[test]
    fn pick_best_unaligned_passes_one_through() {
        let una = parse_sam_line("r\t4\t*\t0\t0\t*\t*\t0\t0\tACGT\tIIII\tNM:i:0").unwrap();
        let merged = merge_sam_group(&[una.clone(), una]).unwrap();
        assert_eq!(merged.len(), 1);
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
