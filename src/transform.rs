//! Script transforms that run after type-checking and validation, before interpretation.
//!
//! Mirrors the relevant parts of `NGLess/Transform.hs`. The only transform implemented here is
//! `addOutputHash`, which computes a content hash for each `write`/`collect` output call and
//! injects it as a hidden `__hash` keyword argument. The hash is what the `auto_comments=[{hash}]`
//! feature reports as `# Output hash: <md5>`.
//!
//! The hash must be **byte-identical** to the Haskell implementation, which computes
//! `MD5.md5s . MD5.Str . (versionString ++) . show $ expr'` where:
//!   * `versionString = show nv ++ show (sortOn modName modInfos)` — the language version and the
//!     sorted list of loaded modules (see [`version_string`]);
//!   * `expr'` is the (sub)expression with every variable `Lookup` replaced by a `Lookup` whose
//!     variable name is the hash of the expression that was assigned to it, and every *nested*
//!     (non-top, non-void) function call replaced by such a hashed `Lookup` (this mirrors the
//!     earlier `addTemporaries` pass, which lifts nested calls into `temp$N` bindings).
//!
//! Reproducing Haskell's derived `Show` for the AST (see [`show_expr`]) is therefore load-bearing.

use std::collections::HashMap;

use crate::ast::{BOp, Block, Expression, FuncName, Index, NGLType, UOp, Variable};
use crate::modules::Function;

/// Inject the hidden `__hash` keyword argument into every `write`/`collect` call (mirrors
/// `addOutputHash`). `version` is the script's `(major, minor)` language version, `imports` the
/// header module imports, and `funcs` the full function table (used to look up return types so
/// nested calls can be lifted the way `addTemporaries` would).
pub fn add_output_hash(
    body: &mut [(usize, Expression)],
    version: (i64, i64),
    imports: &[crate::ast::ModInfo],
    funcs: &[Function],
) {
    let vstr = version_string(version, imports);
    let mut ret_types: HashMap<String, NGLType> = HashMap::new();
    for f in funcs {
        ret_types.insert(f.name.0.clone(), f.ret_type.clone());
    }
    let ctx = HashCtx {
        vstr,
        ret_types: &ret_types,
    };
    // `state0` in Haskell seeds the variable map with ARGV -> "ARGV".
    let mut varmap: HashMap<String, String> = HashMap::new();
    varmap.insert("ARGV".to_string(), "ARGV".to_string());

    for (_, expr) in body.iter_mut() {
        match expr {
            Expression::Assignment(Variable(v), rhs) => {
                let h = ctx.hash_of(rhs, &varmap, true);
                varmap.insert(v.clone(), h);
            }
            Expression::FunctionCall(FuncName(fname), oarg, kwargs, _)
                if fname == "write" || fname == "collect" =>
            {
                // The output argument was lifted by addTemporaries if it was a nested call, so it
                // is hashed as a (potentially extracted) sub-expression: is_top = false.
                let h = ctx.hash_of(oarg, &varmap, false);
                kwargs.insert(
                    0,
                    (
                        Variable("__hash".to_string()),
                        Expression::ConstStr(h),
                    ),
                );
            }
            _ => {}
        }
    }
}

/// The parallel module's transform (mirrors `parallelTransform`): rewrite `run_for_all`,
/// `set_parallel_tag`, and inject the lock hash into `lock1`/`run_for_all` calls. `include_for_all`
/// is true for parallel version 1.1+. Runs after [`add_output_hash`], matching the Haskell order
/// (pre-transforms before module transforms).
pub fn parallel_transform(
    body: &mut Vec<(usize, Expression)>,
    include_for_all: bool,
) -> Result<(), String> {
    process_run_for_all(body, include_for_all)?;
    process_set_parallel_tag(body);
    add_lock_hash(body);
    Ok(())
}

fn is_run_for_all(name: &str) -> bool {
    name == "run_for_all" || name == "run_for_all_samples"
}

/// `processRunForAll`. When `!include_for_all`, error if `run_for_all` is used. Otherwise locate
/// the single `v = run_for_all(slist)` assignment, save the iterator/list into hidden variables,
/// optionally emit a `set_parallel_tag`, and inject `current`/`allneeded` into later `collect`s.
fn process_run_for_all(
    body: &mut Vec<(usize, Expression)>,
    include_for_all: bool,
) -> Result<(), String> {
    // Find the run_for_all assignment.
    let mut found: Option<usize> = None;
    for (idx, (lno, e)) in body.iter().enumerate() {
        if let Expression::Assignment(_, rhs) = e {
            if let Expression::FunctionCall(FuncName(fname), _, _, _) = rhs.as_ref() {
                if is_run_for_all(fname) {
                    if !include_for_all {
                        return Err(format!(
                            "Function '{fname}' is only available in parallel module version 1.1+. Please upgrade your import"
                        ));
                    }
                    if let Some(_prev) = found {
                        return Err(format!(
                            "The functions 'run_for_all'/'run_for_all_samples' can only be called once (seen twice, second on line {lno})"
                        ));
                    }
                    found = Some(idx);
                }
            }
        }
    }
    let idx = match found {
        None => return Ok(()),
        Some(i) => i,
    };
    let (lno, var, slist, tag) = {
        let (lno, e) = &body[idx];
        match e {
            Expression::Assignment(v, rhs) => match rhs.as_ref() {
                Expression::FunctionCall(_, slist, kwargs, _) => {
                    let tag = kwargs
                        .iter()
                        .find(|(Variable(k), _)| k == "tag")
                        .map(|(_, e)| e.clone());
                    (*lno, v.clone(), (**slist).clone(), tag)
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    };

    // Inject current/allneeded into every later collect.
    for (_, e) in body.iter_mut().skip(idx + 1) {
        if let Expression::FunctionCall(FuncName(fname), _, kwargs, _) = e {
            if fname == "collect" {
                kwargs.insert(
                    0,
                    (
                        Variable("current".to_string()),
                        Expression::Lookup(
                            Some(NGLType::String),
                            Variable("$parallel$iterator".to_string()),
                        ),
                    ),
                );
                kwargs.insert(
                    0,
                    (
                        Variable("allneeded".to_string()),
                        Expression::Lookup(
                            Some(NGLType::String),
                            Variable("$parallel$list".to_string()),
                        ),
                    ),
                );
            }
        }
    }

    // After the run_for_all line, save the iterator and the list.
    let save_match = (
        lno,
        Expression::Assignment(
            Variable("$parallel$iterator".to_string()),
            Box::new(Expression::Lookup(Some(NGLType::String), var)),
        ),
    );
    let save_list = (
        lno,
        Expression::Assignment(Variable("$parallel$list".to_string()), Box::new(slist)),
    );
    body.insert(idx + 1, save_list);
    body.insert(idx + 1, save_match);

    // If a tag was given, emit a set_parallel_tag call before the run_for_all line.
    if let Some(tagexpr) = tag {
        body.insert(
            idx,
            (
                lno,
                Expression::FunctionCall(
                    FuncName("set_parallel_tag".to_string()),
                    Box::new(tagexpr),
                    vec![],
                    None,
                ),
            ),
        );
    }
    Ok(())
}

/// `processSetParallelTag`. Replace `set_parallel_tag(e)` with `$parallel$tag = e`, and inject the
/// hidden `__parallel_tag` argument into subsequent `lock1`/`collect` calls.
fn process_set_parallel_tag(body: &mut [(usize, Expression)]) {
    let mut has_tag = false;
    for (_, e) in body.iter_mut() {
        let mut changed = false;
        if let Expression::FunctionCall(FuncName(fname), arg, kwargs, None) = e {
            if fname == "set_parallel_tag" && kwargs.is_empty() {
                let arg = (**arg).clone();
                *e = Expression::Assignment(Variable("$parallel$tag".to_string()), Box::new(arg));
                changed = true;
            } else if has_tag && (fname == "lock1" || fname == "collect") {
                kwargs.insert(
                    0,
                    (
                        Variable("__parallel_tag".to_string()),
                        Expression::Lookup(
                            Some(NGLType::String),
                            Variable("$parallel$tag".to_string()),
                        ),
                    ),
                );
                changed = true;
            }
        }
        has_tag = has_tag || changed;
    }
}

/// `addLockHash`. Inject `__hash` into `lock1`/`run_for_all`/`run_for_all_samples` calls. The hash
/// is `md5(show (map snd body))`; it only names lock/stats directories, so it need only be
/// deterministic, but we reproduce the Haskell formula for fidelity.
fn add_lock_hash(body: &mut [(usize, Expression)]) {
    let shown = {
        let mut s = String::from("[");
        for (i, (_, e)) in body.iter().enumerate() {
            if i > 0 {
                s.push(',');
            }
            s.push_str(&show_expr(e));
        }
        s.push(']');
        s
    };
    let h = md5_hex(shown.as_bytes());
    for (_, e) in body.iter_mut() {
        if let Expression::FunctionCall(FuncName(fname), _, kwargs, _) = e {
            if fname == "lock1" || is_run_for_all(fname) {
                kwargs.insert(
                    0,
                    (Variable("__hash".to_string()), Expression::ConstStr(h.clone())),
                );
            }
        }
    }
}

struct HashCtx<'a> {
    vstr: String,
    ret_types: &'a HashMap<String, NGLType>,
}

impl HashCtx<'_> {
    /// Hash of an expression, mirroring `hashOf`: rewrite Lookups (and, when `!is_top`, the call
    /// itself) and then `md5(versionString ++ show rewritten)`.
    fn hash_of(&self, e: &Expression, varmap: &HashMap<String, String>, is_top: bool) -> String {
        let rewritten = self.rewrite(e, varmap, is_top);
        md5_hex(format!("{}{}", self.vstr, show_expr(&rewritten)).as_bytes())
    }

    /// Rewrite an expression for hashing. `is_top` is true only for the outermost call of an
    /// assignment RHS (which `addTemporaries` leaves in place); every nested non-void function
    /// call is replaced by a `Lookup` to a variable whose name is that call's hash.
    fn rewrite(
        &self,
        e: &Expression,
        varmap: &HashMap<String, String>,
        is_top: bool,
    ) -> Expression {
        match e {
            Expression::Lookup(t, Variable(n)) => {
                let name = varmap.get(n).cloned().unwrap_or_else(|| n.clone());
                Expression::Lookup(t.clone(), Variable(name))
            }
            Expression::FunctionCall(fname, arg, kwargs, block) => {
                // Block variables map to themselves (injectBlockVars).
                let mut inner = varmap.clone();
                if let Some(b) = block {
                    inner.insert(b.variable.0.clone(), b.variable.0.clone());
                }
                let arg2 = self.rewrite(arg, &inner, false);
                let kwargs2: Vec<(Variable, Expression)> = kwargs
                    .iter()
                    .map(|(k, v)| (k.clone(), self.rewrite(v, &inner, false)))
                    .collect();
                let block2 = block.as_ref().map(|b| Block {
                    variable: b.variable.clone(),
                    body: Box::new(self.rewrite(&b.body, &inner, false)),
                });
                let call = Expression::FunctionCall(
                    fname.clone(),
                    Box::new(arg2),
                    kwargs2,
                    block2,
                );
                if is_top {
                    return call;
                }
                let ret = self
                    .ret_types
                    .get(&fname.0)
                    .cloned()
                    .unwrap_or(NGLType::Void);
                if ret == NGLType::Void {
                    // Void calls are not lifted into temporaries.
                    call
                } else {
                    let h = md5_hex(format!("{}{}", self.vstr, show_expr(&call)).as_bytes());
                    Expression::Lookup(Some(ret), Variable(h))
                }
            }
            Expression::ListExpression(es) => Expression::ListExpression(
                es.iter().map(|x| self.rewrite(x, varmap, false)).collect(),
            ),
            Expression::UnaryOp(op, a) => {
                Expression::UnaryOp(*op, Box::new(self.rewrite(a, varmap, false)))
            }
            Expression::BinaryOp(op, a, b) => Expression::BinaryOp(
                *op,
                Box::new(self.rewrite(a, varmap, false)),
                Box::new(self.rewrite(b, varmap, false)),
            ),
            Expression::Condition(c, t, f) => Expression::Condition(
                Box::new(self.rewrite(c, varmap, false)),
                Box::new(self.rewrite(t, varmap, false)),
                Box::new(self.rewrite(f, varmap, false)),
            ),
            Expression::IndexExpression(a, ix) => {
                let ix2 = match ix {
                    Index::One(i) => Index::One(Box::new(self.rewrite(i, varmap, false))),
                    Index::Two(a, b) => Index::Two(
                        a.as_ref().map(|x| Box::new(self.rewrite(x, varmap, false))),
                        b.as_ref().map(|x| Box::new(self.rewrite(x, varmap, false))),
                    ),
                };
                Expression::IndexExpression(Box::new(self.rewrite(a, varmap, false)), ix2)
            }
            Expression::Assignment(v, a) => {
                Expression::Assignment(v.clone(), Box::new(self.rewrite(a, varmap, false)))
            }
            Expression::MethodCall(m, self_e, arg, kwargs) => Expression::MethodCall(
                m.clone(),
                Box::new(self.rewrite(self_e, varmap, false)),
                arg.as_ref().map(|x| Box::new(self.rewrite(x, varmap, false))),
                kwargs
                    .iter()
                    .map(|(k, v)| (k.clone(), self.rewrite(v, varmap, false)))
                    .collect(),
            ),
            Expression::Sequence(es) => {
                Expression::Sequence(es.iter().map(|x| self.rewrite(x, varmap, false)).collect())
            }
            // Constants and atoms are unchanged.
            other => other.clone(),
        }
    }
}

/// `versionString = show nv ++ show (sortOn modName modInfos)`.
fn version_string(version: (i64, i64), imports: &[crate::ast::ModInfo]) -> String {
    let (maj, min) = version;
    let mut mods = loaded_modules(version, imports);
    mods.sort_by(|a, b| a.0.cmp(&b.0));
    let mut s = format!("NGLVersion {maj} {min}[");
    for (i, (n, v)) in mods.iter().enumerate() {
        if i > 0 {
            s.push(',');
        }
        s.push_str(&format!(
            "ModInfo {{modName = {}, modVersion = {}}}",
            haskell_string(n),
            haskell_string(v)
        ));
    }
    s.push(']');
    s
}

/// Reproduce the list of loaded modules from `Execs/Main.hs::loadModules` (name, version). The
/// order is irrelevant (the caller sorts by name) but the membership and versions must match.
fn loaded_modules(version: (i64, i64), imports: &[crate::ast::ModInfo]) -> Vec<(String, String)> {
    let ge = |a: (i64, i64), b: (i64, i64)| a >= b;
    let mut mods: Vec<(String, String)> = Vec::new();
    let m = |n: &str, v: &str| (n.to_string(), v.to_string());
    mods.push(m("__builtin__", &format!("{}.{}", version.0, version.1)));
    if ge(version, (0, 6)) {
        mods.push(m("builtin.orffind", "0.6"));
    }
    if ge(version, (1, 2)) {
        mods.push(m("builtin.load_directory", "1.0"));
    }
    if ge(version, (1, 5)) {
        mods.push(m("builtin.samples", "1.5"));
    }
    mods.push(m("builtin.readlines", "0.0"));
    mods.push(m("builtin.argv", "0.0"));
    mods.push(m("builtin.assemble", "0.0"));
    mods.push(m("builtin.as_reads", "0.0"));
    mods.push(m("builtin.checks", "0.0"));
    mods.push(m("builtin.remove", "0.0"));
    mods.push(m("builtin.stats", "0.6"));
    for imp in imports {
        let (name, ver) = (imp.name(), imp.version());
        let info = match name {
            "example" => m("stdlib.example", "1.0"),
            "batch" => m("stdlib.batch", "1.0"),
            "samtools" => m("stdlib.samtools", if ver == "0.0" { "0.0" } else { "1.0" }),
            "mocat" => m("stdlib.mocat", "1.1"),
            "parallel" => m("stdlib.parallel", ver),
            "soap" => m("stdlib.soap", "1.0"),
            "minimap2" => m("stdlib.minimap2", "1.0"),
            // External modules and anything else: best-effort (not exercised by hash tests).
            _ => m(name, ver),
        };
        mods.push(info);
    }
    mods
}

// ---------------------------------------------------------------------------
// Haskell-compatible `show` for the AST (mirrors the `Show Expression` instance in Language.hs).
// ---------------------------------------------------------------------------

/// Reproduce `show :: Expression -> String`.
pub fn show_expr(e: &Expression) -> String {
    match e {
        Expression::Lookup(Some(t), Variable(v)) => format!("Lookup '{v}' as {}", show_ngltype(t, 0)),
        Expression::Lookup(None, Variable(v)) => format!("Lookup '{v}' (type unknown)"),
        Expression::ConstStr(t) => haskell_string(t),
        Expression::ConstInt(n) => n.to_string(),
        Expression::ConstDouble(f) => show_double(*f),
        Expression::ConstBool(b) => if *b { "True" } else { "False" }.to_string(),
        Expression::ConstSymbol(t) => format!("{{{t}}}"),
        Expression::BuiltinConstant(Variable(v)) => v.clone(),
        Expression::ListExpression(es) => show_list(es),
        Expression::Continue => "continue".to_string(),
        Expression::Discard => "discard".to_string(),
        Expression::UnaryOp(UOp::Len, a) => format!("len({})", show_expr(a)),
        Expression::UnaryOp(op, a) => format!("{}({})", show_uop(*op), show_expr(a)),
        Expression::BinaryOp(op, a, b) => {
            format!("BinaryOp({} -{}- {})", show_expr(a), show_bop(*op), show_expr(b))
        }
        Expression::Condition(c, a, b) => format!(
            "if [{}] then {{{}}} else {{{}}}",
            show_expr(c),
            show_expr(a),
            show_expr(b)
        ),
        Expression::IndexExpression(a, ix) => format!("{}[{}]", show_expr(a), show_index(ix)),
        Expression::Assignment(Variable(v), a) => format!("{v} = {}", show_expr(a)),
        Expression::FunctionCall(FuncName(fname), a, args, block) => {
            let blk = match block {
                None => String::new(),
                Some(b) => format!("using {{{}}}", show_block(b)),
            };
            format!("{fname}({}{}){blk}", show_expr(a), show_args(args))
        }
        Expression::MethodCall(m, self_e, a, args) => {
            let arg = match a {
                None => String::new(),
                Some(x) => show_expr(x),
            };
            format!(
                "({}).{}( {arg}{} )",
                show_expr(self_e),
                m.0,
                show_args(args)
            )
        }
        Expression::Sequence(es) => format!("Sequence {}", show_list(es)),
    }
}

fn show_args(args: &[(Variable, Expression)]) -> String {
    let mut s = String::new();
    for (Variable(v), e) in args {
        s.push_str(&format!("; {v}={}", show_expr(e)));
    }
    s
}

fn show_list(es: &[Expression]) -> String {
    let mut s = String::from("[");
    for (i, e) in es.iter().enumerate() {
        if i > 0 {
            s.push(',');
        }
        s.push_str(&show_expr(e));
    }
    s.push(']');
    s
}

fn show_block(b: &Block) -> String {
    // `show (Block v body)` is derived: Block {blockVariable = Variable "v", blockBody = ...}.
    format!(
        "Block {{blockVariable = Variable {}, blockBody = {}}}",
        haskell_string(&b.variable.0),
        show_expr(&b.body)
    )
}

fn show_index(ix: &Index) -> String {
    match ix {
        Index::One(e) => format!("IndexOne ({})", show_expr(e)),
        Index::Two(a, b) => {
            let f = |o: &Option<Box<Expression>>| match o {
                None => "Nothing".to_string(),
                Some(e) => format!("Just ({})", show_expr(e)),
            };
            format!("IndexTwo ({}) ({})", f(a), f(b))
        }
    }
}

fn show_uop(op: UOp) -> &'static str {
    match op {
        UOp::Len => "UOpLen",
        UOp::Minus => "UOpMinus",
        UOp::Not => "UOpNot",
    }
}

fn show_bop(op: BOp) -> &'static str {
    match op {
        BOp::Add => "BOpAdd",
        BOp::Mul => "BOpMul",
        BOp::GT => "BOpGT",
        BOp::GTE => "BOpGTE",
        BOp::LT => "BOpLT",
        BOp::LTE => "BOpLTE",
        BOp::EQ => "BOpEQ",
        BOp::NEQ => "BOpNEQ",
        BOp::PathAppend => "BOpPathAppend",
    }
}

/// Derived `Show NGLType`. `prec` is the surrounding precedence (11 means "as a constructor
/// argument", which forces parentheses around constructor applications that take arguments).
fn show_ngltype(t: &NGLType, prec: u8) -> String {
    let simple = |s: &str| s.to_string();
    let app = |head: &str, inner: String| {
        let body = format!("{head} {inner}");
        if prec >= 11 {
            format!("({body})")
        } else {
            body
        }
    };
    match t {
        NGLType::String => simple("NGLString"),
        NGLType::Integer => simple("NGLInteger"),
        NGLType::Double => simple("NGLDouble"),
        NGLType::Bool => simple("NGLBool"),
        NGLType::Symbol => simple("NGLSymbol"),
        NGLType::Filename => simple("NGLFilename"),
        NGLType::Read => simple("NGLRead"),
        NGLType::ReadSet => simple("NGLReadSet"),
        NGLType::MappedRead => simple("NGLMappedRead"),
        NGLType::MappedReadSet => simple("NGLMappedReadSet"),
        NGLType::SequenceSet => simple("NGLSequenceSet"),
        NGLType::Counts => simple("NGLCounts"),
        NGLType::Void => simple("NGLVoid"),
        NGLType::Any => simple("NGLAny"),
        NGLType::List(inner) => app("NGList", show_ngltype(inner, 11)),
        NGLType::Union(ts) => {
            let body = ts
                .iter()
                .map(|t| show_ngltype(t, 0))
                .collect::<Vec<_>>()
                .join(",");
            app("NGLUnion", format!("[{body}]"))
        }
    }
}

/// Haskell `show` of a `String`/`Text`: double-quoted with escaping of `"`, `\`, and control
/// characters (as `\<decimal>`, with the `\&` separator where Haskell needs it).
fn haskell_string(s: &str) -> String {
    let mut out = String::from("\"");
    let mut prev_numeric_escape = false;
    for c in s.chars() {
        let mut this_numeric_escape = false;
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\t' => out.push_str("\\t"),
            '\r' => out.push_str("\\r"),
            c if (c as u32) < 0x20 || (c as u32) == 0x7f => {
                out.push_str(&format!("\\{}", c as u32));
                this_numeric_escape = true;
            }
            c if (c as u32) > 0x7f => {
                out.push_str(&format!("\\{}", c as u32));
                this_numeric_escape = true;
            }
            c => {
                // Haskell inserts "\&" between a numeric escape and a following digit.
                if prev_numeric_escape && c.is_ascii_digit() {
                    out.push_str("\\&");
                }
                out.push(c);
            }
        }
        prev_numeric_escape = this_numeric_escape;
    }
    out.push('"');
    out
}

/// Best-effort reproduction of Haskell's `show :: Double -> String` for the common cases that
/// appear in hashed expressions. Whole numbers render as `N.0`.
fn show_double(f: f64) -> String {
    if f == f.trunc() && f.is_finite() {
        format!("{f:.1}")
    } else {
        format!("{f}")
    }
}

// ---------------------------------------------------------------------------
// MD5 (RFC 1321). Implemented in-crate to avoid a dependency and to match Haskell's `Data.Hash.MD5`.
// ---------------------------------------------------------------------------

/// Compute the lowercase hex MD5 digest of `input`.
pub fn md5_hex(input: &[u8]) -> String {
    let digest = md5(input);
    let mut s = String::with_capacity(32);
    for b in digest {
        s.push_str(&format!("{b:02x}"));
    }
    s
}

fn md5(input: &[u8]) -> [u8; 16] {
    const S: [u32; 64] = [
        7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 5, 9, 14, 20, 5, 9, 14, 20, 5,
        9, 14, 20, 5, 9, 14, 20, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 6, 10,
        15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21,
    ];
    const K: [u32; 64] = [
        0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee, 0xf57c0faf, 0x4787c62a, 0xa8304613,
        0xfd469501, 0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be, 0x6b901122, 0xfd987193,
        0xa679438e, 0x49b40821, 0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa, 0xd62f105d,
        0x02441453, 0xd8a1e681, 0xe7d3fbc8, 0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed,
        0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a, 0xfffa3942, 0x8771f681, 0x6d9d6122,
        0xfde5380c, 0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70, 0x289b7ec6, 0xeaa127fa,
        0xd4ef3085, 0x04881d05, 0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665, 0xf4292244,
        0x432aff97, 0xab9423a7, 0xfc93a039, 0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1,
        0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1, 0xf7537e82, 0xbd3af235, 0x2ad7d2bb,
        0xeb86d391,
    ];

    let mut a0: u32 = 0x67452301;
    let mut b0: u32 = 0xefcdab89;
    let mut c0: u32 = 0x98badcfe;
    let mut d0: u32 = 0x10325476;

    let mut msg = input.to_vec();
    let bit_len = (input.len() as u64).wrapping_mul(8);
    msg.push(0x80);
    while msg.len() % 64 != 56 {
        msg.push(0);
    }
    msg.extend_from_slice(&bit_len.to_le_bytes());

    for chunk in msg.chunks_exact(64) {
        let mut m = [0u32; 16];
        for (i, w) in m.iter_mut().enumerate() {
            *w = u32::from_le_bytes([
                chunk[i * 4],
                chunk[i * 4 + 1],
                chunk[i * 4 + 2],
                chunk[i * 4 + 3],
            ]);
        }
        let (mut a, mut b, mut c, mut d) = (a0, b0, c0, d0);
        for i in 0..64 {
            let (f, g) = match i {
                0..=15 => ((b & c) | (!b & d), i),
                16..=31 => ((d & b) | (!d & c), (5 * i + 1) % 16),
                32..=47 => (b ^ c ^ d, (3 * i + 5) % 16),
                _ => (c ^ (b | !d), (7 * i) % 16),
            };
            let f = f
                .wrapping_add(a)
                .wrapping_add(K[i])
                .wrapping_add(m[g]);
            a = d;
            d = c;
            c = b;
            b = b.wrapping_add(f.rotate_left(S[i]));
        }
        a0 = a0.wrapping_add(a);
        b0 = b0.wrapping_add(b);
        c0 = c0.wrapping_add(c);
        d0 = d0.wrapping_add(d);
    }

    let mut out = [0u8; 16];
    out[0..4].copy_from_slice(&a0.to_le_bytes());
    out[4..8].copy_from_slice(&b0.to_le_bytes());
    out[8..12].copy_from_slice(&c0.to_le_bytes());
    out[12..16].copy_from_slice(&d0.to_le_bytes());
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn md5_known_vectors() {
        assert_eq!(md5_hex(b""), "d41d8cd98f00b204e9800998ecf8427e");
        assert_eq!(md5_hex(b"abc"), "900150983cd24fb0d6963f7d28e17f72");
        assert_eq!(
            md5_hex(b"The quick brown fox jumps over the lazy dog"),
            "9e107d9d372bb6826bd81d3542a419d6"
        );
    }

    #[test]
    fn ngltype_show() {
        assert_eq!(show_ngltype(&NGLType::Counts, 0), "NGLCounts");
        assert_eq!(
            show_ngltype(&NGLType::List(Box::new(NGLType::String)), 0),
            "NGList NGLString"
        );
    }

    #[test]
    fn write_hash_matches_haskell() {
        // Reproduce the write-hash test's output hash (count(samfile('seq1_2.sam.bz2'), ...)).
        let version = (1, 1); // dummy; overwritten below via explicit version_string check
        let _ = version;
        let imports: Vec<crate::ast::ModInfo> = Vec::new();
        let vstr = version_string((1, 5), &imports);
        let h0 = md5_hex(format!("{vstr}samfile(\"seq1_2.sam.bz2\")").as_bytes());
        let h1 = md5_hex(
            format!(
                "{vstr}count(Lookup '{h0}' as NGLMappedReadSet; features=[\"seqname\"]; multiple={{all1}}; normalization={{fpkm}})"
            )
            .as_bytes(),
        );
        let hout = md5_hex(format!("{vstr}Lookup '{h1}' as NGLCounts").as_bytes());
        assert_eq!(hout, "41496276b6b59c90f8d8b0c4c756772d");
    }
}
