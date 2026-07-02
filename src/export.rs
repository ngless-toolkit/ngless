//! Script export modes, mirroring `NGLess/JSONScript.hs` (`--export-json`) and `NGLess/CWL.hs`
//! (`--export-cwl`). Both are gated on `--experimental-features` (see `cli::run_script`).
//!
//! `write_script_json` serialises the (validated) original script and the post-transform script to
//! a single JSON document; `write_cwl` emits a minimal CWL `CommandLineTool` wrapper derived from
//! the script's `ARGV` usage. Neither is exercised by the functional suite, so the structure mirrors
//! the Haskell encoders rather than being byte-validated against them (aeson's object key ordering is
//! itself unspecified).

use std::fs;
use std::io::Write;

use serde_json::{json, Map, Value};

use crate::ast::{BOp, Block, Expression, Header, Index, ModInfo, Script, UOp};
use crate::errors::{NgError, NgErrorType, NgResult};

/// `writeScriptJSON`: serialise `original` (the validated script) and `transformed` (post-transform)
/// to a JSON document at `fname`. Mirrors `JSONScript.hs::writeScriptJSON`.
pub fn write_script_json(fname: &str, original: &Script, transformed: &Script) -> NgResult<()> {
    let doc = json!({
        "header": original.header.as_ref().map(enc_header).unwrap_or(Value::Null),
        "original-script": enc_body(&original.body),
        "transformed-script": enc_body(&transformed.body),
    });
    let bytes = serde_json::to_vec(&doc).map_err(|e| {
        NgError::new(
            NgErrorType::ShouldNotOccur,
            format!("Could not serialise script to JSON: {e}"),
        )
    })?;
    write_output_file(fname, &bytes)
}

/// `EncHeader`: `{ "ngless-version": ver, "modules": [...] }`.
fn enc_header(h: &Header) -> Value {
    json!({
        "ngless-version": h.version,
        "modules": h.modules.iter().map(enc_mod).collect::<Vec<_>>(),
    })
}

/// `encodeMod`: a regular import is `module`, a local import is `local-module`.
fn enc_mod(m: &ModInfo) -> Value {
    match m {
        ModInfo::Import { name, version } => json!({
            "type": "module",
            "name": name,
            "version": version,
        }),
        ModInfo::LocalImport { name, version } => json!({
            "type": "local-module",
            "name": name,
            "version": version,
        }),
    }
}

/// `second EncExpression <$> nglBody`: each `(lno, expr)` pair becomes a two-element JSON array
/// `[lno, encoded-expr]` (aeson's tuple encoding).
fn enc_body(body: &[(usize, Expression)]) -> Value {
    Value::Array(
        body.iter()
            .map(|(lno, e)| json!([lno, enc_expr(e)]))
            .collect(),
    )
}

/// `toJSONEx`: encode a single expression.
fn enc_expr(e: &Expression) -> Value {
    match e {
        Expression::Lookup(t, v) => json!({
            "type": "lookup",
            "name": v.0,
            "ngless-type": enc_maybe_type(t.as_ref()),
        }),
        Expression::ConstStr(s) => Value::String(s.clone()),
        Expression::ConstInt(i) => json!(i),
        Expression::ConstDouble(d) => json!(d),
        Expression::ConstBool(b) => json!(b),
        Expression::ConstSymbol(s) => Value::String(format!("{{{s}}}")),
        Expression::BuiltinConstant(v) => Value::String(v.0.clone()),
        Expression::ListExpression(es) => Value::Array(es.iter().map(enc_expr).collect()),
        Expression::Continue => json!({ "type": "control0", "op": "continue" }),
        Expression::Discard => json!({ "type": "control0", "op": "discard" }),
        Expression::UnaryOp(op, a) => json!({
            "type": "uop",
            "op": enc_uop(*op),
            "arg": enc_expr(a),
        }),
        Expression::BinaryOp(op, l, r) => json!({
            "type": "binop",
            "op": enc_bop(*op),
            "left": enc_expr(l),
            "right": enc_expr(r),
        }),
        Expression::Condition(c, t, f) => json!({
            "type": "control",
            "op": "if",
            "cond": enc_expr(c),
            "if-true": enc_expr(t),
            "if-false": enc_expr(f),
        }),
        Expression::IndexExpression(a, ix) => json!({
            "type": "index",
            "arg": enc_expr(a),
            "index": enc_index(ix),
        }),
        Expression::Assignment(v, e) => json!({
            "type": "assignment",
            "target": v.0,
            "value": enc_expr(e),
        }),
        Expression::FunctionCall(fname, arg0, kwargs, block) => json!({
            "type": "function",
            "fname": fname.0,
            "arg0": enc_expr(arg0),
            "kwargs": enc_kwargs(kwargs),
            "block": enc_block(block.as_ref()),
        }),
        Expression::MethodCall(mname, ethis, marg, kwargs) => json!({
            "type": "method",
            "mname": mname.0,
            "this": enc_expr(ethis),
            "arg0": marg.as_ref().map(|e| enc_expr(e)).unwrap_or(Value::Null),
            "kwargs": enc_kwargs(kwargs),
        }),
        Expression::Sequence(es) => json!({
            "type": "control",
            "op": "sequence",
            "args": es.iter().map(enc_expr).collect::<Vec<_>>(),
        }),
        Expression::Optimized(oe) => json!({
            "type": "optimized",
            "value": enc_optimized(oe),
        }),
    }
}

/// `encodeOpt`: encode an internally-generated optimized expression. `--export-json` runs on the
/// pre-transform AST, so this is not exercised in practice, but is kept faithful to Haskell.
fn enc_optimized(oe: &crate::ast::OptimizedExpression) -> Value {
    match oe {
        crate::ast::OptimizedExpression::LenThresholdDiscard(v, op, thresh) => json!({
            "type": "len-threshold",
            "name": v.0,
            "op": enc_bop(*op),
            "thresh": thresh,
        }),
        crate::ast::OptimizedExpression::SubstrimReassign(v, mq) => json!({
            "type": "substrim-reassign",
            "name": v.0,
            "minqual": mq,
        }),
    }
}

/// `toJSONIndex`.
fn enc_index(ix: &Index) -> Value {
    match ix {
        Index::One(e) => json!({ "type": "index1", "arg": enc_expr(e) }),
        Index::Two(e0, e1) => json!({
            "type": "index2",
            "left": e0.as_ref().map(|e| enc_expr(e)).unwrap_or(Value::Null),
            "right": e1.as_ref().map(|e| enc_expr(e)).unwrap_or(Value::Null),
        }),
    }
}

/// `toJSONKwArgs`: an object mapping each keyword name to its encoded value.
fn enc_kwargs(kwargs: &[(crate::ast::Variable, Expression)]) -> Value {
    let mut m = Map::new();
    for (v, e) in kwargs {
        m.insert(v.0.clone(), enc_expr(e));
    }
    Value::Object(m)
}

/// `encodeBlock`: `Null` when absent, else `{ "type": "block", "variables": [n], "body": ... }`.
fn enc_block(block: Option<&Block>) -> Value {
    match block {
        None => Value::Null,
        Some(b) => json!({
            "type": "block",
            "variables": [b.variable.0],
            "body": enc_expr(&b.body),
        }),
    }
}

/// `encodeMaybeType`: `Null` for an un-inferred type, else the derived `Show` of the type.
fn enc_maybe_type(t: Option<&crate::ast::NGLType>) -> Value {
    match t {
        None => Value::Null,
        Some(t) => Value::String(crate::transform::show_ngltype(t, 0)),
    }
}

/// `encodeBOp`.
fn enc_bop(op: BOp) -> &'static str {
    match op {
        BOp::Add => "add",
        BOp::Mul => "mul",
        BOp::GT => "gt",
        BOp::GTE => "gte",
        BOp::LT => "lt",
        BOp::LTE => "lte",
        BOp::EQ => "eq",
        BOp::NEQ => "neq",
        BOp::PathAppend => "path_append",
    }
}

/// `encodeUOp`.
fn enc_uop(op: UOp) -> &'static str {
    match op {
        UOp::Len => "len",
        UOp::Minus => "negate",
        UOp::Not => "not",
    }
}

/// `writeCWL`: emit a minimal CWL `CommandLineTool` wrapper for `script` (saved as `script_fname`)
/// to `fp`. Mirrors `CWL.hs::writeCWL`/`buildCWL`.
pub fn write_cwl(script: &Script, script_fname: &str, fp: &str) -> NgResult<()> {
    let cwl = build_cwl(script_fname, script);
    write_output_file(fp, cwl.as_bytes())
}

/// `buildCWL`/`build`: assemble the CWL document from the script's `ARGV` usage.
fn build_cwl(sfname: &str, sc: &Script) -> String {
    let inputs = extract_all_argv_usage(sc);
    let output = extract_output(sc);
    let mut s = String::new();
    s.push_str("cwlVersion: cwl:draft-3\n");
    s.push_str("class: CommandLineTool\n");
    s.push_str(&format!("baseCommand: [ngless, {sfname}]\n"));
    s.push_str("inputs:\n");
    for p in &inputs {
        s.push_str(&format!("- id: input{p}\n"));
        s.push_str("  type: Str\n");
        s.push_str("  inputBinding:\n");
        s.push_str(&format!("    position: {p}\n"));
    }
    s.push_str("outputs:\n");
    s.push_str("  -id: nglessout\n");
    s.push_str("  type: File\n");
    s.push_str("  outputBinding:\n");
    s.push_str(&format!("    glob: $(inputs.input{output})\n"));
    s
}

/// `extractAllARGVUsage`: for each top-level statement, the first `ARGV[<int>]` index it uses (if
/// any).
fn extract_all_argv_usage(sc: &Script) -> Vec<i64> {
    sc.body
        .iter()
        .filter_map(|(_, e)| extract_argv_usage(e))
        .collect()
}

/// `extractOutput`: the `ARGV` index used by the `ofile=` argument of the first `write(...)` call,
/// or `-1` if there is none. Mirrors `extractOutput`/`firstJust`.
fn extract_output(sc: &Script) -> i64 {
    for (_, e) in &sc.body {
        if let Expression::FunctionCall(fname, _, kwargs, _) = e {
            if fname.0 == "write" {
                if let Some((_, ofile)) = kwargs.iter().find(|(v, _)| v.0 == "ofile") {
                    if let Some(ix) = extract_argv_usage(ofile) {
                        return ix;
                    }
                }
            }
        }
    }
    -1
}

/// `extractARGVUsage`: the first `ARGV[<ConstInt>]` index found in a pre-order walk of `e` (mirrors
/// `recursiveAnalyse` with a `callCC` exit on the first match).
///
/// NB: the Haskell pattern is `IndexExpression (Lookup _ (Variable "ARGV")) ...`. In Haskell the
/// parser has no ARGV special-case (`Parse.hs`: every bare identifier becomes a `Lookup`) and
/// `writeCWL` runs on the pre-transform script, so `ARGV[<n>]` stays a `Lookup` and this match
/// *does* fire, producing non-degenerate CWL `inputs:`. In Rust, `ARGV` tokenizes to a
/// `BuiltinConstant` node (`tokens.rs` `CONSTANTS`), so the `Lookup`-matching walk below never
/// fires and Rust emits empty inputs. This is a known divergence (see `rust-migration.md`, the
/// ARGV entry); we keep the `Lookup` pattern here to mirror the Haskell source, but note that the
/// two binaries do *not* produce identical CWL for a script that indexes `ARGV`.
fn extract_argv_usage(e: &Expression) -> Option<i64> {
    let mut found = None;
    recursive_find_argv(e, &mut found);
    found
}

/// Pre-order traversal mirroring `recursiveAnalyse`: visit `e`, then its sub-expressions, stopping
/// at the first `ARGV[<ConstInt>]` index.
fn recursive_find_argv(e: &Expression, found: &mut Option<i64>) {
    if found.is_some() {
        return;
    }
    if let Expression::IndexExpression(inner, Index::One(ix)) = e {
        if let Expression::Lookup(_, v) = inner.as_ref() {
            if v.0 == "ARGV" {
                if let Expression::ConstInt(i) = ix.as_ref() {
                    *found = Some(*i);
                    return;
                }
            }
        }
    }
    // Recurse into sub-expressions in the same order as `recursiveAnalyse'`.
    match e {
        Expression::ListExpression(es) | Expression::Sequence(es) => {
            for s in es {
                recursive_find_argv(s, found);
            }
        }
        Expression::UnaryOp(_, a) => recursive_find_argv(a, found),
        Expression::BinaryOp(_, l, r) => {
            recursive_find_argv(l, found);
            recursive_find_argv(r, found);
        }
        Expression::Condition(c, t, f) => {
            recursive_find_argv(c, found);
            recursive_find_argv(t, found);
            recursive_find_argv(f, found);
        }
        Expression::IndexExpression(inner, ix) => {
            recursive_find_argv(inner, found);
            match ix {
                Index::One(a) => recursive_find_argv(a, found),
                Index::Two(a, b) => {
                    if let Some(a) = a {
                        recursive_find_argv(a, found);
                    }
                    if let Some(b) = b {
                        recursive_find_argv(b, found);
                    }
                }
            }
        }
        Expression::Assignment(_, a) => recursive_find_argv(a, found),
        Expression::FunctionCall(_, arg0, kwargs, block) => {
            recursive_find_argv(arg0, found);
            for (_, a) in kwargs {
                recursive_find_argv(a, found);
            }
            if let Some(b) = block {
                recursive_find_argv(&b.body, found);
            }
        }
        Expression::MethodCall(_, ethis, marg, kwargs) => {
            recursive_find_argv(ethis, found);
            if let Some(a) = marg {
                recursive_find_argv(a, found);
            }
            for (_, a) in kwargs {
                recursive_find_argv(a, found);
            }
        }
        _ => {}
    }
}

/// Write `bytes` to `fname`, mirroring `withOutputFile` (the export modes write directly to the
/// named output path).
fn write_output_file(fname: &str, bytes: &[u8]) -> NgResult<()> {
    let mut f = fs::File::create(fname).map_err(|e| {
        NgError::new(
            NgErrorType::SystemError,
            format!("Could not create output file {fname}: {e}"),
        )
    })?;
    f.write_all(bytes).map_err(|e| {
        NgError::new(
            NgErrorType::SystemError,
            format!("Could not write to {fname}: {e}"),
        )
    })?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{FuncName, Variable};

    fn argv_index(i: i64) -> Expression {
        Expression::IndexExpression(
            Box::new(Expression::Lookup(None, Variable("ARGV".into()))),
            Index::One(Box::new(Expression::ConstInt(i))),
        )
    }

    #[test]
    fn argv_usage_found_in_nested_call() {
        // write(input, ofile=ARGV[3]) -> the ofile arg uses ARGV[3]
        let e = Expression::FunctionCall(
            FuncName("write".into()),
            Box::new(Expression::Lookup(None, Variable("input".into()))),
            vec![(Variable("ofile".into()), argv_index(3))],
            None,
        );
        assert_eq!(extract_argv_usage(&e), Some(3));
    }

    #[test]
    fn extract_output_picks_write_ofile() {
        let sc = Script {
            header: None,
            body: vec![(
                1,
                Expression::FunctionCall(
                    FuncName("write".into()),
                    Box::new(Expression::Lookup(None, Variable("x".into()))),
                    vec![(Variable("ofile".into()), argv_index(1))],
                    None,
                ),
            )],
        };
        assert_eq!(extract_output(&sc), 1);
        assert_eq!(extract_all_argv_usage(&sc), vec![1]);
    }

    #[test]
    fn extract_output_default_minus_one() {
        let sc = Script {
            header: None,
            body: vec![(1, Expression::ConstInt(5))],
        };
        assert_eq!(extract_output(&sc), -1);
    }

    #[test]
    fn json_roundtrip_basic() {
        let sc = Script {
            header: Some(Header {
                version: "1.5".into(),
                modules: vec![ModInfo::Import {
                    name: "samtools".into(),
                    version: "1.0".into(),
                }],
            }),
            body: vec![(1, Expression::ConstStr("hello".into()))],
        };
        let doc = json!({
            "header": sc.header.as_ref().map(enc_header).unwrap_or(Value::Null),
            "original-script": enc_body(&sc.body),
            "transformed-script": enc_body(&sc.body),
        });
        // The header carries the version and module info.
        assert_eq!(doc["header"]["ngless-version"], json!("1.5"));
        assert_eq!(doc["header"]["modules"][0]["type"], json!("module"));
        // The body is a list of [lno, encoded] pairs; a string constant encodes to a JSON string.
        assert_eq!(doc["original-script"][0][0], json!(1));
        assert_eq!(doc["original-script"][0][1], json!("hello"));
    }
}
