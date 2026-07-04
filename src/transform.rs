//! Script transforms that run after type-checking and validation, before interpretation.
//!
//! Mirrors the relevant parts of `NGLess/Transform.hs`. A central transform is `add_output_hash`,
//! which computes a content hash for each `write`/`collect` output call and injects it as a hidden
//! `__hash` keyword argument. The hash is what the `auto_comments=[{hash}]` feature reports as
//! `# Output hash: <md5>`, and what names parallel lock/stats directories.
//!
//! The hash is an **internal, opaque identifier**: it only needs to be deterministic and
//! content-addressed (equal inputs → equal hash), not byte-identical to any other implementation.
//! It is computed as `md5(version_string ++ Debug-serialization(expr'))` where:
//!   * `version_string` encodes the language version and the sorted list of loaded modules (see
//!     [`version_string`]) so scripts differing only in imports hash differently;
//!   * `expr'` is the (sub)expression with every variable `Lookup` replaced by a `Lookup` whose
//!     variable name is the hash of the expression that was assigned to it, and every *nested*
//!     (non-top, non-void) function call replaced by such a hashed `Lookup` (this mirrors the
//!     earlier `addTemporaries` pass, which lifts nested calls into `temp$N` bindings). This
//!     content-addressing is load-bearing — e.g. it makes `collect` and `write` of the same
//!     pipeline hash equal (see the `same-hash-collect` functional test).

use std::collections::HashMap;

use crate::ast::{
    BOp, Block, Expression, FuncName, Index, NGLType, OptimizedExpression, UOp, Variable,
};
use crate::modules::{ArgCheck, Function};

/// Wrap the last top-level statement in `write(<expr>, ofile=STDOUT)` (mirrors `wrapPrint` in
/// `Execs/Main.hs`), used by `-p/--print-last`. Applied right after parsing, before type-checking,
/// so the injected `write` participates in type checking, validation and `uses_STDOUT` detection
/// (which suppresses the run header and switches to quiet mode). Errors if the script already
/// terminates in a `print`/`write` call, mirroring `wrapPrint`'s `throwScriptError`.
pub fn wrap_print(body: &mut Vec<(usize, Expression)>) -> Result<(), String> {
    let Some((_, last)) = body.last() else {
        // An empty script is left untouched (mirrors `wrap [] = return []`).
        return Ok(());
    };
    if !wrapable(last) {
        return Err(
            "Cannot add write() statement at the end of script (the script cannot \
                    terminate with a print/write call)"
                .to_string(),
        );
    }
    let (lno, e) = body.pop().expect("body is non-empty");
    let wrapped = Expression::FunctionCall(
        FuncName("write".to_string()),
        Box::new(e),
        vec![(
            Variable("ofile".to_string()),
            Expression::BuiltinConstant(Variable("STDOUT".to_string())),
        )],
        None,
    );
    body.push((lno, wrapped));
    Ok(())
}

/// Whether the last statement may be wrapped in `write()` (mirrors `wrapable`): anything except a
/// `print`/`write` call (the script must not already terminate by writing/printing).
fn wrapable(e: &Expression) -> bool {
    !matches!(e, Expression::FunctionCall(FuncName(f), ..) if f == "print" || f == "write")
}

/// Insert `__check_ifile`/`__check_ofile` calls that float up to right after the assignment of the
/// variable the file path depends on (mirrors `addFileChecks` in `Transform.hs`).
///
/// A call whose input path (a `FileReadable` argument) or output path (a `FileWritable` argument) is
/// a single-variable expression gets a `__check_ifile`/`__check_ofile(<path>, original_lno=<lno>)`
/// inserted right after that variable's assignment (and a redundant one right before the call,
/// matching Haskell). This surfaces a missing input file or unwritable output directory as early as
/// possible — right when the path becomes known — rather than only when the file is finally
/// read/written. Constant paths are handled eagerly by the IO validation pass
/// ([`crate::validation::validate_io`]) instead.
pub fn add_file_checks(
    body: Vec<(usize, Expression)>,
    funcs: &[Function],
) -> Vec<(usize, Expression)> {
    // The Haskell version works on the reversed script so a check can be placed *before* (in
    // reversed order) the assignment it depends on, i.e. *after* it in source order. The input pass
    // runs first, then the output pass over its result (mirrors `checkIFiles >=> checkOFiles`).
    let rev: Vec<(usize, Expression)> = body.into_iter().rev().collect();
    let rev = add_file_checks_pass(rev, "__check_ifile", &ArgCheck::FileReadable, funcs);
    let rev = add_file_checks_pass(rev, "__check_ofile", &ArgCheck::FileWritable, funcs);
    rev.into_iter().rev().collect()
}

/// One pass of `addFileChecks'`: process the (reversed) list head-to-tail, inserting checks into
/// the tail. Inserted checks are re-processed by the same loop but produce no further checks.
fn add_file_checks_pass(
    list: Vec<(usize, Expression)>,
    check_name: &str,
    tag: &ArgCheck,
    funcs: &[Function],
) -> Vec<(usize, Expression)> {
    let mut remaining = list;
    let mut result: Vec<(usize, Expression)> = Vec::new();
    while !remaining.is_empty() {
        let (lno, e) = remaining.remove(0);
        let vars = get_file_exprs(&e, tag, funcs);
        // `addCheck vars (maybeAddChecks vars rest)`: first float a check up to the assignment,
        // then (for the single-variable case) prepend a check right before this statement.
        remaining = maybe_add_checks(&vars, remaining, check_name, lno);
        remaining = add_check(&vars, remaining, check_name, lno);
        result.push((lno, e));
    }
    result
}

/// Build a `__check_*(complete, original_lno=lno)` call expression.
fn check_file_expression(check_name: &str, complete: Expression, lno: usize) -> Expression {
    Expression::FunctionCall(
        FuncName(check_name.to_string()),
        Box::new(complete),
        vec![(
            Variable("original_lno".to_string()),
            Expression::ConstInt(lno as i64),
        )],
        None,
    )
}

/// `addCheck`: for the single-variable case, prepend a check to the front of the list.
fn add_check(
    vars: &[(Variable, Expression)],
    rest: Vec<(usize, Expression)>,
    check_name: &str,
    lno: usize,
) -> Vec<(usize, Expression)> {
    if vars.len() == 1 {
        let mut out = vec![(
            lno,
            check_file_expression(check_name, vars[0].1.clone(), lno),
        )];
        out.extend(rest);
        out
    } else {
        rest
    }
}

/// `maybeAddChecks`: for the single-variable case, walk the list and insert the check right before
/// the assignment of that variable (the first one found). If the assignment is not present, the
/// list is returned unchanged.
fn maybe_add_checks(
    vars: &[(Variable, Expression)],
    list: Vec<(usize, Expression)>,
    check_name: &str,
    lno: usize,
) -> Vec<(usize, Expression)> {
    if vars.len() != 1 {
        return list;
    }
    let (v, complete) = &vars[0];
    let mut out: Vec<(usize, Expression)> = Vec::new();
    let mut iter = list.into_iter();
    for (lno2, e2) in iter.by_ref() {
        if let Expression::Assignment(v2, _) = &e2 {
            if v2 == v {
                out.push((
                    lno2,
                    check_file_expression(check_name, complete.clone(), lno),
                ));
                out.push((lno2, e2));
                out.extend(iter);
                return out;
            }
        }
        out.push((lno2, e2));
    }
    out
}

/// Collect `(variable, complete-expression)` pairs for every file argument (tagged with `tag`) used
/// anywhere in `e` (mirrors `getFileExpressions` + `extractExpressions`).
fn get_file_exprs(
    e: &Expression,
    tag: &ArgCheck,
    funcs: &[Function],
) -> Vec<(Variable, Expression)> {
    let mut out = Vec::new();
    recursive_collect(e, &mut |sub| {
        if let Expression::FunctionCall(f, arg, args, _) = sub {
            if let Some(finfo) = funcs.iter().find(|fi| &fi.name == f) {
                if finfo.arg_checks.contains(tag) {
                    extract_expressions(arg, &mut out);
                }
                for ainfo in &finfo.kwargs {
                    if ainfo.checks.contains(tag) {
                        if let Some(v) = args.iter().find(|(k, _)| k.0 == ainfo.name) {
                            extract_expressions(&v.1, &mut out);
                        }
                    }
                }
            }
        }
    });
    out
}

/// `extractExpressions`: a file path that is a single `Lookup` or a `BinaryOp` mentioning exactly
/// one variable contributes that variable plus the full path expression. Constants contribute
/// nothing; anything else "bails out" (mentions a sentinel set of variables so the uniqueness check
/// fails and no check is inserted).
fn extract_expressions(ofile: &Expression, out: &mut Vec<(Variable, Expression)>) {
    match ofile {
        Expression::BinaryOp(_, re, le) => {
            let mut vs = valid_variables(re);
            vs.extend(valid_variables(le));
            let uniq = uniq_variables(&vs);
            if uniq.len() == 1 {
                out.push((uniq[0].clone(), ofile.clone()));
            }
        }
        Expression::Lookup(_, v) => out.push((v.clone(), ofile.clone())),
        _ => {}
    }
}

fn valid_variables(e: &Expression) -> Vec<Variable> {
    match e {
        Expression::Lookup(_, v) => vec![v.clone()],
        Expression::BinaryOp(_, re, le) => {
            let mut vs = valid_variables(re);
            vs.extend(valid_variables(le));
            vs
        }
        Expression::ConstStr(_) => vec![],
        // Anything else makes the caller bail out: emit a sentinel set so the uniqueness check
        // never collapses to a single variable.
        _ => vec![
            Variable("this".to_string()),
            Variable("wont".to_string()),
            Variable("work".to_string()),
        ],
    }
}

fn uniq_variables(vs: &[Variable]) -> Vec<Variable> {
    let mut out: Vec<Variable> = Vec::new();
    for v in vs {
        if !out.contains(v) {
            out.push(v.clone());
        }
    }
    out
}

/// Visit `e` and every sub-expression (mirrors `recursiveAnalyse`).
fn recursive_collect(e: &Expression, f: &mut dyn FnMut(&Expression)) {
    f(e);
    match e {
        Expression::ListExpression(es) | Expression::Sequence(es) => {
            for c in es {
                recursive_collect(c, f);
            }
        }
        Expression::UnaryOp(_, a) => recursive_collect(a, f),
        Expression::BinaryOp(_, a, b) => {
            recursive_collect(a, f);
            recursive_collect(b, f);
        }
        Expression::Condition(c, t, fe) => {
            recursive_collect(c, f);
            recursive_collect(t, f);
            recursive_collect(fe, f);
        }
        Expression::IndexExpression(a, ix) => {
            recursive_collect(a, f);
            match ix {
                Index::One(i) => recursive_collect(i, f),
                Index::Two(a, b) => {
                    if let Some(a) = a {
                        recursive_collect(a, f);
                    }
                    if let Some(b) = b {
                        recursive_collect(b, f);
                    }
                }
            }
        }
        Expression::Assignment(_, a) => recursive_collect(a, f),
        Expression::FunctionCall(_, arg, args, block) => {
            recursive_collect(arg, f);
            for (_, v) in args {
                recursive_collect(v, f);
            }
            if let Some(b) = block {
                recursive_collect(&b.body, f);
            }
        }
        Expression::MethodCall(_, self_e, arg, args) => {
            recursive_collect(self_e, f);
            if let Some(a) = arg {
                recursive_collect(a, f);
            }
            for (_, v) in args {
                recursive_collect(v, f);
            }
        }
        _ => {}
    }
}

/// `asSequence` (mirrors `Transform.hs`): a single expression stays as-is; several become a
/// `Sequence`.
fn as_sequence(mut es: Vec<Expression>) -> Expression {
    if es.len() == 1 {
        es.pop().unwrap()
    } else {
        Expression::Sequence(es)
    }
}

/// A check generator: given a statement `(lno, expr)`, either `None` (no check needed) or the
/// variables the check depends on plus the check expression to float up (mirrors the
/// `(Int, Expression) -> Maybe ([Variable], Expression)` argument of `genericCheckUpfloat`).
type CheckFn<'a> = dyn Fn(usize, &Expression) -> Option<(Vec<Variable>, Expression)> + 'a;

/// `addIndexChecks` (mirrors `Transform.hs`): for every `array[<constInt>]` where `array` is a
/// variable, inject a `__check_index_access(array, original_lno=N, index1=ix)` call floated up to
/// just after `array`'s assignment, so an out-of-bounds constant index fails *early* (right after
/// the array is bound) with `Index access on line N is invalid.` rather than only when the index is
/// finally evaluated. A builtin transform, run after `add_output_hash`, so the injected checks do
/// not affect `{hash}`/`{script}` content hashes.
pub fn add_index_checks(body: Vec<(usize, Expression)>) -> Vec<(usize, Expression)> {
    generic_check_upfloat(&index_checks_of, body)
}

/// `addIndexChecks'`: collect every `IndexExpression(Lookup v, IndexOne(ConstInt))` in the statement
/// and, if any, return the referenced variables plus the sequence of `__check_index_access` calls.
fn index_checks_of(lno: usize, e: &Expression) -> Option<(Vec<Variable>, Expression)> {
    let mut found: Vec<(Variable, i64)> = Vec::new();
    recursive_collect(e, &mut |sub| {
        if let Expression::IndexExpression(inner, Index::One(ix)) = sub {
            if let (Expression::Lookup(_, v), Expression::ConstInt(ix1)) =
                (inner.as_ref(), ix.as_ref())
            {
                found.push((v.clone(), *ix1));
            }
        }
    });
    if found.is_empty() {
        return None;
    }
    let vars = found.iter().map(|(v, _)| v.clone()).collect();
    let checks = found
        .into_iter()
        .map(|(v, ix1)| index_check_expr(&v, ix1, lno))
        .collect();
    Some((vars, as_sequence(checks)))
}

/// Build a `__check_index_access(Lookup v, original_lno=lno, index1=ix1)` call (mirrors
/// `indexCheckExpr`).
fn index_check_expr(arr: &Variable, ix1: i64, lno: usize) -> Expression {
    Expression::FunctionCall(
        FuncName("__check_index_access".to_string()),
        Box::new(Expression::Lookup(None, arr.clone())),
        vec![
            (
                Variable("original_lno".to_string()),
                Expression::ConstInt(lno as i64),
            ),
            (Variable("index1".to_string()), Expression::ConstInt(ix1)),
        ],
        None,
    )
}

/// `addRSChecks` (mirrors `Transform.hs`): for every `x = preprocess(v, ...) using ...` assignment,
/// inject a `__check_readset(v, original_lno=N)` call floated up to just after `v`'s assignment, so
/// the readset's backing FASTQ files are verified readable *early* (right after the read set is
/// bound) rather than only when `preprocess` finally streams them. A builtin transform, run after
/// `add_file_checks` and before `add_index_checks`, matching the Haskell order. Runs after
/// `add_output_hash`, so the injected checks do not affect `{hash}`/`{script}` content hashes.
pub fn add_rs_checks(body: Vec<(usize, Expression)>) -> Vec<(usize, Expression)> {
    generic_check_upfloat(&rs_checks_of, body)
}

/// `addRSChecks'`: match `Assignment _ (FunctionCall "preprocess" (Lookup v) _ _)` and return the
/// preprocessed variable plus the `__check_readset` call to float up. Only a direct `Lookup`
/// argument qualifies (matching Haskell); a nested call there would already have been lifted into a
/// temporary by `addTemporaries`.
fn rs_checks_of(lno: usize, e: &Expression) -> Option<(Vec<Variable>, Expression)> {
    let Expression::Assignment(_, rhs) = e else {
        return None;
    };
    let Expression::FunctionCall(FuncName(f), arg, _, _) = rhs.as_ref() else {
        return None;
    };
    if f != "preprocess" {
        return None;
    }
    let Expression::Lookup(t, v) = arg.as_ref() else {
        return None;
    };
    let check = Expression::FunctionCall(
        FuncName("__check_readset".to_string()),
        Box::new(Expression::Lookup(t.clone(), v.clone())),
        vec![(
            Variable("original_lno".to_string()),
            Expression::ConstInt(lno as i64),
        )],
        None,
    );
    Some((vec![v.clone()], check))
}

/// `addCountsCheck` (mirrors `Transform.hs`): for every `count(mapped, ...)` call, inject a
/// `__check_count(__VOID, original_lno=N, <count kwargs>)` call floated up to just after the last
/// variable the count arguments depend on (or to the top of the script when they are all constant),
/// so that when a `functional_map` is used, missing requested features fail *early* rather than only
/// when `count` finally streams the SAM. The last of the three `genericCheckUpfloat` checks; a
/// builtin transform, run after `add_index_checks`, matching the Haskell order. Runs after
/// `add_output_hash`, so the injected checks do not affect `{hash}`/`{script}` content hashes.
pub fn add_counts_check(body: Vec<(usize, Expression)>) -> Vec<(usize, Expression)> {
    generic_check_upfloat(&count_check_of, body)
}

/// `countCheck`: match a `count(...)` call (with no block) and return the variables its keyword
/// arguments depend on plus the `__check_count` call to float up.
fn count_check_of(lno: usize, e: &Expression) -> Option<(Vec<Variable>, Expression)> {
    let Expression::FunctionCall(FuncName(f), _, kwargs, None) = e else {
        return None;
    };
    if f != "count" {
        return None;
    }
    let vars = kwargs.iter().flat_map(|(_, v)| used_variables(v)).collect();
    let check = Expression::FunctionCall(
        FuncName("__check_count".to_string()),
        Box::new(Expression::BuiltinConstant(Variable("__VOID".to_string()))),
        std::iter::once((
            Variable("original_lno".to_string()),
            Expression::ConstInt(lno as i64),
        ))
        .chain(kwargs.iter().cloned())
        .collect(),
        None,
    );
    Some((vars, check))
}

/// `usedVariables` (mirrors `Language.hs`): every variable referenced by a `Lookup` anywhere in the
/// expression.
fn used_variables(e: &Expression) -> Vec<Variable> {
    let mut out = Vec::new();
    recursive_collect(e, &mut |sub| {
        if let Expression::Lookup(_, v) = sub {
            out.push(v.clone());
        }
    });
    out
}

/// `genericCheckUpfloat` (mirrors `Transform.hs`): generalises "emit a check for a statement, then
/// bubble it up past intervening statements to just after the variable(s) it depends on are
/// assigned". Works on the reversed statement list so a check can be placed *before* (reversed) the
/// assignment it needs, i.e. *after* it in source order.
fn generic_check_upfloat(f: &CheckFn, exprs: Vec<(usize, Expression)>) -> Vec<(usize, Expression)> {
    let rev: Vec<(usize, Expression)> = exprs.into_iter().rev().collect();
    let mut out = generic_check_upfloat_rev(f, rev);
    out.reverse();
    out
}

fn generic_check_upfloat_rev(
    f: &CheckFn,
    mut list: Vec<(usize, Expression)>,
) -> Vec<(usize, Expression)> {
    if list.is_empty() {
        return Vec::new();
    }
    let (lno, expr) = list.remove(0);
    match expr {
        // Expand sequences back into individual statements and re-process the whole list.
        Expression::Sequence(es) => {
            let mut new_list: Vec<(usize, Expression)> =
                es.into_iter().rev().map(|e| (lno, e)).collect();
            new_list.extend(list);
            generic_check_upfloat_rev(f, new_list)
        }
        // Conditions are tricky: checks must only float up *within* a branch, never above the
        // condition itself. Each branch is upfloated independently; a check for the condition
        // expression floats into the surrounding statements. Mirrors Haskell exactly, including
        // that processing stops here (the tail is not recursed into again).
        Expression::Condition(ec, et, ef) => {
            let et_p = generic_check_upfloat(f, vec![(lno, *et)]);
            let ef_p = generic_check_upfloat(f, vec![(lno, *ef)]);
            let rest = match f(lno, &ec) {
                None => list,
                Some((vars, ne)) => float_down(&vars, (lno, ne), list),
            };
            let untag = |tagged: Vec<(usize, Expression)>| {
                as_sequence(tagged.into_iter().map(|(_, e)| e).collect())
            };
            let cond = Expression::Condition(ec, Box::new(untag(et_p)), Box::new(untag(ef_p)));
            let mut out = vec![(lno, cond)];
            out.extend(rest);
            out
        }
        _ => {
            let rest = match recursive_call(f, lno, &expr) {
                None => list,
                Some((vars, ne)) => float_down(&vars, (lno, ne), list),
            };
            let mut out = vec![(lno, expr)];
            out.extend(generic_check_upfloat_rev(f, rest));
            out
        }
    }
}

/// `recursiveCall` (mirrors `Transform.hs`): try `f` on `e` and every sub-expression in pre-order,
/// returning the first `Some`.
fn recursive_call(f: &CheckFn, lno: usize, e: &Expression) -> Option<(Vec<Variable>, Expression)> {
    let mut result: Option<(Vec<Variable>, Expression)> = None;
    recursive_collect(e, &mut |sub| {
        if result.is_none() {
            if let Some(r) = f(lno, sub) {
                result = Some(r);
            }
        }
    });
    result
}

/// `floatDown` (mirrors `Transform.hs`): in the reversed list, place `e` just before the first
/// statement that uses any of `vars` (i.e. just after that statement in source order); if none do,
/// `e` goes at the end.
fn float_down(
    vars: &[Variable],
    e: (usize, Expression),
    list: Vec<(usize, Expression)>,
) -> Vec<(usize, Expression)> {
    let mut out: Vec<(usize, Expression)> = Vec::new();
    let mut iter = list.into_iter();
    let mut pending = Some(e);
    for (lno2, e2) in iter.by_ref() {
        if vars.iter().any(|v| is_var_used1(v, &e2)) {
            out.push(pending.take().unwrap());
            out.push((lno2, e2));
            out.extend(iter);
            break;
        }
        out.push((lno2, e2));
    }
    if let Some(e) = pending {
        out.push(e);
    }
    out
}

/// `qcInPreprocess` (mirrors `Transform.hs`): when a read set loaded by
/// `fastq`/`paired`/`load_fastq_directory`/`load_mocat_sample` is *first used* by a `preprocess`,
/// move the QC pass from load time into the preprocess (which already streams the input). The loader
/// gets a hidden `__perform_qc=False` and the matching `preprocess` a hidden `__input_qc=True`, so
/// the interpreter computes the per-input-file QC statistics once, inside `preprocess`, instead of
/// separately at load time. Output-neutral: the same statistics are still produced under the same
/// (input-file) labels. A builtin transform, run after `writeToMove` and before
/// `ifLenDiscardSpecial`; runs after `add_output_hash`, so the injected args do not affect hashes.
pub fn qc_in_preprocess(mut body: Vec<(usize, Expression)>) -> Vec<(usize, Expression)> {
    let mut i = 0;
    while i < body.len() {
        if let Some((fname, v)) = fastq_var(&body[i].1) {
            // The transform is only safe when the *first* use of the loaded variable is the
            // preprocess (otherwise QC-deferral could change an intervening observation).
            if can_qc_preprocess_transform(&v, &body[i + 1..]) {
                let expr = std::mem::replace(&mut body[i].1, Expression::Discard);
                body[i].1 = add_perform_qc_false(&fname, expr);
                rewrite_preprocess_input_qc(&v, &mut body, i + 1);
            }
        }
        i += 1;
    }
    body
}

/// `fastQVar`: a `v = <loader>(...)` assignment whose loader is one of the QC-performing read-set
/// loaders, returning the loader name and the assigned variable.
fn fastq_var(e: &Expression) -> Option<(String, Variable)> {
    let Expression::Assignment(v, rhs) = e else {
        return None;
    };
    let Expression::FunctionCall(FuncName(fname), _, _, _) = rhs.as_ref() else {
        return None;
    };
    if matches!(
        fname.as_str(),
        "fastq" | "paired" | "load_fastq_directory" | "load_mocat_sample"
    ) {
        Some((fname.clone(), v.clone()))
    } else {
        None
    }
}

/// `canQCPreprocessTransform`: true iff the first use of `v` in the following statements is a
/// `preprocess(v, ...)`. Any earlier use of `v` makes the deferral unsafe.
fn can_qc_preprocess_transform(v: &Variable, rest: &[(usize, Expression)]) -> bool {
    for (_, e) in rest {
        // Check the `_ = preprocess(v, ...)` shape first — that assignment also *uses* `v`, so the
        // order matters (mirrors the clause order in Haskell).
        if is_preprocess_of(v, e) {
            return true;
        }
        if is_var_used1(v, e) {
            return false;
        }
    }
    false
}

/// Whether `e` is `_ = preprocess(<Lookup v>, ...)`.
fn is_preprocess_of(v: &Variable, e: &Expression) -> bool {
    let Expression::Assignment(_, rhs) = e else {
        return false;
    };
    let Expression::FunctionCall(FuncName(f), arg, _, _) = rhs.as_ref() else {
        return false;
    };
    f == "preprocess" && matches!(arg.as_ref(), Expression::Lookup(_, v2) if v2 == v)
}

/// `addArgument func (__perform_qc, False)`: prepend a hidden `__perform_qc=False` keyword to the
/// `func` call inside an assignment RHS.
fn add_perform_qc_false(func: &str, expr: Expression) -> Expression {
    match expr {
        Expression::Assignment(v, rhs) => {
            Expression::Assignment(v, Box::new(add_perform_qc_false(func, *rhs)))
        }
        Expression::FunctionCall(FuncName(fname), e, mut args, b) if fname == func => {
            args.insert(
                0,
                (
                    Variable("__perform_qc".to_string()),
                    Expression::ConstBool(false),
                ),
            );
            Expression::FunctionCall(FuncName(fname), e, args, b)
        }
        other => other,
    }
}

/// `rewritePreprocess`: prepend a hidden `__input_qc=True` to the *first* `preprocess(v, ...)` call
/// in `body[start..]`.
fn rewrite_preprocess_input_qc(v: &Variable, body: &mut [(usize, Expression)], start: usize) {
    for stmt in &mut body[start..] {
        if !is_preprocess_of(v, &stmt.1) {
            continue;
        }
        if let Expression::Assignment(_, rhs) = &mut stmt.1 {
            if let Expression::FunctionCall(_, _, args, _) = rhs.as_mut() {
                args.insert(
                    0,
                    (
                        Variable("__input_qc".to_string()),
                        Expression::ConstBool(true),
                    ),
                );
            }
        }
        return;
    }
}

/// `writeToMove` (mirrors `Transform.hs`): when a variable is no longer used *after* a `write()`
/// call that takes it, inject a hidden `__can_move=True` keyword argument into that call. This tells
/// the interpreter it may **move** (rename) the intermediate temp file backing the variable to the
/// output instead of copying it, since the value will never be read again.
///
/// Variables bound to an original input file (`fastq`/`paired`/`samfile`) — or to an alias of such a
/// variable — are *blocked*: their backing files are the user's inputs, not movable temporaries, so
/// they must always be copied. (The interpreter applies a second, runtime guard: it only moves a
/// file it actually created as a temporary, so an incorrectly-injected `__can_move` can never clobber
/// a user file.)
pub fn write_to_move(body: &mut [(usize, Expression)]) {
    let mut blocked: Vec<Variable> = Vec::new();
    for i in 0..body.len() {
        // Variables passed to a `write()` in this statement that are not used in any later statement
        // and are not blocked (mirrors `toRemove`/`unused`).
        let to_remove: Vec<Variable> = function_vars("write", &body[i].1)
            .into_iter()
            .filter(|v| !blocked.contains(v))
            .filter(|v| !body[i + 1..].iter().any(|(_, e)| is_var_used1(v, e)))
            .collect();
        add_move(&to_remove, &mut body[i].1);

        // Extend the blocked set from this statement (mirrors `blockhere`).
        if let Expression::Assignment(var, rhs) = &body[i].1 {
            let block = match rhs.as_ref() {
                Expression::FunctionCall(FuncName(fname), _, _, _) => {
                    matches!(fname.as_str(), "fastq" | "paired" | "samfile")
                }
                Expression::Lookup(_, prev) => blocked.contains(prev),
                _ => false,
            };
            if block {
                blocked.push(var.clone());
            }
        }
    }
}

/// `ifLenDiscardSpecial` (mirrors `Transform.hs`): special-cases the common preprocess idiom
///
/// ```text
/// if len(read) < N:
///     discard
/// ```
///
/// rewriting the `Condition` to `Optimized(LenThresholdDiscard(read, <, N))`, which the
/// interpreter evaluates directly on the read length without building intermediate
/// `NGLessObject`s per read. Output-neutral; runs after `add_output_hash` (matching the Haskell
/// transform ordering) so the rewrite does not perturb `{hash}`/`{script}` content hashes.
pub fn if_len_discard_special(body: &mut [(usize, Expression)]) {
    for (_, e) in body.iter_mut() {
        recursive_transform_mut(e, &mut |sub| {
            if let Some(opt) = as_len_threshold_discard(sub) {
                *sub = Expression::Optimized(opt);
            }
        });
    }
}

/// Match the `if len(v) <op> N: discard` (with an empty `else`) shape and extract its parts;
/// `<op>` must be one of `<`/`<=`/`>`/`>=` (mirrors the guard in `ifLenDiscardSpecial`).
fn as_len_threshold_discard(e: &Expression) -> Option<OptimizedExpression> {
    let Expression::Condition(cond, then_b, else_b) = e else {
        return None;
    };
    let then_is_discard = matches!(then_b.as_ref(),
        Expression::Sequence(s) if matches!(s.as_slice(), [Expression::Discard]));
    let else_is_empty = matches!(else_b.as_ref(),
        Expression::Sequence(s) if s.is_empty());
    if !then_is_discard || !else_is_empty {
        return None;
    }
    let Expression::BinaryOp(b, left, right) = cond.as_ref() else {
        return None;
    };
    if !matches!(b, BOp::LT | BOp::LTE | BOp::GT | BOp::GTE) {
        return None;
    }
    let Expression::UnaryOp(UOp::Len, inner) = left.as_ref() else {
        return None;
    };
    let Expression::Lookup(_, v) = inner.as_ref() else {
        return None;
    };
    let Expression::ConstInt(thresh) = right.as_ref() else {
        return None;
    };
    Some(OptimizedExpression::LenThresholdDiscard(
        v.clone(),
        *b,
        *thresh,
    ))
}

/// `substrimReassign` (mirrors `Transform.hs`): rewrite `read = substrim(read, min_quality=N)` (the
/// same variable on both sides, with `min_quality` as the sole keyword argument and no block) to
/// `Optimized(SubstrimReassign(read, N))`, which the interpreter applies in place on the block
/// variable's read. Output-neutral; runs after `if_len_discard_special` and before `add_file_checks`
/// (matching Haskell's transform ordering), so the rewrite does not perturb `{hash}`/`{script}`
/// content hashes.
pub fn substrim_reassign(body: &mut [(usize, Expression)]) {
    for (_, e) in body.iter_mut() {
        recursive_transform_mut(e, &mut |sub| {
            if let Some(opt) = as_substrim_reassign(sub) {
                *sub = Expression::Optimized(opt);
            }
        });
    }
}

/// Match `v = substrim(v, min_quality=N)` and extract `(v, N)` (mirrors the guard in
/// `substrimReassign`). Requires the exact shape: one `min_quality`/`ConstInt` keyword argument, no
/// block, and the assigned variable identical to the `substrim` input.
fn as_substrim_reassign(e: &Expression) -> Option<OptimizedExpression> {
    let Expression::Assignment(v, rhs) = e else {
        return None;
    };
    let Expression::FunctionCall(FuncName(f), arg, kwargs, None) = rhs.as_ref() else {
        return None;
    };
    if f != "substrim" {
        return None;
    }
    let Expression::Lookup(_, v2) = arg.as_ref() else {
        return None;
    };
    if v != v2 {
        return None;
    }
    match kwargs.as_slice() {
        [(Variable(k), Expression::ConstInt(mq))] if k == "min_quality" => {
            Some(OptimizedExpression::SubstrimReassign(v.clone(), *mq))
        }
        _ => None,
    }
}

/// `sortOFormat` (samtools module transform, mirrors `StandardModules/Samtools.hs`): when a
/// `samtools_sort(...)` result is *only* consumed by a `write()` in BAM format, inject a hidden
/// `__output_bam=True` keyword so the interpreter sorts directly to BAM. This avoids an extra
/// SAM→BAM conversion pass (and the extra `@PG` line it adds), matching Haskell byte-for-byte.
///
/// Runs after `addOutputHash` (so the injected arg does not perturb the `{hash}`) and before
/// `writeToMove`, mirroring the Haskell transform ordering.
pub fn sort_oformat(body: &mut [(usize, Expression)]) {
    for i in 0..body.len() {
        let is_sort = matches!(
            &body[i].1,
            Expression::Assignment(_, rhs)
                if matches!(rhs.as_ref(),
                    Expression::FunctionCall(FuncName(f), _, _, None) if f == "samtools_sort")
        );
        if !is_sort {
            continue;
        }
        let Expression::Assignment(v, _) = &body[i].1 else {
            continue;
        };
        let v = v.clone();
        if output_bam(&v, &body[i + 1..]) {
            if let Expression::Assignment(_, rhs) = &mut body[i].1 {
                if let Expression::FunctionCall(_, _, args, _) = rhs.as_mut() {
                    args.insert(
                        0,
                        (
                            Variable("__output_bam".to_string()),
                            Expression::ConstBool(true),
                        ),
                    );
                }
            }
        }
    }
}

/// Whether variable `v` (bound to a `samtools_sort` result) is used *only* as the input to a BAM
/// `write()` in the following statements `es` (mirrors `outputBam`). Any other use of `v` — before
/// or after such a write — makes it unsafe, so we fall back to SAM (`false`).
fn output_bam(v: &Variable, es: &[(usize, Expression)]) -> bool {
    for (idx, (_, c)) in es.iter().enumerate() {
        // A top-level `write(Lookup v, ...)` consuming exactly `v`.
        if let Expression::FunctionCall(FuncName(f), arg, args, None) = c {
            if f == "write" {
                if let Expression::Lookup(_, v2) = arg.as_ref() {
                    if v2 == v {
                        let rest = &es[idx + 1..];
                        return is_obam(args) && !rest.iter().any(|(_, e)| is_var_used1(v, e));
                    }
                }
            }
        }
        // `v` used any other way (e.g. re-sorted, selected, aliased) → unsafe.
        if is_var_used1(v, c) {
            return false;
        }
    }
    false
}

/// Whether a `write()`'s keyword arguments request BAM output (mirrors `isOBam`): an explicit
/// `format={bam}` wins; otherwise infer from an `ofile=` string that ends in `.bam`. In doubt, SAM.
fn is_obam(args: &[(Variable, Expression)]) -> bool {
    match arg_format(args) {
        Some(true) => return true,
        Some(false) => return false,
        None => {}
    }
    match args.iter().find(|(Variable(k), _)| k == "ofile") {
        Some((_, oname)) => string_will_end_with(oname, ".bam") == Some(true),
        None => false,
    }
}

/// The `format=` argument as a tri-state (mirrors `oFormat`): `Some(true)` for `{bam}`, `Some(false)`
/// for any other symbol, `None` when absent.
fn arg_format(args: &[(Variable, Expression)]) -> Option<bool> {
    args.iter()
        .find(|(Variable(k), _)| k == "format")
        .map(|(_, v)| matches!(v, Expression::ConstSymbol(s) if s == "bam"))
}

/// Whether the string-valued expression provably ends with `post` (mirrors `stringWillEndWith`).
/// A literal is checked directly; a `+` concatenation is decided by its *right* operand (the
/// suffix); anything else is unknown (`None`).
fn string_will_end_with(e: &Expression, post: &str) -> Option<bool> {
    match e {
        Expression::ConstStr(b) => {
            if b.len() >= post.len() {
                Some(b.ends_with(post))
            } else {
                None
            }
        }
        Expression::BinaryOp(BOp::Add, _, right) => string_will_end_with(right, post),
        _ => None,
    }
}

/// Variables used as the (single-`Lookup`) argument to a call of `fname` anywhere in `expr`
/// (mirrors `functionVars`).
fn function_vars(fname: &str, expr: &Expression) -> Vec<Variable> {
    let mut out = Vec::new();
    recursive_collect(expr, &mut |sub| {
        if let Expression::FunctionCall(FuncName(f), arg, _, _) = sub {
            if f == fname {
                if let Expression::Lookup(_, v) = arg.as_ref() {
                    out.push(v.clone());
                }
            }
        }
    });
    out
}

/// Whether `v` is used in `expr` (mirrors `isVarUsed1`): an assignment *to* `v` counts as a use, as
/// does any `Lookup` of `v`.
fn is_var_used1(v: &Variable, expr: &Expression) -> bool {
    let mut used = false;
    recursive_collect(expr, &mut |sub| match sub {
        Expression::Assignment(v2, _) if v2 == v => used = true,
        Expression::Lookup(_, v2) if v2 == v => used = true,
        _ => {}
    });
    used
}

/// Inject `__can_move=True` into every `write(Lookup v, ...)` in `expr` whose `v` is in `dead`
/// (mirrors `addMove`).
fn add_move(dead: &[Variable], expr: &mut Expression) {
    recursive_transform_mut(expr, &mut |e| {
        if let Expression::FunctionCall(FuncName(f), arg, args, _) = e {
            if f == "write" {
                if let Expression::Lookup(_, v) = arg.as_ref() {
                    if dead.contains(v) {
                        args.insert(
                            0,
                            (
                                Variable("__can_move".to_string()),
                                Expression::ConstBool(true),
                            ),
                        );
                    }
                }
            }
        }
    });
}

/// Apply `f` to `e` and every sub-expression, children first (mirrors `recursiveTransform`). The
/// traversal mirrors [`recursive_collect`] but over `&mut`.
fn recursive_transform_mut(e: &mut Expression, f: &mut dyn FnMut(&mut Expression)) {
    match e {
        Expression::ListExpression(es) | Expression::Sequence(es) => {
            for c in es.iter_mut() {
                recursive_transform_mut(c, f);
            }
        }
        Expression::UnaryOp(_, a) => recursive_transform_mut(a, f),
        Expression::BinaryOp(_, a, b) => {
            recursive_transform_mut(a, f);
            recursive_transform_mut(b, f);
        }
        Expression::Condition(c, t, fe) => {
            recursive_transform_mut(c, f);
            recursive_transform_mut(t, f);
            recursive_transform_mut(fe, f);
        }
        Expression::IndexExpression(a, ix) => {
            recursive_transform_mut(a, f);
            match ix {
                Index::One(i) => recursive_transform_mut(i, f),
                Index::Two(a, b) => {
                    if let Some(a) = a {
                        recursive_transform_mut(a, f);
                    }
                    if let Some(b) = b {
                        recursive_transform_mut(b, f);
                    }
                }
            }
        }
        Expression::Assignment(_, a) => recursive_transform_mut(a, f),
        Expression::FunctionCall(_, arg, args, block) => {
            recursive_transform_mut(arg, f);
            for (_, v) in args.iter_mut() {
                recursive_transform_mut(v, f);
            }
            if let Some(b) = block {
                recursive_transform_mut(&mut b.body, f);
            }
        }
        Expression::MethodCall(_, self_e, arg, args) => {
            recursive_transform_mut(self_e, f);
            if let Some(a) = arg {
                recursive_transform_mut(a, f);
            }
            for (_, v) in args.iter_mut() {
                recursive_transform_mut(v, f);
            }
        }
        _ => {}
    }
    f(e);
}

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
                kwargs.insert(0, (Variable("__hash".to_string()), Expression::ConstStr(h)));
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
/// only names lock/stats directories, so it need only be a deterministic function of the script
/// body; we hash the `Debug` serialization of the statement expressions.
fn add_lock_hash(body: &mut [(usize, Expression)]) {
    let exprs: Vec<&Expression> = body.iter().map(|(_, e)| e).collect();
    let h = md5_hex(format!("{exprs:?}").as_bytes());
    for (_, e) in body.iter_mut() {
        inject_lock_hash(e, &h);
    }
}

/// Inject the lock `__hash` into every `lock1`/`run_for_all`/`run_for_all_samples` call reachable
/// from `e`, recursing into sub-expressions. This mirrors Haskell's `addLockHash'` being applied via
/// `pureTransform` (a full recursive rewrite): the call is commonly an assignment RHS
/// (`sample = lock1(...)`) or nested inside another call, not a bare top-level statement.
fn inject_lock_hash(e: &mut Expression, h: &str) {
    match e {
        Expression::FunctionCall(FuncName(fname), arg, kwargs, _) => {
            if fname == "lock1" || is_run_for_all(fname) {
                kwargs.insert(
                    0,
                    (
                        Variable("__hash".to_string()),
                        Expression::ConstStr(h.to_string()),
                    ),
                );
            }
            inject_lock_hash(arg, h);
            for (_, v) in kwargs.iter_mut() {
                inject_lock_hash(v, h);
            }
        }
        Expression::Assignment(_, rhs) => inject_lock_hash(rhs, h),
        Expression::UnaryOp(_, a) => inject_lock_hash(a, h),
        Expression::BinaryOp(_, a, b) => {
            inject_lock_hash(a, h);
            inject_lock_hash(b, h);
        }
        Expression::Condition(c, t, f) => {
            inject_lock_hash(c, h);
            inject_lock_hash(t, h);
            inject_lock_hash(f, h);
        }
        Expression::IndexExpression(a, _) => inject_lock_hash(a, h),
        Expression::MethodCall(_, obj, arg, kwargs) => {
            inject_lock_hash(obj, h);
            if let Some(a) = arg {
                inject_lock_hash(a, h);
            }
            for (_, v) in kwargs.iter_mut() {
                inject_lock_hash(v, h);
            }
        }
        Expression::ListExpression(items) | Expression::Sequence(items) => {
            for it in items.iter_mut() {
                inject_lock_hash(it, h);
            }
        }
        _ => {}
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
        md5_hex(format!("{}{:?}", self.vstr, rewritten).as_bytes())
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
                let call = Expression::FunctionCall(fname.clone(), Box::new(arg2), kwargs2, block2);
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
                    let h = md5_hex(format!("{}{:?}", self.vstr, call).as_bytes());
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
                arg.as_ref()
                    .map(|x| Box::new(self.rewrite(x, varmap, false))),
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

/// A deterministic prefix mixed into every output content hash, so that scripts differing only in
/// their language version or module imports hash differently. The exact format is internal and
/// opaque (it no longer needs to match any other implementation).
fn version_string(version: (i64, i64), imports: &[crate::ast::ModInfo]) -> String {
    let mut mods = loaded_modules(version, imports);
    mods.sort();
    format!("NGLVersion {} {} {:?}", version.0, version.1, mods)
}

/// The list of loaded modules (name, version) that contributes to the content hash: the built-in
/// modules (always loaded at the single supported language version) plus the header imports.
fn loaded_modules(version: (i64, i64), imports: &[crate::ast::ModInfo]) -> Vec<(String, String)> {
    let m = |n: &str, v: &str| (n.to_string(), v.to_string());
    let mut mods: Vec<(String, String)> = vec![
        m("__builtin__", &format!("{}.{}", version.0, version.1)),
        m("builtin.orffind", "0.6"),
        m("builtin.load_directory", "1.0"),
        m("builtin.samples", "1.5"),
        m("builtin.readlines", "0.0"),
        m("builtin.argv", "0.0"),
        m("builtin.assemble", "0.0"),
        m("builtin.as_reads", "0.0"),
        m("builtin.checks", "0.0"),
        m("builtin.remove", "0.0"),
        m("builtin.stats", "0.6"),
    ];
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
// `Show` for NGLType, used by JSON export (`export.rs`).
// ---------------------------------------------------------------------------

/// Derived `Show NGLType`. `prec` is the surrounding precedence (11 means "as a constructor
/// argument", which forces parentheses around constructor applications that take arguments).
pub fn show_ngltype(t: &NGLType, prec: u8) -> String {
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

// ---------------------------------------------------------------------------
// MD5 (RFC 1321). Implemented in-crate to avoid a dependency; used for the internal output
// content hashes (`{hash}` auto-comment, lock/stats directory names).
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
            let f = f.wrapping_add(a).wrapping_add(K[i]).wrapping_add(m[g]);
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

    /// `if len(read) <op> N: discard` inside a block body is rewritten to the optimized node.
    #[test]
    fn if_len_discard_special_rewrites_pattern() {
        let cond = Expression::Condition(
            Box::new(Expression::BinaryOp(
                BOp::LT,
                Box::new(Expression::UnaryOp(
                    UOp::Len,
                    Box::new(Expression::Lookup(None, Variable("read".into()))),
                )),
                Box::new(Expression::ConstInt(10)),
            )),
            Box::new(Expression::Sequence(vec![Expression::Discard])),
            Box::new(Expression::Sequence(vec![])),
        );
        // Wrapped in a preprocess-style call block so the rewrite is exercised recursively.
        let mut body = vec![(
            1,
            Expression::FunctionCall(
                FuncName("preprocess".into()),
                Box::new(Expression::Lookup(None, Variable("input".into()))),
                vec![],
                Some(Block {
                    variable: Variable("read".into()),
                    body: Box::new(Expression::Sequence(vec![cond])),
                }),
            ),
        )];
        if_len_discard_special(&mut body);
        let Expression::FunctionCall(_, _, _, Some(block)) = &body[0].1 else {
            panic!("expected function call with block");
        };
        let Expression::Sequence(stmts) = block.body.as_ref() else {
            panic!("expected sequence body");
        };
        assert_eq!(
            stmts[0],
            Expression::Optimized(OptimizedExpression::LenThresholdDiscard(
                Variable("read".into()),
                BOp::LT,
                10
            ))
        );
    }

    /// A condition with a non-empty else branch, or `==`, must not be rewritten.
    #[test]
    fn if_len_discard_special_leaves_other_conditions() {
        let make = |op: BOp, else_body: Vec<Expression>| {
            Expression::Condition(
                Box::new(Expression::BinaryOp(
                    op,
                    Box::new(Expression::UnaryOp(
                        UOp::Len,
                        Box::new(Expression::Lookup(None, Variable("read".into()))),
                    )),
                    Box::new(Expression::ConstInt(10)),
                )),
                Box::new(Expression::Sequence(vec![Expression::Discard])),
                Box::new(Expression::Sequence(else_body)),
            )
        };
        // `==` is not an ordering operator; a non-empty else disqualifies the rewrite.
        for e in [
            make(BOp::EQ, vec![]),
            make(BOp::LT, vec![Expression::Discard]),
        ] {
            let mut body = vec![(1, e.clone())];
            if_len_discard_special(&mut body);
            assert_eq!(body[0].1, e);
        }
    }

    #[test]
    fn wrap_print_wraps_last_statement() {
        // The last statement becomes `write(<expr>, ofile=STDOUT)`.
        let mut body = vec![
            (
                1,
                Expression::Assignment(Variable("x".into()), Box::new(Expression::ConstInt(1))),
            ),
            (2, Expression::Lookup(None, Variable("x".into()))),
        ];
        wrap_print(&mut body).unwrap();
        assert_eq!(body.len(), 2);
        // Earlier statements are untouched.
        assert!(matches!(body[0].1, Expression::Assignment(..)));
        match &body[1].1 {
            Expression::FunctionCall(FuncName(f), inner, kwargs, None) => {
                assert_eq!(f, "write");
                assert!(matches!(**inner, Expression::Lookup(_, _)));
                assert_eq!(kwargs.len(), 1);
                assert_eq!(kwargs[0].0, Variable("ofile".into()));
                assert!(
                    matches!(&kwargs[0].1, Expression::BuiltinConstant(Variable(v)) if v == "STDOUT")
                );
            }
            other => panic!("expected a write() call, got {other:?}"),
        }
    }

    #[test]
    fn wrap_print_rejects_terminal_print_or_write() {
        for f in ["print", "write"] {
            let mut body = vec![(
                1,
                Expression::FunctionCall(
                    FuncName(f.into()),
                    Box::new(Expression::ConstStr("x".into())),
                    vec![],
                    None,
                ),
            )];
            assert!(wrap_print(&mut body).is_err());
        }
    }

    #[test]
    fn wrap_print_empty_script_is_noop() {
        let mut body: Vec<(usize, Expression)> = vec![];
        wrap_print(&mut body).unwrap();
        assert!(body.is_empty());
    }

    #[test]
    fn ngltype_show() {
        assert_eq!(show_ngltype(&NGLType::Counts, 0), "NGLCounts");
        assert_eq!(
            show_ngltype(&NGLType::List(Box::new(NGLType::String)), 0),
            "NGList NGLString"
        );
    }

    fn lookup(v: &str) -> Expression {
        Expression::Lookup(None, Variable(v.to_string()))
    }

    fn call(name: &str, arg: Expression, kwargs: Vec<(&str, Expression)>) -> Expression {
        Expression::FunctionCall(
            FuncName(name.to_string()),
            Box::new(arg),
            kwargs
                .into_iter()
                .map(|(k, e)| (Variable(k.to_string()), e))
                .collect(),
            None,
        )
    }

    fn assign(v: &str, e: Expression) -> Expression {
        Expression::Assignment(Variable(v.to_string()), Box::new(e))
    }

    fn has_can_move(e: &Expression) -> bool {
        if let Expression::FunctionCall(_, _, kwargs, _) = e {
            kwargs.iter().any(|(Variable(k), v)| {
                k == "__can_move" && matches!(v, Expression::ConstBool(true))
            })
        } else {
            false
        }
    }

    #[test]
    fn write_to_move_marks_dead_variable() {
        // x = preprocess(...); write(x) -> x is dead after the write, so __can_move is injected.
        let mut body = vec![
            (1, assign("x", call("preprocess", lookup("input"), vec![]))),
            (
                2,
                call(
                    "write",
                    lookup("x"),
                    vec![("ofile", Expression::ConstStr("out.fq".to_string()))],
                ),
            ),
        ];
        write_to_move(&mut body);
        assert!(has_can_move(&body[1].1));
    }

    #[test]
    fn write_to_move_keeps_used_variable() {
        // x is used again after the write, so it must NOT be moved.
        let mut body = vec![
            (1, assign("x", call("preprocess", lookup("input"), vec![]))),
            (
                2,
                call(
                    "write",
                    lookup("x"),
                    vec![("ofile", Expression::ConstStr("out.fq".to_string()))],
                ),
            ),
            (
                3,
                call(
                    "write",
                    lookup("x"),
                    vec![("ofile", Expression::ConstStr("out2.fq".to_string()))],
                ),
            ),
        ];
        write_to_move(&mut body);
        assert!(!has_can_move(&body[1].1));
        // The last write of x: x is no longer used afterwards, so it may move.
        assert!(has_can_move(&body[2].1));
    }

    #[test]
    fn write_to_move_blocks_input_files() {
        // A variable bound directly to fastq()/paired()/samfile() (or an alias) is a user input
        // file and must never be moved.
        let mut body = vec![
            (
                1,
                assign(
                    "x",
                    call("fastq", Expression::ConstStr("in.fq".to_string()), vec![]),
                ),
            ),
            (2, assign("y", lookup("x"))),
            (
                3,
                call(
                    "write",
                    lookup("x"),
                    vec![("ofile", Expression::ConstStr("o1.fq".to_string()))],
                ),
            ),
            (
                4,
                call(
                    "write",
                    lookup("y"),
                    vec![("ofile", Expression::ConstStr("o2.fq".to_string()))],
                ),
            ),
        ];
        write_to_move(&mut body);
        assert!(!has_can_move(&body[2].1));
        assert!(!has_can_move(&body[3].1));
    }

    fn has_output_bam(e: &Expression) -> bool {
        if let Expression::Assignment(_, rhs) = e {
            if let Expression::FunctionCall(_, _, kwargs, _) = rhs.as_ref() {
                return kwargs.iter().any(|(Variable(k), v)| {
                    k == "__output_bam" && matches!(v, Expression::ConstBool(true))
                });
            }
        }
        false
    }

    fn symbol(s: &str) -> Expression {
        Expression::ConstSymbol(s.to_string())
    }

    #[test]
    fn sort_oformat_injects_for_bam_write() {
        // s = samtools_sort(x); write(s, ofile='out.bam') -> sort straight to BAM.
        let mut body = vec![
            (1, assign("s", call("samtools_sort", lookup("x"), vec![]))),
            (
                2,
                call(
                    "write",
                    lookup("s"),
                    vec![("ofile", Expression::ConstStr("out.bam".to_string()))],
                ),
            ),
        ];
        sort_oformat(&mut body);
        assert!(has_output_bam(&body[0].1));
    }

    #[test]
    fn sort_oformat_respects_format_symbol() {
        // Explicit format={bam} wins even when ofile does not end in .bam.
        let mut body = vec![
            (1, assign("s", call("samtools_sort", lookup("x"), vec![]))),
            (
                2,
                call(
                    "write",
                    lookup("s"),
                    vec![
                        ("ofile", Expression::ConstStr("out.dat".to_string())),
                        ("format", symbol("bam")),
                    ],
                ),
            ),
        ];
        sort_oformat(&mut body);
        assert!(has_output_bam(&body[0].1));
    }

    #[test]
    fn sort_oformat_skips_sam_write() {
        // A SAM write must not trigger BAM output.
        let mut body = vec![
            (1, assign("s", call("samtools_sort", lookup("x"), vec![]))),
            (
                2,
                call(
                    "write",
                    lookup("s"),
                    vec![("ofile", Expression::ConstStr("out.sam".to_string()))],
                ),
            ),
        ];
        sort_oformat(&mut body);
        assert!(!has_output_bam(&body[0].1));
    }

    #[test]
    fn sort_oformat_skips_when_reused() {
        // If the sorted set is used elsewhere (here: another write), it is unsafe to change format.
        let mut body = vec![
            (1, assign("s", call("samtools_sort", lookup("x"), vec![]))),
            (
                2,
                call(
                    "write",
                    lookup("s"),
                    vec![("ofile", Expression::ConstStr("out.bam".to_string()))],
                ),
            ),
            (
                3,
                call(
                    "write",
                    lookup("s"),
                    vec![("ofile", Expression::ConstStr("copy.bam".to_string()))],
                ),
            ),
        ];
        sort_oformat(&mut body);
        assert!(!has_output_bam(&body[0].1));
    }

    #[test]
    fn sort_oformat_infers_bam_from_concatenated_ofile() {
        // ofile = base + '.bam' should still be recognised as BAM (suffix is the right operand).
        let ofile = Expression::BinaryOp(
            BOp::Add,
            Box::new(lookup("base")),
            Box::new(Expression::ConstStr(".bam".to_string())),
        );
        let mut body = vec![
            (1, assign("s", call("samtools_sort", lookup("x"), vec![]))),
            (2, call("write", lookup("s"), vec![("ofile", ofile)])),
        ];
        sort_oformat(&mut body);
        assert!(has_output_bam(&body[0].1));
    }

    fn index_one(v: &str, ix: i64) -> Expression {
        Expression::IndexExpression(
            Box::new(lookup(v)),
            Index::One(Box::new(Expression::ConstInt(ix))),
        )
    }

    fn is_index_check(e: &Expression) -> Option<(String, i64)> {
        if let Expression::FunctionCall(FuncName(f), arg, kwargs, _) = e {
            if f == "__check_index_access" {
                let var = match arg.as_ref() {
                    Expression::Lookup(_, Variable(v)) => v.clone(),
                    _ => return None,
                };
                let ix = kwargs
                    .iter()
                    .find_map(|(Variable(k), v)| match (k.as_str(), v) {
                        ("index1", Expression::ConstInt(i)) => Some(*i),
                        _ => None,
                    })?;
                return Some((var, ix));
            }
        }
        None
    }

    fn is_rs_check(e: &Expression, expect_var: &str) -> bool {
        if let Expression::FunctionCall(FuncName(f), arg, _, _) = e {
            if f == "__check_readset" {
                if let Expression::Lookup(_, Variable(v)) = arg.as_ref() {
                    return v == expect_var;
                }
            }
        }
        false
    }

    #[test]
    fn add_rs_checks_floats_to_after_assignment() {
        // input = fastq(..); y = other; input = preprocess(input) -> a __check_readset(input) is
        // floated up to just after input's (first) assignment, before `y = other`.
        let body = vec![
            (
                1,
                assign(
                    "input",
                    call("fastq", Expression::ConstStr("f.fq".to_string()), vec![]),
                ),
            ),
            (2, assign("y", lookup("other"))),
            (
                3,
                assign("input", call("preprocess", lookup("input"), vec![])),
            ),
        ];
        let out = add_rs_checks(body);
        assert_eq!(out.len(), 4);
        // The check floats up to just after input's first assignment (index 1).
        assert!(is_rs_check(&out[1].1, "input"));
        // Original line number is preserved on the check (line 3, where the preprocess is).
        assert_eq!(out[1].0, 3);
    }

    #[test]
    fn add_rs_checks_ignores_non_preprocess() {
        // A non-preprocess assignment gets no readset check.
        let body = vec![
            (1, assign("input", call("fastq", lookup("p"), vec![]))),
            (2, assign("m", call("map", lookup("input"), vec![]))),
        ];
        let out = add_rs_checks(body);
        assert_eq!(out.len(), 2);
    }

    #[test]
    fn add_index_checks_floats_to_after_assignment() {
        // x = [..]; y = other; print(x[5]) -> check inserted right after x's assignment.
        let body = vec![
            (1, assign("x", Expression::ListExpression(vec![]))),
            (2, assign("y", lookup("other"))),
            (3, call("print", index_one("x", 5), vec![])),
        ];
        let out = add_index_checks(body);
        assert_eq!(out.len(), 4);
        // The check floats up to just after the x assignment (index 1), before `y = other`.
        assert_eq!(is_index_check(&out[1].1), Some(("x".to_string(), 5)));
        // Original line number is preserved on the check (line 3, where the access is).
        assert_eq!(out[1].0, 3);
    }

    #[test]
    fn add_index_checks_ignores_non_constant_index() {
        // A non-constant index (variable) gets no static check.
        let body = vec![
            (1, assign("x", Expression::ListExpression(vec![]))),
            (
                2,
                call(
                    "print",
                    Expression::IndexExpression(
                        Box::new(lookup("x")),
                        Index::One(Box::new(lookup("i"))),
                    ),
                    vec![],
                ),
            ),
        ];
        let out = add_index_checks(body);
        assert_eq!(out.len(), 2);
    }

    #[test]
    fn add_index_checks_stays_within_condition_branch() {
        // Checks inside an if-branch must not float above the condition.
        let body = vec![
            (1, assign("x", Expression::ListExpression(vec![]))),
            (
                2,
                Expression::Condition(
                    Box::new(Expression::ConstBool(true)),
                    Box::new(call("print", index_one("x", 5), vec![])),
                    Box::new(Expression::Discard),
                ),
            ),
        ];
        let out = add_index_checks(body);
        // No check is inserted at top level between the assignment and the condition.
        assert_eq!(out.len(), 2);
        // The check lives inside the true branch (as a Sequence with the check first).
        if let Expression::Condition(_, t, _) = &out[1].1 {
            if let Expression::Sequence(es) = t.as_ref() {
                assert_eq!(is_index_check(&es[0]), Some(("x".to_string(), 5)));
            } else {
                panic!("expected true branch to be a Sequence, got {:?}", t);
            }
        } else {
            panic!("expected a Condition");
        }
    }
}
