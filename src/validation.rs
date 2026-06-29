//! Pure (non-IO) validation passes, mirroring `NGLess/Validation.hs`.
//!
//! Each pass walks the script and accumulates error messages; `validate` runs them all and
//! fails if any fired. The IO-dependent checks (file existence, reference resolution) live in
//! `ValidationIO.hs` and are deferred to a later milestone.

use crate::ast::*;
use crate::errors::{NgError, NgResult};
use crate::modules::{
    builtin_methods, ArgCheck, ArgInformation, Function, FunctionCheck, MethodInfo,
};

/// Run all pure validation passes. `funcs` are the functions available (from the imported
/// modules); `constants` are module constant names. Methods always come from the builtin set
/// (as in the Haskell `findMethod`). Returns `Ok(())` if the script is valid.
pub fn validate(funcs: &[Function], constants: &[String], script: &Script) -> NgResult<()> {
    let methods = builtin_methods();
    let mut errs: Vec<String> = Vec::new();
    errs.extend(validate_variables(constants, script));
    errs.extend(validate_function_req_args(funcs, script));
    errs.extend(validate_symbol_in_args(funcs, &methods, script));
    errs.extend(validate_stdin_used_once(script));
    errs.extend(validate_map_ref(script));
    errs.extend(validate_no_constant_assignments(constants, script));
    errs.extend(validate_ngless_version_uses(funcs, &methods, script));
    errs.extend(validate_pure_functions(funcs, script));
    errs.extend(validate_write_oname(script));
    errs.extend(validate_block_assignments(script));
    // Samtools-module check: only active when the module's functions are in scope.
    if funcs.iter().any(|f| f.name.0 == "samtools_sort") {
        errs.extend(validate_select_unique_not_sorted(script));
    }
    if errs.is_empty() {
        Ok(())
    } else {
        Err(NgError::script(errs.join("\n")))
    }
}

/// Run the IO-dependent validation pass (mirrors `validateIO` in `ValidationIO.hs`): an eager,
/// up-front search for problems that would otherwise only surface once interpretation reaches the
/// offending call. It checks that constant input files exist and are readable, that constant output
/// directories exist and are writable (warning on overwrite), that `map(..., reference=...)` names a
/// known reference, and runs the `count()` feature check. Collected errors are joined and returned
/// as a single script error (matching Main.hs joining with blank lines); the `count` check throws
/// directly, as in Haskell.
///
/// `funcs` is the full function table (to look up the `FileReadable`/`FileWritable` argument tags);
/// `search_path` is used to expand `<...>`-placeholder input paths exactly as interpretation will.
pub fn validate_io(funcs: &[Function], search_path: &[String], script: &Script) -> NgResult<()> {
    let mut errs: Vec<String> = Vec::new();
    validate_read_inputs(funcs, search_path, script, &mut errs);
    validate_ofile(funcs, script, &mut errs);
    check_references_exist(script, &mut errs);
    // `validateCount` throws directly rather than accumulating (it calls `executeCountCheck`), so it
    // runs last and propagates, mirroring the Haskell ordering.
    validate_count(script)?;
    if errs.is_empty() {
        Ok(())
    } else {
        Err(NgError::script(errs.join("\n\n")))
    }
}

/// `validateReadInputs`: for every call argument tagged `FileReadable` whose value is a constant
/// string (a literal or a variable that traces to a unique constant assignment), check that the file
/// exists and is readable, expanding `<...>` placeholders through the search path first.
fn validate_read_inputs(
    funcs: &[Function],
    search_path: &[String],
    script: &Script,
    errs: &mut Vec<String>,
) {
    for (lno, e) in &script.body {
        let mut local: Vec<String> = Vec::new();
        recursive_analyse(e, &mut |x| {
            if let Expression::FunctionCall(f, arg, args, _) = x {
                if let Some(finfo) = find_function(funcs, f) {
                    let mut check =
                        |fname: &str| check_file_readable_io(fname, search_path, &mut local);
                    if finfo.arg_checks.contains(&ArgCheck::FileReadable) {
                        validate_str_val(arg, script, &mut check);
                    }
                    for ainfo in &finfo.kwargs {
                        if ainfo.checks.contains(&ArgCheck::FileReadable) {
                            if let Some(v) = lookup_arg(args, &ainfo.name) {
                                validate_str_val(v, script, &mut check);
                            }
                        }
                    }
                }
            }
        });
        errs.extend(add_lno(*lno, local));
    }
}

/// `checkFileReadable'`: expand the path through the search path, then report a problem with the
/// first existing candidate (or that no candidate could be found at all).
fn check_file_readable_io(fname: &str, search_path: &[String], local: &mut Vec<String>) {
    let candidates = crate::interpret::expand_path_candidates(fname, search_path);
    match candidates.iter().find(|p| std::path::Path::new(p).exists()) {
        Some(p) => {
            if let Some(err) = crate::suggestion::check_file_readable(p) {
                local.push(err);
            }
        }
        None => local.push(format!("Could not find necessary input file {fname}")),
    }
}

/// `validateOFile`: for every call argument tagged `FileWritable` whose value is a constant string,
/// check that the output directory exists and is writable, and warn when an existing file would be
/// overwritten.
fn validate_ofile(funcs: &[Function], script: &Script, errs: &mut Vec<String>) {
    for (lno, e) in &script.body {
        let mut local: Vec<String> = Vec::new();
        recursive_analyse(e, &mut |x| {
            if let Expression::FunctionCall(f, arg, args, _) = x {
                if let Some(finfo) = find_function(funcs, f) {
                    let mut check = |ofile: &str| check_ofile_io(ofile, &mut local);
                    if finfo.arg_checks.contains(&ArgCheck::FileWritable) {
                        validate_str_val(arg, script, &mut check);
                    }
                    for ainfo in &finfo.kwargs {
                        if ainfo.checks.contains(&ArgCheck::FileWritable) {
                            if let Some(v) = lookup_arg(args, &ainfo.name) {
                                validate_str_val(v, script, &mut check);
                            }
                        }
                    }
                }
            }
        });
        errs.extend(add_lno(*lno, local));
    }
}

/// `checkOFileV`: collect any directory problem, and warn (not error) if the file already exists.
fn check_ofile_io(ofile: &str, local: &mut Vec<String>) {
    if let Some(err) = crate::interpret::check_ofile(ofile) {
        local.push(err);
    }
    if std::path::Path::new(ofile).exists() {
        // Mirrors `outputListLno' WarningOutput`, which carries no explicit line number here.
        crate::output::warn(
            0,
            &format!("Writing to file '{ofile}' will overwrite existing file."),
        );
    }
}

/// `checkReferencesExist`: a `map(..., reference=...)` whose constant value is neither a builtin
/// reference name/alias nor an existing file on disk is an error (with a "did you mean" suggestion).
fn check_references_exist(script: &Script, errs: &mut Vec<String>) {
    let allnames = crate::reference::builtin_reference_names();
    for (lno, e) in &script.body {
        let mut local: Vec<String> = Vec::new();
        recursive_analyse(e, &mut |x| {
            if let Expression::FunctionCall(FuncName(name), _, args, _) = x {
                if name == "map" {
                    if let Some(v) = lookup_arg(args, "reference") {
                        validate_str_val(v, script, &mut |r| {
                            if !allnames.iter().any(|n| n == r) {
                                let exists = std::path::Path::new(r).is_file();
                                let mut msg = format!(
                                    "Could not find reference {r} (it is neither built in nor in any of the loaded modules).\n"
                                );
                                if exists {
                                    msg.push_str(&format!(
                                        "\n\tDid you mean to use the argument `fafile` to specify the FASTA file `{r}`?\n\tmap() uses the argument `reference` for builtin references and `fafile` for a FASTA file path."
                                    ));
                                }
                                msg.push_str(&crate::suggestion::suggestion_message(r, &allnames));
                                msg.push_str("\n\tValid options are:");
                                for v in &allnames {
                                    msg.push_str(&format!("\n\t\t - {v}"));
                                }
                                local.push(msg);
                            }
                        });
                    }
                }
            }
        });
        errs.extend(add_lno(*lno, local));
    }
}

/// `validateCount` (mirrors `ValidationIO.hs`): run `executeCountCheck` on `count()` calls whose
/// keyword arguments are all statically known. The only check performed is that, in functional-map
/// mode, every requested feature is a column of the map file. Errors propagate directly.
fn validate_count(script: &Script) -> NgResult<()> {
    for (lno, e) in &script.body {
        let mut calls: Vec<Vec<(Variable, Expression)>> = Vec::new();
        recursive_analyse(e, &mut |x| {
            if let Expression::FunctionCall(FuncName(name), _, kwargs, None) = x {
                if name == "count" {
                    calls.push(kwargs.clone());
                }
            }
        });
        for kwargs in &calls {
            // Only run when every keyword argument is statically known (`constantKWArgs`).
            if !kwargs.iter().all(|(_, v)| is_static(v)) {
                continue;
            }
            let fmap = kwargs
                .iter()
                .find(|(k, _)| k.0 == "functional_map")
                .and_then(|(_, v)| static_string(v));
            let Some(fmap) = fmap else { continue };
            let features = kwargs
                .iter()
                .find(|(k, _)| k.0 == "features")
                .and_then(|(_, v)| static_string_list(v))
                .unwrap_or_default();
            crate::count::check_functional_features(&fmap, &features, *lno)?;
        }
    }
    Ok(())
}

/// `validateStrVal`: apply `f` to the string value of `e` when it is a constant — either a literal
/// `ConstStr` or a `Lookup` of a variable that traces to a unique constant string assignment.
fn validate_str_val(e: &Expression, script: &Script, f: &mut dyn FnMut(&str)) {
    match e {
        Expression::ConstStr(v) => f(v),
        Expression::Lookup(_, var) => {
            if let Some(v) = try_const_string(var, script) {
                f(&v);
            }
        }
        _ => {}
    }
}

/// `tryConstValue` (for strings): if the script has exactly one top-level assignment to `var` and
/// its right-hand side is a constant string, return that string.
fn try_const_string(var: &Variable, script: &Script) -> Option<String> {
    let assigned: Vec<&Expression> = script
        .body
        .iter()
        .filter_map(|(_, e)| match e {
            Expression::Assignment(v, rhs) if v == var => Some(rhs.as_ref()),
            _ => None,
        })
        .collect();
    match assigned.as_slice() {
        [rhs] => static_string(rhs),
        _ => None,
    }
}

/// Whether `e` is a fully static expression (mirrors `staticValue` returning `Just`).
fn is_static(e: &Expression) -> bool {
    match e {
        Expression::ConstStr(_)
        | Expression::ConstInt(_)
        | Expression::ConstBool(_)
        | Expression::ConstSymbol(_) => true,
        Expression::BinaryOp(_, a, b) => is_static(a) && is_static(b),
        Expression::ListExpression(es) => es.iter().all(is_static),
        _ => false,
    }
}

/// Extract a list of constant strings from a `features=` argument (a single string or a list).
fn static_string_list(e: &Expression) -> Option<Vec<String>> {
    match e {
        Expression::ConstStr(s) => Some(vec![s.clone()]),
        Expression::ListExpression(es) => es.iter().map(static_string).collect(),
        _ => None,
    }
}

// --- generic traversal ----------------------------------------------------

/// Call `f` on `e` and every sub-expression (mirrors `recursiveAnalyse`).
fn recursive_analyse(e: &Expression, f: &mut dyn FnMut(&Expression)) {
    f(e);
    match e {
        Expression::ListExpression(es) | Expression::Sequence(es) => {
            for c in es {
                recursive_analyse(c, f);
            }
        }
        Expression::UnaryOp(_, a) => recursive_analyse(a, f),
        Expression::BinaryOp(_, a, b) => {
            recursive_analyse(a, f);
            recursive_analyse(b, f);
        }
        Expression::Condition(c, t, fe) => {
            recursive_analyse(c, f);
            recursive_analyse(t, f);
            recursive_analyse(fe, f);
        }
        Expression::IndexExpression(a, ix) => {
            recursive_analyse(a, f);
            match ix {
                Index::One(i) => recursive_analyse(i, f),
                Index::Two(a, b) => {
                    if let Some(a) = a {
                        recursive_analyse(a, f);
                    }
                    if let Some(b) = b {
                        recursive_analyse(b, f);
                    }
                }
            }
        }
        Expression::Assignment(_, a) => recursive_analyse(a, f),
        Expression::FunctionCall(_, arg, args, block) => {
            recursive_analyse(arg, f);
            for (_, v) in args {
                recursive_analyse(v, f);
            }
            if let Some(b) = block {
                recursive_analyse(&b.body, f);
            }
        }
        Expression::MethodCall(_, self_e, arg, args) => {
            recursive_analyse(self_e, f);
            if let Some(a) = arg {
                recursive_analyse(a, f);
            }
            for (_, v) in args {
                recursive_analyse(v, f);
            }
        }
        _ => {}
    }
}

fn find_function<'a>(funcs: &'a [Function], f: &FuncName) -> Option<&'a Function> {
    funcs.iter().find(|fi| &fi.name == f)
}

fn find_method<'a>(methods: &'a [MethodInfo], m: &MethodName) -> Option<&'a MethodInfo> {
    methods.iter().find(|mi| &mi.name == m)
}

fn lookup_arg<'a>(args: &'a [(Variable, Expression)], name: &str) -> Option<&'a Expression> {
    args.iter()
        .find(|(Variable(v), _)| v == name)
        .map(|(_, e)| e)
}

/// Prefix each message with `Error on line {lno}: ` (mirrors `addLno`).
fn add_lno(lno: usize, errs: Vec<String>) -> Vec<String> {
    errs.into_iter()
        .map(|e| format!("Error on line {lno}: {e}"))
        .collect()
}

/// `Line {lno}: ` prefix (mirrors `tell1lno`).
fn line_msg(lno: usize, msg: String) -> String {
    format!("Line {lno}: {msg}")
}

// --- individual checks ----------------------------------------------------

fn validate_variables(constants: &[String], script: &Script) -> Vec<String> {
    let mut used: Vec<String> = constants.to_vec();
    let mut errs: Vec<String> = Vec::new();
    for (_, e) in &script.body {
        match e {
            Expression::Assignment(Variable(v), rhs) => {
                recursive_analyse(rhs, &mut |x| check_var_usage(x, &mut used, &mut errs));
                used.push(v.clone());
            }
            _ => recursive_analyse(e, &mut |x| check_var_usage(x, &mut used, &mut errs)),
        }
    }
    errs
}

fn check_var_usage(e: &Expression, used: &mut Vec<String>, errs: &mut Vec<String>) {
    match e {
        Expression::Lookup(_, Variable(v)) => {
            if !used.contains(v) {
                errs.push(format!(
                    "Could not find variable `{v:?}`. {}",
                    crate::suggestion::suggestion_message(v, used)
                ));
            }
        }
        Expression::FunctionCall(_, _, _, Some(block)) => {
            used.push(block.variable.0.clone());
        }
        Expression::Assignment(Variable(v), _) => {
            used.push(v.clone());
        }
        _ => {}
    }
}

fn validate_function_req_args(funcs: &[Function], script: &Script) -> Vec<String> {
    let mut out = Vec::new();
    for (lno, e) in &script.body {
        let mut local = Vec::new();
        recursive_analyse(e, &mut |x| {
            if let Expression::FunctionCall(f, _, args, _) = x {
                match find_function(funcs, f) {
                    None => local.push(format!("Function {f} not found.")),
                    Some(finfo) => {
                        let used: Vec<&str> =
                            args.iter().map(|(Variable(k), _)| k.as_str()).collect();
                        for ai in &finfo.kwargs {
                            if ai.required && !used.contains(&ai.name.as_str()) {
                                local.push(format!("Function {f} requires argument {}.", ai.name));
                            }
                        }
                    }
                }
            }
        });
        out.extend(add_lno(*lno, local));
    }
    out
}

fn validate_symbol_in_args(
    funcs: &[Function],
    methods: &[MethodInfo],
    script: &Script,
) -> Vec<String> {
    fn show_a(legal: &[String]) -> String {
        legal
            .iter()
            .map(|s| format!("{{{s}}}"))
            .collect::<Vec<_>>()
            .join(", ")
    }
    fn check_symbol(
        who: &str,
        argname: &str,
        expr: &Expression,
        legal: &[String],
        out: &mut Vec<String>,
    ) {
        match expr {
            Expression::ConstSymbol(s) => {
                if !legal.contains(s) {
                    match crate::suggestion::find_suggestion(s, legal) {
                        Some(sug) => out.push(format!(
                            "Argument `{argname}` (for {who}) got {{{s}}}.\n\tDid you mean {{{}}} ({})\n\nLegal arguments are: [{}]",
                            sug.suggestion,
                            sug.reason,
                            show_a(legal)
                        )),
                        None => out.push(format!(
                            "Argument: `{argname}` (for {who}) expects one of {} but got {{{s}}}",
                            show_a(legal)
                        )),
                    }
                }
            }
            Expression::ListExpression(es) => {
                for e in es {
                    check_symbol(who, argname, e, legal, out);
                }
            }
            _ => {}
        }
    }
    fn symbol_legal(checks: &[ArgCheck]) -> Option<&[String]> {
        checks.iter().find_map(|c| match c {
            ArgCheck::Symbol(ss) => Some(ss.as_slice()),
            _ => None,
        })
    }

    let mut out = Vec::new();
    for (lno, e) in &script.body {
        let mut local = Vec::new();
        recursive_analyse(e, &mut |x| match x {
            Expression::FunctionCall(f, arg0, args, _) => match find_function(funcs, f) {
                None => local.push(format!("Function '{f}' not found")),
                Some(finfo) => {
                    if let Some(legal) = symbol_legal(&finfo.arg_checks) {
                        check_symbol(
                            &format!("function {f}"),
                            "main arg",
                            arg0,
                            legal,
                            &mut local,
                        );
                    }
                    for (Variable(v), e) in args {
                        match finfo.kwargs.iter().find(|ai| &ai.name == v) {
                            None => {
                                local.push(format!("Function '{f}' does not accept argument '{v}'"))
                            }
                            Some(ai) => {
                                if let Some(legal) = symbol_legal(&ai.checks) {
                                    check_symbol(&format!("function {f}"), v, e, legal, &mut local);
                                }
                            }
                        }
                    }
                }
            },
            Expression::MethodCall(m, _, arg0, args) => match find_method(methods, m) {
                None => local.push(format!("Method'{m}' not found")),
                Some(minfo) => {
                    let mut all: Vec<(String, &Expression)> = Vec::new();
                    if let Some(a) = arg0 {
                        all.push(("__0".to_string(), a));
                    }
                    for (Variable(v), e) in args {
                        all.push((v.clone(), e));
                    }
                    for (v, e) in all {
                        let legal = allowed_method(minfo, &v);
                        check_symbol(&format!("method {m}"), &v, e, &legal, &mut local);
                    }
                }
            },
            _ => {}
        });
        out.extend(add_lno(*lno, local));
    }
    out
}

fn allowed_method(minfo: &MethodInfo, v: &str) -> Vec<String> {
    minfo
        .kwargs
        .iter()
        .find(|ai: &&ArgInformation| ai.name == v)
        .and_then(|ai| {
            ai.checks.iter().find_map(|c| match c {
                ArgCheck::Symbol(ss) => Some(ss.clone()),
                _ => None,
            })
        })
        .unwrap_or_default()
}

fn validate_stdin_used_once(script: &Script) -> Vec<String> {
    let mut out = Vec::new();
    let mut prev: Option<usize> = None;
    for (lno, e) in &script.body {
        if constant_used("STDIN", e) {
            if let Some(p) = prev {
                out.push(line_msg(
                    *lno,
                    format!("STDIN can only be used once (previously used on line {p})."),
                ));
            }
            prev = Some(*lno);
        }
    }
    out
}

/// Whether any expression in the script writes to `STDOUT` (mirrors `uses_STDOUT` in
/// `Validation.hs`). When true, NGLess suppresses the run header so it does not corrupt the
/// data stream (mirrors `setQuiet` on `uses_STDOUT`).
pub fn uses_stdout(script: &Script) -> bool {
    script.body.iter().any(|(_, e)| constant_used("STDOUT", e))
}

fn constant_used(k: &str, e: &Expression) -> bool {
    match e {
        Expression::BuiltinConstant(Variable(k2)) => k == k2,
        Expression::ListExpression(es) | Expression::Sequence(es) => {
            es.iter().any(|e| constant_used(k, e))
        }
        Expression::UnaryOp(_, a) | Expression::Assignment(_, a) => constant_used(k, a),
        Expression::BinaryOp(_, a, b) => constant_used(k, a) || constant_used(k, b),
        Expression::Condition(a, b, c) => {
            constant_used(k, a) || constant_used(k, b) || constant_used(k, c)
        }
        Expression::IndexExpression(a, ix) => {
            constant_used(k, a)
                || match ix {
                    Index::One(i) => constant_used(k, i),
                    Index::Two(a, b) => {
                        a.as_ref().is_some_and(|a| constant_used(k, a))
                            || b.as_ref().is_some_and(|b| constant_used(k, b))
                    }
                }
        }
        Expression::FunctionCall(_, e, args, b) => {
            constant_used(k, e)
                || args.iter().any(|(_, e)| constant_used(k, e))
                || b.as_ref().is_some_and(|b| constant_used(k, &b.body))
        }
        _ => false,
    }
}

fn validate_map_ref(script: &Script) -> Vec<String> {
    let mut out = Vec::new();
    for (lno, e) in &script.body {
        let mut local = Vec::new();
        recursive_analyse(e, &mut |x| {
            if let Expression::FunctionCall(FuncName(name), _, args, _) = x {
                if name == "map" {
                    let r = lookup_arg(args, "reference");
                    let fa = lookup_arg(args, "fafile");
                    match (r, fa) {
                        (None, None) => local.push(
                            "Either fafile or reference must be specified in argument to map function".to_string(),
                        ),
                        (Some(_), Some(_)) => local.push(
                            "You cannot specify both fafile and reference in arguments to map function".to_string(),
                        ),
                        _ => {}
                    }
                }
            }
        });
        out.extend(add_lno(*lno, local));
    }
    out
}

/// `checkUnique` (samtools module): selecting unique reads from a `samtools_sort`ed read set is an
/// error, since uniqueness is computed per read-name group which sorting destroys. For each
/// `v = select(w, keep_if=[..{unique}..])`, trace `w`'s assignment chain backwards; if any link is
/// a `samtools_sort`, report an error.
fn validate_select_unique_not_sorted(script: &Script) -> Vec<String> {
    let mut out = Vec::new();
    for (i, (lno, e)) in script.body.iter().enumerate() {
        if let Expression::Assignment(_, rhs) = e {
            if let Expression::FunctionCall(FuncName(name), arg, kwargs, None) = rhs.as_ref() {
                if name == "select" && selects_unique(kwargs) {
                    if let Expression::Lookup(_, Variable(v)) = arg.as_ref() {
                        // Preceding statements, most-recent first (mirrors `map snd rest`).
                        let preceding: Vec<&Expression> =
                            script.body[..i].iter().rev().map(|(_, e)| e).collect();
                        if traces_to_sort(v, &preceding) {
                            out.push(line_msg(
                                *lno,
                                format!(
                                    "Cannot select unique reads from a sorted mappedreadset (at line {lno}).\n\tConsider selecting, *then* sorting."
                                ),
                            ));
                        }
                    }
                }
            }
        }
    }
    out
}

/// Whether `kwargs` contains `keep_if=` with a `{unique}` symbol (possibly inside a list).
fn selects_unique(kwargs: &[(Variable, Expression)]) -> bool {
    fn has_unique(e: &Expression) -> bool {
        match e {
            Expression::ConstSymbol(s) => s == "unique",
            Expression::ListExpression(vs) => vs.iter().any(has_unique),
            _ => false,
        }
    }
    kwargs
        .iter()
        .any(|(Variable(k), v)| k == "keep_if" && has_unique(v))
}

/// Trace variable `v` back through `preceding` assignments (most-recent first): a `samtools_sort`
/// link means sorted; a `Lookup` link continues the trace through the new variable.
fn traces_to_sort(v: &str, preceding: &[&Expression]) -> bool {
    let target = v.to_string();
    for (idx, e) in preceding.iter().enumerate() {
        if let Expression::Assignment(Variable(v2), expr) = e {
            if *v2 == target {
                match expr.as_ref() {
                    Expression::FunctionCall(FuncName(n), _, _, _) if n == "samtools_sort" => {
                        return true
                    }
                    Expression::Lookup(_, Variable(v3)) => {
                        return traces_to_sort(v3, &preceding[idx + 1..])
                    }
                    _ => return false,
                }
            }
        }
    }
    false
}

fn validate_no_constant_assignments(constants: &[String], script: &Script) -> Vec<String> {
    let mut active: Vec<String> = vec!["STDIN".into(), "STDOUT".into()];
    active.extend(constants.iter().cloned());
    let mut out = Vec::new();
    for (lno, e) in &script.body {
        if let Expression::Assignment(Variable(v), _) = e {
            if active.contains(v) {
                out.push(line_msg(
                    *lno,
                    format!("assignment to constant `{v}` is illegal."),
                ));
            }
            // `T.all isUpper v`: a name whose every character is an uppercase letter is a
            // constant (digits/underscores make `isUpper` false, so they are not).
            if !v.is_empty() && v.chars().all(char::is_uppercase) {
                active.push(v.clone());
            }
        }
    }
    out
}

fn validate_pure_functions(funcs: &[Function], script: &Script) -> Vec<String> {
    let version00 = matches!(&script.header, Some(h) if h.version == "0.0");
    let is_pure = |f: &FuncName| {
        find_function(funcs, f)
            .map(|fi| fi.checks.contains(&FunctionCheck::ReturnAssigned))
            .unwrap_or(false)
    };
    let mut out = Vec::new();
    for (lno, e) in &script.body {
        if let Expression::FunctionCall(fname, _, _, _) = e {
            if fname.0 == "preprocess" {
                if !version00 {
                    out.push(line_msg(*lno, "Preprocess must be assigned to an output (behaviour changed from version 0.0)".to_string()));
                }
            } else if is_pure(fname) {
                out.push(line_msg(*lno, format!("Result of calling function `{}` should be assigned to a variable (this function has no effect otherwise).", fname.0)));
            }
        }
    }
    out
}

/// `validateWriteOName` in the original looks up an argument named "oname" (write actually
/// uses "ofile"), so it is effectively dormant; ported faithfully.
fn validate_write_oname(script: &Script) -> Vec<String> {
    fn suffix_check(t: &NGLType, oname: &str) -> Option<String> {
        match t {
            NGLType::ReadSet => {
                if oname.ends_with(".fa") {
                    Some("Cannot save data in FASTA format.".to_string())
                } else if oname.ends_with(".fq") || oname.ends_with(".fq.gz") {
                    None
                } else {
                    Some(format!(
                        "Cannot determine output format from filename '{oname}'"
                    ))
                }
            }
            _ => None,
        }
    }
    let mut out = Vec::new();
    for (lno, e) in &script.body {
        let mut local = Vec::new();
        recursive_analyse(e, &mut |x| {
            if let Expression::FunctionCall(FuncName(name), arg0, args, _) = x {
                if name == "write" {
                    if let Expression::Lookup(Some(t), _) = arg0.as_ref() {
                        if let Some(oname_e) = lookup_arg(args, "oname") {
                            if let Some(oname) = static_string(oname_e) {
                                if lookup_arg(args, "format").is_none() {
                                    if let Some(msg) = suffix_check(t, &oname) {
                                        local.push(msg);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        });
        out.extend(add_lno(*lno, local));
    }
    out
}

/// Minimal `staticValue` for string constants (enough for `validate_write_oname`).
fn static_string(e: &Expression) -> Option<String> {
    match e {
        Expression::ConstStr(s) => Some(s.clone()),
        _ => None,
    }
}

fn validate_block_assignments(script: &Script) -> Vec<String> {
    fn go(lno: usize, e: &Expression, out: &mut Vec<String>) {
        match e {
            Expression::Assignment(_, rhs) => go(lno, rhs, out),
            Expression::FunctionCall(FuncName(fname), _, _, Some(block)) => {
                let var = &block.variable.0;
                recursive_analyse(&block.body, &mut |x| {
                    if let Expression::Assignment(Variable(v2), _) = x {
                        if v2 != var {
                            out.push(line_msg(lno, format!("Inside blocks, only the block variable (in this case `{var}`) can be assigned to (when analysing function `{fname}`).")));
                        }
                    }
                });
            }
            _ => {}
        }
    }
    let mut out = Vec::new();
    for (lno, e) in &script.body {
        go(*lno, e, &mut out);
    }
    out
}

fn validate_ngless_version_uses(
    funcs: &[Function],
    methods: &[MethodInfo],
    script: &Script,
) -> Vec<String> {
    let version = match &script.header {
        Some(h) => h.version.clone(),
        None => return Vec::new(),
    };
    let mut out = Vec::new();
    for (lno, expr) in &script.body {
        recursive_analyse(expr, &mut |x| match x {
            Expression::FunctionCall(fname, _, kwargs, _) => {
                if let Some(finfo) = find_function(funcs, fname) {
                    if let Some(minv) = min_version_function(&finfo.checks) {
                        check_version(
                            &format!("Function {}", fname.0),
                            minv,
                            &version,
                            *lno,
                            &mut out,
                        );
                    }
                    for (Variable(name), _) in kwargs {
                        if let Some(minv) = check_arg(&finfo.kwargs, name) {
                            check_version(
                                &format!("Using argument {name} to function {}", fname.0),
                                minv,
                                &version,
                                *lno,
                                &mut out,
                            );
                        }
                    }
                }
            }
            Expression::MethodCall(mname, _, _, kwargs) => {
                if let Some(minfo) = find_method(methods, mname) {
                    if let Some(minv) = min_version_method(&minfo.checks) {
                        check_version(
                            &format!("Using method {}", mname.0),
                            minv,
                            &version,
                            *lno,
                            &mut out,
                        );
                    }
                    for (Variable(name), _) in kwargs {
                        if let Some(minv) = check_arg(&minfo.kwargs, name) {
                            check_version(
                                &format!("Using argument {name} to method {}", mname.0),
                                minv,
                                &version,
                                *lno,
                                &mut out,
                            );
                        }
                    }
                }
            }
            _ => {}
        });
    }
    out
}

fn min_version_function(checks: &[FunctionCheck]) -> Option<(i64, i64)> {
    checks.iter().find_map(|c| match c {
        FunctionCheck::MinNGLessVersion(a, b) => Some((*a, *b)),
        _ => None,
    })
}

fn min_version_method(checks: &[FunctionCheck]) -> Option<(i64, i64)> {
    min_version_function(checks)
}

fn check_arg(ainfos: &[ArgInformation], argname: &str) -> Option<(i64, i64)> {
    let ai = ainfos.iter().find(|ai| ai.name == argname)?;
    ai.checks.iter().find_map(|c| match c {
        ArgCheck::MinVersion(a, b) => Some((*a, *b)),
        _ => None,
    })
}

fn check_version(prefix: &str, minv: (i64, i64), version: &str, lno: usize, out: &mut Vec<String>) {
    if version_le(minv, version) {
        return;
    }
    out.push(line_msg(
        lno,
        format!(
            "{prefix} requires ngless version {}.{} (version '{version}' is active).",
            minv.0, minv.1
        ),
    ));
}

fn version_le(minv: (i64, i64), actual: &str) -> bool {
    match parse_version(actual) {
        Some((a_maj, a_min)) => match a_maj.cmp(&minv.0) {
            std::cmp::Ordering::Greater => true,
            std::cmp::Ordering::Equal => a_min >= minv.1,
            std::cmp::Ordering::Less => false,
        },
        None => false,
    }
}

fn parse_version(v: &str) -> Option<(i64, i64)> {
    let (maj_str, rest) = split_digits(v);
    if maj_str.is_empty() || rest.is_empty() {
        return None;
    }
    let maj: i64 = maj_str.parse().ok()?;
    let after_dot = &rest[1..]; // drop the '.'
    let (min_str, _) = split_digits(after_dot);
    if min_str.is_empty() {
        return None;
    }
    let min: i64 = min_str.parse().ok()?;
    Some((maj, min))
}

fn split_digits(s: &str) -> (&str, &str) {
    let end = s.find(|c: char| !c.is_ascii_digit()).unwrap_or(s.len());
    (&s[..end], &s[end..])
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::modules::{builtin_functions, NGLVersion};
    use crate::parser::parse_ngless;

    fn funcs() -> Vec<Function> {
        builtin_functions(NGLVersion::new(1, 3))
    }

    fn is_ok(text: &str) {
        let script = parse_ngless("test", true, text).expect("parse failed");
        if let Err(e) = validate(&funcs(), &[], &script) {
            panic!("Validation should have passed for:\n{text}\ngot: {e}");
        }
    }

    fn is_error(text: &str) {
        let script = parse_ngless("test", true, text).expect("parse failed");
        assert!(
            validate(&funcs(), &[], &script).is_err(),
            "Validation should have errored for:\n{text}"
        );
    }

    #[test]
    fn bad_function_attr_count() {
        is_error("ngless '1.5'\ncount(annotated, features='gene')");
    }

    #[test]
    fn bad_symbol_arg() {
        is_error("ngless '1.4'\ninput = fastq('input.fq.gz')\nwrite(\n    map(input, reference='sacCer3'),\n            ofile='result.sam',\n            format={yolo})\n");
    }

    #[test]
    fn bad_symbol_arg_suggests_closest() {
        // `{sma}` is a near-miss for the legal `{sam}` write format.
        let script = parse_ngless(
            "test",
            true,
            "ngless '1.4'\ninput = fastq('input.fq.gz')\nwrite(\n    map(input, reference='sacCer3'),\n            ofile='result.sam',\n            format={sma})\n",
        )
        .expect("parse failed");
        let msg = validate(&funcs(), &[], &script)
            .expect_err("expected validation error")
            .to_string();
        assert!(
            msg.contains("Did you mean {sam} (closest match)"),
            "got: {msg}"
        );
    }

    #[test]
    fn map_not_assigned() {
        is_error("ngless '1.5'\ninput = fasq('input.fq.gz')\nmap(input,reference='sacCer3')\n");
    }

    #[test]
    fn good_function_attr_map() {
        is_ok("ngless '1.5'\ninput = fastq('input.fq.gz')\nwrite(\n    map(input, reference='sacCer3'),\n            ofile='result.sam',\n            format={sam})\n");
    }

    #[test]
    fn validate_internal_call_empty_mods() {
        // mods = [] => no functions known; every call is "not found".
        let script = parse_ngless(
            "test",
            true,
            "ngless '1.5'\nwrite(select(samfile('f.sam'), keep_if=[{matched}]), ofile=STDOUT)\n",
        )
        .unwrap();
        assert!(validate(&[], &[], &script).is_err());
    }

    #[test]
    fn no_assign_constant() {
        is_error("ngless '1.5'\nCONST = 1\nCONST = 2\n");
    }

    #[test]
    fn assign_variable_ok() {
        is_ok("ngless '1.5'\nnotConst = 1\nnotConst = 2\n");
    }

    // --- IO validation (`validate_io`) ------------------------------------

    /// Create a unique temp FASTQ file and return its path; the caller is responsible for removal.
    fn temp_fastq(tag: &str) -> std::path::PathBuf {
        let p = std::env::temp_dir().join(format!("ngless_io_{tag}_{}.fq", std::process::id()));
        std::fs::write(&p, b"@r\nACGT\n+\nIIII\n").unwrap();
        p
    }

    #[test]
    fn io_missing_input_file_errors() {
        let script = parse_ngless(
            "test",
            true,
            "ngless '1.5'\ninput = fastq('/no/such/path/xyz.fq')\n",
        )
        .unwrap();
        let e = validate_io(&funcs(), &[], &script)
            .expect_err("missing input should error")
            .to_string();
        assert!(e.contains("xyz.fq"), "got: {e}");
    }

    #[test]
    fn io_existing_input_file_ok() {
        let p = temp_fastq("ok");
        let txt = format!("ngless '1.5'\ninput = fastq('{}')\n", p.display());
        let script = parse_ngless("test", true, &txt).unwrap();
        let r = validate_io(&funcs(), &[], &script);
        std::fs::remove_file(&p).ok();
        assert!(r.is_ok(), "got: {:?}", r.err());
    }

    #[test]
    fn io_input_file_via_constant_variable_errors() {
        // A variable that traces to a unique constant string is checked too (`tryConstValue`).
        let script = parse_ngless(
            "test",
            true,
            "ngless '1.5'\nfname = '/no/such/path/traced.fq'\ninput = fastq(fname)\n",
        )
        .unwrap();
        let e = validate_io(&funcs(), &[], &script)
            .expect_err("missing input should error")
            .to_string();
        assert!(e.contains("traced.fq"), "got: {e}");
    }

    #[test]
    fn io_unknown_reference_errors() {
        let p = temp_fastq("ref");
        let ofile = std::env::temp_dir().join("ngless_io_out.sam");
        let txt = format!(
            "ngless '1.5'\ninput = fastq('{}')\nmapped = map(input, reference='no_such_ref')\nwrite(mapped, ofile='{}')\n",
            p.display(),
            ofile.display(),
        );
        let script = parse_ngless("test", true, &txt).unwrap();
        let r = validate_io(&funcs(), &[], &script);
        std::fs::remove_file(&p).ok();
        let e = r.expect_err("unknown reference should error").to_string();
        assert!(
            e.contains("Could not find reference no_such_ref"),
            "got: {e}"
        );
    }

    #[test]
    fn io_builtin_reference_ok() {
        let p = temp_fastq("builtinref");
        let ofile = std::env::temp_dir().join("ngless_io_out2.sam");
        let txt = format!(
            "ngless '1.5'\ninput = fastq('{}')\nmapped = map(input, reference='sacCer3')\nwrite(mapped, ofile='{}')\n",
            p.display(),
            ofile.display(),
        );
        let script = parse_ngless("test", true, &txt).unwrap();
        let r = validate_io(&funcs(), &[], &script);
        std::fs::remove_file(&p).ok();
        assert!(r.is_ok(), "got: {:?}", r.err());
    }

    #[test]
    fn io_missing_output_dir_errors() {
        let txt =
            "ngless '1.5'\ninput = fastq('whatever')\nwrite(input, ofile='/no/such/dir/out.fq')\n";
        let script = parse_ngless("test", true, txt).unwrap();
        let e = validate_io(&funcs(), &[], &script)
            .expect_err("missing output dir should error")
            .to_string();
        assert!(
            e.contains("directory /no/such/dir does not exist"),
            "got: {e}"
        );
    }
}
