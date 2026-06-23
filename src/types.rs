//! Type inference and checking, mirroring `NGLess/Types.hs`.
//!
//! Inference runs top-to-bottom, building a variable type map. Two error classes exist, as in
//! the original: recoverable errors are accumulated (and reported together at the end), while
//! `cannot_continue` aborts immediately. If any error was produced, `checktypes` fails;
//! otherwise every `Lookup` node is annotated with its inferred type.

use std::collections::HashMap;

use crate::ast::*;
use crate::errors::{NgError, NgResult};
use crate::modules::{builtin_functions, builtin_methods, Function, MethodInfo, NGLVersion};

/// Signals an immediate abort of type checking (`cannotContinue`).
struct Abort;
type TR<T> = Result<T, Abort>;

struct TypeChecker {
    line: i64,
    tmap: HashMap<String, NGLType>,
    functions: Vec<Function>,
    methods: Vec<MethodInfo>,
    /// Module constants (name -> type). Empty for the builtin module.
    constants: Vec<(String, NGLType)>,
    errors: Vec<String>,
}

/// Type-check a script against the builtin module for the given language version. On success
/// returns the script with all `Lookup` nodes annotated with their inferred types.
pub fn checktypes(ver: NGLVersion, script: &Script, extra_funcs: &[Function]) -> NgResult<Script> {
    let mut functions = builtin_functions(ver);
    functions.extend_from_slice(extra_funcs);
    let mut tc = TypeChecker {
        line: 0,
        tmap: HashMap::new(),
        functions,
        methods: builtin_methods(),
        constants: Vec::new(),
        errors: Vec::new(),
    };
    let _ = tc.infer_script(&script.body);
    if !tc.errors.is_empty() {
        return Err(NgError::script(tc.errors.join("\n")));
    }
    let body = tc.add_types(&script.body)?;
    Ok(Script {
        header: script.header.clone(),
        body,
    })
}

impl TypeChecker {
    fn error_in_line(&mut self, e: impl Into<String>) {
        let line = self.line;
        self.errors.push(format!(
            "Error in type-checking (line {line}): {}",
            e.into()
        ));
    }

    fn cannot_continue(&mut self) -> Abort {
        self.errors.push("Cannot continue typechecking.".into());
        Abort
    }

    // --- inference -----------------------------------------------------------

    fn infer_script(&mut self, exprs: &[(usize, Expression)]) -> TR<()> {
        for (lno, e) in exprs {
            self.line = *lno as i64;
            self.infer(e)?;
        }
        Ok(())
    }

    fn infer(&mut self, e: &Expression) -> TR<()> {
        match e {
            Expression::Sequence(es) => {
                for e in es {
                    self.infer(e)?;
                }
                Ok(())
            }
            Expression::Assignment(Variable(v), expr) => {
                let ltype = self.env_lookup(None, v)?;
                let mrtype = self.ngl_type_of(expr)?;
                self.check_assignment(ltype, mrtype.clone());
                match mrtype {
                    None => {
                        self.error_in_line("Cannot infer type for right-hand of assignment");
                        Ok(())
                    }
                    Some(rtype) => {
                        self.env_insert(v, rtype);
                        Ok(())
                    }
                }
            }
            Expression::Condition(c, te, fe) => {
                self.check_bool(c)?;
                self.infer(te)?;
                self.infer(fe)
            }
            _ => self.ngl_type_of(e).map(|_| ()),
        }
    }

    fn infer_block(&mut self, f: &FuncName, block: &Option<Block>) -> TR<()> {
        let block = match block {
            None => return Ok(()),
            Some(b) => b,
        };
        let btype = match f.0.as_str() {
            "preprocess" => NGLType::Read,
            "select" => NGLType::MappedRead,
            other => {
                self.error_in_line(format!("Function '{other}' does not accept blocks"));
                return Err(self.cannot_continue());
            }
        };
        self.env_insert(&block.variable.0, btype);
        self.infer(&block.body)
    }

    // --- environment ---------------------------------------------------------

    fn env_lookup(&mut self, mt: Option<NGLType>, v: &str) -> TR<Option<NGLType>> {
        let found = self.env_lookup_inner(v)?;
        match mt {
            None => Ok(found),
            Some(t) => match found {
                None => Ok(Some(t)),
                Some(t2) => {
                    if t == t2 {
                        Ok(Some(t))
                    } else {
                        self.error_in_line(format!(
                            "Incompatible types detected for variable '{v}': previously assigned to type {t:?}, now being detected as {t2:?}"
                        ));
                        Ok(Some(t))
                    }
                }
            },
        }
    }

    fn env_lookup_inner(&mut self, v: &str) -> TR<Option<NGLType>> {
        if let Some(t) = self.constant_lookup(v)? {
            return Ok(Some(t));
        }
        Ok(self.tmap.get(v).cloned())
    }

    fn constant_lookup(&mut self, v: &str) -> TR<Option<NGLType>> {
        let matches: Vec<&(String, NGLType)> =
            self.constants.iter().filter(|(n, _)| n == v).collect();
        match matches.as_slice() {
            [] => Ok(None),
            [(_, t)] => Ok(Some(t.clone())),
            _ => {
                self.error_in_line(format!("Multiple matches for constant: {v}"));
                Err(self.cannot_continue())
            }
        }
    }

    fn env_insert(&mut self, v: &str, t: NGLType) {
        self.tmap.insert(v.to_string(), t);
    }

    fn check_assignment(&mut self, a: Option<NGLType>, b: Option<NGLType>) {
        if b == Some(NGLType::Void) {
            self.error_in_line("Assigning void value to variable");
            return;
        }
        if a.is_none() {
            return;
        }
        if a != b {
            self.error_in_line(format!(
                "Assigning type {b:?} to a variable that has type {a:?}"
            ));
        }
    }

    // --- the core: nglTypeOf -------------------------------------------------

    fn ngl_type_of(&mut self, e: &Expression) -> TR<Option<NGLType>> {
        use NGLType::*;
        match e {
            Expression::FunctionCall(f, arg, args, b) => {
                self.infer_block(f, b)?;
                self.check_func_kwargs(f, args)?;
                self.check_func_unnamed(f, arg)
            }
            Expression::MethodCall(m, self_e, arg, args) => {
                self.check_method_call(m, self_e, arg.as_deref(), args)
            }
            Expression::Lookup(mt, Variable(v)) => self.env_lookup(mt.clone(), v),
            Expression::BuiltinConstant(Variable(v)) => Ok(type_of_constant(v)),
            Expression::ConstStr(_) => Ok(Some(String)),
            Expression::ConstInt(_) => Ok(Some(Integer)),
            Expression::ConstDouble(_) => Ok(Some(Double)),
            Expression::ConstBool(_) => Ok(Some(Bool)),
            Expression::ConstSymbol(_) => Ok(Some(Symbol)),
            Expression::ListExpression(_) => match self.check_indexable(e)? {
                None => Ok(None),
                Some(t) => Ok(Some(List(Box::new(t)))),
            },
            Expression::Continue => Ok(None),
            Expression::Discard => Ok(None),
            Expression::Assignment(_, expr) => self.ngl_type_of(expr),
            Expression::UnaryOp(uop, expr) => self.check_uop(*uop, expr),
            Expression::BinaryOp(bop, a, b) => self.check_bop(*bop, a, b),
            Expression::IndexExpression(expr, index) => self.check_index(expr, index),
            // These should never reach nglTypeOf.
            Expression::Condition(..) | Expression::Sequence(_) => Ok(None),
        }
    }

    fn check_uop(&mut self, uop: UOp, e: &Expression) -> TR<Option<NGLType>> {
        match uop {
            UOp::Len => {
                self.check_indexable(e)?;
                Ok(Some(NGLType::Integer))
            }
            UOp::Minus => self.check_num(e),
            UOp::Not => self.check_bool(e),
        }
    }

    fn check_bop(&mut self, bop: BOp, a: &Expression, b: &Expression) -> TR<Option<NGLType>> {
        use NGLType::*;
        match bop {
            BOp::Add => {
                let t = match self.soft_check_pair(&Integer, a, b)? {
                    Some(t) => Some(t),
                    None => self.soft_check_pair(&String, a, b)?,
                };
                if t.is_none() {
                    self.error_in_line(
                        "Addition operator (+) must be applied to a pair of strings or integers",
                    );
                }
                Ok(t)
            }
            BOp::Mul => {
                self.check_num(a)?;
                self.check_num(b)
            }
            BOp::GT | BOp::GTE | BOp::LT | BOp::LTE => {
                self.check_num(a)?;
                self.check_num(b)?;
                Ok(Some(Bool))
            }
            BOp::PathAppend => {
                self.soft_check(&String, a)?;
                self.soft_check(&String, b)?;
                Ok(Some(String))
            }
            BOp::EQ | BOp::NEQ => {
                let t = match self.soft_check_pair(&Integer, a, b)? {
                    Some(t) => Some(t),
                    None => match self.soft_check_pair(&Double, a, b)? {
                        Some(t) => Some(t),
                        None => self.soft_check_pair(&String, a, b)?,
                    },
                };
                if t.is_none() {
                    self.error_in_line("Comparison operators (== or !=) must be applied to a pair of strings or numbers");
                }
                Ok(Some(Bool))
            }
        }
    }

    fn soft_check(&mut self, expected: &NGLType, expr: &Expression) -> TR<Option<NGLType>> {
        let t = self.ngl_type_of(expr)?;
        Ok(if t.as_ref() != Some(expected) {
            None
        } else {
            t
        })
    }

    fn soft_check_pair(
        &mut self,
        t: &NGLType,
        a: &Expression,
        b: &Expression,
    ) -> TR<Option<NGLType>> {
        let ta = self.soft_check(t, a)?;
        let tb = self.soft_check(t, b)?;
        Ok(if ta == tb && tb.as_ref() == Some(t) {
            ta
        } else {
            None
        })
    }

    fn check_bool(&mut self, e: &Expression) -> TR<Option<NGLType>> {
        if let Expression::ConstBool(_) = e {
            return Ok(Some(NGLType::Bool));
        }
        let t = self.ngl_type_of(e)?;
        if t.as_ref() != Some(&NGLType::Bool) {
            self.error_in_line(format!(
                "Expected boolean expression, got {t:?} for expression {e:?}"
            ));
        }
        Ok(Some(NGLType::Bool))
    }

    fn check_integer(&mut self, e: &Expression) -> TR<Option<NGLType>> {
        if let Expression::ConstInt(_) = e {
            return Ok(Some(NGLType::Integer));
        }
        let t = self.ngl_type_of(e)?;
        if t.as_ref() != Some(&NGLType::Integer) {
            self.error_in_line(format!(
                "Expected integer expression, got {t:?} for expression {e:?}"
            ));
        }
        Ok(Some(NGLType::Integer))
    }

    fn check_num(&mut self, e: &Expression) -> TR<Option<NGLType>> {
        let t = self.ngl_type_of(e)?;
        if matches!(t, Some(NGLType::Integer) | Some(NGLType::Double) | None) {
            Ok(t)
        } else {
            self.error_in_line(format!(
                "Expected numeric expression, got {t:?} for expression {e:?}"
            ));
            Ok(Some(NGLType::Double)) // a decent guess most of the time
        }
    }

    fn check_index(&mut self, expr: &Expression, index: &Index) -> TR<Option<NGLType>> {
        match index {
            Index::One(e) => {
                self.check_integer(e)?;
            }
            Index::Two(a, b) => {
                if let Some(a) = a {
                    self.check_integer(a)?;
                }
                if let Some(b) = b {
                    self.check_integer(b)?;
                }
            }
        }
        self.check_indexable(expr)
    }

    fn check_indexable(&mut self, e: &Expression) -> TR<Option<NGLType>> {
        if let Expression::ListExpression(es) = e {
            if es.is_empty() {
                return Ok(Some(NGLType::Void));
            }
            let mut ts = Vec::new();
            for e in es {
                if let Some(t) = self.ngl_type_of(e)? {
                    ts.push(t);
                }
            }
            return match ts.split_first() {
                None => Ok(None),
                Some((t, rest)) => {
                    if !rest.iter().all(|r| r == t) {
                        self.error_in_line("List of mixed type");
                    }
                    Ok(Some(t.clone()))
                }
            };
        }
        let t = self.ngl_type_of(e)?;
        match t {
            Some(NGLType::List(btype)) => Ok(Some(*btype)),
            Some(NGLType::Read) => Ok(Some(NGLType::Read)),
            other => {
                self.error_in_line(format!("List expected. Type {other:?} provided."));
                Ok(Some(NGLType::Void))
            }
        }
    }

    // --- functions & methods -------------------------------------------------

    fn func_info(&mut self, fn_: &FuncName) -> TR<Function> {
        let matched: Vec<Function> = self
            .functions
            .iter()
            .filter(|f| &f.name == fn_)
            .cloned()
            .collect();
        match matched.len() {
            1 => Ok(matched.into_iter().next().unwrap()),
            0 => {
                self.error_in_line(format!("Unknown function '{}'.", fn_.0));
                Err(self.cannot_continue())
            }
            _ => {
                self.error_in_line(format!("Too many matches for function '{}'", fn_.0));
                Err(self.cannot_continue())
            }
        }
    }

    fn find_method_info(&mut self, m: &MethodName, self_e: &Expression) -> TR<MethodInfo> {
        let by_name: Vec<MethodInfo> = self
            .methods
            .iter()
            .filter(|mi| &mi.name == m)
            .cloned()
            .collect();
        match by_name.len() {
            1 => Ok(by_name.into_iter().next().unwrap()),
            0 => {
                self.error_in_line(format!("Cannot find method `{}`.", m.0));
                Err(self.cannot_continue())
            }
            _ => match self.ngl_type_of(self_e)? {
                None => {
                    self.error_in_line(format!(
                        "Cannot disambiguate method `{}` as it is called on an expression of unknown type ({self_e:?}).",
                        m.0
                    ));
                    Err(self.cannot_continue())
                }
                Some(self_type) => {
                    let by_type: Vec<MethodInfo> = by_name
                        .into_iter()
                        .filter(|mi| mi.self_type == self_type)
                        .collect();
                    if by_type.len() == 1 {
                        Ok(by_type.into_iter().next().unwrap())
                    } else {
                        self.error_in_line(format!(
                            "Cannot disambiguate method `{}` as it was called on an unsupported type",
                            m.0
                        ));
                        Err(self.cannot_continue())
                    }
                }
            },
        }
    }

    fn check_func_unnamed(&mut self, f: &FuncName, arg: &Expression) -> TR<Option<NGLType>> {
        let targ = self.ngl_type_of(arg)?;
        let fi = self.func_info(f)?;
        match &fi.arg_type {
            Some(etype) => match targ {
                Some(NGLType::List(t)) if fi.allows_auto_comprehension => {
                    self.check_func_type(f, etype, &t);
                    Ok(Some(NGLType::List(Box::new(fi.ret_type.clone()))))
                }
                Some(t) => {
                    self.check_func_type(f, etype, &t);
                    Ok(Some(fi.ret_type.clone()))
                }
                None => {
                    self.error_in_line(format!(
                        "While checking types for function {}.\n\tCould not infer type of argument (saw :{arg:?})",
                        f.0
                    ));
                    Err(self.cannot_continue())
                }
            },
            None => Ok(None),
        }
    }

    fn check_func_type(&mut self, f: &FuncName, etype: &NGLType, actual: &NGLType) {
        use NGLType::*;
        match (etype, actual) {
            (Any, Void) => self.error_in_line(format!(
                "Function '{}' can take any type, but the input is of illegal type Void.",
                f.0
            )),
            (Any, _) => {}
            (String, SequenceSet) => {}
            (Union(ts), t) => {
                if !ts.iter().any(|et| trivial_convertable(et, t)) {
                    self.error_in_line(format!(
                        "Function '{}' can take any of {ts:?} but the input is of illegal type {t:?}.",
                        f.0
                    ));
                }
            }
            (t, t2) => {
                if !trivial_convertable(t2, t) {
                    self.error_in_line(format!(
                        "Bad type in function call (function '{}' expects {t:?} got {t2:?}).",
                        f.0
                    ));
                }
            }
        }
    }

    fn check_func_kwargs(&mut self, f: &FuncName, args: &[(Variable, Expression)]) -> TR<()> {
        let fi = self.func_info(f)?;
        let ferr = format!("function '{}'", f.0);
        for a in args {
            self.check_one_arg(&ferr, &fi.kwargs, a)?;
        }
        Ok(())
    }

    fn check_one_arg(
        &mut self,
        ferr: &str,
        arginfo: &[crate::modules::ArgInformation],
        (Variable(v), e): &(Variable, Expression),
    ) -> TR<()> {
        let e_type = self.ngl_type_of(e)?;
        let ainfo = arginfo.iter().find(|ai| &ai.name == v);
        match (ainfo, e_type) {
            (None, _) => {
                let mut msg = format!("Bad argument '{v}' for {ferr}.\nThis function takes the following arguments:\n");
                for ai in arginfo {
                    msg.push('\t');
                    msg.push_str(&ai.name);
                    msg.push('\n');
                }
                self.error_in_line(msg);
            }
            (_, None) => {}
            (Some(ai), Some(t2)) => {
                if !trivial_convertable(&t2, &ai.atype) {
                    self.error_in_line(format!(
                        "Bad argument type in {ferr}, argument {v:?} (expected {:?} got {t2:?}).",
                        ai.atype
                    ));
                }
            }
        }
        Ok(())
    }

    fn require_type(&mut self, def_t: NGLType, e: &Expression) -> TR<NGLType> {
        match self.ngl_type_of(e)? {
            None => {
                self.error_in_line(format!(
                    "Could not infer required type of expression ({e:?})"
                ));
                Ok(def_t)
            }
            Some(t) => Ok(t),
        }
    }

    fn check_method_call(
        &mut self,
        m: &MethodName,
        self_e: &Expression,
        arg: Option<&Expression>,
        args: &[(Variable, Expression)],
    ) -> TR<Option<NGLType>> {
        let minfo = self.find_method_info(m, self_e)?;
        let req_self = minfo.self_type.clone();
        let req_arg = minfo.arg_type.clone();
        let stype = self.require_type(req_self.clone(), self_e)?;
        if stype != req_self {
            self.error_in_line(format!(
                "Wrong type for method {}. This method is defined for type {req_self:?}, but expression ({self_e:?}) has type {stype:?}",
                m.0
            ));
        }
        let actual_type = match arg {
            Some(a) => self.ngl_type_of(a)?,
            None => None,
        };
        match (actual_type, req_arg) {
            (None, _) => {}
            (Some(_), None) => self.error_in_line(format!(
                "Method {} does not take any unnamed argument (saw {arg:?})",
                m.0
            )),
            (Some(t), Some(t2)) => {
                if t != t2 {
                    self.error_in_line(format!("Method {} expects type {t2:?} got {t:?}", m.0));
                }
            }
        }
        let ferr = format!("method '{}'", m.0);
        for a in args {
            self.check_one_arg(&ferr, &minfo.kwargs, a)?;
        }
        for ai in minfo.kwargs.iter().filter(|ai| ai.required) {
            let n = args.iter().filter(|(Variable(v), _)| v == &ai.name).count();
            if n == 0 {
                self.error_in_line(format!(
                    "Required argument {} is missing in method call {}.",
                    ai.name, m.0
                ));
            }
        }
        Ok(Some(minfo.return_type))
    }

    // --- annotate Lookups ----------------------------------------------------

    fn add_types(&self, exprs: &[(usize, Expression)]) -> NgResult<Vec<(usize, Expression)>> {
        exprs
            .iter()
            .map(|(lno, e)| Ok((*lno, self.add_types_expr(e)?)))
            .collect()
    }

    fn add_types_expr(&self, e: &Expression) -> NgResult<Expression> {
        use Expression::*;
        Ok(match e {
            Lookup(None, v @ Variable(n)) => match self.tmap.get(n) {
                Some(t) => Lookup(Some(t.clone()), v.clone()),
                None => {
                    return Err(NgError::script(format!(
                        "Could not assign type to variable '{n}'."
                    )))
                }
            },
            Lookup(Some(_), _) => e.clone(),
            ListExpression(es) => ListExpression(self.add_types_vec(es)?),
            UnaryOp(op, a) => UnaryOp(*op, Box::new(self.add_types_expr(a)?)),
            BinaryOp(op, a, b) => BinaryOp(
                *op,
                Box::new(self.add_types_expr(a)?),
                Box::new(self.add_types_expr(b)?),
            ),
            Condition(c, t, f) => Condition(
                Box::new(self.add_types_expr(c)?),
                Box::new(self.add_types_expr(t)?),
                Box::new(self.add_types_expr(f)?),
            ),
            IndexExpression(a, ix) => {
                IndexExpression(Box::new(self.add_types_expr(a)?), self.add_types_index(ix)?)
            }
            Assignment(v, a) => Assignment(v.clone(), Box::new(self.add_types_expr(a)?)),
            FunctionCall(fname, a, args, block) => {
                let a = Box::new(self.add_types_expr(a)?);
                let args = self.add_types_args(args)?;
                let block = match block {
                    Some(b) => Some(Block {
                        variable: b.variable.clone(),
                        body: Box::new(self.add_types_expr(&b.body)?),
                    }),
                    None => None,
                };
                FunctionCall(fname.clone(), a, args, block)
            }
            MethodCall(mname, self_e, arg, args) => {
                let self_e = Box::new(self.add_types_expr(self_e)?);
                let arg = match arg {
                    Some(a) => Some(Box::new(self.add_types_expr(a)?)),
                    None => None,
                };
                let args = self.add_types_args(args)?;
                MethodCall(mname.clone(), self_e, arg, args)
            }
            Sequence(es) => Sequence(self.add_types_vec(es)?),
            other => other.clone(),
        })
    }

    fn add_types_vec(&self, es: &[Expression]) -> NgResult<Vec<Expression>> {
        es.iter().map(|e| self.add_types_expr(e)).collect()
    }

    fn add_types_args(
        &self,
        args: &[(Variable, Expression)],
    ) -> NgResult<Vec<(Variable, Expression)>> {
        args.iter()
            .map(|(v, e)| Ok((v.clone(), self.add_types_expr(e)?)))
            .collect()
    }

    fn add_types_index(&self, ix: &Index) -> NgResult<Index> {
        Ok(match ix {
            Index::One(e) => Index::One(Box::new(self.add_types_expr(e)?)),
            Index::Two(a, b) => Index::Two(
                match a {
                    Some(a) => Some(Box::new(self.add_types_expr(a)?)),
                    None => None,
                },
                match b {
                    Some(b) => Some(Box::new(self.add_types_expr(b)?)),
                    None => None,
                },
            ),
        })
    }
}

fn type_of_constant(v: &str) -> Option<NGLType> {
    match v {
        "STDIN" | "STDOUT" => Some(NGLType::String),
        _ => None,
    }
}

fn trivial_convertable(a: &NGLType, b: &NGLType) -> bool {
    matches!((a, b), (NGLType::SequenceSet, NGLType::String)) || a == b
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_ngless;

    fn mods() -> NGLVersion {
        NGLVersion::new(1, 3)
    }

    fn is_ok_text(text: &str) {
        let script = parse_ngless("test", true, text).expect("parse failed");
        if let Err(e) = checktypes(mods(), &script, &[]) {
            panic!("Type error on good code: {e} for script:\n{text}");
        }
    }

    fn is_error_text(text: &str) {
        let script = parse_ngless("test", true, text).expect("parse failed");
        assert!(
            checktypes(mods(), &script, &[]).is_err(),
            "expected type error for script:\n{text}"
        );
    }

    fn func_call(name: &str, arg: Expression, args: Vec<(Variable, Expression)>) -> Script {
        Script {
            header: None,
            body: vec![(
                0,
                Expression::FunctionCall(FuncName(name.into()), Box::new(arg), args, None),
            )],
        }
    }

    #[test]
    fn bad_type_fastq() {
        assert!(checktypes(
            mods(),
            &func_call("fastq", Expression::ConstInt(3), vec![]),
            &[]
        )
        .is_err());
    }

    #[test]
    fn good_type_fastq() {
        assert!(checktypes(
            mods(),
            &func_call("fastq", Expression::ConstStr("fastq.fq".into()), vec![]),
            &[]
        )
        .is_ok());
    }

    #[test]
    fn invalid_func_fastq_bad_kwarg() {
        let s = func_call(
            "fastq",
            Expression::ConstStr("fastq.fq".into()),
            vec![(Variable("fname".into()), Expression::ConstInt(10))],
        );
        assert!(checktypes(mods(), &s, &[]).is_err());
    }

    #[test]
    fn type_complete() {
        is_ok_text(
            "ngless '0.0'\nreads = fastq('input1.fq')\nreads = unique(reads,max_copies=2)\npreprocess(reads) using |read|:\n    read = read[5:]\n    read = substrim(read, min_quality=24)\n    if len(read) < 30:\n        discard\n",
        );
    }

    #[test]
    fn indent_empty_line() {
        is_ok_text(
            "ngless '0.0'\nreads = fastq('input1.fq')\npreprocess(reads) using |read|:\n    read = read[5:]\n    \n    if len(read) < 24:\n        discard\n",
        );
    }

    #[test]
    fn valid_unique_mc() {
        is_ok_text("ngless '0.0'\nx = fastq('fq')\nunique(x, max_copies=10)\n");
    }

    #[test]
    fn invalid_unique_mc() {
        is_error_text("ngless '0.0'\nx = fastq('fq')\nunique(x, max_copies='test')\n");
    }

    #[test]
    fn valid_substrim_mq() {
        is_ok_text(
            "ngless '0.0'\nx = fastq('fq')\npreprocess(x) using |read|:\n    read = read[5:]\n    read = substrim(read, min_quality=2)\n",
        );
    }

    #[test]
    fn invalid_substrim_mq() {
        is_error_text(
            "ngless '0.0'\nx = fastq('fq')\npreprocess(x) using |read|:\n    read = read[5:]\n    read = substrim(read, min_quality='2')\n",
        );
    }

    #[test]
    fn invalid_map_ref() {
        is_error_text("ngless '0.0'\nx = fastq('fq')\nmap(x, reference=10)\n");
    }

    #[test]
    fn valid_map_ref() {
        is_ok_text("ngless '0.0'\nx = fastq('fq')\nmap(x, reference='xpto')\n");
    }

    #[test]
    fn valid_count_gff() {
        is_ok_text(
            "ngless '0.0'\nx = fastq('fq')\ny = map(x, reference='xpto')\ncount(y, gff_file='xpto')",
        );
    }

    #[test]
    fn invalid_count_gff() {
        is_error_text(
            "ngless '0.0'\nx = fastq('fq')\ny = map(x, reference='xpto')\ncount(y, gff_file={xpto})",
        );
    }

    #[test]
    fn valid_count_mode() {
        is_ok_text(
            "ngless '0.0'\nx = fastq('fq')\ny = map(x, reference='xpto')\ncount(y, mode={union})",
        );
    }

    #[test]
    fn invalid_count_mode() {
        is_error_text(
            "ngless '0.0'\nx = fastq('fq')\ny = map(x, reference='xpto')\ncount(y, mode='union')",
        );
    }

    #[test]
    fn valid_count_features() {
        is_ok_text(
            "ngless '0.0'\nx = fastq('fq')\ny = map(x, reference='xpto')\ncount(y, features=['gene'])",
        );
    }

    #[test]
    fn invalid_count_features() {
        is_error_text(
            "ngless '0.0'\nx = fastq('fq')\ny = map(x, reference='xpto')\ncount(y, features={gene})",
        );
    }

    #[test]
    fn valid_count_min() {
        is_ok_text(
            "ngless '0.0'\nx = fastq('fq')\ny = map(x, reference='xpto')\nz = count(y, features=['gene'], min=10)",
        );
    }

    #[test]
    fn valid_write_format() {
        is_ok_text(
            "ngless '0.0'\nx = fastq('fq')\ny = map(x, reference='xpto')\nz = count(y, features=['gene'])\nwrite(z, format={tsv})",
        );
    }

    #[test]
    fn invalid_write_format() {
        is_error_text(
            "ngless '0.0'\nx = fastq('fq')\ny = map(x, reference='xpto')\nz = count(y, features=[{gene}])\nwrite(count(z), format='tsv')",
        );
    }

    #[test]
    fn mixed_addition() {
        is_error_text("ngless '0.0'\nnglessIsNotJS = 1 + '2'\n");
    }
}
