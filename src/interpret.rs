//! Interpreter for the currently-supported subset of NGLess, mirroring `NGLess/Interpret.hs`.
//!
//! Implemented: pure expression evaluation (constants, variables, operators, indexing, lists),
//! conditionals and assignment, and the simple builtins `print`, `println`, `read_int`,
//! `read_double`, `__assert`, plus the `to_string` method. Everything that needs the
//! FASTQ/SAM/counts subsystems (`fastq`, `map`, `count`, `write`, blocks, ...) returns a clear
//! "not implemented yet" error.

use std::collections::HashMap;
use std::io::Write;

use crate::ast::*;
use crate::errors::{NgError, NgResult};
use crate::values::{eval_binary, eval_index, eval_unary, show_double, NGLessObject};

/// Interpret a script body (already type-checked and validated).
pub fn interpret(body: &[(usize, Expression)]) -> NgResult<()> {
    let mut interp = Interpreter {
        env: HashMap::new(),
    };
    for (_lno, e) in body {
        interp.interpret_top(e)?;
    }
    Ok(())
}

struct Interpreter {
    env: HashMap<String, NGLessObject>,
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

    /// Evaluate a pure expression (no function calls, read-only environment).
    fn interpret_expr(&self, e: &Expression) -> NgResult<NGLessObject> {
        match e {
            Expression::Lookup(_, Variable(v)) => self.env.get(v).cloned().ok_or_else(|| {
                NgError::should_not_occur(format!("Variable lookup error. Variable: {v}"))
            }),
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
                let v = self.interpret_expr(v)?;
                eval_unary(*op, &v)
            }
            Expression::BinaryOp(op, a, b) => {
                let a = self.interpret_expr(a)?;
                let b = self.interpret_expr(b)?;
                eval_binary(*op, &a, &b)
            }
            Expression::IndexExpression(expr, ix) => {
                let v = self.interpret_expr(expr)?;
                let indices = self.interpret_index(ix)?;
                eval_index(&v, &indices)
            }
            Expression::ListExpression(es) => {
                let mut out = Vec::with_capacity(es.len());
                for e in es {
                    out.push(self.interpret_expr(e)?);
                }
                Ok(NGLessObject::List(out))
            }
            Expression::MethodCall(met, self_e, arg, args) => {
                let self_v = self.interpret_expr(self_e)?;
                let arg_v = match arg {
                    Some(a) => Some(self.interpret_expr(a)?),
                    None => None,
                };
                let mut argvs = Vec::new();
                for (Variable(v), e) in args {
                    argvs.push((v.clone(), self.interpret_expr(e)?));
                }
                execute_method(met, &self_v, arg_v.as_ref(), &argvs)
            }
            other => Err(NgError::should_not_occur(format!(
                "Expected an expression, received {other:?} (in interpret_expr)"
            ))),
        }
    }

    fn interpret_index(&self, ix: &Index) -> NgResult<Vec<Option<NGLessObject>>> {
        match ix {
            Index::One(a) => Ok(vec![Some(self.interpret_expr(a)?)]),
            Index::Two(a, b) => {
                let a = match a {
                    Some(a) => Some(self.interpret_expr(a)?),
                    None => None,
                };
                let b = match b {
                    Some(b) => Some(self.interpret_expr(b)?),
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
        execute_function(f, &expr_v, &argvs)
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
        .map_err(|e| NgError::new(crate::errors::NgErrorType::SystemError, e.to_string()))?;
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
                        crate::errors::NgErrorType::DataError,
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
                    crate::errors::NgErrorType::DataError,
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
        (other, _) => Err(NgError::script(format!(
            "Method `{other}` is not implemented in this build yet."
        ))),
    }
}

fn lookup_arg<'a>(args: &'a [(String, NGLessObject)], name: &str) -> Option<&'a NGLessObject> {
    args.iter().find(|(k, _)| k == name).map(|(_, v)| v)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_ngless;

    /// Interpret a (header-carrying) script, returning Ok/Err. stdout side effects are not
    /// captured here; correctness of output is checked end-to-end via the binary.
    fn run(text: &str) -> NgResult<()> {
        let script = parse_ngless("test", true, text).expect("parse failed");
        interpret(&script.body)
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
        assert!(run("ngless '1.5'\nx = fastq('foo.fq')\n").is_err());
    }
}
