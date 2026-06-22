//! Parser for the NGLess language.
//!
//! Mirrors `NGLess/Parse.hs`. The original is a Parsec parser that uses `try` (backtracking)
//! extensively and ordered choice (`<|>`). This is a hand-written recursive-descent parser
//! over the token stream that uses full backtracking with ordered choice; for any input that
//! parses successfully it produces the same AST. Parse-error *messages* are not (yet)
//! byte-identical to the Haskell ones.

use crate::ast::*;
use crate::errors::{NgError, NgResult};
use crate::tokens::{tokenize, PosToken, Token};

/// Remove spaces that do not follow newlines, and any spaces/newlines between brackets
/// (round or square). Mirrors `_cleanupindents` in Parse.hs.
pub fn cleanup_indents(toks: &[PosToken]) -> Vec<PosToken> {
    fn is_open(o: char) -> bool {
        o == '(' || o == '['
    }
    fn close_of(o: char) -> char {
        match o {
            '[' => ']',
            '(' => ')',
            _ => unreachable!("we should not close anything but [ & ("),
        }
    }
    let is_newline = |t: &Token| matches!(t, Token::NewLine);
    let is_indent = |t: &Token| matches!(t, Token::Indent(_));

    let mut out = Vec::with_capacity(toks.len());
    let mut stack: Vec<char> = Vec::new();
    let mut i = 0;
    while i < toks.len() {
        let t = &toks[i];
        // clause 1: an opening operator (regardless of stack)
        if let Token::Operator(o) = t.tok {
            if is_open(o) {
                out.push(t.clone());
                stack.push(close_of(o));
                i += 1;
                continue;
            }
        }
        // clause 2: a matching closing operator
        if let Some(&c) = stack.last() {
            if let Token::Operator(cp) = t.tok {
                if cp == c {
                    out.push(t.clone());
                    stack.pop();
                    i += 1;
                    continue;
                }
            }
        }
        // clauses 3 & 4: inside brackets, drop newlines and indents
        if !stack.is_empty() && (is_newline(&t.tok) || is_indent(&t.tok)) {
            i += 1;
            continue;
        }
        if stack.is_empty() {
            // clause 5: NewLine Indent NewLine -> drop the NewLine and Indent, keep from 2nd NL
            if is_newline(&t.tok)
                && i + 2 < toks.len()
                && is_indent(&toks[i + 1].tok)
                && is_newline(&toks[i + 2].tok)
            {
                i += 2;
                continue;
            }
            // clause 6: NewLine Indent <other> -> keep both
            if is_newline(&t.tok) && i + 1 < toks.len() && is_indent(&toks[i + 1].tok) {
                out.push(toks[i].clone());
                out.push(toks[i + 1].clone());
                i += 2;
                continue;
            }
            // clause 7: a leading indent (not after newline) -> drop
            if is_indent(&t.tok) {
                i += 1;
                continue;
            }
        }
        // default: keep
        out.push(t.clone());
        i += 1;
    }
    out
}

/// Parse a full script. `req_version` mirrors the `reqversion` argument: whether the
/// `ngless "x.y"` header is mandatory.
pub fn parse_ngless(fname: &str, req_version: bool, input: &str) -> NgResult<Script> {
    let toks = tokenize(fname, input)?;
    let cleaned = cleanup_indents(&toks);
    let mut p = Parser::new(cleaned);
    let header = if req_version {
        Some(
            p.opt(|p| p.ngless_header())
                .ok_or_else(|| NgError::script("expected ngless version declaration"))?,
        )
    } else {
        p.opt(|p| p.ngless_header())
    };
    while p.eol().is_some() {}
    let body = p.nglbody()?;
    if !p.at_end() {
        return Err(NgError::script(format!(
            "Parsing error on '{}': unexpected token {:?}",
            fname,
            p.peek().map(|t| &t.tok)
        )));
    }
    Ok(Script { header, body })
}

struct Parser {
    toks: Vec<PosToken>,
    pos: usize,
}

impl Parser {
    fn new(toks: Vec<PosToken>) -> Self {
        Parser { toks, pos: 0 }
    }

    fn at_end(&self) -> bool {
        self.pos >= self.toks.len()
    }

    fn peek(&self) -> Option<&PosToken> {
        self.toks.get(self.pos)
    }

    fn cur_line(&self) -> usize {
        self.peek().map(|t| t.line).unwrap_or(0)
    }

    /// Run `f`; if it returns `None`, restore the position (full backtracking, like `try`).
    fn opt<T>(&mut self, f: impl FnOnce(&mut Self) -> Option<T>) -> Option<T> {
        let save = self.pos;
        match f(self) {
            Some(x) => Some(x),
            None => {
                self.pos = save;
                None
            }
        }
    }

    // --- token-level parsers -------------------------------------------------

    fn operator(&mut self, c: char) -> Option<()> {
        match self.peek() {
            Some(PosToken {
                tok: Token::Operator(o),
                ..
            }) if *o == c => {
                self.pos += 1;
                Some(())
            }
            _ => None,
        }
    }

    fn word(&mut self) -> Option<String> {
        match self.peek() {
            Some(PosToken {
                tok: Token::Word(w),
                ..
            }) => {
                let w = w.clone();
                self.pos += 1;
                Some(w)
            }
            _ => None,
        }
    }

    fn match_word(&mut self, w: &str) -> Option<()> {
        match self.peek() {
            Some(PosToken {
                tok: Token::Word(w2),
                ..
            }) if w2 == w => {
                self.pos += 1;
                Some(())
            }
            _ => None,
        }
    }

    fn reserved(&mut self, r: &str) -> Option<()> {
        match self.peek() {
            Some(PosToken {
                tok: Token::Reserved(r2),
                ..
            }) if r2 == r => {
                self.pos += 1;
                Some(())
            }
            _ => None,
        }
    }

    fn eol(&mut self) -> Option<()> {
        match self.peek() {
            Some(PosToken {
                tok: Token::NewLine,
                ..
            }) => {
                self.pos += 1;
                Some(())
            }
            _ => None,
        }
    }

    fn indentation(&mut self) -> Option<usize> {
        match self.peek() {
            Some(PosToken {
                tok: Token::Indent(i),
                ..
            }) => {
                let i = *i;
                self.pos += 1;
                Some(i)
            }
            _ => None,
        }
    }

    fn binop(&mut self) -> Option<BOp> {
        match self.peek() {
            Some(PosToken {
                tok: Token::Bop(b), ..
            }) => {
                let b = *b;
                self.pos += 1;
                Some(b)
            }
            _ => None,
        }
    }

    fn rawexpr(&mut self) -> Option<Expression> {
        match self.peek() {
            Some(PosToken {
                tok: Token::Expr(e),
                ..
            }) => {
                let e = e.clone();
                self.pos += 1;
                Some(e)
            }
            _ => None,
        }
    }

    fn string_const(&mut self) -> Option<String> {
        match self.peek() {
            Some(PosToken {
                tok: Token::Expr(Expression::ConstStr(s)),
                ..
            }) => {
                let s = s.clone();
                self.pos += 1;
                Some(s)
            }
            _ => None,
        }
    }

    fn variable(&mut self) -> Option<Variable> {
        self.word().map(Variable)
    }

    // --- grammar -------------------------------------------------------------

    fn nglbody(&mut self) -> NgResult<Vec<(usize, Expression)>> {
        let mut out = Vec::new();
        if self.at_end() {
            return Ok(out);
        }
        loop {
            let line = self.cur_line();
            match self.opt(|p| p.expression()) {
                Some(e) => out.push((line, e)),
                None => break,
            }
        }
        Ok(out)
    }

    fn expression(&mut self) -> Option<Expression> {
        let e = self.expression_inner()?;
        while self.eol().is_some() {}
        Some(e)
    }

    fn expression_inner(&mut self) -> Option<Expression> {
        if let Some(e) = self.opt(|p| p.conditional()) {
            return Some(e);
        }
        if self.opt(|p| p.reserved("discard")).is_some() {
            return Some(Expression::Discard);
        }
        if self.opt(|p| p.reserved("continue")).is_some() {
            return Some(Expression::Continue);
        }
        if let Some(e) = self.opt(|p| p.assignment()) {
            return Some(e);
        }
        self.opt(|p| p.innerexpression())
    }

    fn innerexpression(&mut self) -> Option<Expression> {
        let left = self.left_expression()?;
        if let Some(be) = self.opt(|p| {
            let bop = p.binop()?;
            let right = p.innerexpression()?;
            Some((bop, right))
        }) {
            let (bop, right) = be;
            Some(Expression::BinaryOp(bop, Box::new(left), Box::new(right)))
        } else {
            Some(left)
        }
    }

    fn left_expression(&mut self) -> Option<Expression> {
        if let Some(e) = self.opt(|p| p.uoperator()) {
            return Some(e);
        }
        if let Some(e) = self.opt(|p| p.method_call()) {
            return Some(e);
        }
        if let Some(e) = self.opt(|p| p.indexexpr()) {
            return Some(e);
        }
        self.base_expression()
    }

    fn base_expression(&mut self) -> Option<Expression> {
        if let Some(e) = self.opt(|p| p.pexpression()) {
            return Some(e);
        }
        if let Some(e) = self.opt(|p| p.funccall()) {
            return Some(e);
        }
        if let Some(e) = self.opt(|p| p.listexpr()) {
            return Some(e);
        }
        if let Some(e) = self.opt(|p| p.rawexpr()) {
            return Some(e);
        }
        self.variable().map(|v| Expression::Lookup(None, v))
    }

    fn pexpression(&mut self) -> Option<Expression> {
        self.operator('(')?;
        let e = self.innerexpression()?;
        self.operator(')')?;
        Some(e)
    }

    fn uoperator(&mut self) -> Option<Expression> {
        // len(expr)
        if let Some(e) = self.opt(|p| {
            p.reserved("len")?;
            p.operator('(')?;
            let e = p.expression()?;
            p.operator(')')?;
            Some(e)
        }) {
            return Some(Expression::UnaryOp(UOp::Len, Box::new(e)));
        }
        // -base
        if let Some(e) = self.opt(|p| {
            p.operator('-')?;
            p.base_expression()
        }) {
            return Some(Expression::UnaryOp(UOp::Minus, Box::new(e)));
        }
        // not inner
        if let Some(e) = self.opt(|p| {
            p.reserved("not")?;
            p.innerexpression()
        }) {
            return Some(Expression::UnaryOp(UOp::Not, Box::new(e)));
        }
        None
    }

    fn funccall(&mut self) -> Option<Expression> {
        if let Some(e) = self.opt(|p| p.paired()) {
            return Some(e);
        }
        // funcname '(' innerexpression kwargs ')' funcblock
        let fname = self.opt(|p| {
            let w = p.word()?;
            p.operator('(')?;
            Some(FuncName(w))
        })?;
        let arg = self.innerexpression()?;
        let kwargs = self.kwargs();
        self.operator(')')?;
        let block = self.opt(|p| p.funcblock());
        Some(Expression::FunctionCall(
            fname,
            Box::new(arg),
            kwargs,
            block,
        ))
    }

    fn funcblock(&mut self) -> Option<Block> {
        self.reserved("using")?;
        self.operator('|')?;
        let var = self.variable()?;
        self.operator('|')?;
        self.operator(':')?;
        let body = self.block()?;
        Some(Block {
            variable: var,
            body: Box::new(body),
        })
    }

    fn paired(&mut self) -> Option<Expression> {
        self.match_word("paired")?;
        self.operator('(')?;
        let first = self.innerexpression()?;
        self.operator(',')?;
        // pairedKwArgs: (second = expression) ++ kwargs ')'
        let second = self.expression()?;
        let mut kwargs = vec![(Variable("second".into()), second)];
        kwargs.extend(self.kwargs());
        self.operator(')')?;
        Some(Expression::FunctionCall(
            FuncName("paired".into()),
            Box::new(first),
            kwargs,
            None,
        ))
    }

    fn kwargs(&mut self) -> Vec<(Variable, Expression)> {
        let mut out = Vec::new();
        while let Some(kw) = self.opt(|p| {
            p.operator(',')?;
            p.kwarg()
        }) {
            out.push(kw);
        }
        out
    }

    fn kwarg(&mut self) -> Option<(Variable, Expression)> {
        let v = self.variable()?;
        self.operator('=')?;
        let e = self.innerexpression()?;
        Some((v, e))
    }

    fn assignment(&mut self) -> Option<Expression> {
        let v = self.variable()?;
        self.operator('=')?;
        let e = self.expression()?;
        Some(Expression::Assignment(v, Box::new(e)))
    }

    fn method_call(&mut self) -> Option<Expression> {
        let self_e = self.base_expression()?;
        self.operator('.')?;
        let met = MethodName(self.word()?);
        self.operator('(')?;
        // optional positional arg not followed by '='
        let arg = self.opt(|p| {
            let e = p.innerexpression()?;
            // notFollowedBy (operator '=')
            if matches!(
                p.peek(),
                Some(PosToken {
                    tok: Token::Operator('='),
                    ..
                })
            ) {
                None
            } else {
                Some(e)
            }
        });
        self.opt(|p| p.operator(','));
        let kws = self.sep_by(|p| p.kwarg(), ',');
        self.operator(')')?;
        Some(Expression::MethodCall(
            met,
            Box::new(self_e),
            arg.map(Box::new),
            kws,
        ))
    }

    fn indexexpr(&mut self) -> Option<Expression> {
        let base = self.base_expression()?;
        let ix = self.indexing()?;
        Some(Expression::IndexExpression(Box::new(base), ix))
    }

    fn indexing(&mut self) -> Option<Index> {
        // IndexTwo: '[' may_int ':' may_int ']'
        if let Some(ix) = self.opt(|p| {
            p.operator('[')?;
            let a = p.opt(|p| p.innerexpression());
            p.operator(':')?;
            let b = p.opt(|p| p.innerexpression());
            p.operator(']')?;
            Some(Index::Two(a.map(Box::new), b.map(Box::new)))
        }) {
            return Some(ix);
        }
        // IndexOne: '[' innerexpression ']'
        self.opt(|p| {
            p.operator('[')?;
            let e = p.innerexpression()?;
            p.operator(']')?;
            Some(Index::One(Box::new(e)))
        })
    }

    fn listexpr(&mut self) -> Option<Expression> {
        self.operator('[')?;
        let items = self.sep_end_by(|p| p.innerexpression(), ',');
        self.operator(']')?;
        Some(Expression::ListExpression(items))
    }

    fn conditional(&mut self) -> Option<Expression> {
        self.reserved("if")?;
        let cond = self.innerexpression()?;
        self.operator(':')?;
        let then_b = self.block()?;
        let else_b = self.mayelse();
        Some(Expression::Condition(
            Box::new(cond),
            Box::new(then_b),
            Box::new(else_b),
        ))
    }

    fn mayelse(&mut self) -> Expression {
        self.opt(|p| {
            p.reserved("else")?;
            p.operator(':')?;
            p.block()
        })
        .unwrap_or_else(|| Expression::Sequence(Vec::new()))
    }

    fn block(&mut self) -> Option<Expression> {
        self.eol()?;
        let level = self.indentation()?;
        let first = self.expression()?;
        let mut body = vec![first];
        while let Some(e) = self.opt(|p| {
            let level2 = p.indentation()?;
            if level != level2 {
                return None;
            }
            p.expression()
        }) {
            body.push(e);
        }
        Some(Expression::Sequence(body))
    }

    fn ngless_header(&mut self) -> Option<Header> {
        while self.eol().is_some() {}
        let version = self.opt(|p| {
            p.reserved("ngless")?;
            let v = p.string_const()?;
            p.eol()?;
            Some(v)
        })?;
        while self.eol().is_some() {}
        let mut modules = Vec::new();
        while let Some(m) = self.opt(|p| p.import_mod()) {
            modules.push(m);
            while self.eol().is_some() {}
        }
        Some(Header { version, modules })
    }

    fn import_mod(&mut self) -> Option<ModInfo> {
        // local import "name" version "ver"
        if let Some(m) = self.opt(|p| {
            p.reserved("local")?;
            p.reserved("import")?;
            let name = p.string_const()?;
            p.match_word("version")?;
            let version = p.string_const()?;
            p.eol()?;
            Some(ModInfo::LocalImport { name, version })
        }) {
            return Some(m);
        }
        self.opt(|p| {
            p.reserved("import")?;
            let name = p.string_const()?;
            p.match_word("version")?;
            let version = p.string_const()?;
            p.eol()?;
            Some(ModInfo::Import { name, version })
        })
    }

    // --- generic combinators -------------------------------------------------

    fn sep_by<T>(&mut self, mut f: impl FnMut(&mut Self) -> Option<T>, sep: char) -> Vec<T> {
        let mut out = Vec::new();
        match self.opt(|p| f(p)) {
            Some(x) => out.push(x),
            None => return out,
        }
        while let Some(x) = self.opt(|p| {
            p.operator(sep)?;
            f(p)
        }) {
            out.push(x);
        }
        out
    }

    fn sep_end_by<T>(&mut self, mut f: impl FnMut(&mut Self) -> Option<T>, sep: char) -> Vec<T> {
        let mut out = Vec::new();
        loop {
            match self.opt(|p| f(p)) {
                Some(x) => out.push(x),
                None => break,
            }
            if self.opt(|p| p.operator(sep)).is_none() {
                break;
            }
        }
        out
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Parse just the body, returning the expressions (mirrors `parseBody`).
    fn parse_body(s: &str) -> Vec<Expression> {
        let toks = tokenize("test", s).unwrap();
        let cleaned = cleanup_indents(&toks);
        let mut p = Parser::new(cleaned);
        p.nglbody().unwrap().into_iter().map(|(_, e)| e).collect()
    }

    fn lookup(v: &str) -> Expression {
        Expression::Lookup(None, Variable(v.into()))
    }

    #[test]
    fn parse_symbol() {
        assert_eq!(
            parse_body("{symbol}"),
            vec![Expression::ConstSymbol("symbol".into())]
        );
    }

    #[test]
    fn parse_fastq() {
        assert_eq!(
            parse_body("fastq(\"input.fq\")"),
            vec![Expression::FunctionCall(
                FuncName("fastq".into()),
                Box::new(Expression::ConstStr("input.fq".into())),
                vec![],
                None
            )]
        );
    }

    #[test]
    fn parse_paired() {
        assert_eq!(
            parse_body("paired(\"input.fq\", \"pair.fq\")"),
            vec![Expression::FunctionCall(
                FuncName("paired".into()),
                Box::new(Expression::ConstStr("input.fq".into())),
                vec![(
                    Variable("second".into()),
                    Expression::ConstStr("pair.fq".into())
                )],
                None
            )]
        );
    }

    #[test]
    fn parse_count() {
        assert_eq!(
            parse_body("count(annotated, count={gene})"),
            vec![Expression::FunctionCall(
                FuncName("count".into()),
                Box::new(lookup("annotated")),
                vec![(
                    Variable("count".into()),
                    Expression::ConstSymbol("gene".into())
                )],
                None
            )]
        );
    }

    #[test]
    fn parse_count_mult_counts() {
        assert_eq!(
            parse_body("count(annotated, count=[{gene},{cds}])"),
            vec![Expression::FunctionCall(
                FuncName("count".into()),
                Box::new(lookup("annotated")),
                vec![(
                    Variable("count".into()),
                    Expression::ListExpression(vec![
                        Expression::ConstSymbol("gene".into()),
                        Expression::ConstSymbol("cds".into())
                    ])
                )],
                None
            )]
        );
    }

    #[test]
    fn parse_assignment() {
        assert_eq!(
            parse_body("reads = \"something\""),
            vec![Expression::Assignment(
                Variable("reads".into()),
                Box::new(Expression::ConstStr("something".into()))
            )]
        );
    }

    #[test]
    fn parse_sequence() {
        let a = Expression::Assignment(
            Variable("reads".into()),
            Box::new(Expression::ConstStr("something".into())),
        );
        assert_eq!(
            parse_body("reads = 'something'\nreads = 'something'"),
            vec![a.clone(), a]
        );
    }

    #[test]
    fn parse_num_hex() {
        assert_eq!(
            parse_body("a = 0x10"),
            vec![Expression::Assignment(
                Variable("a".into()),
                Box::new(Expression::ConstInt(16))
            )]
        );
    }

    #[test]
    fn parse_double() {
        assert_eq!(
            parse_body("d = 1.2"),
            vec![Expression::Assignment(
                Variable("d".into()),
                Box::new(Expression::ConstDouble(1.2))
            )]
        );
    }

    #[test]
    fn parse_bool() {
        assert_eq!(
            parse_body("a = true"),
            vec![Expression::Assignment(
                Variable("a".into()),
                Box::new(Expression::ConstBool(true))
            )]
        );
    }

    #[test]
    fn parse_if_else() {
        assert_eq!(
            parse_body("if true:\n 0\n 1\nelse:\n 2\n"),
            vec![Expression::Condition(
                Box::new(Expression::ConstBool(true)),
                Box::new(Expression::Sequence(vec![
                    Expression::ConstInt(0),
                    Expression::ConstInt(1)
                ])),
                Box::new(Expression::Sequence(vec![Expression::ConstInt(2)]))
            )]
        );
    }

    #[test]
    fn parse_if() {
        assert_eq!(
            parse_body("if true:\n 0\n 1\n"),
            vec![Expression::Condition(
                Box::new(Expression::ConstBool(true)),
                Box::new(Expression::Sequence(vec![
                    Expression::ConstInt(0),
                    Expression::ConstInt(1)
                ])),
                Box::new(Expression::Sequence(vec![]))
            )]
        );
    }

    #[test]
    fn parse_if_end() {
        assert_eq!(
            parse_body("if true:\n 0\n 1\n2\n"),
            vec![
                Expression::Condition(
                    Box::new(Expression::ConstBool(true)),
                    Box::new(Expression::Sequence(vec![
                        Expression::ConstInt(0),
                        Expression::ConstInt(1)
                    ])),
                    Box::new(Expression::Sequence(vec![]))
                ),
                Expression::ConstInt(2)
            ]
        );
    }

    #[test]
    fn parse_ngless_header() {
        assert_eq!(
            parse_ngless("test", true, "ngless '0.0'\n").unwrap(),
            Script {
                header: Some(Header {
                    version: "0.0".into(),
                    modules: vec![]
                }),
                body: vec![]
            }
        );
    }

    #[test]
    fn parse_import() {
        assert_eq!(
            parse_ngless(
                "test",
                true,
                "ngless '0.0'\nimport 'testing' version '3.2-x'\n"
            )
            .unwrap(),
            Script {
                header: Some(Header {
                    version: "0.0".into(),
                    modules: vec![ModInfo::Import {
                        name: "testing".into(),
                        version: "3.2-x".into()
                    }]
                }),
                body: vec![]
            }
        );
    }

    #[test]
    fn parse_comment_before_import() {
        let src = "\nngless \"0.0\"\n# This should not fail\nimport \"parallel\" version \"0.0\"\n\n# This should not fail either\nsample = lock1(readlines('input.txt'))\ninput = fastq(sample)\n";
        assert!(parse_ngless("test", true, src).is_ok());
    }

    #[test]
    fn parse_empty_line_before_import() {
        let src = "\nngless \"0.0\"\n\nimport \"parallel\" version \"0.0\"\n\nsample = lock1(readlines('input.txt'))\ninput = fastq(sample)\n";
        assert!(parse_ngless("test", true, src).is_ok());
    }

    // index expressions
    fn parse_one(s: &str) -> Expression {
        parse_body(s).into_iter().next().unwrap()
    }

    #[test]
    fn parse_list() {
        assert_eq!(
            parse_one("[a,b]"),
            Expression::ListExpression(vec![lookup("a"), lookup("b")])
        );
    }

    #[test]
    fn parse_indexexpr_two() {
        let j1 = || Some(Box::new(Expression::ConstInt(1)));
        assert_eq!(
            parse_one("read[1:1]"),
            Expression::IndexExpression(Box::new(lookup("read")), Index::Two(j1(), j1()))
        );
        assert_eq!(
            parse_one("read[1:]"),
            Expression::IndexExpression(Box::new(lookup("read")), Index::Two(j1(), None))
        );
        assert_eq!(
            parse_one("read[:1]"),
            Expression::IndexExpression(Box::new(lookup("read")), Index::Two(None, j1()))
        );
        assert_eq!(
            parse_one("read[:]"),
            Expression::IndexExpression(Box::new(lookup("read")), Index::Two(None, None))
        );
    }

    #[test]
    fn parse_indexexpr_one() {
        assert_eq!(
            parse_one("read[1]"),
            Expression::IndexExpression(
                Box::new(lookup("read")),
                Index::One(Box::new(Expression::ConstInt(1)))
            )
        );
        assert_eq!(
            parse_one("read[var]"),
            Expression::IndexExpression(
                Box::new(lookup("read")),
                Index::One(Box::new(lookup("var")))
            )
        );
    }

    #[test]
    fn parse_kwargs() {
        assert_eq!(
            parse_body("unique(reads,maxCopies=2)\n"),
            vec![Expression::FunctionCall(
                FuncName("unique".into()),
                Box::new(lookup("reads")),
                vec![(Variable("maxCopies".into()), Expression::ConstInt(2))],
                None
            )]
        );
    }

    #[test]
    fn parse_double_colon() {
        assert_eq!(
            parse_body("module::function(arg)\n"),
            vec![Expression::FunctionCall(
                FuncName("module::function".into()),
                Box::new(lookup("arg")),
                vec![],
                None
            )]
        );
    }

    #[test]
    fn parse_methods_kwargs_only() {
        assert_eq!(
            parse_body("sf.filter(min_identity_pc=90)\n"),
            vec![Expression::MethodCall(
                MethodName("filter".into()),
                Box::new(lookup("sf")),
                None,
                vec![(Variable("min_identity_pc".into()), Expression::ConstInt(90))]
            )]
        );
    }

    // cleanup_indents unit tests
    fn tokcleanup(ts: Vec<Token>) -> Vec<Token> {
        let pts: Vec<PosToken> = ts
            .into_iter()
            .map(|t| PosToken {
                line: 0,
                col: 0,
                tok: t,
            })
            .collect();
        cleanup_indents(&pts).into_iter().map(|p| p.tok).collect()
    }

    #[test]
    fn cleanup_indents_cases() {
        assert_eq!(tokcleanup(vec![Token::Indent(1)]), vec![]);
        assert_eq!(tokcleanup(vec![Token::NewLine]), vec![Token::NewLine]);
        assert_eq!(
            tokcleanup(vec![Token::Indent(1), Token::NewLine]),
            vec![Token::NewLine]
        );
        assert_eq!(
            tokcleanup(vec![
                Token::Operator('('),
                Token::NewLine,
                Token::Indent(2),
                Token::Operator(')')
            ]),
            vec![Token::Operator('('), Token::Operator(')')]
        );
    }

    #[test]
    fn cleanup_indents_inside_brackets() {
        let toks = vec![
            Token::Word("write".into()),
            Token::Operator('('),
            Token::Word("A".into()),
            Token::Operator(','),
            Token::NewLine,
            Token::Indent(16),
            Token::NewLine,
            Token::Indent(16),
            Token::Word("format".into()),
            Token::Operator('='),
            Token::Expr(Expression::ConstSymbol("csv".into())),
            Token::Operator(')'),
            Token::NewLine,
        ];
        let expected = vec![
            Token::Word("write".into()),
            Token::Operator('('),
            Token::Word("A".into()),
            Token::Operator(','),
            Token::Word("format".into()),
            Token::Operator('='),
            Token::Expr(Expression::ConstSymbol("csv".into())),
            Token::Operator(')'),
            Token::NewLine,
        ];
        assert_eq!(tokcleanup(toks), expected);
    }
}
