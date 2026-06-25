//! Tokenizer for the NGLess language.
//!
//! Mirrors `NGLess/Tokens.hs`. Note that spaces are emitted as `Indent` tokens (rather than
//! being skipped); `crate::parser::cleanup_indents` later removes the irrelevant ones. The
//! tokenizer never skips significant whitespace on its own.

use crate::ast::{BOp, Expression};
use crate::errors::{NgError, NgResult};

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    /// Constant expressions: strings, ints, doubles, bools, symbols, builtin constants.
    Expr(Expression),
    Bop(BOp),
    Reserved(String),
    Word(String),
    Operator(char),
    NewLine,
    Indent(usize),
}

/// A token together with the (1-based) source position where it starts.
#[derive(Clone, Debug, PartialEq)]
pub struct PosToken {
    pub line: usize,
    pub col: usize,
    pub tok: Token,
}

const RESERVED_WORDS: &[&str] = &[
    "if", "else", "ngless", "len", "discard", "continue", "not", "local", "import", "using",
];

const CONSTANTS: &[&str] = &["STDIN", "STDOUT", "ARGV"];

/// Binary operators, longest-match-first within each starting character, in the same order as
/// `Tokens.hs` so ordered matching is identical.
const BOPERATORS: &[(&str, BOp)] = &[
    ("!=", BOp::NEQ),
    ("==", BOp::EQ),
    ("</>", BOp::PathAppend),
    ("<=", BOp::LTE),
    ("<", BOp::LT),
    (">=", BOp::GTE),
    (">", BOp::GT),
    ("+", BOp::Add),
    ("*", BOp::Mul),
];

const OPERATOR_CHARS: &str = "=,+-*():[]<>.|";

struct Lexer {
    chars: Vec<char>,
    pos: usize,
    line: usize,
    col: usize,
    fname: String,
}

impl Lexer {
    fn new(fname: &str, input: &str) -> Self {
        Lexer {
            chars: input.chars().collect(),
            pos: 0,
            line: 1,
            col: 1,
            fname: fname.to_string(),
        }
    }

    fn err(&self, msg: impl Into<String>) -> NgError {
        NgError::script(format!(
            "Tokenization error on '{}' (line {}, column {}): {}",
            self.fname,
            self.line,
            self.col,
            msg.into()
        ))
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.pos).copied()
    }

    fn peek_at(&self, offset: usize) -> Option<char> {
        self.chars.get(self.pos + offset).copied()
    }

    fn starts_with(&self, s: &str) -> bool {
        s.chars()
            .enumerate()
            .all(|(i, c)| self.peek_at(i) == Some(c))
    }

    fn at_eof(&self) -> bool {
        self.pos >= self.chars.len()
    }

    /// Consume one character, updating line/column the way Parsec's default does.
    fn bump(&mut self) -> Option<char> {
        let c = self.peek()?;
        self.pos += 1;
        if c == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        Some(c)
    }

    fn bump_str(&mut self, s: &str) {
        for _ in s.chars() {
            self.bump();
        }
    }
}

pub fn tokenize(fname: &str, input: &str) -> NgResult<Vec<PosToken>> {
    let mut lex = Lexer::new(fname, input);
    let mut out = Vec::new();
    while !lex.at_eof() {
        let line = lex.line;
        let col = lex.col;
        let tok = next_token(&mut lex)?;
        out.push(PosToken { line, col, tok });
    }
    Ok(out)
}

fn next_token(lex: &mut Lexer) -> NgResult<Token> {
    // Order matches `ngltoken` in Tokens.hs exactly.
    if let Some(t) = comment(lex)? {
        return Ok(t);
    }
    if let Some(t) = symbol(lex)? {
        return Ok(t);
    }
    if let Some(t) = tstring(lex)? {
        return Ok(t);
    }
    if let Some(t) = double(lex) {
        return Ok(t);
    }
    if let Some(t) = integer(lex) {
        return Ok(t);
    }
    if let Some(t) = word(lex) {
        return Ok(t);
    }
    if let Some(t) = boperator(lex)? {
        return Ok(t);
    }
    if let Some(t) = operator(lex) {
        return Ok(t);
    }
    if let Some(t) = indent(lex) {
        return Ok(t);
    }
    if lex.peek() == Some('\t') {
        return Err(lex.err("Tabs are not used in NGLess. Please use spaces."));
    }
    if let Some(t) = eol(lex) {
        return Ok(t);
    }
    Err(lex.err(format!(
        "unexpected character {:?}",
        lex.peek().unwrap_or('\0')
    )))
}

/// `#...` / `//...` single-line comments return a NewLine token; `/* ... */` returns
/// `Indent(0)` (mirroring Tokens.hs).
fn comment(lex: &mut Lexer) -> NgResult<Option<Token>> {
    match lex.peek() {
        Some('#') => {
            lex.bump();
            Ok(Some(skip_to_eol(lex)))
        }
        Some('/') if lex.peek_at(1) == Some('/') => {
            lex.bump_str("//");
            Ok(Some(skip_to_eol(lex)))
        }
        Some('/') if lex.peek_at(1) == Some('*') => {
            lex.bump_str("/*");
            loop {
                if lex.starts_with("*/") {
                    lex.bump_str("*/");
                    return Ok(Some(Token::Indent(0)));
                }
                if lex.at_eof() {
                    return Err(lex.err("Unexpected End Of File inside a comment"));
                }
                lex.bump();
            }
        }
        _ => Ok(None),
    }
}

/// Consume up to and including the end of line; the eol itself becomes the returned token.
/// At end of input (no trailing newline) we treat it as a line end.
fn skip_to_eol(lex: &mut Lexer) -> Token {
    loop {
        if let Some(t) = eol(lex) {
            return t;
        }
        if lex.at_eof() {
            return Token::NewLine;
        }
        lex.bump();
    }
}

fn symbol(lex: &mut Lexer) -> NgResult<Option<Token>> {
    if lex.peek() != Some('{') {
        return Ok(None);
    }
    lex.bump();
    let mut name = String::new();
    while let Some(c) = lex.peek() {
        if c == '_' || c.is_alphanumeric() {
            name.push(c);
            lex.bump();
        } else {
            break;
        }
    }
    if name.is_empty() {
        return Err(lex.err("expected a symbol name after '{'"));
    }
    if lex.peek() == Some('-') {
        return Err(
            lex.err("Symbols cannot contain hyphens (-), perhaps you meant to use an underscore.")
        );
    }
    if lex.peek() != Some('}') {
        return Err(lex.err("expected '}' to close symbol"));
    }
    lex.bump();
    Ok(Some(Token::Expr(Expression::ConstSymbol(name))))
}

fn tstring(lex: &mut Lexer) -> NgResult<Option<Token>> {
    let term = match lex.peek() {
        Some('\'') => '\'',
        Some('"') => '"',
        _ => return Ok(None),
    };
    lex.bump();
    let mut s = String::new();
    loop {
        match lex.peek() {
            None => return Err(lex.err("unterminated string")),
            Some(c) if c == term => {
                lex.bump();
                return Ok(Some(Token::Expr(Expression::ConstStr(s))));
            }
            Some('\\') => {
                lex.bump();
                match lex.bump() {
                    Some('n') => s.push('\n'),
                    Some('t') => s.push('\t'),
                    Some('\\') => s.push('\\'),
                    Some(other) => s.push(other),
                    None => return Err(lex.err("unterminated string escape")),
                }
            }
            Some(c) => {
                s.push(c);
                lex.bump();
            }
        }
    }
}

/// `many1 digit '.' many1 digit`, with backtracking (Parsec `try double'`).
fn double(lex: &mut Lexer) -> Option<Token> {
    let start = lex.pos;
    let (sl, sc) = (lex.line, lex.col);
    let mut int_part = String::new();
    while let Some(c) = lex.peek() {
        if c.is_ascii_digit() {
            int_part.push(c);
            lex.bump();
        } else {
            break;
        }
    }
    if int_part.is_empty()
        || lex.peek() != Some('.')
        || !matches!(lex.peek_at(1), Some(d) if d.is_ascii_digit())
    {
        lex.pos = start;
        lex.line = sl;
        lex.col = sc;
        return None;
    }
    lex.bump(); // '.'
    let mut frac = String::new();
    while let Some(c) = lex.peek() {
        if c.is_ascii_digit() {
            frac.push(c);
            lex.bump();
        } else {
            break;
        }
    }
    let v: f64 = format!("{int_part}.{frac}").parse().unwrap();
    Some(Token::Expr(Expression::ConstDouble(v)))
}

/// Hexadecimal (`0x...`) or decimal integer.
fn integer(lex: &mut Lexer) -> Option<Token> {
    if lex.starts_with("0x") {
        // Need at least one hex digit after 0x.
        if matches!(lex.peek_at(2), Some(d) if d.is_ascii_hexdigit()) {
            lex.bump_str("0x");
            let mut digits = String::new();
            while let Some(c) = lex.peek() {
                if c.is_ascii_hexdigit() {
                    digits.push(c);
                    lex.bump();
                } else {
                    break;
                }
            }
            let v = i64::from_str_radix(&digits, 16).unwrap();
            return Some(Token::Expr(Expression::ConstInt(v)));
        }
        return None;
    }
    if matches!(lex.peek(), Some(c) if c.is_ascii_digit()) {
        let mut digits = String::new();
        while let Some(c) = lex.peek() {
            if c.is_ascii_digit() {
                digits.push(c);
                lex.bump();
            } else {
                break;
            }
        }
        let v: i64 = digits.parse().unwrap();
        return Some(Token::Expr(Expression::ConstInt(v)));
    }
    None
}

fn is_var_start(c: char) -> bool {
    c == '_' || c.is_alphabetic()
}

fn is_var_cont(c: char) -> bool {
    c == '_' || c.is_alphanumeric()
}

/// Identifiers, possibly `module::function`, classified into bool/reserved/constant/word.
fn word(lex: &mut Lexer) -> Option<Token> {
    if !matches!(lex.peek(), Some(c) if is_var_start(c)) {
        return None;
    }
    let mut k = read_var_simple(lex);
    // optional `::simple`
    if lex.starts_with("::") && matches!(lex.peek_at(2), Some(c) if is_var_start(c)) {
        lex.bump_str("::");
        let second = read_var_simple(lex);
        k = format!("{k}::{second}");
    }
    Some(match k.as_str() {
        "true" | "True" => Token::Expr(Expression::ConstBool(true)),
        "false" | "False" => Token::Expr(Expression::ConstBool(false)),
        _ if RESERVED_WORDS.contains(&k.as_str()) => Token::Reserved(k),
        _ if CONSTANTS.contains(&k.as_str()) => {
            Token::Expr(Expression::BuiltinConstant(crate::ast::Variable(k)))
        }
        _ => Token::Word(k),
    })
}

fn read_var_simple(lex: &mut Lexer) -> String {
    let mut s = String::new();
    // first char (already known to be var-start)
    if let Some(c) = lex.peek() {
        s.push(c);
        lex.bump();
    }
    while let Some(c) = lex.peek() {
        if is_var_cont(c) {
            s.push(c);
            lex.bump();
        } else {
            break;
        }
    }
    s
}

fn boperator(lex: &mut Lexer) -> NgResult<Option<Token>> {
    if lex.starts_with("<\\>") {
        return Err(lex.err("<\\> found. Did you mean </>?"));
    }
    for (long, bop) in BOPERATORS {
        if lex.starts_with(long) {
            lex.bump_str(long);
            return Ok(Some(Token::Bop(*bop)));
        }
    }
    Ok(None)
}

fn operator(lex: &mut Lexer) -> Option<Token> {
    match lex.peek() {
        Some(c) if OPERATOR_CHARS.contains(c) => {
            lex.bump();
            Some(Token::Operator(c))
        }
        _ => None,
    }
}

fn indent(lex: &mut Lexer) -> Option<Token> {
    if lex.peek() != Some(' ') {
        return None;
    }
    let mut n = 0;
    while lex.peek() == Some(' ') {
        n += 1;
        lex.bump();
    }
    Some(Token::Indent(n))
}

/// `;` (followed by skipped spaces) or a real newline, both producing `NewLine`.
fn eol(lex: &mut Lexer) -> Option<Token> {
    match lex.peek() {
        Some(';') => {
            lex.bump();
            while lex.peek() == Some(' ') {
                lex.bump();
            }
            Some(Token::NewLine)
        }
        Some('\r') if lex.peek_at(1) == Some('\n') => {
            lex.bump_str("\r\n");
            Some(Token::NewLine)
        }
        Some('\n') => {
            lex.bump();
            Some(Token::NewLine)
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Variable;

    fn toks(s: &str) -> Vec<Token> {
        tokenize("test", s)
            .unwrap()
            .into_iter()
            .map(|p| p.tok)
            .collect()
    }

    #[test]
    fn symbol_token() {
        assert_eq!(
            toks("{symbol}"),
            vec![Token::Expr(Expression::ConstSymbol("symbol".into()))]
        );
    }

    #[test]
    fn hex_and_decimal() {
        assert_eq!(toks("0x10"), vec![Token::Expr(Expression::ConstInt(16))]);
        assert_eq!(toks("16"), vec![Token::Expr(Expression::ConstInt(16))]);
    }

    #[test]
    fn double_vs_int() {
        assert_eq!(toks("1.2"), vec![Token::Expr(Expression::ConstDouble(1.2))]);
    }

    #[test]
    fn booleans_and_reserved_and_constants() {
        assert_eq!(toks("true"), vec![Token::Expr(Expression::ConstBool(true))]);
        assert_eq!(
            toks("False"),
            vec![Token::Expr(Expression::ConstBool(false))]
        );
        assert_eq!(toks("if"), vec![Token::Reserved("if".into())]);
        assert_eq!(
            toks("STDIN"),
            vec![Token::Expr(Expression::BuiltinConstant(Variable(
                "STDIN".into()
            )))]
        );
    }

    #[test]
    fn double_colon_word() {
        assert_eq!(
            toks("module::function"),
            vec![Token::Word("module::function".into())]
        );
    }

    #[test]
    fn binops_vs_operators() {
        // '<' and '>' and '+' '*' are binary operators; '-' is a plain operator.
        assert_eq!(toks("<"), vec![Token::Bop(BOp::LT)]);
        assert_eq!(toks("</>"), vec![Token::Bop(BOp::PathAppend)]);
        assert_eq!(toks("<="), vec![Token::Bop(BOp::LTE)]);
        assert_eq!(toks("-"), vec![Token::Operator('-')]);
    }

    #[test]
    fn string_escapes() {
        assert_eq!(
            toks(r#"'a\nb'"#),
            vec![Token::Expr(Expression::ConstStr("a\nb".into()))]
        );
    }

    #[test]
    fn single_line_comment_is_newline() {
        assert_eq!(toks("# hello\n"), vec![Token::NewLine]);
        assert_eq!(toks("// hello\n"), vec![Token::NewLine]);
    }

    #[test]
    fn spaces_become_indent() {
        assert_eq!(toks("  "), vec![Token::Indent(2)]);
    }

    // Mirrors Tests.hs `case_tok_multi_line_comment`: a `/* ... */` block (spanning lines)
    // collapses to an `Indent(0)`, leaving the surrounding tokens intact.
    #[test]
    fn multi_line_comment() {
        assert_eq!(
            toks("a=0/* This\n\nwith\nlines*/\nb=1\n"),
            vec![
                Token::Word("a".into()),
                Token::Operator('='),
                Token::Expr(Expression::ConstInt(0)),
                Token::Indent(0),
                Token::NewLine,
                Token::Word("b".into()),
                Token::Operator('='),
                Token::Expr(Expression::ConstInt(1)),
                Token::NewLine,
            ]
        );
    }

    // Mirrors Tests.hs `case_tok_cr`: a CRLF line ending is a single NewLine.
    #[test]
    fn crlf_is_newline() {
        assert_eq!(
            toks("a=0\r\nb=1\r\n"),
            vec![
                Token::Word("a".into()),
                Token::Operator('='),
                Token::Expr(Expression::ConstInt(0)),
                Token::NewLine,
                Token::Word("b".into()),
                Token::Operator('='),
                Token::Expr(Expression::ConstInt(1)),
                Token::NewLine,
            ]
        );
    }

    // Mirrors Tests.hs `case_tok_word_`: underscores are word characters.
    #[test]
    fn word_with_underscore() {
        assert_eq!(
            toks("word_with_underscore"),
            vec![Token::Word("word_with_underscore".into())]
        );
    }

    #[test]
    fn tabs_are_rejected() {
        assert!(tokenize("test", "\t").is_err());
    }
}
