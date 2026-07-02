//! Abstract syntax tree for the NGLess language.
//!
//! Mirrors `NGLess/Language.hs`. The runtime value type (`NGLessObject`) lives in `values.rs`; the
//! AST here is what the front end (tokenizer → parser → type checker → validation) and the
//! post-typecheck transforms need. `Optimized` (see [`OptimizedExpression`]) is generated only by
//! those transforms, never parsed from user input.

use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Variable(pub String);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FuncName(pub String);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct MethodName(pub String);

/// Unary operators.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UOp {
    Len,
    Minus,
    Not,
}

/// Binary operators. Note there is no subtraction operator in NGLess.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BOp {
    Add,
    Mul,
    GT,
    GTE,
    LT,
    LTE,
    EQ,
    NEQ,
    PathAppend,
}

/// What is inside an index expression: `[a]` (One) or `[a:b]` (Two).
#[derive(Clone, Debug, PartialEq)]
pub enum Index {
    One(Box<Expression>),
    Two(Option<Box<Expression>>, Option<Box<Expression>>),
}

/// A block, as in `f(a) using |var|:` followed by an indented body.
#[derive(Clone, Debug, PartialEq)]
pub struct Block {
    pub variable: Variable,
    pub body: Box<Expression>,
}

/// Static (declared/inferred) types of the language.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum NGLType {
    String,
    Integer,
    Double,
    Bool,
    Symbol,
    Filename,
    Read,
    ReadSet,
    MappedRead,
    MappedReadSet,
    SequenceSet,
    Counts,
    Void,
    Union(Vec<NGLType>),
    Any,
    List(Box<NGLType>),
}

/// An internally-generated, optimized expression (mirrors `OptimizedExpression` in
/// `Language.hs`). These never appear in user scripts; they are produced by post-typecheck
/// transforms (see `transform.rs`) to special-case hot patterns inside `preprocess` blocks.
#[derive(Clone, Debug, PartialEq)]
pub enum OptimizedExpression {
    /// `if len(read) <op> <int>: discard` — the block variable, the comparison operator (one of
    /// `<`/`<=`/`>`/`>=`), and the integer threshold.
    LenThresholdDiscard(Variable, BOp, i64),
    /// `read = substrim(read, min_quality=<int>)` (the same variable on both sides) — the block
    /// variable and the `min_quality` threshold. Applies `substrim` in place without rebuilding the
    /// intermediate `NGLessObject`.
    SubstrimReassign(Variable, i64),
}

/// The main AST type.
#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    /// Variable lookup, carrying an optional inferred type.
    Lookup(Option<NGLType>, Variable),
    ConstStr(String),
    ConstInt(i64),
    ConstDouble(f64),
    ConstBool(bool),
    ConstSymbol(String),
    BuiltinConstant(Variable),
    ListExpression(Vec<Expression>),
    Continue,
    Discard,
    UnaryOp(UOp, Box<Expression>),
    BinaryOp(BOp, Box<Expression>, Box<Expression>),
    /// `if c: t else: f`
    Condition(Box<Expression>, Box<Expression>, Box<Expression>),
    IndexExpression(Box<Expression>, Index),
    Assignment(Variable, Box<Expression>),
    FunctionCall(
        FuncName,
        Box<Expression>,
        Vec<(Variable, Expression)>,
        Option<Block>,
    ),
    MethodCall(
        MethodName,
        Box<Expression>,
        Option<Box<Expression>>,
        Vec<(Variable, Expression)>,
    ),
    Sequence(Vec<Expression>),
    /// An internally-generated optimized expression (see [`OptimizedExpression`]). Produced by a
    /// post-typecheck transform; never parsed from user input.
    Optimized(OptimizedExpression),
}

/// Module import information (the header `import "name" version "v"`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ModInfo {
    Import { name: String, version: String },
    LocalImport { name: String, version: String },
}

impl ModInfo {
    pub fn name(&self) -> &str {
        match self {
            ModInfo::Import { name, .. } | ModInfo::LocalImport { name, .. } => name,
        }
    }
    pub fn version(&self) -> &str {
        match self {
            ModInfo::Import { version, .. } | ModInfo::LocalImport { version, .. } => version,
        }
    }
}

/// Script header: the version declaration plus module imports.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Header {
    pub version: String,
    pub modules: Vec<ModInfo>,
}

/// A parsed script: an optional header followed by line-numbered expressions.
#[derive(Clone, Debug, PartialEq)]
pub struct Script {
    pub header: Option<Header>,
    pub body: Vec<(usize, Expression)>,
}

impl fmt::Display for FuncName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

impl fmt::Display for MethodName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}
