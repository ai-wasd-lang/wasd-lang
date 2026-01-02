//! Abstract Syntax Tree definitions for WASD.

use crate::lexer::Span;

/// A complete WASD program.
#[derive(Debug, Clone)]
pub struct Program {
    pub items: Vec<Item>,
}

/// Top-level items in a WASD program.
#[derive(Debug, Clone)]
pub enum Item {
    Function(Function),
    Struct(StructDef),
    Enum(EnumDef),
}

/// A function definition.
#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: Option<Type>,
    pub effects: Vec<String>,
    pub body: Vec<Stmt>,
    pub span: Span,
}

/// A function parameter.
#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub ty: Type,
    pub span: Span,
}

/// A struct definition.
#[derive(Debug, Clone)]
pub struct StructDef {
    pub name: String,
    pub generics: Vec<String>,
    pub fields: Vec<Field>,
    pub span: Span,
}

/// A struct field.
#[derive(Debug, Clone)]
pub struct Field {
    pub name: String,
    pub ty: Type,
    pub span: Span,
}

/// An enum definition.
#[derive(Debug, Clone)]
pub struct EnumDef {
    pub name: String,
    pub generics: Vec<String>,
    pub variants: Vec<Variant>,
    pub span: Span,
}

/// An enum variant.
#[derive(Debug, Clone)]
pub struct Variant {
    pub name: String,
    pub fields: Vec<Type>,
    pub span: Span,
}

/// Type representations.
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Named(String),
    Generic(String, Vec<Type>),
    Reference(Box<Type>, bool), // (inner, is_mut)
    Heap(Box<Type>),
    Rc(Box<Type>),
    Arc(Box<Type>),
    Function(Vec<Type>, Box<Type>),
    Unit,
}

/// Statements in WASD.
#[derive(Debug, Clone)]
pub enum Stmt {
    Let {
        name: String,
        mutable: bool,
        ty: Option<Type>,
        value: Expr,
        span: Span,
    },
    Expr(Expr),
    Return(Option<Expr>, Span),
}

/// Expressions in WASD.
#[derive(Debug, Clone)]
pub enum Expr {
    Int(i64, Span),
    Float(f64, Span),
    String(String, Span),
    Bool(bool, Span),
    Ident(String, Span),
    Binary(Box<Expr>, BinOp, Box<Expr>, Span),
    Unary(UnaryOp, Box<Expr>, Span),
    Call(Box<Expr>, Vec<Expr>, Span),
    FieldAccess(Box<Expr>, String, Span),
    If(Box<Expr>, Vec<Stmt>, Option<Vec<Stmt>>, Span),
    Match(Box<Expr>, Vec<MatchArm>, Span),
    Block(Vec<Stmt>, Span),
}

/// Binary operators.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    NotEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    And,
    Or,
}

/// Unary operators.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOp {
    Neg,
    Not,
    Ref,
    RefMut,
    Deref,
}

/// A match arm.
#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Expr,
    pub span: Span,
}

/// Patterns for matching.
#[derive(Debug, Clone)]
pub enum Pattern {
    Wildcard(Span),
    Ident(String, Span),
    Literal(Expr),
    Constructor(String, Vec<Pattern>, Span),
}
