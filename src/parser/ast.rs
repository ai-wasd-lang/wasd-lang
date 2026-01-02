//! Abstract Syntax Tree definitions for WASD.

#![allow(dead_code)]

use crate::lexer::Span;

/// A complete WASD program.
#[derive(Debug, Clone)]
pub struct Program {
    pub items: Vec<Item>,
}

/// Top-level items in a WASD program.
#[derive(Debug, Clone)]
pub enum Item {
    Use(UseStmt),
    Function(Function),
    Struct(StructDef),
    Enum(EnumDef),
    Trait(TraitDef),
    Impl(ImplDef),
}

/// A use/import statement.
#[derive(Debug, Clone)]
pub struct UseStmt {
    /// The path being imported (e.g., "std.io.print" or "std.io")
    pub path: Vec<String>,
    /// Whether this is a wildcard import (use std.io.*)
    pub wildcard: bool,
    /// Optional alias (use std.io.print as p)
    pub alias: Option<String>,
    pub span: Span,
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

/// A trait definition.
#[derive(Debug, Clone)]
pub struct TraitDef {
    pub name: String,
    pub generics: Vec<String>,
    pub methods: Vec<TraitMethod>,
    pub span: Span,
}

/// A trait method signature (may have default impl).
#[derive(Debug, Clone)]
pub struct TraitMethod {
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: Option<Type>,
    pub body: Option<Vec<Stmt>>, // None = required, Some = default implementation
    pub span: Span,
}

/// An impl block.
#[derive(Debug, Clone)]
pub struct ImplDef {
    /// The trait being implemented (None for inherent impls)
    pub trait_name: Option<String>,
    /// Generic parameters on the trait (e.g., Add[Point, Point])
    pub trait_generics: Vec<Type>,
    /// The type receiving the impl
    pub target_type: Type,
    /// Methods in the impl
    pub methods: Vec<Function>,
    pub span: Span,
}

/// Type representations.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    Assign {
        target: Expr,
        value: Expr,
        span: Span,
    },
    Expr(Expr),
    Return(Option<Expr>, Span),
    While {
        condition: Expr,
        body: Vec<Stmt>,
        span: Span,
    },
    For {
        var: String,
        iterable: Expr,
        body: Vec<Stmt>,
        span: Span,
    },
    Break(Span),
    Continue(Span),
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
    StructConstruct {
        name: String,
        fields: Vec<(String, Expr)>,
        span: Span,
    },
    EnumConstruct {
        enum_name: Option<String>,
        variant: String,
        value: Option<Box<Expr>>,
        span: Span,
    },
    If(Box<Expr>, Vec<Stmt>, Option<Vec<Stmt>>, Span),
    Match(Box<Expr>, Vec<MatchArm>, Span),
    Block(Vec<Stmt>, Span),
    /// Heap allocation: heap expr
    HeapAlloc(Box<Expr>, Span),
    /// Reference-counted allocation: rc expr
    RcAlloc(Box<Expr>, Span),
    /// Atomically reference-counted allocation: arc expr
    ArcAlloc(Box<Expr>, Span),
    /// Closure/lambda: |params| expr or |params| { body }
    Lambda {
        params: Vec<Param>,
        body: Box<Expr>,
        span: Span,
    },
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
