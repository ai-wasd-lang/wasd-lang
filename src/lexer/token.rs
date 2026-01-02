//! Token definitions for the WASD lexer.

use std::fmt;

/// Represents a source code location for error reporting.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

/// Tokens produced by the WASD lexer.
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Literals
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),

    // Identifiers and keywords
    Ident(String),
    Let,
    Mut,
    Fn,
    Struct,
    Enum,
    Trait,
    Impl,
    Match,
    If,
    Else,
    While,
    For,
    In,
    Return,
    Break,
    Continue,
    Loop,
    With,
    Use,
    Import,
    Pub,
    Extern,
    As,
    Heap,
    Rc,
    Arc,
    SelfKeyword,
    Async,
    Await,

    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Eq,
    EqEq,
    NotEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    And,
    Or,
    Not,
    Ampersand,
    Pipe,
    Arrow,
    FatArrow,
    Question,

    // Delimiters
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Comma,
    Colon,
    DoubleColon,
    Dot,
    DotDot,

    // Indentation
    Indent,
    Dedent,
    Newline,

    // Special
    Eof,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Int(n) => write!(f, "{}", n),
            Token::Float(n) => write!(f, "{}", n),
            Token::String(s) => write!(f, "\"{}\"", s),
            Token::Bool(b) => write!(f, "{}", b),
            Token::Ident(s) => write!(f, "{}", s),
            Token::Let => write!(f, "let"),
            Token::Mut => write!(f, "mut"),
            Token::Fn => write!(f, "fn"),
            Token::Struct => write!(f, "struct"),
            Token::Enum => write!(f, "enum"),
            Token::Trait => write!(f, "trait"),
            Token::Impl => write!(f, "impl"),
            Token::SelfKeyword => write!(f, "self"),
            Token::Match => write!(f, "match"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::While => write!(f, "while"),
            Token::For => write!(f, "for"),
            Token::In => write!(f, "in"),
            Token::Return => write!(f, "return"),
            Token::Break => write!(f, "break"),
            Token::Continue => write!(f, "continue"),
            Token::Loop => write!(f, "loop"),
            Token::With => write!(f, "with"),
            Token::Use => write!(f, "use"),
            Token::Import => write!(f, "import"),
            Token::Pub => write!(f, "pub"),
            Token::Extern => write!(f, "extern"),
            Token::As => write!(f, "as"),
            Token::Heap => write!(f, "heap"),
            Token::Rc => write!(f, "rc"),
            Token::Arc => write!(f, "arc"),
            Token::Async => write!(f, "async"),
            Token::Await => write!(f, "await"),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Star => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Percent => write!(f, "%"),
            Token::Eq => write!(f, "="),
            Token::EqEq => write!(f, "=="),
            Token::NotEq => write!(f, "!="),
            Token::Lt => write!(f, "<"),
            Token::LtEq => write!(f, "<="),
            Token::Gt => write!(f, ">"),
            Token::GtEq => write!(f, ">="),
            Token::And => write!(f, "and"),
            Token::Or => write!(f, "or"),
            Token::Not => write!(f, "not"),
            Token::Ampersand => write!(f, "&"),
            Token::Pipe => write!(f, "|"),
            Token::Arrow => write!(f, "->"),
            Token::FatArrow => write!(f, "=>"),
            Token::Question => write!(f, "?"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBracket => write!(f, "["),
            Token::RBracket => write!(f, "]"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::Comma => write!(f, ","),
            Token::Colon => write!(f, ":"),
            Token::DoubleColon => write!(f, "::"),
            Token::Dot => write!(f, "."),
            Token::DotDot => write!(f, ".."),
            Token::Indent => write!(f, "<INDENT>"),
            Token::Dedent => write!(f, "<DEDENT>"),
            Token::Newline => write!(f, "<NEWLINE>"),
            Token::Eof => write!(f, "<EOF>"),
        }
    }
}
