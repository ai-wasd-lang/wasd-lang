//! Lexer module for tokenizing Lux source code.
//!
//! The lexer handles indentation-based block parsing by tracking
//! indent levels and emitting `Indent`/`Dedent` tokens.

mod lexer;
mod token;

pub use lexer::Lexer;
pub use token::{Span, Token};
