//! Lexer module for tokenizing WASD source code.
//!
//! The lexer handles indentation-based block parsing by tracking
//! indent levels and emitting `Indent`/`Dedent` tokens.

pub mod lexer;
#[cfg(test)]
mod lexer_tests;
pub mod token;

pub use lexer::Lexer;
pub use token::{Span, Token};
