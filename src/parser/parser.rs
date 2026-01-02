//! Recursive descent parser for WASD.

#![allow(dead_code)]

use super::ast::*;
use crate::lexer::{Lexer, Span, Token};

/// The WASD parser.
pub struct Parser<'a> {
    #[allow(dead_code)]
    lexer: Lexer<'a>,
    tokens: Vec<(Token, Span)>,
    pub(super) pos: usize,
}

impl<'a> Parser<'a> {
    /// Create a new parser for the given source code.
    pub fn new(source: &'a str) -> Self {
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize();
        Self {
            lexer,
            tokens,
            pos: 0,
        }
    }

    /// Parse the source into a Program AST.
    pub fn parse(&mut self) -> Result<Program, String> {
        let mut items = Vec::new();

        while !self.is_at_end() {
            if self.check(&Token::Newline) {
                self.advance();
                continue;
            }
            items.push(self.parse_item()?);
        }

        Ok(Program { items })
    }

    // Helper methods

    pub(super) fn peek(&self) -> Token {
        self.tokens
            .get(self.pos)
            .map(|(t, _): &(Token, Span)| t.clone())
            .unwrap_or(Token::Eof)
    }

    pub(super) fn current_span(&self) -> Span {
        self.tokens
            .get(self.pos)
            .map(|(_, s)| *s)
            .unwrap_or(Span::new(0, 0))
    }

    pub(super) fn advance(&mut self) -> Token {
        let token = self.peek();
        self.pos += 1;
        token
    }

    pub(super) fn check(&self, token: &Token) -> bool {
        std::mem::discriminant(&self.peek()) == std::mem::discriminant(token)
    }

    pub(super) fn expect(&mut self, token: &Token) -> Result<(), String> {
        if self.check(token) {
            self.advance();
            Ok(())
        } else {
            Err(format!("Expected {:?}, found {:?}", token, self.peek()))
        }
    }

    pub(super) fn expect_ident(&mut self) -> Result<String, String> {
        match self.peek() {
            Token::Ident(name) => {
                self.advance();
                Ok(name)
            }
            _ => Err(format!("Expected identifier, found {:?}", self.peek())),
        }
    }

    pub(super) fn skip_newlines(&mut self) {
        while self.check(&Token::Newline) {
            self.advance();
        }
    }

    pub(super) fn is_at_end(&self) -> bool {
        matches!(self.peek(), Token::Eof)
    }
}
