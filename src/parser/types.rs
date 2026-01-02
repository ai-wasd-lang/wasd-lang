//! Type parsing for WASD.

use super::ast::*;
use super::Parser;
use crate::lexer::Token;

impl<'a> Parser<'a> {
    /// Parse a type annotation.
    pub(super) fn parse_type(&mut self) -> Result<Type, String> {
        if self.check(&Token::Ampersand) {
            self.advance();
            let is_mut = if self.check(&Token::Mut) {
                self.advance();
                true
            } else {
                false
            };
            let inner = self.parse_type()?;
            return Ok(Type::Reference(Box::new(inner), is_mut));
        }

        if self.check(&Token::Heap) {
            self.advance();
            let inner = self.parse_type()?;
            return Ok(Type::Heap(Box::new(inner)));
        }

        if self.check(&Token::Rc) {
            self.advance();
            let inner = self.parse_type()?;
            return Ok(Type::Rc(Box::new(inner)));
        }

        if self.check(&Token::Arc) {
            self.advance();
            let inner = self.parse_type()?;
            return Ok(Type::Arc(Box::new(inner)));
        }

        // Function type: fn(args) -> RetType
        if self.check(&Token::Fn) {
            self.advance();
            self.expect(&Token::LParen)?;
            let mut param_types = Vec::new();
            while !self.check(&Token::RParen) {
                param_types.push(self.parse_type()?);
                if !self.check(&Token::RParen) {
                    self.expect(&Token::Comma)?;
                }
            }
            self.expect(&Token::RParen)?;

            // Return type
            let return_type = if self.check(&Token::Arrow) {
                self.advance();
                self.parse_type()?
            } else {
                Type::Named("void".to_string())
            };

            return Ok(Type::Function(param_types, Box::new(return_type)));
        }

        let name = self.expect_ident()?;

        if self.check(&Token::LBracket) {
            self.advance();
            let mut args = Vec::new();
            while !self.check(&Token::RBracket) {
                args.push(self.parse_type()?);
                if !self.check(&Token::RBracket) {
                    self.expect(&Token::Comma)?;
                }
            }
            self.expect(&Token::RBracket)?;
            Ok(Type::Generic(name, args))
        } else {
            Ok(Type::Named(name))
        }
    }
}
