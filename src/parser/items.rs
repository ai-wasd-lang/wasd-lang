//! Item parsing for WASD (functions, use statements, parameters).

use super::ast::*;
use super::Parser;
use crate::lexer::Token;

impl<'a> Parser<'a> {
    /// Parse a top-level item.
    pub(super) fn parse_item(&mut self) -> Result<Item, String> {
        // Check for visibility modifier
        let is_pub = if self.check(&Token::Pub) {
            self.advance();
            true
        } else {
            false
        };

        // Check for async modifier
        let is_async = if self.check(&Token::Async) {
            self.advance();
            true
        } else {
            false
        };

        match self.peek() {
            Token::Use => self.parse_use().map(Item::Use),
            Token::Import => self.parse_use().map(Item::Use), // import is an alias for use
            Token::Fn => self.parse_function_impl(is_pub, is_async).map(Item::Function),
            Token::Struct => self.parse_struct_with_visibility(is_pub).map(Item::Struct),
            Token::Enum => self.parse_enum_with_visibility(is_pub).map(Item::Enum),
            Token::Trait => self.parse_trait().map(Item::Trait),
            Token::Impl => self.parse_impl().map(Item::Impl),
            Token::Extern => self.parse_extern_fn().map(Item::ExternFn),
            _ => Err(format!("Expected item, found {:?}", self.peek())),
        }
    }

    /// Parse a use statement.
    pub(super) fn parse_use(&mut self) -> Result<UseStmt, String> {
        let span = self.current_span();
        // Accept either 'use' or 'import'
        if self.check(&Token::Use) {
            self.advance();
        } else if self.check(&Token::Import) {
            self.advance();
        } else {
            return Err("Expected 'use' or 'import'".to_string());
        }

        // Parse the path: std.io.print or std.io
        let mut path = Vec::new();
        path.push(self.expect_ident()?);

        let mut wildcard = false;

        while self.check(&Token::Dot) {
            self.advance(); // consume the dot

            // Check for wildcard: std.io.*
            if self.check(&Token::Star) {
                self.advance();
                wildcard = true;
                break;
            }

            path.push(self.expect_ident()?);
        }

        // Check for alias: use std.io.print as p
        let alias = if self.check(&Token::As) {
            self.advance();
            Some(self.expect_ident()?)
        } else {
            None
        };

        // Consume optional newline
        self.skip_newlines();

        Ok(UseStmt {
            path,
            wildcard,
            alias,
            span,
        })
    }

    /// Parse a function definition (for impls and traits).
    pub(super) fn parse_function(&mut self) -> Result<Function, String> {
        self.parse_function_with_visibility(false)
    }

    /// Parse a function definition with visibility.
    pub(super) fn parse_function_with_visibility(&mut self, is_pub: bool) -> Result<Function, String> {
        self.parse_function_impl(is_pub, false)
    }

    /// Parse a function definition with visibility and async flag.
    pub(super) fn parse_function_impl(&mut self, is_pub: bool, is_async: bool) -> Result<Function, String> {
        let start = self.current_span();
        self.expect(&Token::Fn)?;
        let name = self.expect_ident()?;

        // Parse optional generic parameters [T, U, ...]
        let generics = if self.check(&Token::LBracket) {
            self.advance();
            let mut params = Vec::new();
            while !self.check(&Token::RBracket) {
                params.push(self.expect_ident()?);
                if !self.check(&Token::RBracket) {
                    self.expect(&Token::Comma)?;
                }
            }
            self.expect(&Token::RBracket)?;
            params
        } else {
            Vec::new()
        };

        self.expect(&Token::LParen)?;
        let params = self.parse_params()?;
        self.expect(&Token::RParen)?;

        let return_type = if self.check(&Token::Arrow) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };

        // Parse explicit effects or auto-inject Async for async functions
        let mut effects = if self.check(&Token::With) {
            self.advance();
            self.parse_effects()?
        } else {
            Vec::new()
        };

        // Async functions automatically have the Async effect
        if is_async && !effects.contains(&"Async".to_string()) {
            effects.push("Async".to_string());
        }

        self.skip_newlines();
        self.expect(&Token::Indent)?;
        let body = self.parse_block()?;

        let visibility = if is_pub {
            Visibility::Public
        } else {
            Visibility::Private
        };

        Ok(Function {
            visibility,
            is_async,
            name,
            generics,
            params,
            return_type,
            effects,
            body,
            span: start,
        })
    }

    /// Parse function parameters.
    pub(super) fn parse_params(&mut self) -> Result<Vec<Param>, String> {
        let mut params = Vec::new();
        while !self.check(&Token::RParen) {
            // Handle `self` as a special parameter
            if self.check(&Token::SelfKeyword) {
                let span = self.current_span();
                self.advance();
                params.push(Param {
                    name: "self".to_string(),
                    ty: Type::Named("Self".to_string()), // Placeholder type
                    span,
                });
            } else {
                let name = self.expect_ident()?;
                self.expect(&Token::Colon)?;
                let ty = self.parse_type()?;
                params.push(Param {
                    name,
                    ty,
                    span: self.current_span(),
                });
            }
            if !self.check(&Token::RParen) {
                self.expect(&Token::Comma)?;
            }
        }
        Ok(params)
    }

    /// Parse generic type parameters.
    pub(super) fn parse_generics(&mut self) -> Result<Vec<String>, String> {
        if !self.check(&Token::LBracket) {
            return Ok(Vec::new());
        }
        self.advance();
        let mut generics = Vec::new();
        while !self.check(&Token::RBracket) {
            generics.push(self.expect_ident()?);
            if !self.check(&Token::RBracket) {
                self.expect(&Token::Comma)?;
            }
        }
        self.expect(&Token::RBracket)?;
        Ok(generics)
    }

    /// Parse effect annotations.
    pub(super) fn parse_effects(&mut self) -> Result<Vec<String>, String> {
        self.expect(&Token::LBracket)?;
        let mut effects = Vec::new();
        while !self.check(&Token::RBracket) {
            effects.push(self.expect_ident()?);
            if !self.check(&Token::RBracket) {
                self.expect(&Token::Comma)?;
            }
        }
        self.expect(&Token::RBracket)?;
        Ok(effects)
    }

    /// Parse an extern function declaration (FFI).
    /// Syntax: extern fn name(param: Type, ...) -> ReturnType
    pub(super) fn parse_extern_fn(&mut self) -> Result<ExternFn, String> {
        let start = self.current_span();
        self.expect(&Token::Extern)?;
        self.expect(&Token::Fn)?;
        let name = self.expect_ident()?;

        self.expect(&Token::LParen)?;
        let params = self.parse_params()?;
        self.expect(&Token::RParen)?;

        let return_type = if self.check(&Token::Arrow) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };

        // Consume optional newline
        self.skip_newlines();

        Ok(ExternFn {
            name,
            params,
            return_type,
            span: start,
        })
    }
}
