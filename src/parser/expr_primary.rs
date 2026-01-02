//! Primary expression parsing for WASD.

use super::ast::*;
use super::Parser;
use crate::lexer::Token;

impl<'a> Parser<'a> {
    pub(super) fn parse_primary(&mut self) -> Result<Expr, String> {
        let span = self.current_span();
        match self.peek() {
            Token::Int(n) => {
                let val = n;
                self.advance();
                Ok(Expr::Int(val, span))
            }
            Token::Float(n) => {
                let val = n;
                self.advance();
                Ok(Expr::Float(val, span))
            }
            Token::String(s) => {
                let val = s.clone();
                self.advance();
                Ok(Expr::String(val, span))
            }
            Token::Bool(b) => {
                let val = b;
                self.advance();
                Ok(Expr::Bool(val, span))
            }
            Token::Ident(name) => {
                let val = name.clone();
                self.advance();

                // Check for enum construction syntax: Type::Variant(value)
                if self.check(&Token::DoubleColon) {
                    self.advance();
                    let variant = self.expect_ident()?;

                    // Check for value in parentheses
                    let value = if self.check(&Token::LParen) {
                        self.advance();
                        if self.check(&Token::RParen) {
                            self.advance();
                            None
                        } else {
                            let v = self.parse_expr()?;
                            self.expect(&Token::RParen)?;
                            Some(Box::new(v))
                        }
                    } else {
                        None
                    };

                    return Ok(Expr::EnumConstruct {
                        enum_name: Some(val),
                        variant,
                        value,
                        span,
                    });
                }

                Ok(Expr::Ident(val, span))
            }
            Token::LParen => {
                self.advance();
                let expr = self.parse_expr()?;
                self.expect(&Token::RParen)?;
                Ok(expr)
            }
            Token::If => self.parse_if(),
            Token::Match => self.parse_match(),
            Token::SelfKeyword => {
                self.advance();
                Ok(Expr::Ident("self".to_string(), span))
            }
            Token::Pipe => self.parse_closure(),
            _ => Err(format!("Unexpected token: {:?}", self.peek())),
        }
    }

    /// Parse a closure expression: |params| expr or || expr
    fn parse_closure(&mut self) -> Result<Expr, String> {
        let span = self.current_span();
        self.expect(&Token::Pipe)?;

        // Parse parameters
        let mut params = Vec::new();
        if !self.check(&Token::Pipe) {
            loop {
                let param_span = self.current_span();
                let name = self.expect_ident()?;

                // Optional type annotation
                let ty = if self.check(&Token::Colon) {
                    self.advance();
                    self.parse_type()?
                } else {
                    // Infer type later
                    Type::Named("_".to_string())
                };

                params.push(Param {
                    name,
                    ty,
                    span: param_span,
                });

                if self.check(&Token::Comma) {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        self.expect(&Token::Pipe)?;

        // Parse body expression
        let body = self.parse_expr()?;

        Ok(Expr::Lambda {
            params,
            body: Box::new(body),
            span,
        })
    }

    fn parse_if(&mut self) -> Result<Expr, String> {
        let span = self.current_span();
        self.expect(&Token::If)?;
        let condition = self.parse_expr()?;
        self.skip_newlines();
        self.expect(&Token::Indent)?;
        let then_branch = self.parse_block()?;

        let else_branch = if self.check(&Token::Else) {
            self.advance();
            self.skip_newlines();
            self.expect(&Token::Indent)?;
            Some(self.parse_block()?)
        } else {
            None
        };

        Ok(Expr::If(
            Box::new(condition),
            then_branch,
            else_branch,
            span,
        ))
    }

    fn parse_match(&mut self) -> Result<Expr, String> {
        let span = self.current_span();
        self.expect(&Token::Match)?;
        let value = self.parse_expr()?;
        self.skip_newlines();
        self.expect(&Token::Indent)?;

        let mut arms = Vec::new();
        while !self.check(&Token::Dedent) && !self.is_at_end() {
            self.skip_newlines();
            if self.check(&Token::Dedent) {
                break;
            }
            let pattern = self.parse_pattern()?;
            self.expect(&Token::FatArrow)?;
            let body = self.parse_expr()?;
            arms.push(MatchArm {
                pattern,
                body,
                span: self.current_span(),
            });
            self.skip_newlines();
        }

        if self.check(&Token::Dedent) {
            self.advance();
        }

        Ok(Expr::Match(Box::new(value), arms, span))
    }

    pub(super) fn parse_pattern(&mut self) -> Result<Pattern, String> {
        let span = self.current_span();
        match self.peek() {
            Token::Ident(name) if name == "_" => {
                self.advance();
                Ok(Pattern::Wildcard(span))
            }
            Token::Ident(name) => {
                let name = name.clone();
                self.advance();

                // Check for EnumType::Variant or EnumType::Variant(patterns) syntax
                if self.check(&Token::DoubleColon) {
                    self.advance();
                    let variant = self.expect_ident()?;

                    // Check for patterns in parentheses
                    if self.check(&Token::LParen) {
                        self.advance();
                        let mut patterns = Vec::new();
                        while !self.check(&Token::RParen) {
                            patterns.push(self.parse_pattern()?);
                            if !self.check(&Token::RParen) {
                                self.expect(&Token::Comma)?;
                            }
                        }
                        self.expect(&Token::RParen)?;
                        // Use "EnumType::Variant" as the constructor name for qualified patterns
                        Ok(Pattern::Constructor(format!("{}::{}", name, variant), patterns, span))
                    } else {
                        // No payload - unit variant like Option::None
                        Ok(Pattern::Constructor(format!("{}::{}", name, variant), vec![], span))
                    }
                } else if self.check(&Token::LParen) {
                    self.advance();
                    let mut patterns = Vec::new();
                    while !self.check(&Token::RParen) {
                        patterns.push(self.parse_pattern()?);
                        if !self.check(&Token::RParen) {
                            self.expect(&Token::Comma)?;
                        }
                    }
                    self.expect(&Token::RParen)?;
                    Ok(Pattern::Constructor(name, patterns, span))
                } else {
                    Ok(Pattern::Ident(name, span))
                }
            }
            Token::Int(_) | Token::Float(_) | Token::String(_) | Token::Bool(_) => {
                let expr = self.parse_primary()?;
                Ok(Pattern::Literal(expr))
            }
            _ => Err(format!("Expected pattern, found {:?}", self.peek())),
        }
    }
}
