//! Expression parsing for WASD.

use super::ast::*;
use super::Parser;
use crate::lexer::Token;

impl<'a> Parser<'a> {
    /// Parse an expression.
    pub(super) fn parse_expr(&mut self) -> Result<Expr, String> {
        self.parse_or()
    }

    fn parse_or(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_and()?;
        while self.check(&Token::Or) {
            let span = self.current_span();
            self.advance();
            let right = self.parse_and()?;
            left = Expr::Binary(Box::new(left), BinOp::Or, Box::new(right), span);
        }
        Ok(left)
    }

    fn parse_and(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_equality()?;
        while self.check(&Token::And) {
            let span = self.current_span();
            self.advance();
            let right = self.parse_equality()?;
            left = Expr::Binary(Box::new(left), BinOp::And, Box::new(right), span);
        }
        Ok(left)
    }

    fn parse_equality(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_comparison()?;
        while self.check(&Token::EqEq) || self.check(&Token::NotEq) {
            let span = self.current_span();
            let op = if self.check(&Token::EqEq) {
                BinOp::Eq
            } else {
                BinOp::NotEq
            };
            self.advance();
            let right = self.parse_comparison()?;
            left = Expr::Binary(Box::new(left), op, Box::new(right), span);
        }
        Ok(left)
    }

    fn parse_comparison(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_range()?;
        while self.check(&Token::Lt)
            || self.check(&Token::LtEq)
            || self.check(&Token::Gt)
            || self.check(&Token::GtEq)
        {
            let span = self.current_span();
            let op = match self.peek() {
                Token::Lt => BinOp::Lt,
                Token::LtEq => BinOp::LtEq,
                Token::Gt => BinOp::Gt,
                Token::GtEq => BinOp::GtEq,
                _ => unreachable!(),
            };
            self.advance();
            let right = self.parse_range()?;
            left = Expr::Binary(Box::new(left), op, Box::new(right), span);
        }
        Ok(left)
    }

    fn parse_range(&mut self) -> Result<Expr, String> {
        let left = self.parse_term()?;
        if self.check(&Token::DotDot) {
            let span = self.current_span();
            self.advance();
            let right = self.parse_term()?;
            Ok(Expr::Range {
                start: Box::new(left),
                end: Box::new(right),
                span,
            })
        } else {
            Ok(left)
        }
    }

    fn parse_term(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_factor()?;
        while self.check(&Token::Plus) || self.check(&Token::Minus) {
            let span = self.current_span();
            let op = if self.check(&Token::Plus) {
                BinOp::Add
            } else {
                BinOp::Sub
            };
            self.advance();
            let right = self.parse_factor()?;
            left = Expr::Binary(Box::new(left), op, Box::new(right), span);
        }
        Ok(left)
    }

    fn parse_factor(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_unary()?;
        while self.check(&Token::Star) || self.check(&Token::Slash) || self.check(&Token::Percent) {
            let span = self.current_span();
            let op = match self.peek() {
                Token::Star => BinOp::Mul,
                Token::Slash => BinOp::Div,
                Token::Percent => BinOp::Mod,
                _ => unreachable!(),
            };
            self.advance();
            let right = self.parse_unary()?;
            left = Expr::Binary(Box::new(left), op, Box::new(right), span);
        }
        Ok(left)
    }

    fn parse_unary(&mut self) -> Result<Expr, String> {
        if self.check(&Token::Minus) {
            let span = self.current_span();
            self.advance();
            let expr = self.parse_unary()?;
            return Ok(Expr::Unary(UnaryOp::Neg, Box::new(expr), span));
        }
        if self.check(&Token::Not) {
            let span = self.current_span();
            self.advance();
            let expr = self.parse_unary()?;
            return Ok(Expr::Unary(UnaryOp::Not, Box::new(expr), span));
        }
        // Heap allocation: heap expr
        if self.check(&Token::Heap) {
            let span = self.current_span();
            self.advance();
            let expr = self.parse_unary()?;
            return Ok(Expr::HeapAlloc(Box::new(expr), span));
        }
        // Reference-counted allocation: rc expr
        if self.check(&Token::Rc) {
            let span = self.current_span();
            self.advance();
            let expr = self.parse_unary()?;
            return Ok(Expr::RcAlloc(Box::new(expr), span));
        }
        // Atomically reference-counted allocation: arc expr
        if self.check(&Token::Arc) {
            let span = self.current_span();
            self.advance();
            let expr = self.parse_unary()?;
            return Ok(Expr::ArcAlloc(Box::new(expr), span));
        }
        self.parse_call()
    }

    fn parse_call(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_primary()?;

        loop {
            // Check for generic type arguments: ident[T, U, ...](args)
            if self.check(&Token::LBracket) {
                let span = self.current_span();
                self.advance();
                let mut type_args = Vec::new();
                while !self.check(&Token::RBracket) {
                    type_args.push(self.parse_type()?);
                    if !self.check(&Token::RBracket) {
                        self.expect(&Token::Comma)?;
                    }
                }
                self.expect(&Token::RBracket)?;

                // Now we must have a function call
                self.expect(&Token::LParen)?;
                let mut args = Vec::new();
                while !self.check(&Token::RParen) {
                    args.push(self.parse_expr()?);
                    if !self.check(&Token::RParen) {
                        self.expect(&Token::Comma)?;
                    }
                }
                self.expect(&Token::RParen)?;
                expr = Expr::Call(Box::new(expr), type_args, args, span);
            } else if self.check(&Token::LParen) {
                let span = self.current_span();
                self.advance();

                // Check if this is a struct construction with named fields
                // by looking ahead for "ident:"
                if let Expr::Ident(name, _) = &expr {
                    let name = name.clone();
                    if let Some(fields) = self.try_parse_struct_fields()? {
                        self.expect(&Token::RParen)?;
                        expr = Expr::StructConstruct {
                            name,
                            fields,
                            span,
                        };
                        continue;
                    }
                }

                // Regular function call (no type arguments)
                let mut args = Vec::new();
                while !self.check(&Token::RParen) {
                    args.push(self.parse_expr()?);
                    if !self.check(&Token::RParen) {
                        self.expect(&Token::Comma)?;
                    }
                }
                self.expect(&Token::RParen)?;
                expr = Expr::Call(Box::new(expr), Vec::new(), args, span);
            } else if self.check(&Token::Dot) {
                let span = self.current_span();
                self.advance();
                let field = self.expect_ident()?;
                expr = Expr::FieldAccess(Box::new(expr), field, span);
            } else {
                break;
            }
        }

        Ok(expr)
    }

    /// Try to parse struct fields like `x: 1, y: 2`
    /// Returns None if not a struct construction (no colon after first ident)
    fn try_parse_struct_fields(&mut self) -> Result<Option<Vec<(String, Expr)>>, String> {
        // Save position
        let saved_pos = self.pos;

        // Check if first arg looks like "ident:"
        if let Token::Ident(first_name) = self.peek() {
            let first_name = first_name.clone();
            self.advance();

            if self.check(&Token::Colon) {
                self.advance();
                let first_value = self.parse_expr()?;

                let mut fields = vec![(first_name, first_value)];

                while self.check(&Token::Comma) {
                    self.advance();
                    if self.check(&Token::RParen) {
                        break;
                    }
                    let field_name = self.expect_ident()?;
                    self.expect(&Token::Colon)?;
                    let field_value = self.parse_expr()?;
                    fields.push((field_name, field_value));
                }

                return Ok(Some(fields));
            }

            // Not a struct construction, restore position
            self.pos = saved_pos;
        }

        Ok(None)
    }
}
