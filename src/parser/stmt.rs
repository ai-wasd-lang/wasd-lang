//! Statement parsing for WASD.

use super::ast::*;
use super::Parser;
use crate::lexer::Token;

impl<'a> Parser<'a> {
    /// Parse a block of statements (after indent, until dedent).
    pub(super) fn parse_block(&mut self) -> Result<Vec<Stmt>, String> {
        let mut stmts = Vec::new();
        while !self.check(&Token::Dedent) && !self.is_at_end() {
            self.skip_newlines();
            if self.check(&Token::Dedent) {
                break;
            }
            stmts.push(self.parse_stmt()?);
            self.skip_newlines();
        }
        if self.check(&Token::Dedent) {
            self.advance();
        }
        Ok(stmts)
    }

    /// Parse a single statement.
    pub(super) fn parse_stmt(&mut self) -> Result<Stmt, String> {
        if self.check(&Token::Let) {
            self.parse_let()
        } else if self.check(&Token::Return) {
            self.parse_return()
        } else if self.check(&Token::While) {
            self.parse_while()
        } else if self.check(&Token::For) {
            self.parse_for()
        } else if self.check(&Token::Break) {
            let span = self.current_span();
            self.advance();
            Ok(Stmt::Break(span))
        } else if self.check(&Token::Continue) {
            let span = self.current_span();
            self.advance();
            Ok(Stmt::Continue(span))
        } else {
            // Could be an expression or an assignment
            let span = self.current_span();
            let expr = self.parse_expr()?;

            // Check for assignment
            if self.check(&Token::Eq) {
                self.advance();
                let value = self.parse_expr()?;
                Ok(Stmt::Assign {
                    target: expr,
                    value,
                    span,
                })
            } else {
                Ok(Stmt::Expr(expr))
            }
        }
    }

    fn parse_while(&mut self) -> Result<Stmt, String> {
        let span = self.current_span();
        self.expect(&Token::While)?;
        let condition = self.parse_expr()?;
        self.skip_newlines();
        self.expect(&Token::Indent)?;
        let body = self.parse_block()?;
        Ok(Stmt::While {
            condition,
            body,
            span,
        })
    }

    fn parse_for(&mut self) -> Result<Stmt, String> {
        let span = self.current_span();
        self.expect(&Token::For)?;
        let var = self.expect_ident()?;
        self.expect(&Token::In)?;
        let iterable = self.parse_expr()?;
        self.skip_newlines();
        self.expect(&Token::Indent)?;
        let body = self.parse_block()?;
        Ok(Stmt::For {
            var,
            iterable,
            body,
            span,
        })
    }

    fn parse_let(&mut self) -> Result<Stmt, String> {
        let span = self.current_span();
        self.expect(&Token::Let)?;
        let mutable = if self.check(&Token::Mut) {
            self.advance();
            true
        } else {
            false
        };
        let name = self.expect_ident()?;
        let ty = if self.check(&Token::Colon) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };
        self.expect(&Token::Eq)?;
        let value = self.parse_expr()?;
        Ok(Stmt::Let {
            name,
            mutable,
            ty,
            value,
            span,
        })
    }

    fn parse_return(&mut self) -> Result<Stmt, String> {
        let span = self.current_span();
        self.expect(&Token::Return)?;
        let value = if !self.check(&Token::Newline) && !self.check(&Token::Dedent) {
            Some(self.parse_expr()?)
        } else {
            None
        };
        Ok(Stmt::Return(value, span))
    }
}
