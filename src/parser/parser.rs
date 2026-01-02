//! Recursive descent parser for WASD.

#![allow(dead_code)]

use super::ast::*;
use crate::lexer::{Lexer, Span, Token};

/// The WASD parser.
pub struct Parser<'a> {
    #[allow(dead_code)]
    lexer: Lexer<'a>,
    tokens: Vec<(Token, Span)>,
    pos: usize,
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

    fn parse_item(&mut self) -> Result<Item, String> {
        match self.peek() {
            Token::Use => self.parse_use().map(Item::Use),
            Token::Fn => self.parse_function().map(Item::Function),
            Token::Struct => self.parse_struct().map(Item::Struct),
            Token::Enum => self.parse_enum().map(Item::Enum),
            _ => Err(format!("Expected item, found {:?}", self.peek())),
        }
    }

    fn parse_use(&mut self) -> Result<UseStmt, String> {
        let span = self.current_span();
        self.expect(&Token::Use)?;

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

    fn parse_function(&mut self) -> Result<Function, String> {
        let start = self.current_span();
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

        let effects = if self.check(&Token::With) {
            self.advance();
            self.parse_effects()?
        } else {
            Vec::new()
        };

        self.skip_newlines();
        self.expect(&Token::Indent)?;
        let body = self.parse_block()?;

        Ok(Function {
            name,
            params,
            return_type,
            effects,
            body,
            span: start,
        })
    }

    fn parse_struct(&mut self) -> Result<StructDef, String> {
        let span = self.current_span();
        self.expect(&Token::Struct)?;
        let name = self.expect_ident()?;
        let generics = self.parse_generics()?;

        self.skip_newlines();
        self.expect(&Token::Indent)?;

        let mut fields = Vec::new();
        while !self.check(&Token::Dedent) && !self.is_at_end() {
            self.skip_newlines();
            if self.check(&Token::Dedent) {
                break;
            }
            let field_name = self.expect_ident()?;
            self.expect(&Token::Colon)?;
            let ty = self.parse_type()?;
            fields.push(Field {
                name: field_name,
                ty,
                span: self.current_span(),
            });
            self.skip_newlines();
        }

        if self.check(&Token::Dedent) {
            self.advance();
        }

        Ok(StructDef {
            name,
            generics,
            fields,
            span,
        })
    }

    fn parse_enum(&mut self) -> Result<EnumDef, String> {
        let span = self.current_span();
        self.expect(&Token::Enum)?;
        let name = self.expect_ident()?;
        let generics = self.parse_generics()?;

        self.skip_newlines();
        self.expect(&Token::Indent)?;

        let mut variants = Vec::new();
        while !self.check(&Token::Dedent) && !self.is_at_end() {
            self.skip_newlines();
            if self.check(&Token::Dedent) {
                break;
            }
            let variant_name = self.expect_ident()?;
            let fields = if self.check(&Token::LParen) {
                self.advance();
                let mut types = Vec::new();
                while !self.check(&Token::RParen) {
                    types.push(self.parse_type()?);
                    if !self.check(&Token::RParen) {
                        self.expect(&Token::Comma)?;
                    }
                }
                self.expect(&Token::RParen)?;
                types
            } else {
                Vec::new()
            };
            variants.push(Variant {
                name: variant_name,
                fields,
                span: self.current_span(),
            });
            self.skip_newlines();
        }

        if self.check(&Token::Dedent) {
            self.advance();
        }

        Ok(EnumDef {
            name,
            generics,
            variants,
            span,
        })
    }

    fn parse_params(&mut self) -> Result<Vec<Param>, String> {
        let mut params = Vec::new();
        while !self.check(&Token::RParen) {
            let name = self.expect_ident()?;
            self.expect(&Token::Colon)?;
            let ty = self.parse_type()?;
            params.push(Param {
                name,
                ty,
                span: self.current_span(),
            });
            if !self.check(&Token::RParen) {
                self.expect(&Token::Comma)?;
            }
        }
        Ok(params)
    }

    fn parse_generics(&mut self) -> Result<Vec<String>, String> {
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

    fn parse_effects(&mut self) -> Result<Vec<String>, String> {
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

    fn parse_type(&mut self) -> Result<Type, String> {
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

    fn parse_block(&mut self) -> Result<Vec<Stmt>, String> {
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

    fn parse_stmt(&mut self) -> Result<Stmt, String> {
        if self.check(&Token::Let) {
            self.parse_let()
        } else if self.check(&Token::Return) {
            self.parse_return()
        } else {
            Ok(Stmt::Expr(self.parse_expr()?))
        }
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

    fn parse_expr(&mut self) -> Result<Expr, String> {
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
        let mut left = self.parse_term()?;
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
            let right = self.parse_term()?;
            left = Expr::Binary(Box::new(left), op, Box::new(right), span);
        }
        Ok(left)
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
        self.parse_call()
    }

    fn parse_call(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_primary()?;

        loop {
            if self.check(&Token::LParen) {
                let span = self.current_span();
                self.advance();
                let mut args = Vec::new();
                while !self.check(&Token::RParen) {
                    args.push(self.parse_expr()?);
                    if !self.check(&Token::RParen) {
                        self.expect(&Token::Comma)?;
                    }
                }
                self.expect(&Token::RParen)?;
                expr = Expr::Call(Box::new(expr), args, span);
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

    fn parse_primary(&mut self) -> Result<Expr, String> {
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
            _ => Err(format!("Unexpected token: {:?}", self.peek())),
        }
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

    fn parse_pattern(&mut self) -> Result<Pattern, String> {
        let span = self.current_span();
        match self.peek() {
            Token::Ident(name) if name == "_" => {
                self.advance();
                Ok(Pattern::Wildcard(span))
            }
            Token::Ident(name) => {
                let name = name.clone();
                self.advance();
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

    // Helper methods

    fn peek(&self) -> Token {
        self.tokens
            .get(self.pos)
            .map(|(t, _): &(Token, Span)| t.clone())
            .unwrap_or(Token::Eof)
    }

    fn current_span(&self) -> Span {
        self.tokens
            .get(self.pos)
            .map(|(_, s)| *s)
            .unwrap_or(Span::new(0, 0))
    }

    fn advance(&mut self) -> Token {
        let token = self.peek();
        self.pos += 1;
        token
    }

    fn check(&self, token: &Token) -> bool {
        std::mem::discriminant(&self.peek()) == std::mem::discriminant(token)
    }

    fn expect(&mut self, token: &Token) -> Result<(), String> {
        if self.check(token) {
            self.advance();
            Ok(())
        } else {
            Err(format!("Expected {:?}, found {:?}", token, self.peek()))
        }
    }

    fn expect_ident(&mut self) -> Result<String, String> {
        match self.peek() {
            Token::Ident(name) => {
                self.advance();
                Ok(name)
            }
            _ => Err(format!("Expected identifier, found {:?}", self.peek())),
        }
    }

    fn skip_newlines(&mut self) {
        while self.check(&Token::Newline) {
            self.advance();
        }
    }

    fn is_at_end(&self) -> bool {
        matches!(self.peek(), Token::Eof)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_function() {
        let source = r#"fn add(a: i64, b: i64) -> i64
    a + b
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();
        assert_eq!(program.items.len(), 1);

        if let Item::Function(f) = &program.items[0] {
            assert_eq!(f.name, "add");
            assert_eq!(f.params.len(), 2);
            assert_eq!(f.params[0].name, "a");
            assert_eq!(f.params[1].name, "b");
            assert!(f.return_type.is_some());
        } else {
            panic!("Expected function");
        }
    }

    #[test]
    fn test_parse_struct() {
        let source = r#"struct Point
    x: f64
    y: f64
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();
        assert_eq!(program.items.len(), 1);

        if let Item::Struct(s) = &program.items[0] {
            assert_eq!(s.name, "Point");
            assert_eq!(s.fields.len(), 2);
            assert_eq!(s.fields[0].name, "x");
            assert_eq!(s.fields[1].name, "y");
        } else {
            panic!("Expected struct");
        }
    }

    #[test]
    fn test_parse_generic_struct() {
        let source = r#"struct Container[T]
    value: T
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Struct(s) = &program.items[0] {
            assert_eq!(s.name, "Container");
            assert_eq!(s.generics, vec!["T"]);
        } else {
            panic!("Expected struct");
        }
    }

    #[test]
    fn test_parse_enum() {
        let source = r#"enum Option[T]
    Some(T)
    None
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Enum(e) = &program.items[0] {
            assert_eq!(e.name, "Option");
            assert_eq!(e.generics, vec!["T"]);
            assert_eq!(e.variants.len(), 2);
            assert_eq!(e.variants[0].name, "Some");
            assert_eq!(e.variants[0].fields.len(), 1);
            assert_eq!(e.variants[1].name, "None");
            assert_eq!(e.variants[1].fields.len(), 0);
        } else {
            panic!("Expected enum");
        }
    }

    #[test]
    fn test_parse_let_statement() {
        let source = r#"fn main()
    let x = 42
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Function(f) = &program.items[0] {
            assert_eq!(f.body.len(), 1);
            if let Stmt::Let {
                name,
                mutable,
                value,
                ..
            } = &f.body[0]
            {
                assert_eq!(name, "x");
                assert!(!mutable);
                if let Expr::Int(n, _) = value {
                    assert_eq!(*n, 42);
                } else {
                    panic!("Expected int literal");
                }
            } else {
                panic!("Expected let statement");
            }
        }
    }

    #[test]
    fn test_parse_mutable_let() {
        let source = r#"fn main()
    let mut counter = 0
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Function(f) = &program.items[0] {
            if let Stmt::Let { mutable, .. } = &f.body[0] {
                assert!(mutable);
            } else {
                panic!("Expected let statement");
            }
        }
    }

    #[test]
    fn test_parse_binary_expressions() {
        let source = r#"fn main()
    1 + 2 * 3
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Function(f) = &program.items[0] {
            if let Stmt::Expr(Expr::Binary(left, BinOp::Add, right, _)) = &f.body[0] {
                // Verify precedence: should be 1 + (2 * 3)
                if let Expr::Int(1, _) = left.as_ref() {
                } else {
                    panic!("Expected 1 on left");
                }
                if let Expr::Binary(_, BinOp::Mul, _, _) = right.as_ref() {
                } else {
                    panic!("Expected multiplication on right");
                }
            } else {
                panic!("Expected binary expression");
            }
        }
    }

    #[test]
    fn test_parse_comparison_operators() {
        let source = r#"fn main()
    x < 10
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Function(f) = &program.items[0] {
            if let Stmt::Expr(Expr::Binary(_, BinOp::Lt, _, _)) = &f.body[0] {
            } else {
                panic!("Expected comparison expression");
            }
        }
    }

    #[test]
    fn test_parse_function_call() {
        let source = r#"fn main()
    add(1, 2)
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Function(f) = &program.items[0] {
            if let Stmt::Expr(Expr::Call(callee, args, _)) = &f.body[0] {
                if let Expr::Ident(name, _) = callee.as_ref() {
                    assert_eq!(name, "add");
                }
                assert_eq!(args.len(), 2);
            } else {
                panic!("Expected function call");
            }
        }
    }

    #[test]
    fn test_parse_return_statement() {
        let source = r#"fn add(a: i64, b: i64) -> i64
    return a + b
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Function(f) = &program.items[0] {
            if let Stmt::Return(Some(_), _) = &f.body[0] {
            } else {
                panic!("Expected return statement");
            }
        }
    }

    #[test]
    fn test_parse_effects() {
        let source = r#"fn read_file() -> String with [IO]
    file_contents
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Function(f) = &program.items[0] {
            assert_eq!(f.effects, vec!["IO"]);
        } else {
            panic!("Expected function");
        }
    }

    #[test]
    fn test_parse_reference_types() {
        let source = r#"fn borrow(x: &i64) -> i64
    x
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Function(f) = &program.items[0] {
            if let Type::Reference(inner, is_mut) = &f.params[0].ty {
                assert!(!is_mut);
                if let Type::Named(name) = inner.as_ref() {
                    assert_eq!(name, "i64");
                }
            } else {
                panic!("Expected reference type");
            }
        }
    }

    #[test]
    fn test_parse_mutable_reference() {
        let source = r#"fn mutate(x: &mut i64)
    x
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Function(f) = &program.items[0] {
            if let Type::Reference(_, is_mut) = &f.params[0].ty {
                assert!(is_mut);
            } else {
                panic!("Expected mutable reference type");
            }
        }
    }

    #[test]
    fn test_parse_unary_operators() {
        let source = r#"fn main()
    -x
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Function(f) = &program.items[0] {
            if let Stmt::Expr(Expr::Unary(UnaryOp::Neg, _, _)) = &f.body[0] {
            } else {
                panic!("Expected unary negation");
            }
        }
    }

    #[test]
    fn test_parse_boolean_literal() {
        let source = r#"fn main()
    let flag = true
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Function(f) = &program.items[0] {
            if let Stmt::Let {
                value: Expr::Bool(true, _),
                ..
            } = &f.body[0]
            {
            } else {
                panic!("Expected boolean true");
            }
        }
    }

    #[test]
    fn test_parse_float_literal() {
        let source = r#"fn main()
    let pi = 3.14
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Function(f) = &program.items[0] {
            if let Stmt::Let {
                value: Expr::Float(n, _),
                ..
            } = &f.body[0]
            {
                assert!((*n - 3.14).abs() < 0.001);
            } else {
                panic!("Expected float literal");
            }
        }
    }

    #[test]
    fn test_parse_logical_operators() {
        let source = r#"fn main()
    a and b or c
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Function(f) = &program.items[0] {
            if let Stmt::Expr(Expr::Binary(_, BinOp::Or, _, _)) = &f.body[0] {
                // And has higher precedence, so Or should be outer
            } else {
                panic!("Expected logical expression with Or as outer operator");
            }
        }
    }

    #[test]
    fn test_parse_field_access() {
        let source = r#"fn main()
    point.x
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Function(f) = &program.items[0] {
            if let Stmt::Expr(Expr::FieldAccess(obj, field, _)) = &f.body[0] {
                if let Expr::Ident(name, _) = obj.as_ref() {
                    assert_eq!(name, "point");
                }
                assert_eq!(field, "x");
            } else {
                panic!("Expected field access");
            }
        }
    }

    #[test]
    fn test_parse_heap_type() {
        let source = r#"fn alloc() -> heap i64
    x
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Function(f) = &program.items[0] {
            if let Some(Type::Heap(inner)) = &f.return_type {
                if let Type::Named(name) = inner.as_ref() {
                    assert_eq!(name, "i64");
                }
            } else {
                panic!("Expected heap type");
            }
        }
    }
}
