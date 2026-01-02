//! Type and impl parsing for WASD (structs, enums, traits, impls).

use super::ast::*;
use super::Parser;
use crate::lexer::Token;

impl<'a> Parser<'a> {
    /// Parse a struct definition.
    #[allow(dead_code)]
    pub(super) fn parse_struct(&mut self) -> Result<StructDef, String> {
        self.parse_struct_with_visibility(false)
    }

    /// Parse a struct definition with visibility.
    pub(super) fn parse_struct_with_visibility(&mut self, is_pub: bool) -> Result<StructDef, String> {
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

        let visibility = if is_pub {
            Visibility::Public
        } else {
            Visibility::Private
        };

        Ok(StructDef {
            visibility,
            name,
            generics,
            fields,
            span,
        })
    }

    /// Parse an enum definition.
    #[allow(dead_code)]
    pub(super) fn parse_enum(&mut self) -> Result<EnumDef, String> {
        self.parse_enum_with_visibility(false)
    }

    /// Parse an enum definition with visibility.
    pub(super) fn parse_enum_with_visibility(&mut self, is_pub: bool) -> Result<EnumDef, String> {
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

        let visibility = if is_pub {
            Visibility::Public
        } else {
            Visibility::Private
        };

        Ok(EnumDef {
            visibility,
            name,
            generics,
            variants,
            span,
        })
    }

    /// Parse a trait definition.
    pub(super) fn parse_trait(&mut self) -> Result<TraitDef, String> {
        let span = self.current_span();
        self.expect(&Token::Trait)?;

        let name = self.expect_ident()?;
        let generics = self.parse_generics()?;

        self.skip_newlines();

        let mut methods = Vec::new();

        // Expect indented block of methods
        if !self.check(&Token::Indent) {
            return Ok(TraitDef {
                name,
                generics,
                methods,
                span,
            });
        }
        self.advance(); // consume Indent

        while !self.check(&Token::Dedent) && !self.is_at_end() {
            self.skip_newlines();
            if self.check(&Token::Dedent) {
                break;
            }
            methods.push(self.parse_trait_method()?);
            self.skip_newlines();
        }

        if self.check(&Token::Dedent) {
            self.advance();
        }

        Ok(TraitDef {
            name,
            generics,
            methods,
            span,
        })
    }

    /// Parse a trait method signature.
    pub(super) fn parse_trait_method(&mut self) -> Result<TraitMethod, String> {
        let span = self.current_span();
        self.expect(&Token::Fn)?;

        let name = self.expect_ident()?;
        self.expect(&Token::LParen)?;
        let params = self.parse_params()?;
        self.expect(&Token::RParen)?;

        // Parse optional return type
        let return_type = if self.check(&Token::Arrow) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };

        // Check for default implementation (indented body)
        self.skip_newlines();
        let body = if self.check(&Token::Indent) {
            self.advance();
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
            Some(stmts)
        } else {
            None
        };

        Ok(TraitMethod {
            name,
            params,
            return_type,
            body,
            span,
        })
    }

    /// Parse an impl block.
    pub(super) fn parse_impl(&mut self) -> Result<ImplDef, String> {
        let span = self.current_span();
        self.expect(&Token::Impl)?;

        // Parse trait name (may have generics)
        let trait_or_type = self.expect_ident()?;
        let trait_generics = if self.check(&Token::LBracket) {
            self.advance();
            let mut args = Vec::new();
            while !self.check(&Token::RBracket) {
                args.push(self.parse_type()?);
                if !self.check(&Token::RBracket) {
                    self.expect(&Token::Comma)?;
                }
            }
            self.expect(&Token::RBracket)?;
            args
        } else {
            Vec::new()
        };

        // Check if this is "impl Trait for Type" or "impl Type"
        let (trait_name, target_type) = if self.check(&Token::For) {
            self.advance();
            let target = self.parse_type()?;
            (Some(trait_or_type), target)
        } else {
            // Inherent impl: impl Type
            (None, Type::Named(trait_or_type))
        };

        self.skip_newlines();

        let mut methods = Vec::new();

        // Parse method implementations
        if !self.check(&Token::Indent) {
            return Ok(ImplDef {
                trait_name,
                trait_generics,
                target_type,
                methods,
                span,
            });
        }
        self.advance();

        while !self.check(&Token::Dedent) && !self.is_at_end() {
            self.skip_newlines();
            if self.check(&Token::Dedent) {
                break;
            }
            methods.push(self.parse_function()?);
            self.skip_newlines();
        }

        if self.check(&Token::Dedent) {
            self.advance();
        }

        Ok(ImplDef {
            trait_name,
            trait_generics,
            target_type,
            methods,
            span,
        })
    }
}
