//! Transform functions for monomorphization.

use super::monomorphize::{MonoKey, Monomorphizer};
use crate::parser::{Expr, Function, Param, Pattern, Stmt, Type};

impl Monomorphizer {
    pub(super) fn transform_function(&self, func: &Function) -> Function {
        // Transform all types in the function to use specialized names
        let params: Vec<Param> = func
            .params
            .iter()
            .map(|p| Param {
                name: p.name.clone(),
                ty: self.transform_type(&p.ty),
                span: p.span.clone(),
            })
            .collect();

        let return_type = func.return_type.as_ref().map(|t| self.transform_type(t));

        let body: Vec<Stmt> = func.body.iter().map(|s| self.transform_stmt(s)).collect();

        Function {
            visibility: func.visibility,
            is_async: func.is_async,
            name: func.name.clone(),
            generics: Vec::new(), // Generics are resolved during monomorphization
            params,
            return_type,
            effects: func.effects.clone(),
            body,
            span: func.span.clone(),
        }
    }

    pub(super) fn transform_type(&self, ty: &Type) -> Type {
        match ty {
            Type::Generic(name, args) => {
                // Transform to specialized name
                let new_args: Vec<Type> = args.iter().map(|a| self.transform_type(a)).collect();
                let key = MonoKey::new(name.clone(), new_args);
                Type::Named(key.mangled_name())
            }
            Type::Reference(inner, is_mut) => {
                Type::Reference(Box::new(self.transform_type(inner)), *is_mut)
            }
            Type::Heap(inner) => Type::Heap(Box::new(self.transform_type(inner))),
            Type::Rc(inner) => Type::Rc(Box::new(self.transform_type(inner))),
            Type::Arc(inner) => Type::Arc(Box::new(self.transform_type(inner))),
            Type::Function(params, ret) => {
                let new_params: Vec<Type> = params.iter().map(|p| self.transform_type(p)).collect();
                let new_ret = self.transform_type(ret);
                Type::Function(new_params, Box::new(new_ret))
            }
            _ => ty.clone(),
        }
    }

    pub(super) fn transform_stmt(&self, stmt: &Stmt) -> Stmt {
        match stmt {
            Stmt::Let {
                name,
                ty,
                mutable,
                value,
                span,
            } => Stmt::Let {
                name: name.clone(),
                ty: ty.as_ref().map(|t| self.transform_type(t)),
                mutable: *mutable,
                value: self.transform_expr(value),
                span: span.clone(),
            },
            Stmt::Assign {
                target,
                value,
                span,
            } => Stmt::Assign {
                target: self.transform_expr(target),
                value: self.transform_expr(value),
                span: span.clone(),
            },
            Stmt::Expr(e) => Stmt::Expr(self.transform_expr(e)),
            Stmt::Return(Some(e), span) => Stmt::Return(Some(self.transform_expr(e)), span.clone()),
            Stmt::Return(None, span) => Stmt::Return(None, span.clone()),
            Stmt::While {
                condition,
                body,
                span,
            } => Stmt::While {
                condition: self.transform_expr(condition),
                body: body.iter().map(|s| self.transform_stmt(s)).collect(),
                span: span.clone(),
            },
            Stmt::For {
                var,
                iterable,
                body,
                span,
            } => Stmt::For {
                var: var.clone(),
                iterable: self.transform_expr(iterable),
                body: body.iter().map(|s| self.transform_stmt(s)).collect(),
                span: span.clone(),
            },
            Stmt::Break(span) => Stmt::Break(span.clone()),
            Stmt::Continue(span) => Stmt::Continue(span.clone()),
        }
    }

    pub(super) fn transform_expr(&self, expr: &Expr) -> Expr {
        match expr {
            Expr::Int(n, span) => Expr::Int(*n, span.clone()),
            Expr::Float(n, span) => Expr::Float(*n, span.clone()),
            Expr::String(s, span) => Expr::String(s.clone(), span.clone()),
            Expr::Bool(b, span) => Expr::Bool(*b, span.clone()),
            Expr::Ident(name, span) => Expr::Ident(name.clone(), span.clone()),
            Expr::Binary(l, op, r, span) => Expr::Binary(
                Box::new(self.transform_expr(l)),
                *op,
                Box::new(self.transform_expr(r)),
                span.clone(),
            ),
            Expr::Unary(op, e, span) => {
                Expr::Unary(*op, Box::new(self.transform_expr(e)), span.clone())
            }
            Expr::Call(callee, type_args, args, span) => Expr::Call(
                Box::new(self.transform_expr(callee)),
                type_args.iter().map(|t| self.transform_type(t)).collect(),
                args.iter().map(|a| self.transform_expr(a)).collect(),
                span.clone(),
            ),
            Expr::FieldAccess(base, field, span) => {
                Expr::FieldAccess(Box::new(self.transform_expr(base)), field.clone(), span.clone())
            }
            Expr::StructConstruct { name, fields, span } => {
                // TODO: Transform name if it's a generic instantiation
                Expr::StructConstruct {
                    name: name.clone(),
                    fields: fields
                        .iter()
                        .map(|(n, e)| (n.clone(), self.transform_expr(e)))
                        .collect(),
                    span: span.clone(),
                }
            }
            Expr::EnumConstruct {
                enum_name,
                variant,
                value,
                span,
            } => Expr::EnumConstruct {
                enum_name: enum_name.clone(),
                variant: variant.clone(),
                value: value.as_ref().map(|v| Box::new(self.transform_expr(v))),
                span: span.clone(),
            },
            Expr::If(cond, then_branch, else_branch, span) => Expr::If(
                Box::new(self.transform_expr(cond)),
                then_branch.iter().map(|s| self.transform_stmt(s)).collect(),
                else_branch
                    .as_ref()
                    .map(|stmts| stmts.iter().map(|s| self.transform_stmt(s)).collect()),
                span.clone(),
            ),
            Expr::Match(value, arms, span) => Expr::Match(
                Box::new(self.transform_expr(value)),
                arms.iter()
                    .map(|arm| crate::parser::MatchArm {
                        pattern: self.transform_pattern(&arm.pattern),
                        body: self.transform_expr(&arm.body),
                        span: arm.span.clone(),
                    })
                    .collect(),
                span.clone(),
            ),
            Expr::Block(stmts, span) => Expr::Block(
                stmts.iter().map(|s| self.transform_stmt(s)).collect(),
                span.clone(),
            ),
            Expr::HeapAlloc(e, span) => {
                Expr::HeapAlloc(Box::new(self.transform_expr(e)), span.clone())
            }
            Expr::RcAlloc(e, span) => {
                Expr::RcAlloc(Box::new(self.transform_expr(e)), span.clone())
            }
            Expr::ArcAlloc(e, span) => {
                Expr::ArcAlloc(Box::new(self.transform_expr(e)), span.clone())
            }
            Expr::Lambda { params, body, span } => Expr::Lambda {
                params: params
                    .iter()
                    .map(|p| Param {
                        name: p.name.clone(),
                        ty: self.transform_type(&p.ty),
                        span: p.span.clone(),
                    })
                    .collect(),
                body: Box::new(self.transform_expr(body)),
                span: span.clone(),
            },
            Expr::Range { start, end, span } => Expr::Range {
                start: Box::new(self.transform_expr(start)),
                end: Box::new(self.transform_expr(end)),
                span: span.clone(),
            },
            Expr::Await(e, span) => {
                Expr::Await(Box::new(self.transform_expr(e)), span.clone())
            }
        }
    }

    pub(super) fn transform_pattern(&self, pattern: &Pattern) -> Pattern {
        match pattern {
            Pattern::Wildcard(span) => Pattern::Wildcard(span.clone()),
            Pattern::Ident(name, span) => Pattern::Ident(name.clone(), span.clone()),
            Pattern::Literal(e) => Pattern::Literal(self.transform_expr(e)),
            Pattern::Constructor(name, patterns, span) => Pattern::Constructor(
                name.clone(),
                patterns.iter().map(|p| self.transform_pattern(p)).collect(),
                span.clone(),
            ),
        }
    }
}
