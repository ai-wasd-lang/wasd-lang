//! Type inference for expressions.

use super::checker::TypeChecker;
use super::types::WasdType;
use crate::parser::{BinOp, Expr, Pattern, Stmt, UnaryOp};

impl TypeChecker {
    pub fn check_stmt(&mut self, stmt: &Stmt) -> Result<WasdType, String> {
        match stmt {
            Stmt::Let {
                name, ty, value, ..
            } => {
                let value_ty = self.infer_expr(value)?;
                if let Some(declared_ty) = ty {
                    let declared = self.ast_type_to_wasd_type(declared_ty)?;
                    self.unify(&declared, &value_ty)?;
                }
                self.env.insert(name.clone(), value_ty.clone());
                Ok(value_ty)
            }
            Stmt::Assign { target, value, .. } => {
                let target_ty = self.infer_expr(target)?;
                let value_ty = self.infer_expr(value)?;
                self.unify(&target_ty, &value_ty)?;
                Ok(WasdType::Unit)
            }
            Stmt::Expr(e) => self.infer_expr(e),
            Stmt::Return(Some(e), _) => self.infer_expr(e),
            Stmt::Return(None, _) => Ok(WasdType::Unit),
            Stmt::While { condition, body, .. } => {
                let cond_ty = self.infer_expr(condition)?;
                self.unify(&WasdType::Bool, &cond_ty)?;
                for stmt in body {
                    self.check_stmt(stmt)?;
                }
                Ok(WasdType::Unit)
            }
            Stmt::For { var, iterable, body, .. } => {
                let _iter_ty = self.infer_expr(iterable)?;
                let saved = self.env.get(var).cloned();
                self.env.insert(var.clone(), WasdType::I64);
                for stmt in body {
                    self.check_stmt(stmt)?;
                }
                if let Some(prev) = saved {
                    self.env.insert(var.clone(), prev);
                } else {
                    self.env.remove(var);
                }
                Ok(WasdType::Unit)
            }
            Stmt::Break(_) | Stmt::Continue(_) => Ok(WasdType::Unit),
        }
    }

    pub fn infer_expr(&mut self, expr: &Expr) -> Result<WasdType, String> {
        match expr {
            Expr::Int(_, _) => Ok(WasdType::I64),
            Expr::Float(_, _) => Ok(WasdType::F64),
            Expr::String(_, _) => Ok(WasdType::String),
            Expr::Bool(_, _) => Ok(WasdType::Bool),
            Expr::Ident(name, _) => self
                .env
                .get(name)
                .cloned()
                .ok_or_else(|| format!("Undefined variable: {}", name)),
            Expr::Binary(left, op, right, _) => {
                let left_ty = self.infer_expr(left)?;
                let right_ty = self.infer_expr(right)?;
                self.check_binary_op(*op, &left_ty, &right_ty)
            }
            Expr::Unary(op, inner, _) => {
                let inner_ty = self.infer_expr(inner)?;
                self.check_unary_op(*op, &inner_ty)
            }
            Expr::Call(callee, type_args, args, _) => {
                // Skip type checking for generic function calls (they'll be monomorphized)
                if !type_args.is_empty() {
                    // Still type check the arguments
                    for arg in args {
                        self.infer_expr(arg)?;
                    }
                    return Ok(WasdType::Unknown);
                }

                // Handle method calls (callee is FieldAccess)
                if let Expr::FieldAccess(base, _method_name, _) = callee.as_ref() {
                    // Type check the receiver and arguments
                    self.infer_expr(base)?;
                    for arg in args {
                        self.infer_expr(arg)?;
                    }
                    // Return Unknown for now - method return types need impl lookup
                    return Ok(WasdType::Unknown);
                }

                let callee_ty = self.infer_expr(callee)?;
                match callee_ty {
                    WasdType::Function { params, ret, effects } => {
                        if params.len() != args.len() {
                            return Err(format!(
                                "Expected {} arguments, got {}",
                                params.len(),
                                args.len()
                            ));
                        }
                        for (param, arg) in params.iter().zip(args.iter()) {
                            let arg_ty = self.infer_expr(arg)?;
                            self.unify(param, &arg_ty)?;
                        }
                        // Validate effects: called function's effects must be subset of caller's effects
                        for effect in &effects {
                            if !self.current_effects.contains(effect) {
                                let callee_name = if let Expr::Ident(name, _) = callee.as_ref() {
                                    name.clone()
                                } else {
                                    "function".to_string()
                                };
                                return Err(format!(
                                    "Function '{}' has effect [{}] but caller does not declare it",
                                    callee_name, effect
                                ));
                            }
                        }
                        Ok(*ret)
                    }
                    _ => Err("Cannot call non-function".to_string()),
                }
            }
            Expr::If(cond, then_branch, else_branch, _) => {
                let cond_ty = self.infer_expr(cond)?;
                self.unify(&WasdType::Bool, &cond_ty)?;

                let then_ty = self.check_block(then_branch)?;
                if let Some(else_branch) = else_branch {
                    let else_ty = self.check_block(else_branch)?;
                    self.unify(&then_ty, &else_ty)?;
                }
                Ok(then_ty)
            }
            Expr::Block(stmts, _) => self.check_block(stmts),
            Expr::FieldAccess(base, _field, _) => {
                let _base_ty = self.infer_expr(base)?;
                Ok(WasdType::Unknown)
            }
            Expr::StructConstruct { name, fields, .. } => {
                for (_, value) in fields {
                    self.infer_expr(value)?;
                }
                Ok(WasdType::Named(name.clone()))
            }
            Expr::EnumConstruct { enum_name, value, .. } => {
                if let Some(v) = value {
                    self.infer_expr(v)?;
                }
                if let Some(name) = enum_name {
                    Ok(WasdType::Named(name.clone()))
                } else {
                    Ok(WasdType::Unknown)
                }
            }
            Expr::Match(value, arms, _) => {
                let value_ty = self.infer_expr(value)?;

                // Try to determine the enum type for exhaustiveness checking
                let enum_name = match &value_ty {
                    WasdType::Named(name) => Some(name.clone()),
                    WasdType::Generic(name, _) => Some(name.clone()),
                    _ => None,
                };

                // Check each arm
                for arm in arms {
                    let old_env = self.env.clone();
                    // Add pattern bindings to environment
                    self.add_pattern_bindings(&arm.pattern);
                    self.infer_expr(&arm.body)?;
                    self.env = old_env;
                }

                // Check exhaustiveness if we know the enum type
                if let Some(name) = enum_name {
                    if let Err(missing) = self.exhaustiveness.check_match(&name, arms) {
                        return Err(format!(
                            "Non-exhaustive match: missing patterns {}",
                            missing.join(", ")
                        ));
                    }
                }

                Ok(WasdType::Unknown)
            }
            Expr::HeapAlloc(inner, _) => {
                let inner_ty = self.infer_expr(inner)?;
                Ok(WasdType::Heap(Box::new(inner_ty)))
            }
            Expr::RcAlloc(inner, _) => {
                let inner_ty = self.infer_expr(inner)?;
                Ok(WasdType::Rc(Box::new(inner_ty)))
            }
            Expr::ArcAlloc(inner, _) => {
                let inner_ty = self.infer_expr(inner)?;
                Ok(WasdType::Arc(Box::new(inner_ty)))
            }
            Expr::Lambda { params, body, .. } => {
                let old_env = self.env.clone();

                for param in params {
                    let ty = self.ast_type_to_wasd_type(&param.ty)?;
                    self.env.insert(param.name.clone(), ty);
                }

                let ret_ty = self.infer_expr(body)?;
                self.env = old_env;

                let mut param_types = Vec::new();
                for p in params {
                    param_types.push(self.ast_type_to_wasd_type(&p.ty)?);
                }

                Ok(WasdType::Function {
                    params: param_types,
                    ret: Box::new(ret_ty),
                    effects: vec![],
                })
            }
            Expr::Range { start, end, .. } => {
                // Both start and end should be integers
                let start_ty = self.infer_expr(start)?;
                let end_ty = self.infer_expr(end)?;
                self.unify(&start_ty, &WasdType::I64)?;
                self.unify(&end_ty, &WasdType::I64)?;
                // Range is a special type - for now treat as tuple of (start, end)
                Ok(WasdType::Named("Range".to_string()))
            }
            Expr::Await(expr, _) => {
                // Validate that we're in an async context
                if !self.current_effects.contains(&"Async".to_string()) {
                    return Err("'await' can only be used in async functions (functions with [Async] effect)".to_string());
                }
                // Type check the awaited expression and return its type
                let inner_ty = self.infer_expr(expr)?;
                Ok(inner_ty)
            }
            Expr::Try(expr, _) => {
                // The ? operator propagates errors from Result/Option types
                // Full implementation would:
                // 1. Check that expr returns Result[T, E] or Option[T]
                // 2. Check that current function can return E
                // 3. Return T as the type of the expression
                let inner_ty = self.infer_expr(expr)?;

                // For Result[T, E] or Option[T], unwrap to T
                match &inner_ty {
                    WasdType::Generic(name, args) if name == "Result" || name == "Option" => {
                        // Return the success type (first type argument)
                        if !args.is_empty() {
                            Ok(args[0].clone())
                        } else {
                            Ok(inner_ty)
                        }
                    }
                    _ => {
                        // For non-Result/Option types, the ? operator is a no-op
                        // Just return the same type (allows using ? on any expression)
                        Ok(inner_ty)
                    }
                }
            }
        }
    }

    pub fn check_block(&mut self, stmts: &[Stmt]) -> Result<WasdType, String> {
        let mut last_ty = WasdType::Unit;
        for stmt in stmts {
            last_ty = self.check_stmt(stmt)?;
        }
        Ok(last_ty)
    }

    pub fn check_binary_op(
        &self,
        op: BinOp,
        left: &WasdType,
        right: &WasdType,
    ) -> Result<WasdType, String> {
        match op {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => {
                if left != right {
                    return Err(format!("Type mismatch: {:?} vs {:?}", left, right));
                }
                Ok(left.clone())
            }
            BinOp::Eq | BinOp::NotEq | BinOp::Lt | BinOp::LtEq | BinOp::Gt | BinOp::GtEq => {
                if left != right {
                    return Err(format!("Type mismatch: {:?} vs {:?}", left, right));
                }
                Ok(WasdType::Bool)
            }
            BinOp::And | BinOp::Or => {
                if *left != WasdType::Bool || *right != WasdType::Bool {
                    return Err("Boolean operators require bool operands".to_string());
                }
                Ok(WasdType::Bool)
            }
        }
    }

    pub fn check_unary_op(&self, op: UnaryOp, inner: &WasdType) -> Result<WasdType, String> {
        match op {
            UnaryOp::Neg => Ok(inner.clone()),
            UnaryOp::Not => {
                if *inner != WasdType::Bool {
                    return Err("'not' requires bool operand".to_string());
                }
                Ok(WasdType::Bool)
            }
            UnaryOp::Ref => Ok(WasdType::Ref(Box::new(inner.clone()), false)),
            UnaryOp::RefMut => Ok(WasdType::Ref(Box::new(inner.clone()), true)),
            UnaryOp::Deref => match inner {
                WasdType::Ref(inner, _) => Ok(*inner.clone()),
                _ => Err("Cannot dereference non-reference".to_string()),
            },
        }
    }

    /// Add bindings from a pattern to the type environment.
    fn add_pattern_bindings(&mut self, pattern: &Pattern) {
        match pattern {
            Pattern::Ident(name, _) => {
                // Add as Unknown type - will be inferred from context
                self.env.insert(name.clone(), WasdType::Unknown);
            }
            Pattern::Constructor(_, bindings, _) => {
                // Recursively add bindings from nested patterns
                for binding in bindings {
                    self.add_pattern_bindings(binding);
                }
            }
            Pattern::Wildcard(_) | Pattern::Literal(_) => {
                // No bindings introduced
            }
        }
    }
}
