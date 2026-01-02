//! Expression lowering from AST to WASD IR.

use crate::ir::wasd_ir::*;
use super::Lowerer;
use crate::parser as ast;

impl Lowerer {
    pub(super) fn lower_expr(&mut self, expr: &ast::Expr) -> Option<IrValue> {
        match expr {
            ast::Expr::Int(n, _) => Some(IrValue::ConstInt(*n, IrType::I64)),
            ast::Expr::Float(n, _) => Some(IrValue::ConstFloat(*n, IrType::F64)),
            ast::Expr::Bool(b, _) => Some(IrValue::ConstBool(*b)),
            ast::Expr::String(s, _) => Some(IrValue::ConstString(s.clone())),
            ast::Expr::Ident(name, _) => Some(IrValue::Var(name.clone())),
            ast::Expr::Binary(left, op, right, _) => {
                let left_val = self.lower_expr(left)?;
                let right_val = self.lower_expr(right)?;
                let dest = self.fresh_var();
                let ir_op = self.lower_binop(*op);

                self.current_block.push(IrInst::BinOp {
                    dest: dest.clone(),
                    op: ir_op,
                    left: left_val,
                    right: right_val,
                });

                Some(IrValue::Var(dest))
            }
            ast::Expr::Call(callee, _type_args, args, _) => {
                // Check if this is a method call (callee is FieldAccess)
                match callee.as_ref() {
                    ast::Expr::FieldAccess(base, method_name, _) => {
                        // Method call: receiver.method(args)
                        self.lower_method_call(base, method_name, args)
                    }
                    ast::Expr::Ident(name, _) => {
                        // Check if this is a closure call
                        let (func_name, captures) = if let Some(closure_func) = self.closure_bindings.get(name) {
                            let caps = self.closure_captures.get(closure_func).cloned().unwrap_or_default();
                            (closure_func.clone(), caps)
                        } else {
                            (name.clone(), Vec::new())
                        };

                        // Build args: captured vars first, then explicit args
                        let mut ir_args: Vec<IrValue> = captures
                            .iter()
                            .map(|cap_name| IrValue::Var(cap_name.clone()))
                            .collect();
                        ir_args.extend(args.iter().filter_map(|a| self.lower_expr(a)));

                        let dest = self.fresh_var();

                        self.current_block.push(IrInst::Call {
                            dest: Some(dest.clone()),
                            func: func_name,
                            args: ir_args,
                        });

                        Some(IrValue::Var(dest))
                    }
                    _ => None,
                }
            }
            ast::Expr::StructConstruct { name, fields, .. } => {
                self.lower_struct_construct(name, fields)
            }
            ast::Expr::FieldAccess(base, field_name, _) => {
                self.lower_field_access(base, field_name)
            }
            ast::Expr::EnumConstruct { enum_name, variant, value, .. } => {
                self.lower_enum_construct(enum_name, variant, value)
            }
            ast::Expr::Match(value, arms, _) => {
                self.lower_match(value, arms)
            }
            ast::Expr::If(cond, then_branch, else_branch, _) => {
                self.lower_if(cond, then_branch, else_branch.as_deref())
            }
            ast::Expr::Block(stmts, _) => {
                let mut last_val = None;
                for stmt in stmts {
                    last_val = self.lower_stmt(stmt);
                }
                last_val
            }
            ast::Expr::HeapAlloc(inner, _) => {
                let inner_val = self.lower_expr(inner);
                let dest = self.fresh_var();
                self.current_block.push(IrInst::HeapAlloc {
                    dest: dest.clone(),
                    ty: IrType::I64,
                    value: inner_val,
                });
                Some(IrValue::Var(dest))
            }
            ast::Expr::RcAlloc(inner, _) => {
                let inner_val = self.lower_expr(inner);
                let dest = self.fresh_var();
                self.current_block.push(IrInst::RcAlloc {
                    dest: dest.clone(),
                    ty: IrType::I64,
                    value: inner_val,
                });
                Some(IrValue::Var(dest))
            }
            ast::Expr::ArcAlloc(inner, _) => {
                let inner_val = self.lower_expr(inner);
                let dest = self.fresh_var();
                self.current_block.push(IrInst::ArcAlloc {
                    dest: dest.clone(),
                    ty: IrType::I64,
                    value: inner_val,
                });
                Some(IrValue::Var(dest))
            }
            ast::Expr::Lambda { params, body, .. } => {
                self.lower_lambda(params, body)
            }
            ast::Expr::Range { start: _, end, .. } => {
                // Range expressions are handled specially in for loops
                // If used as a value, just return the end (for simple use cases like `for i in n`)
                self.lower_expr(end)
            }
            ast::Expr::Await(expr, _) => {
                // For now, await is a no-op at IR level (just returns the awaited value)
                // Real async runtime support would transform this into state machine
                self.lower_expr(expr)
            }
            ast::Expr::Try(expr, _) => {
                // For now, the ? operator is a pass-through at IR level
                // Full implementation would:
                // 1. Check if value is error variant
                // 2. If error, return early with the error
                // 3. If success, unwrap and continue
                // For now, just evaluate the inner expression
                self.lower_expr(expr)
            }
            _ => None,
        }
    }

    fn lower_struct_construct(
        &mut self,
        name: &str,
        fields: &[(String, ast::Expr)],
    ) -> Option<IrValue> {
        let dest = self.fresh_var();
        let struct_type = IrType::Struct(name.to_string());
        self.current_block.push(IrInst::Alloca {
            dest: dest.clone(),
            ty: struct_type.clone(),
        });

        for (i, (_field_name, field_expr)) in fields.iter().enumerate() {
            if let Some(field_val) = self.lower_expr(field_expr) {
                let field_ptr = self.fresh_var();
                self.current_block.push(IrInst::GetElementPtr {
                    dest: field_ptr.clone(),
                    ptr: dest.clone(),
                    indices: vec![
                        IrValue::ConstInt(0, IrType::I32),
                        IrValue::ConstInt(i as i64, IrType::I32),
                    ],
                    base_type: Some(struct_type.clone()),
                });
                self.current_block.push(IrInst::Store {
                    value: field_val,
                    ptr: field_ptr,
                });
            }
        }

        Some(IrValue::Var(dest))
    }

    fn lower_field_access(
        &mut self,
        base: &ast::Expr,
        field_name: &str,
    ) -> Option<IrValue> {
        // Handle base expression to get the pointer and type
        let (base_ptr, struct_type_name) = match base {
            ast::Expr::Ident(name, _) => {
                // Simple identifier - look up variable type
                let type_name = self.variable_types.get(name).cloned()
                    .unwrap_or_else(|| name.clone());
                (name.clone(), type_name)
            }
            ast::Expr::FieldAccess(inner_base, inner_field, _) => {
                // Nested field access - first get the intermediate struct pointer
                let inner_result = self.lower_field_access_to_ptr(inner_base, inner_field)?;
                (inner_result.0, inner_result.1)
            }
            _ => return None,
        };

        let field_index = self.get_field_index(&struct_type_name, field_name);

        // Get the field type if available
        let field_ty = self.struct_fields.get(&struct_type_name)
            .and_then(|fields| fields.get(field_index))
            .map(|(_, ty)| ty.clone())
            .unwrap_or(IrType::I64);

        let field_ptr = self.fresh_var();
        self.current_block.push(IrInst::GetElementPtr {
            dest: field_ptr.clone(),
            ptr: base_ptr,
            indices: vec![
                IrValue::ConstInt(0, IrType::I32),
                IrValue::ConstInt(field_index as i64, IrType::I32),
            ],
            base_type: Some(IrType::Struct(struct_type_name)),
        });

        let dest = self.fresh_var();
        self.current_block.push(IrInst::Load {
            dest: dest.clone(),
            ptr: field_ptr,
            ty: field_ty,
        });

        Some(IrValue::Var(dest))
    }

    /// Lower field access but return the pointer instead of loading the value.
    /// Returns (ptr_var_name, field_type_name)
    fn lower_field_access_to_ptr(
        &mut self,
        base: &ast::Expr,
        field_name: &str,
    ) -> Option<(String, String)> {
        // Handle base expression to get the pointer and type
        let (base_ptr, struct_type_name) = match base {
            ast::Expr::Ident(name, _) => {
                let type_name = self.variable_types.get(name).cloned()
                    .unwrap_or_else(|| name.clone());
                (name.clone(), type_name)
            }
            ast::Expr::FieldAccess(inner_base, inner_field, _) => {
                self.lower_field_access_to_ptr(inner_base, inner_field)?
            }
            _ => return None,
        };

        let field_index = self.get_field_index(&struct_type_name, field_name);

        // Get the field type name (for nested struct access)
        let field_type_name = self.struct_fields.get(&struct_type_name)
            .and_then(|fields| fields.get(field_index))
            .and_then(|(_, ty)| match ty {
                IrType::Struct(name) => Some(name.clone()),
                _ => None,
            })
            .unwrap_or_else(|| field_name.to_string());

        let field_ptr = self.fresh_var();
        self.current_block.push(IrInst::GetElementPtr {
            dest: field_ptr.clone(),
            ptr: base_ptr,
            indices: vec![
                IrValue::ConstInt(0, IrType::I32),
                IrValue::ConstInt(field_index as i64, IrType::I32),
            ],
            base_type: Some(IrType::Struct(struct_type_name)),
        });

        Some((field_ptr, field_type_name))
    }

    fn lower_enum_construct(
        &mut self,
        enum_name: &Option<String>,
        variant: &str,
        value: &Option<Box<ast::Expr>>,
    ) -> Option<IrValue> {
        let enum_type_name = enum_name.as_ref().cloned().unwrap_or_else(|| "Unknown".to_string());
        let dest = self.fresh_var();
        let enum_type = IrType::Struct(enum_type_name.clone());

        self.current_block.push(IrInst::Alloca {
            dest: dest.clone(),
            ty: enum_type.clone(),
        });

        let tag_value = self.get_variant_tag(&enum_type_name, variant);
        let tag_ptr = self.fresh_var();
        self.current_block.push(IrInst::GetElementPtr {
            dest: tag_ptr.clone(),
            ptr: dest.clone(),
            indices: vec![
                IrValue::ConstInt(0, IrType::I32),
                IrValue::ConstInt(0, IrType::I32),
            ],
            base_type: Some(enum_type.clone()),
        });
        self.current_block.push(IrInst::Store {
            value: IrValue::ConstInt(tag_value as i64, IrType::I32),
            ptr: tag_ptr,
        });

        if let Some(val_expr) = value {
            if let Some(payload) = self.lower_expr(val_expr) {
                let payload_ptr = self.fresh_var();
                self.current_block.push(IrInst::GetElementPtr {
                    dest: payload_ptr.clone(),
                    ptr: dest.clone(),
                    indices: vec![
                        IrValue::ConstInt(0, IrType::I32),
                        IrValue::ConstInt(1, IrType::I32),
                    ],
                    base_type: Some(enum_type.clone()),
                });
                self.current_block.push(IrInst::Store {
                    value: payload,
                    ptr: payload_ptr,
                });
            }
        }

        Some(IrValue::Var(dest))
    }

    fn lower_method_call(
        &mut self,
        receiver: &ast::Expr,
        method_name: &str,
        args: &[ast::Expr],
    ) -> Option<IrValue> {
        // Get the receiver variable name and its type
        let receiver_var = match receiver {
            ast::Expr::Ident(name, _) => name.clone(),
            _ => return None,
        };

        // Look up the struct type for this receiver
        let struct_type_name = self.variable_types.get(&receiver_var).cloned()
            .unwrap_or_else(|| receiver_var.clone());

        // Mangle the method name: TypeName_methodName
        let mangled_name = format!("{}_{}", struct_type_name, method_name);

        // Build args: self (receiver ptr) + other args
        // Use VarPtr to pass the pointer directly, not load the struct value
        let mut ir_args = vec![IrValue::VarPtr(receiver_var)];
        for arg in args {
            if let Some(val) = self.lower_expr(arg) {
                ir_args.push(val);
            }
        }

        let dest = self.fresh_var();
        self.current_block.push(IrInst::Call {
            dest: Some(dest.clone()),
            func: mangled_name,
            args: ir_args,
        });

        Some(IrValue::Var(dest))
    }

    fn lower_lambda(
        &mut self,
        params: &[ast::Param],
        body: &ast::Expr,
    ) -> Option<IrValue> {
        let closure_name = format!("__closure_{}", self.fresh_var());

        // Collect parameter names (these are not captures)
        let param_names: std::collections::HashSet<String> = params.iter().map(|p| p.name.clone()).collect();

        // Find free variables in the body that need to be captured
        let mut free_vars = Vec::new();
        self.collect_free_vars(body, &param_names, &mut free_vars);

        // Remove duplicates while preserving order
        let mut seen = std::collections::HashSet::new();
        free_vars.retain(|v| seen.insert(v.clone()));

        // Store the captures for this closure
        self.closure_captures.insert(closure_name.clone(), free_vars.clone());

        let saved_block = std::mem::take(&mut self.current_block);
        let saved_blocks = std::mem::take(&mut self.blocks);
        let saved_label = std::mem::replace(&mut self.current_label, "entry".to_string());

        // Build closure params: captured vars first, then explicit params
        let mut closure_params: Vec<(String, IrType)> = free_vars
            .iter()
            .map(|name| {
                let ty = self.scope_vars.get(name).cloned().unwrap_or(IrType::I64);
                (name.clone(), ty)
            })
            .collect();

        for p in params {
            closure_params.push((p.name.clone(), self.lower_type(&p.ty)));
        }

        let body_val = self.lower_expr(body);

        let entry_block = IrBlock {
            label: "entry".to_string(),
            instructions: std::mem::take(&mut self.current_block),
            terminator: IrTerminator::Return(body_val),
        };

        let closure_func = IrFunction {
            name: closure_name.clone(),
            params: closure_params,
            return_type: IrType::I64,
            blocks: vec![entry_block],
        };

        self.closure_funcs.push(closure_func);

        self.current_block = saved_block;
        self.blocks = saved_blocks;
        self.current_label = saved_label;

        Some(IrValue::Var(closure_name))
    }

    /// Collect free variables from an expression that need to be captured.
    /// `bound` contains variable names that are bound (params, local lets).
    fn collect_free_vars(
        &self,
        expr: &ast::Expr,
        bound: &std::collections::HashSet<String>,
        free: &mut Vec<String>,
    ) {
        match expr {
            ast::Expr::Ident(name, _) => {
                // If the variable is in scope but not bound locally, it's a capture
                if !bound.contains(name) && self.scope_vars.contains_key(name) {
                    free.push(name.clone());
                }
            }
            ast::Expr::Binary(left, _, right, _) => {
                self.collect_free_vars(left, bound, free);
                self.collect_free_vars(right, bound, free);
            }
            ast::Expr::Call(callee, _, args, _) => {
                self.collect_free_vars(callee, bound, free);
                for arg in args {
                    self.collect_free_vars(arg, bound, free);
                }
            }
            ast::Expr::If(cond, then_branch, else_branch, _) => {
                self.collect_free_vars(cond, bound, free);
                let mut then_bound = bound.clone();
                for stmt in then_branch {
                    self.collect_free_vars_stmt(stmt, &mut then_bound, free);
                }
                if let Some(else_stmts) = else_branch {
                    let mut else_bound = bound.clone();
                    for stmt in else_stmts {
                        self.collect_free_vars_stmt(stmt, &mut else_bound, free);
                    }
                }
            }
            ast::Expr::Block(stmts, _) => {
                let mut local_bound = bound.clone();
                for stmt in stmts {
                    self.collect_free_vars_stmt(stmt, &mut local_bound, free);
                }
            }
            ast::Expr::Lambda { params, body, .. } => {
                let mut inner_bound = bound.clone();
                for p in params {
                    inner_bound.insert(p.name.clone());
                }
                self.collect_free_vars(body, &inner_bound, free);
            }
            ast::Expr::FieldAccess(base, _, _) => {
                self.collect_free_vars(base, bound, free);
            }
            ast::Expr::StructConstruct { fields, .. } => {
                for (_, field_expr) in fields {
                    self.collect_free_vars(field_expr, bound, free);
                }
            }
            ast::Expr::EnumConstruct { value, .. } => {
                if let Some(v) = value {
                    self.collect_free_vars(v, bound, free);
                }
            }
            ast::Expr::Match(scrutinee, arms, _) => {
                self.collect_free_vars(scrutinee, bound, free);
                for arm in arms {
                    // Pattern bindings would extend bound, but for simplicity we just check the body
                    self.collect_free_vars(&arm.body, bound, free);
                }
            }
            ast::Expr::HeapAlloc(inner, _)
            | ast::Expr::RcAlloc(inner, _)
            | ast::Expr::ArcAlloc(inner, _)
            | ast::Expr::Await(inner, _)
            | ast::Expr::Try(inner, _) => {
                self.collect_free_vars(inner, bound, free);
            }
            ast::Expr::Range { start, end, .. } => {
                self.collect_free_vars(start, bound, free);
                self.collect_free_vars(end, bound, free);
            }
            // Literals and unary ops
            ast::Expr::Int(_, _)
            | ast::Expr::Float(_, _)
            | ast::Expr::Bool(_, _)
            | ast::Expr::String(_, _) => {}
            ast::Expr::Unary(_, inner, _) => {
                self.collect_free_vars(inner, bound, free);
            }
        }
    }

    fn collect_free_vars_stmt(
        &self,
        stmt: &ast::Stmt,
        bound: &mut std::collections::HashSet<String>,
        free: &mut Vec<String>,
    ) {
        match stmt {
            ast::Stmt::Let { name, value, .. } => {
                self.collect_free_vars(value, bound, free);
                bound.insert(name.clone());
            }
            ast::Stmt::Assign { target, value, .. } => {
                self.collect_free_vars(target, bound, free);
                self.collect_free_vars(value, bound, free);
            }
            ast::Stmt::Expr(e) | ast::Stmt::Return(Some(e), _) => {
                self.collect_free_vars(e, bound, free);
            }
            ast::Stmt::Return(None, _) | ast::Stmt::Break(_) | ast::Stmt::Continue(_) => {}
            ast::Stmt::While { condition, body, .. } => {
                self.collect_free_vars(condition, bound, free);
                for s in body {
                    self.collect_free_vars_stmt(s, bound, free);
                }
            }
            ast::Stmt::For { var, iterable, body, .. } => {
                self.collect_free_vars(iterable, bound, free);
                let mut loop_bound = bound.clone();
                loop_bound.insert(var.clone());
                for s in body {
                    self.collect_free_vars_stmt(s, &mut loop_bound, free);
                }
            }
        }
    }
}
