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
                        let func_name = if let Some(closure_func) = self.closure_bindings.get(name) {
                            closure_func.clone()
                        } else {
                            name.clone()
                        };

                        // Regular function call
                        let ir_args: Vec<_> = args.iter().filter_map(|a| self.lower_expr(a)).collect();
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
        // Get the variable name from the base expression
        let var_name = match base {
            ast::Expr::Ident(name, _) => name.clone(),
            _ => return None,
        };

        // Look up the struct type for this variable
        let struct_type_name = self.variable_types.get(&var_name).cloned()
            .unwrap_or_else(|| var_name.clone());

        let field_index = self.get_field_index(&struct_type_name, field_name);

        // Get the field type if available
        let field_ty = self.struct_fields.get(&struct_type_name)
            .and_then(|fields| fields.get(field_index))
            .map(|(_, ty)| ty.clone())
            .unwrap_or(IrType::I64);

        let field_ptr = self.fresh_var();
        self.current_block.push(IrInst::GetElementPtr {
            dest: field_ptr.clone(),
            ptr: var_name,
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

        let saved_block = std::mem::take(&mut self.current_block);
        let saved_blocks = std::mem::take(&mut self.blocks);
        let saved_label = std::mem::replace(&mut self.current_label, "entry".to_string());

        let body_val = self.lower_expr(body);

        let entry_block = IrBlock {
            label: "entry".to_string(),
            instructions: std::mem::take(&mut self.current_block),
            terminator: IrTerminator::Return(body_val),
        };

        let closure_func = IrFunction {
            name: closure_name.clone(),
            params: params
                .iter()
                .map(|p| (p.name.clone(), self.lower_type(&p.ty)))
                .collect(),
            return_type: IrType::I64,
            blocks: vec![entry_block],
        };

        self.closure_funcs.push(closure_func);

        self.current_block = saved_block;
        self.blocks = saved_blocks;
        self.current_label = saved_label;

        Some(IrValue::Var(closure_name))
    }
}
