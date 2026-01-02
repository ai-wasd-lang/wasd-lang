//! Statement and loop lowering from AST to WASD IR.

use crate::ir::wasd_ir::*;
use super::Lowerer;
use crate::parser as ast;

impl Lowerer {
    pub(super) fn lower_stmt(&mut self, stmt: &ast::Stmt) -> Option<IrValue> {
        match stmt {
            ast::Stmt::Let {
                name, value, ty, ..
            } => {
                // Check if this is a closure binding
                if let ast::Expr::Lambda { .. } = value {
                    if let Some(IrValue::Var(closure_name)) = self.lower_expr(value) {
                        // Track the closure binding
                        self.closure_bindings.insert(name.clone(), closure_name);
                    }
                    return None;
                }

                // Infer the type from the value expression if not explicitly provided
                let ir_type = if let Some(t) = ty {
                    self.lower_type(t)
                } else if let ast::Expr::StructConstruct { name: struct_name, .. } = value {
                    // Track the struct type for this variable
                    self.variable_types.insert(name.clone(), struct_name.clone());
                    IrType::Struct(struct_name.clone())
                } else if let ast::Expr::EnumConstruct { enum_name: Some(enum_name), .. } = value {
                    // Track the enum type for this variable
                    self.variable_types.insert(name.clone(), enum_name.clone());
                    IrType::Struct(enum_name.clone())
                } else {
                    IrType::I64
                };

                // Track this variable in scope for closure capture analysis
                self.scope_vars.insert(name.clone(), ir_type.clone());

                self.current_block.push(IrInst::Alloca {
                    dest: name.clone(),
                    ty: ir_type.clone(),
                });

                if let Some(value) = self.lower_expr(value) {
                    self.current_block.push(IrInst::Store {
                        value,
                        ptr: name.clone(),
                    });
                }
                None
            }
            ast::Stmt::Assign { target, value, .. } => {
                if let Some(val) = self.lower_expr(value) {
                    if let ast::Expr::Ident(name, _) = target {
                        self.current_block.push(IrInst::Store {
                            value: val,
                            ptr: name.clone(),
                        });
                    }
                }
                None
            }
            ast::Stmt::Expr(e) => self.lower_expr(e),
            ast::Stmt::Return(Some(e), _) => self.lower_expr(e),
            ast::Stmt::Return(None, _) => Some(IrValue::Unit),
            ast::Stmt::While { condition, body, .. } => {
                self.lower_while(condition, body);
                None
            }
            ast::Stmt::For { var, iterable, body, .. } => {
                self.lower_for(var, iterable, body);
                None
            }
            ast::Stmt::Break(_) => {
                if let Some((_, break_label)) = self.loop_stack.last() {
                    let break_label = break_label.clone();
                    self.finish_block(IrTerminator::Branch(break_label.clone()));
                    let dead_label = self.fresh_label("dead");
                    self.start_block(dead_label);
                }
                None
            }
            ast::Stmt::Continue(_) => {
                if let Some((continue_label, _)) = self.loop_stack.last() {
                    let continue_label = continue_label.clone();
                    self.finish_block(IrTerminator::Branch(continue_label.clone()));
                    let dead_label = self.fresh_label("dead");
                    self.start_block(dead_label);
                }
                None
            }
        }
    }

    pub(super) fn lower_while(&mut self, condition: &ast::Expr, body: &[ast::Stmt]) {
        let cond_label = self.fresh_label("while_cond");
        let body_label = self.fresh_label("while_body");
        let end_label = self.fresh_label("while_end");

        self.finish_block(IrTerminator::Branch(cond_label.clone()));

        self.start_block(cond_label.clone());
        let cond_value = self.lower_expr(condition).unwrap_or(IrValue::ConstBool(false));
        self.finish_block(IrTerminator::CondBranch {
            cond: cond_value,
            true_block: body_label.clone(),
            false_block: end_label.clone(),
        });

        self.start_block(body_label);
        self.loop_stack.push((cond_label.clone(), end_label.clone()));
        for stmt in body {
            self.lower_stmt(stmt);
        }
        self.loop_stack.pop();
        self.finish_block(IrTerminator::Branch(cond_label));

        self.start_block(end_label);
    }

    pub(super) fn lower_for(&mut self, var: &str, iterable: &ast::Expr, body: &[ast::Stmt]) {
        self.current_block.push(IrInst::Alloca {
            dest: var.to_string(),
            ty: IrType::I64,
        });

        // Check if iterable is a Range expression
        let (start_val, end_val) = match iterable {
            ast::Expr::Range { start, end, .. } => {
                let s = self.lower_expr(start).unwrap_or(IrValue::ConstInt(0, IrType::I64));
                let e = self.lower_expr(end).unwrap_or(IrValue::ConstInt(0, IrType::I64));
                (s, e)
            }
            _ => {
                // For non-range, start at 0 and iterate to the value
                let e = self.lower_expr(iterable).unwrap_or(IrValue::ConstInt(0, IrType::I64));
                (IrValue::ConstInt(0, IrType::I64), e)
            }
        };

        self.current_block.push(IrInst::Store {
            value: start_val,
            ptr: var.to_string(),
        });

        let cond_label = self.fresh_label("for_cond");
        let body_label = self.fresh_label("for_body");
        let incr_label = self.fresh_label("for_incr");
        let end_label = self.fresh_label("for_end");

        self.finish_block(IrTerminator::Branch(cond_label.clone()));

        self.start_block(cond_label.clone());
        let cond_temp = self.fresh_var();
        self.current_block.push(IrInst::BinOp {
            dest: cond_temp.clone(),
            op: IrBinOp::Lt,
            left: IrValue::Var(var.to_string()),
            right: end_val,
        });
        self.finish_block(IrTerminator::CondBranch {
            cond: IrValue::Var(cond_temp),
            true_block: body_label.clone(),
            false_block: end_label.clone(),
        });

        self.start_block(body_label);
        self.loop_stack.push((incr_label.clone(), end_label.clone()));
        for stmt in body {
            self.lower_stmt(stmt);
        }
        self.loop_stack.pop();
        self.finish_block(IrTerminator::Branch(incr_label.clone()));

        self.start_block(incr_label);
        let incr_temp = self.fresh_var();
        self.current_block.push(IrInst::BinOp {
            dest: incr_temp.clone(),
            op: IrBinOp::Add,
            left: IrValue::Var(var.to_string()),
            right: IrValue::ConstInt(1, IrType::I64),
        });
        self.current_block.push(IrInst::Store {
            value: IrValue::Var(incr_temp),
            ptr: var.to_string(),
        });
        self.finish_block(IrTerminator::Branch(cond_label));

        self.start_block(end_label);
    }
}
