//! Control flow lowering (if and match expressions).

use crate::ir::wasd_ir::*;
use super::Lowerer;
use crate::parser as ast;

impl Lowerer {
    pub(super) fn lower_if(
        &mut self,
        cond: &ast::Expr,
        then_branch: &[ast::Stmt],
        else_branch: Option<&[ast::Stmt]>,
    ) -> Option<IrValue> {
        let then_label = self.fresh_label("if_then");
        let else_label = self.fresh_label("if_else");
        let end_label = self.fresh_label("if_end");

        let cond_val = self.lower_expr(cond).unwrap_or(IrValue::ConstBool(false));

        let result_var = self.fresh_var();
        self.current_block.push(IrInst::Alloca {
            dest: result_var.clone(),
            ty: IrType::I64,
        });

        self.finish_block(IrTerminator::CondBranch {
            cond: cond_val,
            true_block: then_label.clone(),
            false_block: if else_branch.is_some() {
                else_label.clone()
            } else {
                end_label.clone()
            },
        });

        self.start_block(then_label);
        let mut then_val = None;
        for stmt in then_branch {
            then_val = self.lower_stmt(stmt);
        }
        if let Some(val) = then_val {
            self.current_block.push(IrInst::Store {
                value: val,
                ptr: result_var.clone(),
            });
        }
        self.finish_block(IrTerminator::Branch(end_label.clone()));

        if let Some(else_stmts) = else_branch {
            self.start_block(else_label);
            let mut else_val = None;
            for stmt in else_stmts {
                else_val = self.lower_stmt(stmt);
            }
            if let Some(val) = else_val {
                self.current_block.push(IrInst::Store {
                    value: val,
                    ptr: result_var.clone(),
                });
            }
            self.finish_block(IrTerminator::Branch(end_label.clone()));
        }

        self.start_block(end_label);

        let result_dest = self.fresh_var();
        self.current_block.push(IrInst::Load {
            dest: result_dest.clone(),
            ptr: result_var,
            ty: IrType::I64,
        });
        Some(IrValue::Var(result_dest))
    }

    pub(super) fn lower_match(
        &mut self,
        value: &ast::Expr,
        arms: &[ast::MatchArm],
    ) -> Option<IrValue> {
        let match_val = self.lower_expr(value)?;

        // Get the type of the value being matched (for enums)
        let match_type_name = match value {
            ast::Expr::Ident(name, _) => self.variable_types.get(name).cloned(),
            _ => None,
        };
        let match_type = match_type_name.as_ref()
            .map(|n| IrType::Struct(n.clone()))
            .unwrap_or(IrType::I64);

        let arm_labels: Vec<_> = arms.iter().map(|_| self.fresh_label("match_arm")).collect();
        let end_label = self.fresh_label("match_end");

        let result_var = self.fresh_var();
        self.current_block.push(IrInst::Alloca {
            dest: result_var.clone(),
            ty: IrType::I64,
        });

        let match_val_ptr = self.fresh_var();
        self.current_block.push(IrInst::Alloca {
            dest: match_val_ptr.clone(),
            ty: match_type.clone(),
        });
        self.current_block.push(IrInst::Store {
            value: match_val.clone(),
            ptr: match_val_ptr.clone(),
        });

        for (i, arm) in arms.iter().enumerate() {
            let arm_label = arm_labels[i].clone();
            let next_label = if i + 1 < arms.len() {
                self.fresh_label("match_check")
            } else {
                end_label.clone()
            };

            match &arm.pattern {
                ast::Pattern::Wildcard(_) | ast::Pattern::Ident(_, _) => {
                    self.finish_block(IrTerminator::Branch(arm_label.clone()));
                }
                ast::Pattern::Literal(lit_expr) => {
                    let lit_val = self.lower_expr(lit_expr)?;
                    let cmp_result = self.fresh_var();
                    self.current_block.push(IrInst::BinOp {
                        dest: cmp_result.clone(),
                        op: IrBinOp::Eq,
                        left: match_val.clone(),
                        right: lit_val,
                    });
                    self.finish_block(IrTerminator::CondBranch {
                        cond: IrValue::Var(cmp_result),
                        true_block: arm_label.clone(),
                        false_block: next_label.clone(),
                    });
                }
                ast::Pattern::Constructor(variant_name, bindings, _) => {
                    // Parse qualified variant names like "Option::Some" -> ("Option", "Some")
                    let (enum_name, just_variant) = if variant_name.contains("::") {
                        let parts: Vec<&str> = variant_name.split("::").collect();
                        (parts[0].to_string(), parts[1].to_string())
                    } else {
                        (match_type_name.clone().unwrap_or_default(), variant_name.clone())
                    };

                    let tag_ptr = self.fresh_var();
                    self.current_block.push(IrInst::GetElementPtr {
                        dest: tag_ptr.clone(),
                        ptr: match_val_ptr.clone(),
                        indices: vec![
                            IrValue::ConstInt(0, IrType::I32),
                            IrValue::ConstInt(0, IrType::I32),
                        ],
                        base_type: Some(match_type.clone()),
                    });
                    let tag_val = self.fresh_var();
                    self.current_block.push(IrInst::Load {
                        dest: tag_val.clone(),
                        ptr: tag_ptr,
                        ty: IrType::I32,
                    });

                    let expected_tag = self.get_variant_tag(&enum_name, &just_variant);
                    let cmp_result = self.fresh_var();
                    self.current_block.push(IrInst::BinOp {
                        dest: cmp_result.clone(),
                        op: IrBinOp::Eq,
                        left: IrValue::Var(tag_val),
                        right: IrValue::ConstInt(expected_tag as i64, IrType::I32),
                    });
                    self.finish_block(IrTerminator::CondBranch {
                        cond: IrValue::Var(cmp_result),
                        true_block: arm_label.clone(),
                        false_block: next_label.clone(),
                    });

                    self.start_block(arm_label.clone());
                    if !bindings.is_empty() {
                        if let ast::Pattern::Ident(binding_name, _) = &bindings[0] {
                            let payload_ptr = self.fresh_var();
                            self.current_block.push(IrInst::GetElementPtr {
                                dest: payload_ptr.clone(),
                                ptr: match_val_ptr.clone(),
                                indices: vec![
                                    IrValue::ConstInt(0, IrType::I32),
                                    IrValue::ConstInt(1, IrType::I32),
                                ],
                                base_type: Some(match_type.clone()),
                            });
                            self.current_block.push(IrInst::Alloca {
                                dest: binding_name.clone(),
                                ty: IrType::I64,
                            });
                            let payload_val = self.fresh_var();
                            self.current_block.push(IrInst::Load {
                                dest: payload_val.clone(),
                                ptr: payload_ptr,
                                ty: IrType::I64,
                            });
                            self.current_block.push(IrInst::Store {
                                value: IrValue::Var(payload_val),
                                ptr: binding_name.clone(),
                            });
                        }
                    }
                    if let Some(body_val) = self.lower_expr(&arm.body) {
                        self.current_block.push(IrInst::Store {
                            value: body_val,
                            ptr: result_var.clone(),
                        });
                    }
                    self.finish_block(IrTerminator::Branch(end_label.clone()));
                    self.start_block(next_label);
                    continue;
                }
            }

            self.start_block(arm_label);

            if let ast::Pattern::Ident(name, _) = &arm.pattern {
                self.current_block.push(IrInst::Alloca {
                    dest: name.clone(),
                    ty: IrType::I64,
                });
                self.current_block.push(IrInst::Store {
                    value: match_val.clone(),
                    ptr: name.clone(),
                });
            }

            if let Some(body_val) = self.lower_expr(&arm.body) {
                self.current_block.push(IrInst::Store {
                    value: body_val,
                    ptr: result_var.clone(),
                });
            }
            self.finish_block(IrTerminator::Branch(end_label.clone()));

            if i + 1 < arms.len() {
                self.start_block(next_label);
            }
        }

        self.start_block(end_label);
        let result_dest = self.fresh_var();
        self.current_block.push(IrInst::Load {
            dest: result_dest.clone(),
            ptr: result_var,
            ty: IrType::I64,
        });
        Some(IrValue::Var(result_dest))
    }
}
