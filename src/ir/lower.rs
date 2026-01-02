//! Lowering from AST to WASD IR.

use super::wasd_ir::*;
use crate::parser as ast;

pub use super::wasd_ir::{IrEnum, IrEnumVariant};

/// Lower a program AST to WASD IR.
pub fn lower_program(program: &ast::Program) -> IrModule {
    let mut lowerer = Lowerer::new();
    lowerer.lower(program)
}

struct Lowerer {
    current_block: Vec<IrInst>,
    current_label: String,
    blocks: Vec<IrBlock>,
    block_counter: usize,
    var_counter: usize,
    // Stack of (continue_label, break_label) for nested loops
    loop_stack: Vec<(String, String)>,
    // Closure functions generated during lowering
    closure_funcs: Vec<IrFunction>,
}

impl Lowerer {
    fn new() -> Self {
        Self {
            current_block: Vec::new(),
            current_label: "entry".to_string(),
            blocks: Vec::new(),
            block_counter: 0,
            var_counter: 0,
            loop_stack: Vec::new(),
            closure_funcs: Vec::new(),
        }
    }

    fn fresh_label(&mut self, prefix: &str) -> String {
        let label = format!("{}_{}", prefix, self.block_counter);
        self.block_counter += 1;
        label
    }

    fn lower(&mut self, program: &ast::Program) -> IrModule {
        let mut functions = Vec::new();
        let mut structs = Vec::new();
        let mut enums = Vec::new();

        for item in &program.items {
            match item {
                ast::Item::Use(_) => {
                    // Use statements don't generate IR - they're resolved at type check time
                }
                ast::Item::Function(f) => {
                    if let Some(ir_func) = self.lower_function(f) {
                        functions.push(ir_func);
                    }
                }
                ast::Item::Struct(s) => {
                    structs.push(self.lower_struct(s));
                }
                ast::Item::Enum(e) => {
                    enums.push(self.lower_enum(e));
                }
                ast::Item::Trait(_) => {
                    // Traits are used for type checking, not IR generation
                }
                ast::Item::Impl(impl_def) => {
                    // Lower methods in impl blocks
                    for method in &impl_def.methods {
                        if let Some(ir_func) = self.lower_function(method) {
                            functions.push(ir_func);
                        }
                    }
                }
            }
        }

        // Add closure functions to the module
        functions.extend(std::mem::take(&mut self.closure_funcs));

        IrModule { functions, structs, enums }
    }

    fn lower_enum(&self, e: &ast::EnumDef) -> IrEnum {
        let variants: Vec<_> = e
            .variants
            .iter()
            .enumerate()
            .map(|(i, v)| IrEnumVariant {
                name: v.name.clone(),
                tag: i as u32,
                payload_ty: v.fields.first().map(|t| self.lower_type(t)),
            })
            .collect();

        IrEnum {
            name: e.name.clone(),
            variants,
        }
    }

    fn lower_function(&mut self, func: &ast::Function) -> Option<IrFunction> {
        self.current_block.clear();
        self.current_label = "entry".to_string();
        self.blocks.clear();
        self.block_counter = 0;
        self.loop_stack.clear();

        let params: Vec<_> = func
            .params
            .iter()
            .map(|p| (p.name.clone(), self.lower_type(&p.ty)))
            .collect();

        let return_type = func
            .return_type
            .as_ref()
            .map(|t| self.lower_type(t))
            .unwrap_or(IrType::Void);

        // Lower body
        let mut last_value = None;
        for stmt in &func.body {
            last_value = self.lower_stmt(stmt);
        }

        // Create final block with return
        let terminator = IrTerminator::Return(last_value);
        self.finish_block(terminator);

        Some(IrFunction {
            name: func.name.clone(),
            params,
            return_type,
            blocks: std::mem::take(&mut self.blocks),
        })
    }

    fn lower_struct(&self, s: &ast::StructDef) -> IrStruct {
        let fields: Vec<_> = s
            .fields
            .iter()
            .map(|f| (f.name.clone(), self.lower_type(&f.ty)))
            .collect();

        IrStruct {
            name: s.name.clone(),
            fields,
        }
    }

    fn lower_stmt(&mut self, stmt: &ast::Stmt) -> Option<IrValue> {
        match stmt {
            ast::Stmt::Let {
                name, value, ty, ..
            } => {
                let ir_type = ty
                    .as_ref()
                    .map(|t| self.lower_type(t))
                    .unwrap_or(IrType::I64);

                self.current_block.push(IrInst::Alloca {
                    dest: name.clone(),
                    ty: ir_type,
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
                    // For now, only support simple variable assignment
                    if let ast::Expr::Ident(name, _) = target {
                        self.current_block.push(IrInst::Store {
                            value: val,
                            ptr: name.clone(),
                        });
                    }
                    // TODO: Support field assignment (target.field = value)
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
                    // Start a dead block (unreachable code after break)
                    let dead_label = self.fresh_label("dead");
                    self.start_block(dead_label);
                }
                None
            }
            ast::Stmt::Continue(_) => {
                if let Some((continue_label, _)) = self.loop_stack.last() {
                    let continue_label = continue_label.clone();
                    self.finish_block(IrTerminator::Branch(continue_label.clone()));
                    // Start a dead block (unreachable code after continue)
                    let dead_label = self.fresh_label("dead");
                    self.start_block(dead_label);
                }
                None
            }
        }
    }

    fn lower_while(&mut self, condition: &ast::Expr, body: &[ast::Stmt]) {
        // Create labels for the loop structure
        let cond_label = self.fresh_label("while_cond");
        let body_label = self.fresh_label("while_body");
        let end_label = self.fresh_label("while_end");

        // Jump to condition block
        self.finish_block(IrTerminator::Branch(cond_label.clone()));

        // Condition block
        self.start_block(cond_label.clone());
        let cond_value = self.lower_expr(condition).unwrap_or(IrValue::ConstBool(false));
        self.finish_block(IrTerminator::CondBranch {
            cond: cond_value,
            true_block: body_label.clone(),
            false_block: end_label.clone(),
        });

        // Body block
        self.start_block(body_label);
        self.loop_stack.push((cond_label.clone(), end_label.clone()));
        for stmt in body {
            self.lower_stmt(stmt);
        }
        self.loop_stack.pop();
        // Jump back to condition
        self.finish_block(IrTerminator::Branch(cond_label));

        // End block - continue from here
        self.start_block(end_label);
    }

    fn lower_if(
        &mut self,
        cond: &ast::Expr,
        then_branch: &[ast::Stmt],
        else_branch: Option<&[ast::Stmt]>,
    ) -> Option<IrValue> {
        let then_label = self.fresh_label("if_then");
        let else_label = self.fresh_label("if_else");
        let end_label = self.fresh_label("if_end");

        // Evaluate condition
        let cond_val = self.lower_expr(cond).unwrap_or(IrValue::ConstBool(false));

        // Allocate result variable (for if-expression value)
        let result_var = self.fresh_var();
        self.current_block.push(IrInst::Alloca {
            dest: result_var.clone(),
            ty: IrType::I64, // Simplified: assume i64 result
        });

        // Branch based on condition
        self.finish_block(IrTerminator::CondBranch {
            cond: cond_val,
            true_block: then_label.clone(),
            false_block: if else_branch.is_some() {
                else_label.clone()
            } else {
                end_label.clone()
            },
        });

        // Then block
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

        // Else block (if present)
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

        // End block
        self.start_block(end_label);

        // Load and return result
        let result_dest = self.fresh_var();
        self.current_block.push(IrInst::Load {
            dest: result_dest.clone(),
            ptr: result_var,
            ty: IrType::I64,
        });
        Some(IrValue::Var(result_dest))
    }

    fn lower_match(&mut self, value: &ast::Expr, arms: &[ast::MatchArm]) -> Option<IrValue> {
        // Evaluate the match value
        let match_val = self.lower_expr(value)?;

        // Create labels for each arm and the end
        let arm_labels: Vec<_> = arms.iter().map(|_| self.fresh_label("match_arm")).collect();
        let end_label = self.fresh_label("match_end");

        // Result variable for the match expression value
        let result_var = self.fresh_var();
        self.current_block.push(IrInst::Alloca {
            dest: result_var.clone(),
            ty: IrType::I64, // Simplified: assume i64 result
        });

        // For now, we implement a simple chain of conditional branches
        // This handles literal patterns and wildcards
        // Constructor patterns for enums need tag comparison

        // Store match value for later comparisons
        let match_val_ptr = self.fresh_var();
        self.current_block.push(IrInst::Alloca {
            dest: match_val_ptr.clone(),
            ty: IrType::I64,
        });
        self.current_block.push(IrInst::Store {
            value: match_val.clone(),
            ptr: match_val_ptr.clone(),
        });

        // Generate comparison chain
        for (i, arm) in arms.iter().enumerate() {
            let arm_label = arm_labels[i].clone();
            let next_label = if i + 1 < arms.len() {
                self.fresh_label("match_check")
            } else {
                end_label.clone()
            };

            match &arm.pattern {
                ast::Pattern::Wildcard(_) | ast::Pattern::Ident(_, _) => {
                    // Wildcard or binding always matches - jump directly to arm
                    self.finish_block(IrTerminator::Branch(arm_label.clone()));
                }
                ast::Pattern::Literal(lit_expr) => {
                    // Compare match value with literal
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
                    // For enum matching, compare the tag
                    // Load tag from match value (assumes enum is tagged union)
                    let tag_ptr = self.fresh_var();
                    self.current_block.push(IrInst::GetElementPtr {
                        dest: tag_ptr.clone(),
                        ptr: match_val_ptr.clone(),
                        indices: vec![
                            IrValue::ConstInt(0, IrType::I32),
                            IrValue::ConstInt(0, IrType::I32),
                        ],
                    });
                    let tag_val = self.fresh_var();
                    self.current_block.push(IrInst::Load {
                        dest: tag_val.clone(),
                        ptr: tag_ptr,
                        ty: IrType::I32,
                    });

                    // Compare with expected tag
                    let expected_tag = self.get_variant_tag("", variant_name);
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

                    // Create binding for payload in arm block
                    self.start_block(arm_label.clone());
                    if !bindings.is_empty() {
                        if let ast::Pattern::Ident(binding_name, _) = &bindings[0] {
                            // Load payload and bind it
                            let payload_ptr = self.fresh_var();
                            self.current_block.push(IrInst::GetElementPtr {
                                dest: payload_ptr.clone(),
                                ptr: match_val_ptr.clone(),
                                indices: vec![
                                    IrValue::ConstInt(0, IrType::I32),
                                    IrValue::ConstInt(1, IrType::I32),
                                ],
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
                    // Evaluate arm body
                    if let Some(body_val) = self.lower_expr(&arm.body) {
                        self.current_block.push(IrInst::Store {
                            value: body_val,
                            ptr: result_var.clone(),
                        });
                    }
                    self.finish_block(IrTerminator::Branch(end_label.clone()));
                    self.start_block(next_label);
                    continue; // Skip normal arm processing
                }
            }

            // Arm block for non-constructor patterns
            self.start_block(arm_label);

            // Bind value if pattern is identifier
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

            // Evaluate arm body
            if let Some(body_val) = self.lower_expr(&arm.body) {
                self.current_block.push(IrInst::Store {
                    value: body_val,
                    ptr: result_var.clone(),
                });
            }
            self.finish_block(IrTerminator::Branch(end_label.clone()));

            // Next check block (if not last arm)
            if i + 1 < arms.len() {
                self.start_block(next_label);
            }
        }

        // End block
        self.start_block(end_label);
        let result_dest = self.fresh_var();
        self.current_block.push(IrInst::Load {
            dest: result_dest.clone(),
            ptr: result_var,
            ty: IrType::I64,
        });
        Some(IrValue::Var(result_dest))
    }

    fn lower_for(&mut self, var: &str, iterable: &ast::Expr, body: &[ast::Stmt]) {
        // For now, we'll implement a simple for-range loop pattern
        // This assumes iterable is a range-like expression
        // TODO: Support proper iterators

        // Create loop variable
        self.current_block.push(IrInst::Alloca {
            dest: var.to_string(),
            ty: IrType::I64,
        });

        // Initialize to start value (for now assume iterable gives start..end)
        // For simplicity, initialize to 0
        self.current_block.push(IrInst::Store {
            value: IrValue::ConstInt(0, IrType::I64),
            ptr: var.to_string(),
        });

        // Create labels
        let cond_label = self.fresh_label("for_cond");
        let body_label = self.fresh_label("for_body");
        let incr_label = self.fresh_label("for_incr");
        let end_label = self.fresh_label("for_end");

        // Jump to condition
        self.finish_block(IrTerminator::Branch(cond_label.clone()));

        // Condition: check if var < iterable (end value)
        self.start_block(cond_label.clone());
        let iter_val = self.lower_expr(iterable).unwrap_or(IrValue::ConstInt(0, IrType::I64));
        let cond_temp = self.fresh_var();
        self.current_block.push(IrInst::BinOp {
            dest: cond_temp.clone(),
            op: IrBinOp::Lt,
            left: IrValue::Var(var.to_string()),
            right: iter_val,
        });
        self.finish_block(IrTerminator::CondBranch {
            cond: IrValue::Var(cond_temp),
            true_block: body_label.clone(),
            false_block: end_label.clone(),
        });

        // Body
        self.start_block(body_label);
        self.loop_stack.push((incr_label.clone(), end_label.clone()));
        for stmt in body {
            self.lower_stmt(stmt);
        }
        self.loop_stack.pop();
        self.finish_block(IrTerminator::Branch(incr_label.clone()));

        // Increment
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

        // End
        self.start_block(end_label);
    }

    fn lower_expr(&mut self, expr: &ast::Expr) -> Option<IrValue> {
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
            ast::Expr::Call(callee, args, _) => {
                let func_name = match callee.as_ref() {
                    ast::Expr::Ident(name, _) => name.clone(),
                    _ => return None,
                };

                let ir_args: Vec<_> = args.iter().filter_map(|a| self.lower_expr(a)).collect();
                let dest = self.fresh_var();

                self.current_block.push(IrInst::Call {
                    dest: Some(dest.clone()),
                    func: func_name,
                    args: ir_args,
                });

                Some(IrValue::Var(dest))
            }
            ast::Expr::StructConstruct { name, fields, .. } => {
                // Allocate space for the struct
                let dest = self.fresh_var();
                self.current_block.push(IrInst::Alloca {
                    dest: dest.clone(),
                    ty: IrType::Struct(name.clone()),
                });

                // Store each field value using GetElementPtr
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
                        });
                        self.current_block.push(IrInst::Store {
                            value: field_val,
                            ptr: field_ptr,
                        });
                    }
                }

                Some(IrValue::Var(dest))
            }
            ast::Expr::FieldAccess(base, field_name, _) => {
                let _base_val = self.lower_expr(base)?;

                // Get pointer to field
                let field_ptr = self.fresh_var();

                // Get struct name from base expression (simplified - assumes variable)
                let struct_name = match base.as_ref() {
                    ast::Expr::Ident(name, _) => name.clone(),
                    _ => return None,
                };

                // For now, we'll need to look up the field index
                // This is simplified - real implementation would lookup field in struct def
                let field_index = self.get_field_index(&struct_name, field_name);

                self.current_block.push(IrInst::GetElementPtr {
                    dest: field_ptr.clone(),
                    ptr: struct_name,
                    indices: vec![
                        IrValue::ConstInt(0, IrType::I32),
                        IrValue::ConstInt(field_index as i64, IrType::I32),
                    ],
                });

                // Load the field value
                let dest = self.fresh_var();
                self.current_block.push(IrInst::Load {
                    dest: dest.clone(),
                    ptr: field_ptr,
                    ty: IrType::I64, // TODO: Get actual field type
                });

                Some(IrValue::Var(dest))
            }
            ast::Expr::EnumConstruct { enum_name, variant, value, .. } => {
                // For simplicity, represent as a struct with tag and optional payload
                let enum_type_name = enum_name.as_ref().cloned().unwrap_or_else(|| "Unknown".to_string());
                let dest = self.fresh_var();

                // Allocate space for the enum (tag + payload)
                self.current_block.push(IrInst::Alloca {
                    dest: dest.clone(),
                    ty: IrType::Struct(format!("{}_{}", enum_type_name, variant)),
                });

                // For now, just store the tag value (simplified)
                // In a full implementation, we'd also store the payload
                let tag_value = self.get_variant_tag(&enum_type_name, variant);
                let tag_ptr = self.fresh_var();
                self.current_block.push(IrInst::GetElementPtr {
                    dest: tag_ptr.clone(),
                    ptr: dest.clone(),
                    indices: vec![
                        IrValue::ConstInt(0, IrType::I32),
                        IrValue::ConstInt(0, IrType::I32),
                    ],
                });
                self.current_block.push(IrInst::Store {
                    value: IrValue::ConstInt(tag_value as i64, IrType::I32),
                    ptr: tag_ptr,
                });

                // Store payload if present
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
                        });
                        self.current_block.push(IrInst::Store {
                            value: payload,
                            ptr: payload_ptr,
                        });
                    }
                }

                Some(IrValue::Var(dest))
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
                    ty: IrType::I64, // TODO: infer type from inner
                    value: inner_val,
                });
                Some(IrValue::Var(dest))
            }
            ast::Expr::RcAlloc(inner, _) => {
                let inner_val = self.lower_expr(inner);
                let dest = self.fresh_var();
                self.current_block.push(IrInst::RcAlloc {
                    dest: dest.clone(),
                    ty: IrType::I64, // TODO: infer type from inner
                    value: inner_val,
                });
                Some(IrValue::Var(dest))
            }
            ast::Expr::ArcAlloc(inner, _) => {
                let inner_val = self.lower_expr(inner);
                let dest = self.fresh_var();
                self.current_block.push(IrInst::ArcAlloc {
                    dest: dest.clone(),
                    ty: IrType::I64, // TODO: infer type from inner
                    value: inner_val,
                });
                Some(IrValue::Var(dest))
            }
            ast::Expr::Lambda { params, body, .. } => {
                // Generate a unique name for this closure
                let closure_name = format!("__closure_{}", self.fresh_var());

                // Save current state
                let saved_block = std::mem::take(&mut self.current_block);
                let saved_blocks = std::mem::take(&mut self.blocks);
                let saved_label = std::mem::replace(&mut self.current_label, "entry".to_string());

                // Lower the body expression
                let body_val = self.lower_expr(body);

                // Create the entry block with body instructions and return terminator
                let entry_block = IrBlock {
                    label: "entry".to_string(),
                    instructions: std::mem::take(&mut self.current_block),
                    terminator: IrTerminator::Return(body_val),
                };

                // Create the closure function
                let closure_func = IrFunction {
                    name: closure_name.clone(),
                    params: params
                        .iter()
                        .map(|p| (p.name.clone(), self.lower_type(&p.ty)))
                        .collect(),
                    return_type: IrType::I64, // TODO: infer from body
                    blocks: vec![entry_block],
                };

                // Add the function to be emitted later
                self.closure_funcs.push(closure_func);

                // Restore state
                self.current_block = saved_block;
                self.blocks = saved_blocks;
                self.current_label = saved_label;

                // Return a reference to the closure function
                Some(IrValue::Var(closure_name))
            }
            _ => None,
        }
    }

    // Get variant tag - in real impl, would lookup from enum definition
    fn get_variant_tag(&self, _enum_name: &str, _variant: &str) -> u32 {
        // For now, return 0 for None-like variants, 1 for Some-like variants
        // Real implementation would look this up from the enum definition
        0
    }

    // Simplified field index lookup - in real impl, would use struct definitions
    fn get_field_index(&self, _struct_name: &str, _field_name: &str) -> usize {
        // TODO: Look up actual field index from struct definition
        0
    }

    fn lower_type(&self, ty: &ast::Type) -> IrType {
        match ty {
            ast::Type::Named(name) => match name.as_str() {
                "i8" => IrType::I8,
                "i16" => IrType::I16,
                "i32" => IrType::I32,
                "i64" => IrType::I64,
                "u8" => IrType::U8,
                "u16" => IrType::U16,
                "u32" => IrType::U32,
                "u64" => IrType::U64,
                "f32" => IrType::F32,
                "f64" => IrType::F64,
                "bool" => IrType::Bool,
                _ => IrType::Struct(name.clone()),
            },
            ast::Type::Reference(inner, _) => IrType::Ptr(Box::new(self.lower_type(inner))),
            ast::Type::Heap(inner) | ast::Type::Rc(inner) | ast::Type::Arc(inner) => {
                IrType::Ptr(Box::new(self.lower_type(inner)))
            }
            ast::Type::Unit => IrType::Void,
            _ => IrType::I64, // Default for now
        }
    }

    fn lower_binop(&self, op: ast::BinOp) -> IrBinOp {
        match op {
            ast::BinOp::Add => IrBinOp::Add,
            ast::BinOp::Sub => IrBinOp::Sub,
            ast::BinOp::Mul => IrBinOp::Mul,
            ast::BinOp::Div => IrBinOp::Div,
            ast::BinOp::Mod => IrBinOp::Rem,
            ast::BinOp::Eq => IrBinOp::Eq,
            ast::BinOp::NotEq => IrBinOp::Ne,
            ast::BinOp::Lt => IrBinOp::Lt,
            ast::BinOp::LtEq => IrBinOp::Le,
            ast::BinOp::Gt => IrBinOp::Gt,
            ast::BinOp::GtEq => IrBinOp::Ge,
            ast::BinOp::And => IrBinOp::And,
            ast::BinOp::Or => IrBinOp::Or,
        }
    }

    fn fresh_var(&mut self) -> String {
        let var = format!("_t{}", self.var_counter);
        self.var_counter += 1;
        var
    }

    fn finish_block(&mut self, terminator: IrTerminator) {
        let block = IrBlock {
            label: std::mem::take(&mut self.current_label),
            instructions: std::mem::take(&mut self.current_block),
            terminator,
        };
        self.blocks.push(block);
    }

    fn start_block(&mut self, label: String) {
        self.current_label = label;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;

    fn lower(source: &str) -> IrModule {
        let mut parser = Parser::new(source);
        let program = parser.parse().expect("Parse error");
        lower_program(&program)
    }

    #[test]
    fn test_simple_function_lowering() {
        let source = r#"fn main() -> i64
    42
"#;
        let module = lower(source);
        assert_eq!(module.functions.len(), 1);
        assert_eq!(module.functions[0].name, "main");
        assert_eq!(module.functions[0].return_type, IrType::I64);
    }

    #[test]
    fn test_function_with_params() {
        let source = r#"fn add(a: i64, b: i64) -> i64
    a + b
"#;
        let module = lower(source);
        assert_eq!(module.functions[0].params.len(), 2);
        assert_eq!(module.functions[0].params[0].0, "a");
        assert_eq!(module.functions[0].params[1].0, "b");
    }

    #[test]
    fn test_let_binding_generates_alloca() {
        let source = r#"fn main()
    let x = 42
"#;
        let module = lower(source);
        let block = &module.functions[0].blocks[0];

        let has_alloca = block
            .instructions
            .iter()
            .any(|inst| matches!(inst, IrInst::Alloca { dest, .. } if dest == "x"));
        assert!(has_alloca);
    }

    #[test]
    fn test_let_binding_generates_store() {
        let source = r#"fn main()
    let x = 42
"#;
        let module = lower(source);
        let block = &module.functions[0].blocks[0];

        let has_store = block
            .instructions
            .iter()
            .any(|inst| matches!(inst, IrInst::Store { ptr, .. } if ptr == "x"));
        assert!(has_store);
    }

    #[test]
    fn test_binary_op_generates_binop() {
        let source = r#"fn main() -> i64
    1 + 2
"#;
        let module = lower(source);
        let block = &module.functions[0].blocks[0];

        let has_binop = block.instructions.iter().any(|inst| {
            matches!(
                inst,
                IrInst::BinOp {
                    op: IrBinOp::Add,
                    ..
                }
            )
        });
        assert!(has_binop);
    }

    #[test]
    fn test_function_call_generates_call() {
        let source = r#"fn helper() -> i64
    42

fn main() -> i64
    helper()
"#;
        let module = lower(source);
        let main_func = module.functions.iter().find(|f| f.name == "main").unwrap();
        let block = &main_func.blocks[0];

        let has_call = block
            .instructions
            .iter()
            .any(|inst| matches!(inst, IrInst::Call { func, .. } if func == "helper"));
        assert!(has_call);
    }

    #[test]
    fn test_struct_lowering() {
        let source = r#"struct Point
    x: f64
    y: f64

fn main()
    42
"#;
        let module = lower(source);
        assert_eq!(module.structs.len(), 1);
        assert_eq!(module.structs[0].name, "Point");
        assert_eq!(module.structs[0].fields.len(), 2);
    }

    #[test]
    fn test_return_terminator() {
        let source = r#"fn main() -> i64
    42
"#;
        let module = lower(source);
        let block = &module.functions[0].blocks[0];

        assert!(matches!(block.terminator, IrTerminator::Return(Some(_))));
    }

    #[test]
    fn test_void_return() {
        let source = r#"fn main()
    let x = 1
"#;
        let module = lower(source);
        assert_eq!(module.functions[0].return_type, IrType::Void);
    }

    #[test]
    fn test_type_lowering() {
        let source = r#"fn test(a: i32, b: f64, c: bool) -> i64
    a
"#;
        let module = lower(source);
        let params = &module.functions[0].params;
        assert_eq!(params[0].1, IrType::I32);
        assert_eq!(params[1].1, IrType::F64);
        assert_eq!(params[2].1, IrType::Bool);
    }

    #[test]
    fn test_multiple_statements() {
        let source = r#"fn main()
    let x = 1
    let y = 2
    let z = x + y
"#;
        let module = lower(source);
        let block = &module.functions[0].blocks[0];

        // Should have 3 allocas and 3 stores for x, y, z, plus binop for x + y
        let alloca_count = block
            .instructions
            .iter()
            .filter(|i| matches!(i, IrInst::Alloca { .. }))
            .count();
        assert_eq!(alloca_count, 3);
    }

    #[test]
    fn test_match_lowering() {
        let source = r#"fn test(x: i64) -> i64
    match x
        1 => 10
        2 => 20
        _ => 0
"#;
        let module = lower(source);
        assert_eq!(module.functions.len(), 1);
        // Match produces multiple blocks for each arm
        assert!(module.functions[0].blocks.len() >= 3);
    }

    #[test]
    fn test_if_lowering() {
        let source = r#"fn test(x: bool) -> i64
    if x
        1
    else
        0
"#;
        let module = lower(source);
        assert_eq!(module.functions.len(), 1);
        // If-else produces: entry, then, else, end blocks
        assert!(module.functions[0].blocks.len() >= 3);
    }

    #[test]
    fn test_enum_lowering() {
        let source = r#"enum Option
    Some(i64)
    None

fn main()
    let x = Option::Some(42)
"#;
        let module = lower(source);
        assert_eq!(module.enums.len(), 1);
        assert_eq!(module.enums[0].name, "Option");
        assert_eq!(module.enums[0].variants.len(), 2);
    }
}
