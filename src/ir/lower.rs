//! Lowering from AST to Lux IR.

use super::lux_ir::*;
use crate::parser as ast;

/// Lower a program AST to Lux IR.
pub fn lower_program(program: &ast::Program) -> IrModule {
    let mut lowerer = Lowerer::new();
    lowerer.lower(program)
}

struct Lowerer {
    current_block: Vec<IrInst>,
    blocks: Vec<IrBlock>,
    block_counter: usize,
    var_counter: usize,
}

impl Lowerer {
    fn new() -> Self {
        Self {
            current_block: Vec::new(),
            blocks: Vec::new(),
            block_counter: 0,
            var_counter: 0,
        }
    }

    fn lower(&mut self, program: &ast::Program) -> IrModule {
        let mut functions = Vec::new();
        let mut structs = Vec::new();

        for item in &program.items {
            match item {
                ast::Item::Function(f) => {
                    if let Some(ir_func) = self.lower_function(f) {
                        functions.push(ir_func);
                    }
                }
                ast::Item::Struct(s) => {
                    structs.push(self.lower_struct(s));
                }
                ast::Item::Enum(_) => {
                    // TODO: Lower enums to tagged unions
                }
            }
        }

        IrModule { functions, structs }
    }

    fn lower_function(&mut self, func: &ast::Function) -> Option<IrFunction> {
        self.current_block.clear();
        self.blocks.clear();
        self.block_counter = 0;

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
        self.finish_block("entry".to_string(), terminator);

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
            ast::Stmt::Expr(e) => self.lower_expr(e),
            ast::Stmt::Return(Some(e), _) => self.lower_expr(e),
            ast::Stmt::Return(None, _) => Some(IrValue::Unit),
        }
    }

    fn lower_expr(&mut self, expr: &ast::Expr) -> Option<IrValue> {
        match expr {
            ast::Expr::Int(n, _) => Some(IrValue::ConstInt(*n, IrType::I64)),
            ast::Expr::Float(n, _) => Some(IrValue::ConstFloat(*n, IrType::F64)),
            ast::Expr::Bool(b, _) => Some(IrValue::ConstBool(*b)),
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
            _ => None,
        }
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

    fn finish_block(&mut self, label: String, terminator: IrTerminator) {
        let block = IrBlock {
            label,
            instructions: std::mem::take(&mut self.current_block),
            terminator,
        };
        self.blocks.push(block);
    }
}
