//! Type checker implementation for WASD.

use super::types::WasdType;
use crate::parser::{
    BinOp, EnumDef, Expr, Function, Item, Program, Stmt, StructDef, Type, UnaryOp,
};
use std::collections::HashMap;

/// The WASD type checker.
pub struct TypeChecker {
    /// Type environment mapping names to types
    env: HashMap<String, WasdType>,
    /// Counter for generating fresh type variables
    next_var: usize,
    /// Type substitutions from unification
    substitutions: HashMap<usize, WasdType>,
}

impl TypeChecker {
    /// Create a new type checker.
    pub fn new() -> Self {
        Self {
            env: HashMap::new(),
            next_var: 0,
            substitutions: HashMap::new(),
        }
    }

    /// Type check a complete program.
    pub fn check_program(&mut self, program: &Program) -> Result<(), Vec<String>> {
        let mut errors = Vec::new();

        for item in &program.items {
            if let Err(e) = self.check_item(item) {
                errors.push(e);
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    fn check_item(&mut self, item: &Item) -> Result<(), String> {
        match item {
            Item::Function(f) => self.check_function(f),
            Item::Struct(s) => self.register_struct(s),
            Item::Enum(e) => self.register_enum(e),
        }
    }

    fn check_function(&mut self, func: &Function) -> Result<(), String> {
        // Add parameters to environment
        for param in &func.params {
            let ty = self.ast_type_to_wasd_type(&param.ty)?;
            self.env.insert(param.name.clone(), ty);
        }

        // Check body statements
        for stmt in &func.body {
            self.check_stmt(stmt)?;
        }

        Ok(())
    }

    fn register_struct(&mut self, _s: &StructDef) -> Result<(), String> {
        // TODO: Register struct type
        Ok(())
    }

    fn register_enum(&mut self, _e: &EnumDef) -> Result<(), String> {
        // TODO: Register enum type
        Ok(())
    }

    fn check_stmt(&mut self, stmt: &Stmt) -> Result<WasdType, String> {
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
            Stmt::Expr(e) => self.infer_expr(e),
            Stmt::Return(Some(e), _) => self.infer_expr(e),
            Stmt::Return(None, _) => Ok(WasdType::Unit),
        }
    }

    fn infer_expr(&mut self, expr: &Expr) -> Result<WasdType, String> {
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
            Expr::Call(callee, args, _) => {
                let callee_ty = self.infer_expr(callee)?;
                match callee_ty {
                    WasdType::Function { params, ret, .. } => {
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
            _ => Ok(WasdType::Unknown),
        }
    }

    fn check_block(&mut self, stmts: &[Stmt]) -> Result<WasdType, String> {
        let mut last_ty = WasdType::Unit;
        for stmt in stmts {
            last_ty = self.check_stmt(stmt)?;
        }
        Ok(last_ty)
    }

    fn check_binary_op(
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

    fn check_unary_op(&self, op: UnaryOp, inner: &WasdType) -> Result<WasdType, String> {
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

    fn ast_type_to_wasd_type(&self, ty: &Type) -> Result<WasdType, String> {
        match ty {
            Type::Named(name) => match name.as_str() {
                "i8" => Ok(WasdType::I8),
                "i16" => Ok(WasdType::I16),
                "i32" => Ok(WasdType::I32),
                "i64" => Ok(WasdType::I64),
                "u8" => Ok(WasdType::U8),
                "u16" => Ok(WasdType::U16),
                "u32" => Ok(WasdType::U32),
                "u64" => Ok(WasdType::U64),
                "f32" => Ok(WasdType::F32),
                "f64" => Ok(WasdType::F64),
                "bool" => Ok(WasdType::Bool),
                "String" => Ok(WasdType::String),
                _ => Ok(WasdType::Named(name.clone())),
            },
            Type::Generic(name, args) => {
                let wasd_args: Result<Vec<_>, _> =
                    args.iter().map(|a| self.ast_type_to_wasd_type(a)).collect();
                Ok(WasdType::Generic(name.clone(), wasd_args?))
            }
            Type::Reference(inner, is_mut) => {
                let inner_ty = self.ast_type_to_wasd_type(inner)?;
                Ok(WasdType::Ref(Box::new(inner_ty), *is_mut))
            }
            Type::Heap(inner) => {
                let inner_ty = self.ast_type_to_wasd_type(inner)?;
                Ok(WasdType::Heap(Box::new(inner_ty)))
            }
            Type::Rc(inner) => {
                let inner_ty = self.ast_type_to_wasd_type(inner)?;
                Ok(WasdType::Rc(Box::new(inner_ty)))
            }
            Type::Arc(inner) => {
                let inner_ty = self.ast_type_to_wasd_type(inner)?;
                Ok(WasdType::Arc(Box::new(inner_ty)))
            }
            Type::Unit => Ok(WasdType::Unit),
            Type::Function(params, ret) => {
                let wasd_params: Result<Vec<_>, _> = params
                    .iter()
                    .map(|p| self.ast_type_to_wasd_type(p))
                    .collect();
                let wasd_ret = self.ast_type_to_wasd_type(ret)?;
                Ok(WasdType::Function {
                    params: wasd_params?,
                    ret: Box::new(wasd_ret),
                    effects: Vec::new(),
                })
            }
        }
    }

    fn fresh_var(&mut self) -> WasdType {
        let var = self.next_var;
        self.next_var += 1;
        WasdType::Var(var)
    }

    fn unify(&mut self, a: &WasdType, b: &WasdType) -> Result<(), String> {
        if a == b {
            return Ok(());
        }
        match (a, b) {
            (WasdType::Var(v), ty) | (ty, WasdType::Var(v)) => {
                self.substitutions.insert(*v, ty.clone());
                Ok(())
            }
            (WasdType::Ref(a, a_mut), WasdType::Ref(b, b_mut)) if a_mut == b_mut => self.unify(a, b),
            _ => Err(format!("Cannot unify {:?} with {:?}", a, b)),
        }
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}
