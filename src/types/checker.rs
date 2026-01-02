//! Type checker implementation for WASD.

#![allow(dead_code)]

use super::types::WasdType;
use crate::parser::{
    BinOp, EnumDef, Expr, Function, ImplDef, Item, Program, Stmt, StructDef, TraitDef, Type, UnaryOp, UseStmt,
};
use crate::stdlib;
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

    /// Process a use statement and add imported symbols to the environment.
    fn process_use(&mut self, use_stmt: &UseStmt) -> Result<(), String> {
        let path = use_stmt.path.join(".");

        // Resolve the import from the standard library
        if let Some(imports) = stdlib::resolve_import(&path) {
            if use_stmt.wildcard {
                // Wildcard import: add all items
                for (name, ty) in imports {
                    self.env.insert(name, ty);
                }
            } else if let Some(alias) = &use_stmt.alias {
                // Aliased import: use the alias name
                // For single item imports, get the last component
                let item_name = use_stmt.path.last().unwrap();
                if let Some(ty) = imports.get(item_name) {
                    let ty: WasdType = ty.clone();
                    self.env.insert(alias.clone(), ty);
                }
            } else {
                // Regular import: add all resolved items
                for (name, ty) in imports {
                    self.env.insert(name, ty);
                }
            }
            Ok(())
        } else {
            Err(format!("Unknown import path: {}", path))
        }
    }

    /// Type check a complete program.
    pub fn check_program(&mut self, program: &Program) -> Result<(), Vec<String>> {
        let mut errors = Vec::new();

        // First pass: process all use statements
        for item in &program.items {
            if let Item::Use(use_stmt) = item {
                if let Err(e) = self.process_use(use_stmt) {
                    errors.push(e);
                }
            }
        }

        // Second pass: register all function signatures
        for item in &program.items {
            if let Item::Function(f) = item {
                if let Err(e) = self.register_function(f) {
                    errors.push(e);
                }
            }
        }

        // Third pass: check function bodies and other items
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

    fn register_function(&mut self, func: &Function) -> Result<(), String> {
        let param_types: Result<Vec<WasdType>, String> = func
            .params
            .iter()
            .map(|p| self.ast_type_to_wasd_type(&p.ty))
            .collect();

        let ret_type = func
            .return_type
            .as_ref()
            .map(|t| self.ast_type_to_wasd_type(t))
            .transpose()?
            .unwrap_or(WasdType::Unit);

        let fn_type = WasdType::Function {
            params: param_types?,
            ret: Box::new(ret_type),
            effects: func.effects.clone(),
        };

        self.env.insert(func.name.clone(), fn_type);
        Ok(())
    }

    fn check_item(&mut self, item: &Item) -> Result<(), String> {
        match item {
            Item::Use(_) => Ok(()), // Already processed in first pass
            Item::Function(f) => self.check_function(f),
            Item::Struct(s) => self.register_struct(s),
            Item::Enum(e) => self.register_enum(e),
            Item::Trait(t) => self.register_trait(t),
            Item::Impl(impl_def) => self.check_impl(impl_def),
        }
    }

    fn register_trait(&mut self, _trait_def: &TraitDef) -> Result<(), String> {
        // TODO: Register trait in type environment
        // For now, just accept it
        Ok(())
    }

    fn check_impl(&mut self, impl_def: &ImplDef) -> Result<(), String> {
        // Check each method in the impl
        for method in &impl_def.methods {
            self.check_function(method)?;
        }
        Ok(())
    }

    fn check_function(&mut self, func: &Function) -> Result<(), String> {
        // Save current environment
        let saved_env = self.env.clone();

        // Add parameters to environment
        for param in &func.params {
            let ty = self.ast_type_to_wasd_type(&param.ty)?;
            self.env.insert(param.name.clone(), ty);
        }

        // Check body statements
        for stmt in &func.body {
            self.check_stmt(stmt)?;
        }

        // Restore environment (remove local variables)
        self.env = saved_env;

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
                // For now, we'll assume the iterable is a range or something similar
                // In the future, we'll need to check for an iterator trait
                let _iter_ty = self.infer_expr(iterable)?;
                // Temporarily add the loop variable to the environment
                let saved = self.env.get(var).cloned();
                self.env.insert(var.clone(), WasdType::I64); // TODO: infer from iterable
                for stmt in body {
                    self.check_stmt(stmt)?;
                }
                // Restore previous value if there was one
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
            Expr::FieldAccess(base, _field, _) => {
                let _base_ty = self.infer_expr(base)?;
                // TODO: Look up field type in struct definition
                // For now, return Unknown
                Ok(WasdType::Unknown)
            }
            Expr::StructConstruct { name, fields, .. } => {
                // Check each field expression
                for (_, value) in fields {
                    self.infer_expr(value)?;
                }
                // Return the struct type
                Ok(WasdType::Named(name.clone()))
            }
            Expr::EnumConstruct { enum_name, value, .. } => {
                // Check the value expression if present
                if let Some(v) = value {
                    self.infer_expr(v)?;
                }
                // Return the enum type
                if let Some(name) = enum_name {
                    Ok(WasdType::Named(name.clone()))
                } else {
                    Ok(WasdType::Unknown)
                }
            }
            Expr::Match(value, arms, _) => {
                self.infer_expr(value)?;
                // For now, assume match returns Unknown
                // TODO: Check arm types are compatible
                for arm in arms {
                    self.infer_expr(&arm.body)?;
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
                // Create a new scope for the closure
                let old_env = self.env.clone();

                // Add parameters to the environment
                for param in params {
                    let ty = self.ast_type_to_wasd_type(&param.ty)?;
                    self.env.insert(param.name.clone(), ty);
                }

                // Infer the body type
                let ret_ty = self.infer_expr(body)?;

                // Restore the environment
                self.env = old_env;

                // Build the function type
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
            (WasdType::Ref(a, a_mut), WasdType::Ref(b, b_mut)) if a_mut == b_mut => {
                self.unify(a, b)
            }
            _ => Err(format!("Cannot unify {:?} with {:?}", a, b)),
        }
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;

    fn check(source: &str) -> Result<(), Vec<String>> {
        let mut parser = Parser::new(source);
        let program = parser.parse().expect("Parse error");
        let mut checker = TypeChecker::new();
        checker.check_program(&program)
    }

    #[test]
    fn test_simple_function() {
        let source = r#"fn add(a: i64, b: i64) -> i64
    a + b
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_function_call() {
        let source = r#"fn add(a: i64, b: i64) -> i64
    a + b

fn main() -> i64
    add(1, 2)
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_undefined_variable() {
        let source = r#"fn main()
    undefined_var
"#;
        let result = check(source);
        assert!(result.is_err());
        assert!(result.unwrap_err()[0].contains("Undefined variable"));
    }

    #[test]
    fn test_undefined_function() {
        let source = r#"fn main()
    undefined_func()
"#;
        let result = check(source);
        assert!(result.is_err());
    }

    #[test]
    fn test_let_binding() {
        let source = r#"fn main()
    let x = 42
    x
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_type_annotation() {
        let source = r#"fn main()
    let x: i64 = 42
    x
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_binary_op_type() {
        let source = r#"fn main() -> i64
    1 + 2
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_comparison_op() {
        let source = r#"fn main() -> bool
    1 < 2
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_logical_op() {
        let source = r#"fn main() -> bool
    true and false
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_struct_type_registered() {
        let source = r#"struct Point
    x: f64
    y: f64

fn main()
    let x = 1
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_enum_type_registered() {
        let source = r#"enum Option[T]
    Some(T)
    None

fn main()
    let x = 1
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_effects_annotation() {
        let source = r#"fn read_file() -> String with [IO]
    file_contents
"#;
        // This tests that effects are captured from parsing
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();
        let mut checker = TypeChecker::new();
        // Should register the function with effects
        let _ = checker.check_program(&program);

        if let Some(WasdType::Function { effects, .. }) = checker.env.get("read_file") {
            assert_eq!(effects, &vec!["IO".to_string()]);
        } else {
            panic!("Expected function with effects");
        }
    }

    #[test]
    fn test_unary_negation() {
        let source = r#"fn main() -> i64
    -42
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_unary_not() {
        let source = r#"fn main() -> bool
    not true
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_multiple_functions() {
        let source = r#"fn helper(x: i64) -> i64
    x + 1

fn main() -> i64
    helper(41)
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_use_stdlib_io() {
        let source = r#"use std.io

fn main()
    print("hello")
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_use_stdlib_prelude() {
        let source = r#"use std.prelude.*

fn main()
    println("hello")
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_use_specific_function() {
        let source = r#"use std.io.println

fn main()
    println("world")
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_use_unknown_module() {
        let source = r#"use unknown.module

fn main()
    something()
"#;
        let result = check(source);
        assert!(result.is_err());
        assert!(result.unwrap_err()[0].contains("Unknown import path"));
    }

    #[test]
    fn test_use_stdlib_types() {
        // Note: Some(42) would need proper generic handling
        // For now, just test that the import works
        let source = r#"use std.types

fn main()
    let x = None
"#;
        let result = check(source);
        if let Err(e) = &result {
            eprintln!("Errors: {:?}", e);
        }
        assert!(result.is_ok());
    }

    #[test]
    fn test_use_stdlib_collections() {
        let source = r#"use std.collections

fn main()
    let v = Vec_new()
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_use_stdlib_string() {
        let source = r#"use std.string

fn main()
    let s = String_new()
"#;
        assert!(check(source).is_ok());
    }
}
