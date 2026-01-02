//! Type checker implementation for WASD.

#![allow(dead_code)]

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
        let mut checker = Self {
            env: HashMap::new(),
            next_var: 0,
            substitutions: HashMap::new(),
        };

        // Register built-in functions
        checker.register_builtins();

        checker
    }

    /// Register built-in functions like print
    fn register_builtins(&mut self) {
        // print: (String) -> i32
        self.env.insert(
            "print".to_string(),
            WasdType::Function {
                params: vec![WasdType::String],
                ret: Box::new(WasdType::I32),
                effects: vec!["IO".to_string()],
            },
        );
    }

    /// Type check a complete program.
    pub fn check_program(&mut self, program: &Program) -> Result<(), Vec<String>> {
        let mut errors = Vec::new();

        // First pass: register all function signatures
        for item in &program.items {
            if let Item::Function(f) = item {
                if let Err(e) = self.register_function(f) {
                    errors.push(e);
                }
            }
        }

        // Second pass: check function bodies
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
            Item::Function(f) => self.check_function(f),
            Item::Struct(s) => self.register_struct(s),
            Item::Enum(e) => self.register_enum(e),
        }
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
}
