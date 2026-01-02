//! Type checker implementation for WASD.

#![allow(dead_code)]

use super::types::WasdType;
use crate::parser::{EnumDef, Function, ImplDef, Item, Program, StructDef, TraitDef, Type, UseStmt};
use crate::stdlib;
use std::collections::HashMap;

/// The WASD type checker.
pub struct TypeChecker {
    /// Type environment mapping names to types
    pub env: HashMap<String, WasdType>,
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

        if let Some(imports) = stdlib::resolve_import(&path) {
            if use_stmt.wildcard {
                for (name, ty) in imports {
                    self.env.insert(name, ty);
                }
            } else if let Some(alias) = &use_stmt.alias {
                let item_name = use_stmt.path.last().unwrap();
                if let Some(ty) = imports.get(item_name) {
                    let ty: WasdType = ty.clone();
                    self.env.insert(alias.clone(), ty);
                }
            } else {
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

        for item in &program.items {
            if let Item::Use(use_stmt) = item {
                if let Err(e) = self.process_use(use_stmt) {
                    errors.push(e);
                }
            }
        }

        for item in &program.items {
            if let Item::Function(f) = item {
                if let Err(e) = self.register_function(f) {
                    errors.push(e);
                }
            }
        }

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
        // Skip registering generic functions (they need monomorphization)
        if !func.generics.is_empty() {
            return Ok(());
        }

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
            Item::Use(_) => Ok(()),
            Item::Function(f) => self.check_function(f),
            Item::Struct(s) => self.register_struct(s),
            Item::Enum(e) => self.register_enum(e),
            Item::Trait(t) => self.register_trait(t),
            Item::Impl(impl_def) => self.check_impl(impl_def),
        }
    }

    fn register_trait(&mut self, _trait_def: &TraitDef) -> Result<(), String> {
        Ok(())
    }

    fn check_impl(&mut self, impl_def: &ImplDef) -> Result<(), String> {
        for method in &impl_def.methods {
            self.check_function(method)?;
        }
        Ok(())
    }

    fn check_function(&mut self, func: &Function) -> Result<(), String> {
        // Skip type checking for generic functions (they'll be monomorphized later)
        if !func.generics.is_empty() {
            return Ok(());
        }

        let saved_env = self.env.clone();

        for param in &func.params {
            let ty = self.ast_type_to_wasd_type(&param.ty)?;
            self.env.insert(param.name.clone(), ty);
        }

        for stmt in &func.body {
            self.check_stmt(stmt)?;
        }

        self.env = saved_env;
        Ok(())
    }

    fn register_struct(&mut self, _s: &StructDef) -> Result<(), String> {
        Ok(())
    }

    fn register_enum(&mut self, _e: &EnumDef) -> Result<(), String> {
        Ok(())
    }

    pub fn ast_type_to_wasd_type(&self, ty: &Type) -> Result<WasdType, String> {
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

    #[allow(dead_code)]
    fn fresh_var(&mut self) -> WasdType {
        let var = self.next_var;
        self.next_var += 1;
        WasdType::Var(var)
    }

    pub fn unify(&mut self, a: &WasdType, b: &WasdType) -> Result<(), String> {
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
