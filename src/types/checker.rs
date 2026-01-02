//! Type checker implementation for WASD.

#![allow(dead_code)]

use super::exhaustiveness::ExhaustivenessChecker;
use super::types::WasdType;
use crate::parser::{EnumDef, ExternFn, Function, ImplDef, Item, Program, StructDef, TraitDef, Type, UseStmt};
use crate::stdlib;
use std::collections::HashMap;

/// Trait method signature for checking implementations.
#[derive(Debug, Clone)]
pub struct TraitMethodSig {
    pub name: String,
    pub params: Vec<WasdType>,
    pub return_type: WasdType,
}

/// Registered trait information.
#[derive(Debug, Clone)]
pub struct TraitInfo {
    pub name: String,
    pub generics: Vec<String>,
    pub methods: Vec<TraitMethodSig>,
}

/// The WASD type checker.
pub struct TypeChecker {
    /// Type environment mapping names to types
    pub env: HashMap<String, WasdType>,
    /// Counter for generating fresh type variables
    next_var: usize,
    /// Type substitutions from unification
    substitutions: HashMap<usize, WasdType>,
    /// Exhaustiveness checker for pattern matching
    pub exhaustiveness: ExhaustivenessChecker,
    /// Current function's declared effects (for effect validation)
    pub current_effects: Vec<String>,
    /// Registered traits
    traits: HashMap<String, TraitInfo>,
    /// Implementations: maps (trait_name, type_name) -> impl info
    impls: HashMap<(String, String), Vec<String>>, // method names
}

impl TypeChecker {
    /// Create a new type checker.
    pub fn new() -> Self {
        Self {
            env: HashMap::new(),
            next_var: 0,
            substitutions: HashMap::new(),
            exhaustiveness: ExhaustivenessChecker::new(),
            current_effects: Vec::new(),
            traits: HashMap::new(),
            impls: HashMap::new(),
        }
    }

    /// Process a use statement and add imported symbols to the environment.
    fn process_use(&mut self, use_stmt: &UseStmt) -> Result<(), String> {
        let path = use_stmt.path.join(".");

        // Only try to resolve stdlib imports (std.*)
        // Local module imports are handled by the module loader before type checking
        if use_stmt.path.first().map(|s| s.as_str()) != Some("std") {
            // Local import - items are already added by the module loader
            return Ok(());
        }

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

        // Process use statements first
        for item in &program.items {
            if let Item::Use(use_stmt) = item {
                if let Err(e) = self.process_use(use_stmt) {
                    errors.push(e);
                }
            }
        }

        // Register enums for exhaustiveness checking
        for item in &program.items {
            if let Item::Enum(e) = item {
                self.exhaustiveness.register_enum(e);
            }
        }

        // Register traits before functions (traits can be used in bounds)
        for item in &program.items {
            if let Item::Trait(t) = item {
                if let Err(e) = self.register_trait(t) {
                    errors.push(e);
                }
            }
        }

        // Register functions
        for item in &program.items {
            if let Item::Function(f) = item {
                if let Err(e) = self.register_function(f) {
                    errors.push(e);
                }
            }
        }

        // Check all items
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
            Item::ExternFn(f) => self.register_extern_fn(f),
            Item::Struct(s) => self.register_struct(s),
            Item::Enum(e) => self.register_enum(e),
            Item::Trait(t) => self.register_trait(t),
            Item::Impl(impl_def) => self.check_impl(impl_def),
        }
    }

    fn register_extern_fn(&mut self, func: &ExternFn) -> Result<(), String> {
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
            effects: Vec::new(),
        };

        self.env.insert(func.name.clone(), fn_type);
        Ok(())
    }

    fn register_trait(&mut self, trait_def: &TraitDef) -> Result<(), String> {
        let mut methods = Vec::new();

        for method in &trait_def.methods {
            let params: Result<Vec<WasdType>, String> = method
                .params
                .iter()
                .map(|p| self.ast_type_to_wasd_type(&p.ty))
                .collect();

            let return_type = method
                .return_type
                .as_ref()
                .map(|t| self.ast_type_to_wasd_type(t))
                .transpose()?
                .unwrap_or(WasdType::Unit);

            methods.push(TraitMethodSig {
                name: method.name.clone(),
                params: params?,
                return_type,
            });
        }

        let trait_info = TraitInfo {
            name: trait_def.name.clone(),
            generics: trait_def.generics.clone(),
            methods,
        };

        self.traits.insert(trait_def.name.clone(), trait_info);
        Ok(())
    }

    fn check_impl(&mut self, impl_def: &ImplDef) -> Result<(), String> {
        // Type check all methods in the impl
        for method in &impl_def.methods {
            self.check_function(method)?;
        }

        // If implementing a trait, verify all required methods are present
        if let Some(trait_name) = &impl_def.trait_name {
            if let Some(trait_info) = self.traits.get(trait_name).cloned() {
                // Get the type name being implemented for
                let type_name = match &impl_def.target_type {
                    Type::Named(n) => n.clone(),
                    Type::Generic(n, _) => n.clone(),
                    _ => return Ok(()), // Skip complex types for now
                };

                // Check that all required trait methods are implemented
                let impl_method_names: Vec<&str> = impl_def
                    .methods
                    .iter()
                    .map(|m| m.name.as_str())
                    .collect();

                for required_method in &trait_info.methods {
                    if !impl_method_names.contains(&required_method.name.as_str()) {
                        return Err(format!(
                            "Missing trait method '{}' in impl {} for {}",
                            required_method.name, trait_name, type_name
                        ));
                    }
                }

                // Register this implementation
                self.impls.insert(
                    (trait_name.clone(), type_name),
                    impl_method_names.iter().map(|s| s.to_string()).collect(),
                );
            }
        }

        Ok(())
    }

    /// Check if a type implements a trait
    pub fn type_implements_trait(&self, type_name: &str, trait_name: &str) -> bool {
        self.impls.contains_key(&(trait_name.to_string(), type_name.to_string()))
    }

    fn check_function(&mut self, func: &Function) -> Result<(), String> {
        // Skip type checking for generic functions (they'll be monomorphized later)
        if !func.generics.is_empty() {
            return Ok(());
        }

        let saved_env = self.env.clone();
        // Save and set current effects context for effect validation
        let saved_effects = std::mem::replace(&mut self.current_effects, func.effects.clone());

        for param in &func.params {
            let ty = self.ast_type_to_wasd_type(&param.ty)?;
            self.env.insert(param.name.clone(), ty);
        }

        for stmt in &func.body {
            self.check_stmt(stmt)?;
        }

        self.env = saved_env;
        self.current_effects = saved_effects;
        Ok(())
    }

    fn register_struct(&mut self, _s: &StructDef) -> Result<(), String> {
        Ok(())
    }

    fn register_enum(&mut self, e: &EnumDef) -> Result<(), String> {
        self.exhaustiveness.register_enum(e);
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
