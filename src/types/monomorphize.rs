//! Monomorphization pass for generics.
//!
//! This pass specializes generic functions and types for each concrete type
//! instantiation, replacing generic parameters with their actual types.

use crate::parser::{EnumDef, Expr, Function, Item, Program, Stmt, StructDef, Type};
use std::collections::{HashMap, HashSet};

/// A monomorphization key: the generic name plus concrete type arguments.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MonoKey {
    pub name: String,
    pub type_args: Vec<Type>,
}

impl MonoKey {
    pub fn new(name: String, type_args: Vec<Type>) -> Self {
        Self { name, type_args }
    }

    /// Generate a mangled name for this instantiation.
    pub fn mangled_name(&self) -> String {
        if self.type_args.is_empty() {
            self.name.clone()
        } else {
            let args: Vec<String> = self.type_args.iter().map(type_to_string).collect();
            format!("{}_{}", self.name, args.join("_"))
        }
    }
}

/// Convert a type to a string for name mangling.
fn type_to_string(ty: &Type) -> String {
    match ty {
        Type::Named(name) => name.clone(),
        Type::Generic(name, args) => {
            let arg_strs: Vec<String> = args.iter().map(type_to_string).collect();
            format!("{}_{}", name, arg_strs.join("_"))
        }
        Type::Reference(inner, is_mut) => {
            let prefix = if *is_mut { "refmut" } else { "ref" };
            format!("{}_{}", prefix, type_to_string(inner))
        }
        Type::Heap(inner) => format!("heap_{}", type_to_string(inner)),
        Type::Rc(inner) => format!("rc_{}", type_to_string(inner)),
        Type::Arc(inner) => format!("arc_{}", type_to_string(inner)),
        Type::Function(params, ret) => {
            let param_strs: Vec<String> = params.iter().map(type_to_string).collect();
            format!("fn_{}_{}", param_strs.join("_"), type_to_string(ret))
        }
        Type::Unit => "unit".to_string(),
    }
}

/// The monomorphizer collects generic instantiations and generates specialized versions.
pub struct Monomorphizer {
    /// Generic struct definitions
    pub(super) generic_structs: HashMap<String, StructDef>,
    /// Generic enum definitions
    pub(super) generic_enums: HashMap<String, EnumDef>,
    /// Generic function definitions
    #[allow(dead_code)]
    generic_functions: HashMap<String, Function>,
    /// Collected struct instantiations that need specialization
    struct_instantiations: HashSet<MonoKey>,
    /// Collected enum instantiations that need specialization
    enum_instantiations: HashSet<MonoKey>,
}

impl Monomorphizer {
    pub fn new() -> Self {
        Self {
            generic_structs: HashMap::new(),
            generic_enums: HashMap::new(),
            generic_functions: HashMap::new(),
            struct_instantiations: HashSet::new(),
            enum_instantiations: HashSet::new(),
        }
    }

    /// Run monomorphization on a program.
    pub fn monomorphize(&mut self, program: &Program) -> Program {
        // First pass: collect generic definitions
        for item in &program.items {
            match item {
                Item::Struct(s) if !s.generics.is_empty() => {
                    self.generic_structs.insert(s.name.clone(), s.clone());
                }
                Item::Enum(e) if !e.generics.is_empty() => {
                    self.generic_enums.insert(e.name.clone(), e.clone());
                }
                Item::Function(_f) => {
                    // TODO: Support generic functions
                }
                _ => {}
            }
        }

        // Second pass: collect instantiations by walking the AST
        for item in &program.items {
            if let Item::Function(f) = item {
                self.collect_function_instantiations(f);
            }
        }

        // Third pass: generate specialized versions and output new program
        let mut new_items = Vec::new();

        // Keep non-generic items
        for item in &program.items {
            match item {
                Item::Struct(s) if s.generics.is_empty() => {
                    new_items.push(item.clone());
                }
                Item::Enum(e) if e.generics.is_empty() => {
                    new_items.push(item.clone());
                }
                Item::Function(f) => {
                    // Transform function to use specialized types
                    let transformed = self.transform_function(f);
                    new_items.push(Item::Function(transformed));
                }
                Item::Use(u) => {
                    new_items.push(Item::Use(u.clone()));
                }
                Item::Trait(t) => {
                    // Keep traits as-is for now
                    new_items.push(Item::Trait(t.clone()));
                }
                Item::Impl(impl_def) => {
                    // Transform impl to use specialized types
                    new_items.push(Item::Impl(impl_def.clone()));
                }
                _ => {} // Skip generic definitions (they'll be specialized)
            }
        }

        // Add specialized struct instantiations
        for key in &self.struct_instantiations {
            if let Some(generic_def) = self.generic_structs.get(&key.name) {
                let specialized = self.specialize_struct(generic_def, &key.type_args);
                new_items.push(Item::Struct(specialized));
            }
        }

        // Add specialized enum instantiations
        for key in &self.enum_instantiations {
            if let Some(generic_def) = self.generic_enums.get(&key.name) {
                let specialized = self.specialize_enum(generic_def, &key.type_args);
                new_items.push(Item::Enum(specialized));
            }
        }

        Program { items: new_items }
    }

    fn collect_function_instantiations(&mut self, func: &Function) {
        for param in &func.params {
            self.collect_type_instantiations(&param.ty);
        }
        if let Some(ret_ty) = &func.return_type {
            self.collect_type_instantiations(ret_ty);
        }
        for stmt in &func.body {
            self.collect_stmt_instantiations(stmt);
        }
    }

    fn collect_stmt_instantiations(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Let { ty, value, .. } => {
                if let Some(t) = ty {
                    self.collect_type_instantiations(t);
                }
                self.collect_expr_instantiations(value);
            }
            Stmt::Assign { target, value, .. } => {
                self.collect_expr_instantiations(target);
                self.collect_expr_instantiations(value);
            }
            Stmt::Expr(e) => self.collect_expr_instantiations(e),
            Stmt::Return(Some(e), _) => self.collect_expr_instantiations(e),
            Stmt::Return(None, _) => {}
            Stmt::While { condition, body, .. } => {
                self.collect_expr_instantiations(condition);
                for s in body {
                    self.collect_stmt_instantiations(s);
                }
            }
            Stmt::For { iterable, body, .. } => {
                self.collect_expr_instantiations(iterable);
                for s in body {
                    self.collect_stmt_instantiations(s);
                }
            }
            Stmt::Break(_) | Stmt::Continue(_) => {}
        }
    }

    fn collect_expr_instantiations(&mut self, expr: &Expr) {
        match expr {
            Expr::Binary(l, _, r, _) => {
                self.collect_expr_instantiations(l);
                self.collect_expr_instantiations(r);
            }
            Expr::Unary(_, e, _) => self.collect_expr_instantiations(e),
            Expr::Call(callee, type_args, args, _) => {
                self.collect_expr_instantiations(callee);
                for type_arg in type_args {
                    self.collect_type_instantiations(type_arg);
                }
                for arg in args {
                    self.collect_expr_instantiations(arg);
                }
            }
            Expr::FieldAccess(base, _, _) => self.collect_expr_instantiations(base),
            Expr::StructConstruct { name, fields, .. } => {
                // Check if this is a generic struct instantiation
                if self.generic_structs.contains_key(name) {
                    // Try to infer type args from field types
                    // For now, just register with empty type args
                    // Real implementation would infer from context
                }
                for (_, field_expr) in fields {
                    self.collect_expr_instantiations(field_expr);
                }
            }
            Expr::EnumConstruct { value, .. } => {
                if let Some(v) = value {
                    self.collect_expr_instantiations(v);
                }
            }
            Expr::If(cond, then_branch, else_branch, _) => {
                self.collect_expr_instantiations(cond);
                for s in then_branch {
                    self.collect_stmt_instantiations(s);
                }
                if let Some(else_stmts) = else_branch {
                    for s in else_stmts {
                        self.collect_stmt_instantiations(s);
                    }
                }
            }
            Expr::Match(value, arms, _) => {
                self.collect_expr_instantiations(value);
                for arm in arms {
                    self.collect_expr_instantiations(&arm.body);
                }
            }
            Expr::Block(stmts, _) => {
                for s in stmts {
                    self.collect_stmt_instantiations(s);
                }
            }
            Expr::HeapAlloc(e, _) | Expr::RcAlloc(e, _) | Expr::ArcAlloc(e, _) => {
                self.collect_expr_instantiations(e);
            }
            Expr::Lambda { body, .. } => {
                self.collect_expr_instantiations(body);
            }
            Expr::Range { start, end, .. } => {
                self.collect_expr_instantiations(start);
                self.collect_expr_instantiations(end);
            }
            _ => {}
        }
    }

    fn collect_type_instantiations(&mut self, ty: &Type) {
        match ty {
            Type::Generic(name, args) => {
                // Register this instantiation
                if self.generic_structs.contains_key(name) {
                    self.struct_instantiations
                        .insert(MonoKey::new(name.clone(), args.clone()));
                }
                if self.generic_enums.contains_key(name) {
                    self.enum_instantiations
                        .insert(MonoKey::new(name.clone(), args.clone()));
                }
                // Recurse into type args
                for arg in args {
                    self.collect_type_instantiations(arg);
                }
            }
            Type::Reference(inner, _)
            | Type::Heap(inner)
            | Type::Rc(inner)
            | Type::Arc(inner) => {
                self.collect_type_instantiations(inner);
            }
            Type::Function(params, ret) => {
                for p in params {
                    self.collect_type_instantiations(p);
                }
                self.collect_type_instantiations(ret);
            }
            _ => {}
        }
    }
}

impl Default for Monomorphizer {
    fn default() -> Self {
        Self::new()
    }
}

/// Convenience function to monomorphize a program.
pub fn monomorphize(program: &Program) -> Program {
    let mut mono = Monomorphizer::new();
    mono.monomorphize(program)
}
