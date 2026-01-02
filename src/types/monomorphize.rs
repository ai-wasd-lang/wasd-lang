//! Monomorphization pass for generics.
//!
//! This pass specializes generic functions and types for each concrete type
//! instantiation, replacing generic parameters with their actual types.

use crate::parser::{
    EnumDef, Expr, Field, Function, Item, Param, Pattern, Program, Stmt, StructDef, Type, Variant,
};
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
    generic_structs: HashMap<String, StructDef>,
    /// Generic enum definitions
    generic_enums: HashMap<String, EnumDef>,
    /// Generic function definitions
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
            match item {
                Item::Function(f) => self.collect_function_instantiations(f),
                _ => {}
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
            Expr::Call(callee, args, _) => {
                self.collect_expr_instantiations(callee);
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

    fn specialize_struct(&self, generic_def: &StructDef, type_args: &[Type]) -> StructDef {
        // Create substitution map: generic param -> concrete type
        let mut substitutions: HashMap<String, Type> = HashMap::new();
        for (i, generic_param) in generic_def.generics.iter().enumerate() {
            if let Some(concrete_type) = type_args.get(i) {
                substitutions.insert(generic_param.clone(), concrete_type.clone());
            }
        }

        // Create specialized fields
        let fields: Vec<Field> = generic_def
            .fields
            .iter()
            .map(|f| Field {
                name: f.name.clone(),
                ty: self.substitute_type(&f.ty, &substitutions),
                span: f.span.clone(),
            })
            .collect();

        // Create mangled name
        let key = MonoKey::new(generic_def.name.clone(), type_args.to_vec());

        StructDef {
            name: key.mangled_name(),
            generics: Vec::new(), // No longer generic
            fields,
            span: generic_def.span.clone(),
        }
    }

    fn specialize_enum(&self, generic_def: &EnumDef, type_args: &[Type]) -> EnumDef {
        let mut substitutions: HashMap<String, Type> = HashMap::new();
        for (i, generic_param) in generic_def.generics.iter().enumerate() {
            if let Some(concrete_type) = type_args.get(i) {
                substitutions.insert(generic_param.clone(), concrete_type.clone());
            }
        }

        let variants: Vec<Variant> = generic_def
            .variants
            .iter()
            .map(|v| Variant {
                name: v.name.clone(),
                fields: v
                    .fields
                    .iter()
                    .map(|t| self.substitute_type(t, &substitutions))
                    .collect(),
                span: v.span.clone(),
            })
            .collect();

        let key = MonoKey::new(generic_def.name.clone(), type_args.to_vec());

        EnumDef {
            name: key.mangled_name(),
            generics: Vec::new(),
            variants,
            span: generic_def.span.clone(),
        }
    }

    fn substitute_type(&self, ty: &Type, substitutions: &HashMap<String, Type>) -> Type {
        match ty {
            Type::Named(name) => {
                // Check if this is a generic parameter that should be substituted
                if let Some(concrete) = substitutions.get(name) {
                    concrete.clone()
                } else {
                    ty.clone()
                }
            }
            Type::Generic(name, args) => {
                // Substitute in the type arguments
                let new_args: Vec<Type> = args
                    .iter()
                    .map(|a| self.substitute_type(a, substitutions))
                    .collect();

                // Check if the base type is a generic parameter
                if let Some(concrete) = substitutions.get(name) {
                    // This shouldn't normally happen for Generic types
                    concrete.clone()
                } else {
                    // Create specialized name
                    let key = MonoKey::new(name.clone(), new_args.clone());
                    Type::Named(key.mangled_name())
                }
            }
            Type::Reference(inner, is_mut) => {
                Type::Reference(Box::new(self.substitute_type(inner, substitutions)), *is_mut)
            }
            Type::Heap(inner) => {
                Type::Heap(Box::new(self.substitute_type(inner, substitutions)))
            }
            Type::Rc(inner) => Type::Rc(Box::new(self.substitute_type(inner, substitutions))),
            Type::Arc(inner) => Type::Arc(Box::new(self.substitute_type(inner, substitutions))),
            Type::Function(params, ret) => {
                let new_params: Vec<Type> = params
                    .iter()
                    .map(|p| self.substitute_type(p, substitutions))
                    .collect();
                let new_ret = self.substitute_type(ret, substitutions);
                Type::Function(new_params, Box::new(new_ret))
            }
            Type::Unit => Type::Unit,
        }
    }

    fn transform_function(&self, func: &Function) -> Function {
        // Transform all types in the function to use specialized names
        let params: Vec<Param> = func
            .params
            .iter()
            .map(|p| Param {
                name: p.name.clone(),
                ty: self.transform_type(&p.ty),
                span: p.span.clone(),
            })
            .collect();

        let return_type = func.return_type.as_ref().map(|t| self.transform_type(t));

        let body: Vec<Stmt> = func.body.iter().map(|s| self.transform_stmt(s)).collect();

        Function {
            name: func.name.clone(),
            params,
            return_type,
            effects: func.effects.clone(),
            body,
            span: func.span.clone(),
        }
    }

    fn transform_type(&self, ty: &Type) -> Type {
        match ty {
            Type::Generic(name, args) => {
                // Transform to specialized name
                let new_args: Vec<Type> = args.iter().map(|a| self.transform_type(a)).collect();
                let key = MonoKey::new(name.clone(), new_args);
                Type::Named(key.mangled_name())
            }
            Type::Reference(inner, is_mut) => {
                Type::Reference(Box::new(self.transform_type(inner)), *is_mut)
            }
            Type::Heap(inner) => Type::Heap(Box::new(self.transform_type(inner))),
            Type::Rc(inner) => Type::Rc(Box::new(self.transform_type(inner))),
            Type::Arc(inner) => Type::Arc(Box::new(self.transform_type(inner))),
            Type::Function(params, ret) => {
                let new_params: Vec<Type> = params.iter().map(|p| self.transform_type(p)).collect();
                let new_ret = self.transform_type(ret);
                Type::Function(new_params, Box::new(new_ret))
            }
            _ => ty.clone(),
        }
    }

    fn transform_stmt(&self, stmt: &Stmt) -> Stmt {
        match stmt {
            Stmt::Let {
                name,
                ty,
                mutable,
                value,
                span,
            } => Stmt::Let {
                name: name.clone(),
                ty: ty.as_ref().map(|t| self.transform_type(t)),
                mutable: *mutable,
                value: self.transform_expr(value),
                span: span.clone(),
            },
            Stmt::Assign {
                target,
                value,
                span,
            } => Stmt::Assign {
                target: self.transform_expr(target),
                value: self.transform_expr(value),
                span: span.clone(),
            },
            Stmt::Expr(e) => Stmt::Expr(self.transform_expr(e)),
            Stmt::Return(Some(e), span) => Stmt::Return(Some(self.transform_expr(e)), span.clone()),
            Stmt::Return(None, span) => Stmt::Return(None, span.clone()),
            Stmt::While {
                condition,
                body,
                span,
            } => Stmt::While {
                condition: self.transform_expr(condition),
                body: body.iter().map(|s| self.transform_stmt(s)).collect(),
                span: span.clone(),
            },
            Stmt::For {
                var,
                iterable,
                body,
                span,
            } => Stmt::For {
                var: var.clone(),
                iterable: self.transform_expr(iterable),
                body: body.iter().map(|s| self.transform_stmt(s)).collect(),
                span: span.clone(),
            },
            Stmt::Break(span) => Stmt::Break(span.clone()),
            Stmt::Continue(span) => Stmt::Continue(span.clone()),
        }
    }

    fn transform_expr(&self, expr: &Expr) -> Expr {
        match expr {
            Expr::Int(n, span) => Expr::Int(*n, span.clone()),
            Expr::Float(n, span) => Expr::Float(*n, span.clone()),
            Expr::String(s, span) => Expr::String(s.clone(), span.clone()),
            Expr::Bool(b, span) => Expr::Bool(*b, span.clone()),
            Expr::Ident(name, span) => Expr::Ident(name.clone(), span.clone()),
            Expr::Binary(l, op, r, span) => Expr::Binary(
                Box::new(self.transform_expr(l)),
                *op,
                Box::new(self.transform_expr(r)),
                span.clone(),
            ),
            Expr::Unary(op, e, span) => {
                Expr::Unary(*op, Box::new(self.transform_expr(e)), span.clone())
            }
            Expr::Call(callee, args, span) => Expr::Call(
                Box::new(self.transform_expr(callee)),
                args.iter().map(|a| self.transform_expr(a)).collect(),
                span.clone(),
            ),
            Expr::FieldAccess(base, field, span) => {
                Expr::FieldAccess(Box::new(self.transform_expr(base)), field.clone(), span.clone())
            }
            Expr::StructConstruct { name, fields, span } => {
                // TODO: Transform name if it's a generic instantiation
                Expr::StructConstruct {
                    name: name.clone(),
                    fields: fields
                        .iter()
                        .map(|(n, e)| (n.clone(), self.transform_expr(e)))
                        .collect(),
                    span: span.clone(),
                }
            }
            Expr::EnumConstruct {
                enum_name,
                variant,
                value,
                span,
            } => Expr::EnumConstruct {
                enum_name: enum_name.clone(),
                variant: variant.clone(),
                value: value.as_ref().map(|v| Box::new(self.transform_expr(v))),
                span: span.clone(),
            },
            Expr::If(cond, then_branch, else_branch, span) => Expr::If(
                Box::new(self.transform_expr(cond)),
                then_branch.iter().map(|s| self.transform_stmt(s)).collect(),
                else_branch
                    .as_ref()
                    .map(|stmts| stmts.iter().map(|s| self.transform_stmt(s)).collect()),
                span.clone(),
            ),
            Expr::Match(value, arms, span) => Expr::Match(
                Box::new(self.transform_expr(value)),
                arms.iter()
                    .map(|arm| crate::parser::MatchArm {
                        pattern: self.transform_pattern(&arm.pattern),
                        body: self.transform_expr(&arm.body),
                        span: arm.span.clone(),
                    })
                    .collect(),
                span.clone(),
            ),
            Expr::Block(stmts, span) => Expr::Block(
                stmts.iter().map(|s| self.transform_stmt(s)).collect(),
                span.clone(),
            ),
            Expr::HeapAlloc(e, span) => {
                Expr::HeapAlloc(Box::new(self.transform_expr(e)), span.clone())
            }
            Expr::RcAlloc(e, span) => {
                Expr::RcAlloc(Box::new(self.transform_expr(e)), span.clone())
            }
            Expr::ArcAlloc(e, span) => {
                Expr::ArcAlloc(Box::new(self.transform_expr(e)), span.clone())
            }
            Expr::Lambda { params, body, span } => Expr::Lambda {
                params: params
                    .iter()
                    .map(|p| Param {
                        name: p.name.clone(),
                        ty: self.transform_type(&p.ty),
                        span: p.span.clone(),
                    })
                    .collect(),
                body: Box::new(self.transform_expr(body)),
                span: span.clone(),
            },
        }
    }

    fn transform_pattern(&self, pattern: &Pattern) -> Pattern {
        match pattern {
            Pattern::Wildcard(span) => Pattern::Wildcard(span.clone()),
            Pattern::Ident(name, span) => Pattern::Ident(name.clone(), span.clone()),
            Pattern::Literal(e) => Pattern::Literal(self.transform_expr(e)),
            Pattern::Constructor(name, patterns, span) => Pattern::Constructor(
                name.clone(),
                patterns.iter().map(|p| self.transform_pattern(p)).collect(),
                span.clone(),
            ),
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;

    #[test]
    fn test_mangled_name() {
        let key = MonoKey::new(
            "Option".to_string(),
            vec![Type::Named("i64".to_string())],
        );
        assert_eq!(key.mangled_name(), "Option_i64");
    }

    #[test]
    fn test_mangled_name_multiple_args() {
        let key = MonoKey::new(
            "Map".to_string(),
            vec![
                Type::Named("String".to_string()),
                Type::Named("i32".to_string()),
            ],
        );
        assert_eq!(key.mangled_name(), "Map_String_i32");
    }

    #[test]
    fn test_monomorphize_generic_struct() {
        let source = r#"struct Box[T]
    value: T

fn main()
    let x: Box[i64] = Box(value: 42)
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        let mono_program = monomorphize(&program);

        // Should have specialized Box_i64 struct
        let struct_names: Vec<_> = mono_program
            .items
            .iter()
            .filter_map(|item| {
                if let Item::Struct(s) = item {
                    Some(s.name.clone())
                } else {
                    None
                }
            })
            .collect();

        assert!(struct_names.contains(&"Box_i64".to_string()));
    }
}
