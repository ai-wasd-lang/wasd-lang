//! Specialization functions for monomorphization.

use super::monomorphize::{MonoKey, Monomorphizer};
use crate::parser::{EnumDef, Field, StructDef, Type, Variant};
use std::collections::HashMap;

impl Monomorphizer {
    pub(super) fn specialize_struct(
        &self,
        generic_def: &StructDef,
        type_args: &[Type],
    ) -> StructDef {
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
            visibility: generic_def.visibility,
            name: key.mangled_name(),
            generics: Vec::new(), // No longer generic
            fields,
            span: generic_def.span.clone(),
        }
    }

    pub(super) fn specialize_enum(&self, generic_def: &EnumDef, type_args: &[Type]) -> EnumDef {
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
            visibility: generic_def.visibility,
            name: key.mangled_name(),
            generics: Vec::new(),
            variants,
            span: generic_def.span.clone(),
        }
    }

    pub(super) fn substitute_type(
        &self,
        ty: &Type,
        substitutions: &HashMap<String, Type>,
    ) -> Type {
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
}
