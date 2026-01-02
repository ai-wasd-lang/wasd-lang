//! Lowering from AST to WASD IR.

mod control_flow;
mod expr;
mod stmt;
#[cfg(test)]
mod tests;

use std::collections::HashMap;
use crate::ir::wasd_ir::*;
use crate::parser as ast;

pub use super::wasd_ir::{IrEnum, IrEnumVariant};

/// Lower a program AST to WASD IR.
pub fn lower_program(program: &ast::Program) -> IrModule {
    let mut lowerer = Lowerer::new();
    lowerer.lower(program)
}

pub(super) struct Lowerer {
    pub(super) current_block: Vec<IrInst>,
    pub(super) current_label: String,
    pub(super) blocks: Vec<IrBlock>,
    block_counter: usize,
    var_counter: usize,
    pub(super) loop_stack: Vec<(String, String)>,
    pub(super) closure_funcs: Vec<IrFunction>,
    /// Maps struct name -> list of (field_name, field_type)
    pub(super) struct_fields: HashMap<String, Vec<(String, IrType)>>,
    /// Maps enum name -> list of variant names with their tags
    pub(super) enum_variants: HashMap<String, Vec<(String, u32)>>,
    /// Maps variable name -> struct type name (for field access)
    pub(super) variable_types: HashMap<String, String>,
    /// Maps type name -> list of method names (for impl blocks)
    pub(super) impl_methods: HashMap<String, Vec<String>>,
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
            struct_fields: HashMap::new(),
            enum_variants: HashMap::new(),
            variable_types: HashMap::new(),
            impl_methods: HashMap::new(),
        }
    }

    pub(super) fn fresh_label(&mut self, prefix: &str) -> String {
        let label = format!("{}_{}", prefix, self.block_counter);
        self.block_counter += 1;
        label
    }

    pub(super) fn fresh_var(&mut self) -> String {
        let var = format!("_t{}", self.var_counter);
        self.var_counter += 1;
        var
    }

    fn lower(&mut self, program: &ast::Program) -> IrModule {
        let mut functions = Vec::new();
        let mut structs = Vec::new();
        let mut enums = Vec::new();

        // First pass: collect struct and enum definitions so we can look up field indices
        for item in &program.items {
            match item {
                ast::Item::Struct(s) => {
                    let ir_struct = self.lower_struct(s);
                    self.struct_fields.insert(s.name.clone(), ir_struct.fields.clone());
                    structs.push(ir_struct);
                }
                ast::Item::Enum(e) => {
                    let ir_enum = self.lower_enum(e);
                    let variant_info: Vec<_> = ir_enum.variants.iter()
                        .map(|v| (v.name.clone(), v.tag))
                        .collect();
                    self.enum_variants.insert(e.name.clone(), variant_info);
                    enums.push(ir_enum);

                    // Also create a struct type for the enum as a tagged union {i32, i64}
                    let enum_struct = IrStruct {
                        name: e.name.clone(),
                        fields: vec![
                            ("tag".to_string(), IrType::I32),
                            ("payload".to_string(), IrType::I64),
                        ],
                    };
                    self.struct_fields.insert(e.name.clone(), enum_struct.fields.clone());
                    structs.push(enum_struct);
                }
                _ => {}
            }
        }

        // Second pass: lower functions (which may reference structs/enums)
        for item in &program.items {
            match item {
                ast::Item::Use(_) => {}
                ast::Item::Function(f) => {
                    if let Some(ir_func) = self.lower_function(f) {
                        functions.push(ir_func);
                    }
                }
                ast::Item::Struct(_) | ast::Item::Enum(_) => {} // Already handled
                ast::Item::Trait(_) => {}
                ast::Item::Impl(impl_def) => {
                    // Extract the type name from the target type
                    let type_name = match &impl_def.target_type {
                        ast::Type::Named(name) => name.clone(),
                        ast::Type::Generic(name, _) => name.clone(),
                        _ => continue,
                    };
                    let mut method_names = Vec::new();
                    for method in &impl_def.methods {
                        // Mangle method name: TypeName_methodName
                        let mangled_name = format!("{}_{}", type_name, method.name);
                        method_names.push(method.name.clone());

                        // Create a modified function with mangled name
                        let mut mangled_method = method.clone();
                        mangled_method.name = mangled_name;

                        if let Some(ir_func) = self.lower_function(&mangled_method) {
                            functions.push(ir_func);
                        }
                    }
                    // Register the methods for this type
                    self.impl_methods.insert(type_name, method_names);
                }
            }
        }

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
        self.variable_types.clear();

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

        let mut last_value = None;
        for stmt in &func.body {
            last_value = self.lower_stmt(stmt);
        }

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

    pub(super) fn get_variant_tag(&self, enum_name: &str, variant: &str) -> u32 {
        if let Some(variants) = self.enum_variants.get(enum_name) {
            for (name, tag) in variants {
                if name == variant {
                    return *tag;
                }
            }
        }
        0
    }

    pub(super) fn get_field_index(&self, struct_name: &str, field_name: &str) -> usize {
        if let Some(fields) = self.struct_fields.get(struct_name) {
            for (i, (name, _)) in fields.iter().enumerate() {
                if name == field_name {
                    return i;
                }
            }
        }
        0
    }

    pub(super) fn lower_type(&self, ty: &ast::Type) -> IrType {
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
            _ => IrType::I64,
        }
    }

    pub(super) fn lower_binop(&self, op: ast::BinOp) -> IrBinOp {
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

    pub(super) fn finish_block(&mut self, terminator: IrTerminator) {
        let block = IrBlock {
            label: std::mem::take(&mut self.current_label),
            instructions: std::mem::take(&mut self.current_block),
            terminator,
        };
        self.blocks.push(block);
    }

    pub(super) fn start_block(&mut self, label: String) {
        self.current_label = label;
    }
}
