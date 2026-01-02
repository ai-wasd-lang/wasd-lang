//! LLVM code generation for WASD IR.

mod alloc_codegen;
mod binops;
mod instructions;
mod terminator;
mod values;
#[cfg(test)]
mod tests;

use crate::ir::*;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, StructType};
use inkwell::values::{FunctionValue, PointerValue};
use inkwell::OptimizationLevel;
use std::collections::HashMap;
use std::path::Path;

use inkwell::basic_block::BasicBlock;

/// LLVM code generator for WASD.
pub struct CodeGen<'ctx> {
    pub(super) context: &'ctx Context,
    pub(super) module: Module<'ctx>,
    pub(super) builder: Builder<'ctx>,
    pub(super) variables: HashMap<String, PointerValue<'ctx>>,
    pub(super) variable_types: HashMap<String, BasicTypeEnum<'ctx>>,
    pub(super) functions: HashMap<String, FunctionValue<'ctx>>,
    pub(super) blocks: HashMap<String, BasicBlock<'ctx>>,
    pub(super) string_counter: usize,
    pub(super) gep_results: std::collections::HashSet<String>,
    pub(super) struct_types: HashMap<String, StructType<'ctx>>,
    pub(super) struct_fields: HashMap<String, Vec<(String, IrType)>>,
}

impl<'ctx> CodeGen<'ctx> {
    /// Create a new code generator.
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();

        let mut codegen = Self {
            context,
            module,
            builder,
            variables: HashMap::new(),
            variable_types: HashMap::new(),
            functions: HashMap::new(),
            blocks: HashMap::new(),
            string_counter: 0,
            gep_results: std::collections::HashSet::new(),
            struct_types: HashMap::new(),
            struct_fields: HashMap::new(),
        };

        codegen.declare_c_functions();
        codegen
    }

    /// Declare external C library functions (puts, printf, etc.)
    fn declare_c_functions(&mut self) {
        let i32_type = self.context.i32_type();
        let i8_ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
        let puts_type = i32_type.fn_type(&[i8_ptr_type.into()], false);
        let puts_fn = self.module.add_function("puts", puts_type, None);
        self.functions.insert("puts".to_string(), puts_fn);

        let printf_type = i32_type.fn_type(&[i8_ptr_type.into()], true);
        let printf_fn = self.module.add_function("printf", printf_type, None);
        self.functions.insert("printf".to_string(), printf_fn);
    }

    /// Create a global string constant and return a pointer to it
    pub(super) fn create_global_string(&mut self, value: &str) -> PointerValue<'ctx> {
        let string_name = format!(".str.{}", self.string_counter);
        self.string_counter += 1;

        let string_value = self.context.const_string(value.as_bytes(), true);
        let global = self
            .module
            .add_global(string_value.get_type(), None, &string_name);
        global.set_initializer(&string_value);
        global.set_constant(true);
        global.set_linkage(inkwell::module::Linkage::Private);

        global.as_pointer_value()
    }

    /// Compile an IR module to LLVM IR.
    pub fn compile(&mut self, ir_module: &IrModule) -> Result<(), String> {
        for ir_struct in &ir_module.structs {
            self.declare_struct(ir_struct)?;
        }

        for func in &ir_module.functions {
            self.declare_function(func)?;
        }

        for func in &ir_module.functions {
            self.compile_function(func)?;
        }

        Ok(())
    }

    /// Declare a struct type.
    fn declare_struct(&mut self, ir_struct: &IrStruct) -> Result<(), String> {
        let field_types: Vec<BasicTypeEnum> = ir_struct
            .fields
            .iter()
            .map(|(_, ty)| self.get_llvm_type(ty))
            .collect();

        let struct_type = self.context.struct_type(&field_types, false);
        self.struct_types.insert(ir_struct.name.clone(), struct_type);
        self.struct_fields
            .insert(ir_struct.name.clone(), ir_struct.fields.clone());

        Ok(())
    }

    /// Write the module to an object file.
    pub fn write_object_file(&self, path: &Path) -> Result<(), String> {
        Target::initialize_native(&InitializationConfig::default())
            .map_err(|e| format!("Failed to initialize target: {}", e))?;

        let triple = TargetMachine::get_default_triple();
        let target =
            Target::from_triple(&triple).map_err(|e| format!("Failed to get target: {}", e))?;

        let target_machine = target
            .create_target_machine(
                &triple,
                "generic",
                "",
                OptimizationLevel::Default,
                RelocMode::PIC,
                CodeModel::Default,
            )
            .ok_or("Failed to create target machine")?;

        target_machine
            .write_to_file(&self.module, FileType::Object, path)
            .map_err(|e| format!("Failed to write object file: {}", e))?;

        Ok(())
    }

    /// Get the LLVM IR as a string.
    pub fn get_ir_string(&self) -> String {
        self.module.print_to_string().to_string()
    }

    fn declare_function(&mut self, func: &IrFunction) -> Result<(), String> {
        let param_types: Vec<BasicMetadataTypeEnum> = func
            .params
            .iter()
            .map(|(_, ty)| self.get_llvm_type(ty).into())
            .collect();

        let fn_type = match &func.return_type {
            IrType::Void => self.context.void_type().fn_type(&param_types, false),
            ret_ty => {
                let ret = self.get_llvm_type(ret_ty);
                ret.fn_type(&param_types, false)
            }
        };

        let fn_value = self.module.add_function(&func.name, fn_type, None);
        self.functions.insert(func.name.clone(), fn_value);

        Ok(())
    }

    fn compile_function(&mut self, func: &IrFunction) -> Result<(), String> {
        let fn_value = *self
            .functions
            .get(&func.name)
            .ok_or_else(|| format!("Function not found: {}", func.name))?;

        self.variables.clear();
        self.variable_types.clear();
        self.blocks.clear();
        self.gep_results.clear();

        for ir_block in &func.blocks {
            let bb = self.context.append_basic_block(fn_value, &ir_block.label);
            self.blocks.insert(ir_block.label.clone(), bb);
        }

        if let Some(first_block) = func.blocks.first() {
            let entry = self.blocks.get(&first_block.label).unwrap();
            self.builder.position_at_end(*entry);
        }

        for (i, (name, ty)) in func.params.iter().enumerate() {
            let llvm_ty = self.get_llvm_type(ty);
            let alloca = self
                .builder
                .build_alloca(llvm_ty, name)
                .map_err(|e| format!("Failed to build alloca: {}", e))?;
            let param = fn_value.get_nth_param(i as u32).unwrap();
            self.builder
                .build_store(alloca, param)
                .map_err(|e| format!("Failed to build store: {}", e))?;
            self.variables.insert(name.clone(), alloca);
            self.variable_types.insert(name.clone(), llvm_ty);
        }

        for ir_block in &func.blocks {
            self.compile_block(ir_block, fn_value)?;
        }

        Ok(())
    }

    fn compile_block(
        &mut self,
        block: &IrBlock,
        _fn_value: FunctionValue<'ctx>,
    ) -> Result<(), String> {
        let bb = self
            .blocks
            .get(&block.label)
            .ok_or_else(|| format!("Block not found: {}", block.label))?;
        self.builder.position_at_end(*bb);

        for inst in &block.instructions {
            self.compile_instruction(inst)?;
        }

        self.compile_terminator(&block.terminator)?;

        Ok(())
    }
}
