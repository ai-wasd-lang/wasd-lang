//! LLVM code generation for WASD IR.

use crate::ir::*;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, PointerValue};
use inkwell::OptimizationLevel;
use std::collections::HashMap;
use std::path::Path;

use inkwell::basic_block::BasicBlock;

/// LLVM code generator for WASD.
pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    variables: HashMap<String, PointerValue<'ctx>>,
    variable_types: HashMap<String, BasicTypeEnum<'ctx>>,
    functions: HashMap<String, FunctionValue<'ctx>>,
    blocks: HashMap<String, BasicBlock<'ctx>>,
    string_counter: usize,
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
        };

        // Declare C library functions
        codegen.declare_c_functions();

        codegen
    }

    /// Declare external C library functions (puts, printf, etc.)
    fn declare_c_functions(&mut self) {
        // Declare puts: int puts(const char*)
        let i32_type = self.context.i32_type();
        let i8_ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
        let puts_type = i32_type.fn_type(&[i8_ptr_type.into()], false);
        let puts_fn = self.module.add_function("puts", puts_type, None);
        self.functions.insert("puts".to_string(), puts_fn);

        // Declare printf: int printf(const char*, ...)
        let printf_type = i32_type.fn_type(&[i8_ptr_type.into()], true);
        let printf_fn = self.module.add_function("printf", printf_type, None);
        self.functions.insert("printf".to_string(), printf_fn);
    }

    /// Create a global string constant and return a pointer to it
    fn create_global_string(&mut self, value: &str) -> PointerValue<'ctx> {
        let string_name = format!(".str.{}", self.string_counter);
        self.string_counter += 1;

        // Create the string with null terminator
        let string_value = self.context.const_string(value.as_bytes(), true);
        let global = self
            .module
            .add_global(string_value.get_type(), None, &string_name);
        global.set_initializer(&string_value);
        global.set_constant(true);
        global.set_linkage(inkwell::module::Linkage::Private);

        // Return pointer to the string
        global.as_pointer_value()
    }

    /// Compile an IR module to LLVM IR.
    pub fn compile(&mut self, ir_module: &IrModule) -> Result<(), String> {
        // First pass: declare all functions
        for func in &ir_module.functions {
            self.declare_function(func)?;
        }

        // Second pass: define function bodies
        for func in &ir_module.functions {
            self.compile_function(func)?;
        }

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

        // First pass: create all basic blocks
        for ir_block in &func.blocks {
            let bb = self.context.append_basic_block(fn_value, &ir_block.label);
            self.blocks.insert(ir_block.label.clone(), bb);
        }

        // Position at first block (entry)
        if let Some(first_block) = func.blocks.first() {
            let entry = self.blocks.get(&first_block.label).unwrap();
            self.builder.position_at_end(*entry);
        }

        // Allocate and store parameters (in entry block)
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

        // Second pass: compile each block
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
        // Position builder at this block
        let bb = self.blocks.get(&block.label)
            .ok_or_else(|| format!("Block not found: {}", block.label))?;
        self.builder.position_at_end(*bb);

        for inst in &block.instructions {
            self.compile_instruction(inst)?;
        }

        self.compile_terminator(&block.terminator)?;

        Ok(())
    }

    fn compile_instruction(&mut self, inst: &IrInst) -> Result<(), String> {
        match inst {
            IrInst::Alloca { dest, ty } => {
                let llvm_ty = self.get_llvm_type(ty);
                let alloca = self
                    .builder
                    .build_alloca(llvm_ty, dest)
                    .map_err(|e| format!("Failed to build alloca: {}", e))?;
                self.variables.insert(dest.clone(), alloca);
                self.variable_types.insert(dest.clone(), llvm_ty);
            }
            IrInst::Store { value, ptr } => {
                let llvm_value = self.get_llvm_value(value)?;
                let ptr_value = *self
                    .variables
                    .get(ptr)
                    .ok_or_else(|| format!("Variable not found: {}", ptr))?;
                self.builder
                    .build_store(ptr_value, llvm_value)
                    .map_err(|e| format!("Failed to build store: {}", e))?;
            }
            IrInst::Load { dest, ptr, ty } => {
                let ptr_value = *self
                    .variables
                    .get(ptr)
                    .ok_or_else(|| format!("Variable not found: {}", ptr))?;
                let llvm_ty = self.get_llvm_type(ty);
                let loaded = self
                    .builder
                    .build_load(llvm_ty, ptr_value, dest)
                    .map_err(|e| format!("Failed to build load: {}", e))?;

                // Store the loaded value in a new alloca for future reference
                let alloca = self
                    .builder
                    .build_alloca(llvm_ty, dest)
                    .map_err(|e| format!("Failed to build alloca: {}", e))?;
                self.builder
                    .build_store(alloca, loaded)
                    .map_err(|e| format!("Failed to build store: {}", e))?;
                self.variables.insert(dest.clone(), alloca);
            }
            IrInst::BinOp {
                dest,
                op,
                left,
                right,
            } => {
                let lhs = self.get_llvm_value(left)?;
                let rhs = self.get_llvm_value(right)?;

                let result = self.build_binop(*op, lhs, rhs, dest)?;
                // Use the result type for the alloca
                let ty = result.get_type();
                let alloca = self
                    .builder
                    .build_alloca(ty, dest)
                    .map_err(|e| format!("Failed to build alloca: {}", e))?;
                self.builder
                    .build_store(alloca, result)
                    .map_err(|e| format!("Failed to build store: {}", e))?;
                self.variables.insert(dest.clone(), alloca);
                self.variable_types.insert(dest.clone(), ty);
            }
            IrInst::Call { dest, func, args } => {
                // Map stdlib functions to C library functions
                // println adds a newline, so it uses puts
                // print does not add a newline, so it uses printf with %s format
                let actual_func = match func.as_str() {
                    "println" => "puts",
                    "print" => "printf",
                    other => other,
                };

                let fn_value = *self
                    .functions
                    .get(actual_func)
                    .ok_or_else(|| format!("Function not found: {}", actual_func))?;

                let llvm_args: Result<Vec<BasicMetadataValueEnum>, String> = args
                    .iter()
                    .map(|a| Ok(self.get_llvm_value(a)?.into()))
                    .collect();

                let call = self
                    .builder
                    .build_call(fn_value, &llvm_args?, "call")
                    .map_err(|e| format!("Failed to build call: {}", e))?;

                if let Some(dest) = dest {
                    if let Some(ret_val) = call.try_as_basic_value().left() {
                        let ty = ret_val.get_type();
                        let alloca = self
                            .builder
                            .build_alloca(ty, dest)
                            .map_err(|e| format!("Failed to build alloca: {}", e))?;
                        self.builder
                            .build_store(alloca, ret_val)
                            .map_err(|e| format!("Failed to build store: {}", e))?;
                        self.variables.insert(dest.clone(), alloca);
                    }
                }
            }
            IrInst::GetElementPtr { .. } => {
                // TODO: Implement GEP
            }
            IrInst::HeapAlloc { dest, ty, value } => {
                // Get or declare malloc
                let malloc_fn = self.get_or_declare_malloc()?;
                let llvm_ty = self.get_llvm_type(ty);

                // Calculate size of type
                let size = llvm_ty.size_of()
                    .ok_or_else(|| "Cannot get size of type".to_string())?;

                // Call malloc
                let ptr = self.builder
                    .build_call(malloc_fn, &[size.into()], "heap_alloc")
                    .map_err(|e| format!("Failed to call malloc: {}", e))?
                    .try_as_basic_value()
                    .left()
                    .ok_or_else(|| "malloc returned void".to_string())?;

                // Store initial value if provided
                if let Some(val) = value {
                    let llvm_val = self.get_llvm_value(val)?;
                    self.builder
                        .build_store(ptr.into_pointer_value(), llvm_val)
                        .map_err(|e| format!("Failed to store heap value: {}", e))?;
                }

                // Store pointer in alloca
                let ptr_ty = self.context.ptr_type(inkwell::AddressSpace::default());
                let alloca = self.builder
                    .build_alloca(ptr_ty, dest)
                    .map_err(|e| format!("Failed to build alloca: {}", e))?;
                self.builder
                    .build_store(alloca, ptr)
                    .map_err(|e| format!("Failed to store: {}", e))?;
                self.variables.insert(dest.clone(), alloca);
            }
            IrInst::RcAlloc { dest, ty, value } => {
                // RC allocation: allocate (refcount, value) struct
                // For now, same as heap alloc but with extra space for refcount
                let malloc_fn = self.get_or_declare_malloc()?;
                let llvm_ty = self.get_llvm_type(ty);

                // Size = sizeof(i64) for refcount + sizeof(value)
                let refcount_size = self.context.i64_type().size_of();
                let value_size = llvm_ty.size_of()
                    .ok_or_else(|| "Cannot get size of type".to_string())?;
                let total_size = self.builder
                    .build_int_add(refcount_size, value_size, "total_size")
                    .map_err(|e| format!("Failed to add sizes: {}", e))?;

                // Call malloc
                let ptr = self.builder
                    .build_call(malloc_fn, &[total_size.into()], "rc_alloc")
                    .map_err(|e| format!("Failed to call malloc: {}", e))?
                    .try_as_basic_value()
                    .left()
                    .ok_or_else(|| "malloc returned void".to_string())?;

                // Initialize refcount to 1
                let one = self.context.i64_type().const_int(1, false);
                self.builder
                    .build_store(ptr.into_pointer_value(), one)
                    .map_err(|e| format!("Failed to store refcount: {}", e))?;

                // Store initial value if provided (after refcount)
                if let Some(val) = value {
                    let llvm_val = self.get_llvm_value(val)?;
                    let value_ptr = unsafe {
                        self.builder
                            .build_gep(
                                self.context.i64_type(),
                                ptr.into_pointer_value(),
                                &[self.context.i64_type().const_int(1, false)],
                                "value_ptr"
                            )
                            .map_err(|e| format!("Failed to compute value ptr: {}", e))?
                    };
                    self.builder
                        .build_store(value_ptr, llvm_val)
                        .map_err(|e| format!("Failed to store rc value: {}", e))?;
                }

                // Store pointer
                let ptr_ty = self.context.ptr_type(inkwell::AddressSpace::default());
                let alloca = self.builder
                    .build_alloca(ptr_ty, dest)
                    .map_err(|e| format!("Failed to build alloca: {}", e))?;
                self.builder
                    .build_store(alloca, ptr)
                    .map_err(|e| format!("Failed to store: {}", e))?;
                self.variables.insert(dest.clone(), alloca);
            }
            IrInst::ArcAlloc { dest, ty, value } => {
                // ARC is same as RC for now (atomic operations would differ)
                // TODO: Use atomic operations for refcount
                let malloc_fn = self.get_or_declare_malloc()?;
                let llvm_ty = self.get_llvm_type(ty);

                let refcount_size = self.context.i64_type().size_of();
                let value_size = llvm_ty.size_of()
                    .ok_or_else(|| "Cannot get size of type".to_string())?;
                let total_size = self.builder
                    .build_int_add(refcount_size, value_size, "total_size")
                    .map_err(|e| format!("Failed to add sizes: {}", e))?;

                let ptr = self.builder
                    .build_call(malloc_fn, &[total_size.into()], "arc_alloc")
                    .map_err(|e| format!("Failed to call malloc: {}", e))?
                    .try_as_basic_value()
                    .left()
                    .ok_or_else(|| "malloc returned void".to_string())?;

                let one = self.context.i64_type().const_int(1, false);
                self.builder
                    .build_store(ptr.into_pointer_value(), one)
                    .map_err(|e| format!("Failed to store refcount: {}", e))?;

                if let Some(val) = value {
                    let llvm_val = self.get_llvm_value(val)?;
                    let value_ptr = unsafe {
                        self.builder
                            .build_gep(
                                self.context.i64_type(),
                                ptr.into_pointer_value(),
                                &[self.context.i64_type().const_int(1, false)],
                                "value_ptr"
                            )
                            .map_err(|e| format!("Failed to compute value ptr: {}", e))?
                    };
                    self.builder
                        .build_store(value_ptr, llvm_val)
                        .map_err(|e| format!("Failed to store arc value: {}", e))?;
                }

                let ptr_ty = self.context.ptr_type(inkwell::AddressSpace::default());
                let alloca = self.builder
                    .build_alloca(ptr_ty, dest)
                    .map_err(|e| format!("Failed to build alloca: {}", e))?;
                self.builder
                    .build_store(alloca, ptr)
                    .map_err(|e| format!("Failed to store: {}", e))?;
                self.variables.insert(dest.clone(), alloca);
            }
            IrInst::RcIncRef { ptr } => {
                // Increment refcount at ptr
                let ptr_val = *self.variables.get(ptr)
                    .ok_or_else(|| format!("Variable not found: {}", ptr))?;
                let actual_ptr = self.builder
                    .build_load(self.context.ptr_type(inkwell::AddressSpace::default()), ptr_val, "rc_ptr")
                    .map_err(|e| format!("Failed to load ptr: {}", e))?;
                let count = self.builder
                    .build_load(self.context.i64_type(), actual_ptr.into_pointer_value(), "count")
                    .map_err(|e| format!("Failed to load count: {}", e))?;
                let new_count = self.builder
                    .build_int_add(count.into_int_value(), self.context.i64_type().const_int(1, false), "new_count")
                    .map_err(|e| format!("Failed to increment: {}", e))?;
                self.builder
                    .build_store(actual_ptr.into_pointer_value(), new_count)
                    .map_err(|e| format!("Failed to store count: {}", e))?;
            }
            IrInst::RcDecRef { ptr } => {
                // Decrement refcount, free if zero
                // For now, just decrement (proper implementation would check and free)
                let ptr_val = *self.variables.get(ptr)
                    .ok_or_else(|| format!("Variable not found: {}", ptr))?;
                let actual_ptr = self.builder
                    .build_load(self.context.ptr_type(inkwell::AddressSpace::default()), ptr_val, "rc_ptr")
                    .map_err(|e| format!("Failed to load ptr: {}", e))?;
                let count = self.builder
                    .build_load(self.context.i64_type(), actual_ptr.into_pointer_value(), "count")
                    .map_err(|e| format!("Failed to load count: {}", e))?;
                let new_count = self.builder
                    .build_int_sub(count.into_int_value(), self.context.i64_type().const_int(1, false), "new_count")
                    .map_err(|e| format!("Failed to decrement: {}", e))?;
                self.builder
                    .build_store(actual_ptr.into_pointer_value(), new_count)
                    .map_err(|e| format!("Failed to store count: {}", e))?;
                // TODO: Add conditional free when count reaches 0
            }
        }

        Ok(())
    }

    fn compile_terminator(&mut self, term: &IrTerminator) -> Result<(), String> {
        match term {
            IrTerminator::Return(Some(value)) => {
                let llvm_value = self.get_llvm_value(value)?;
                self.builder
                    .build_return(Some(&llvm_value))
                    .map_err(|e| format!("Failed to build return: {}", e))?;
            }
            IrTerminator::Return(None) => {
                self.builder
                    .build_return(None)
                    .map_err(|e| format!("Failed to build return: {}", e))?;
            }
            IrTerminator::Unreachable => {
                self.builder
                    .build_unreachable()
                    .map_err(|e| format!("Failed to build unreachable: {}", e))?;
            }
            IrTerminator::Branch(target) => {
                let target_bb = self.blocks.get(target)
                    .ok_or_else(|| format!("Branch target not found: {}", target))?;
                self.builder
                    .build_unconditional_branch(*target_bb)
                    .map_err(|e| format!("Failed to build branch: {}", e))?;
            }
            IrTerminator::CondBranch { cond, true_block, false_block } => {
                let cond_value = self.get_llvm_value(cond)?;
                let cond_int = cond_value.into_int_value();

                let true_bb = self.blocks.get(true_block)
                    .ok_or_else(|| format!("True branch target not found: {}", true_block))?;
                let false_bb = self.blocks.get(false_block)
                    .ok_or_else(|| format!("False branch target not found: {}", false_block))?;

                self.builder
                    .build_conditional_branch(cond_int, *true_bb, *false_bb)
                    .map_err(|e| format!("Failed to build conditional branch: {}", e))?;
            }
        }

        Ok(())
    }

    fn build_binop(
        &self,
        op: IrBinOp,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
        name: &str,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        // Check if we're dealing with floats
        if lhs.is_float_value() && rhs.is_float_value() {
            return self.build_float_binop(op, lhs.into_float_value(), rhs.into_float_value(), name);
        }

        let lhs_int = lhs.into_int_value();
        let rhs_int = rhs.into_int_value();

        let result = match op {
            IrBinOp::Add => self.builder.build_int_add(lhs_int, rhs_int, name),
            IrBinOp::Sub => self.builder.build_int_sub(lhs_int, rhs_int, name),
            IrBinOp::Mul => self.builder.build_int_mul(lhs_int, rhs_int, name),
            IrBinOp::Div => self.builder.build_int_signed_div(lhs_int, rhs_int, name),
            IrBinOp::Rem => self.builder.build_int_signed_rem(lhs_int, rhs_int, name),
            IrBinOp::And => self.builder.build_and(lhs_int, rhs_int, name),
            IrBinOp::Or => self.builder.build_or(lhs_int, rhs_int, name),
            IrBinOp::Xor => self.builder.build_xor(lhs_int, rhs_int, name),
            IrBinOp::Shl => self.builder.build_left_shift(lhs_int, rhs_int, name),
            IrBinOp::Shr => self
                .builder
                .build_right_shift(lhs_int, rhs_int, false, name),
            IrBinOp::Eq => {
                self.builder
                    .build_int_compare(inkwell::IntPredicate::EQ, lhs_int, rhs_int, name)
            }
            IrBinOp::Ne => {
                self.builder
                    .build_int_compare(inkwell::IntPredicate::NE, lhs_int, rhs_int, name)
            }
            IrBinOp::Lt => {
                self.builder
                    .build_int_compare(inkwell::IntPredicate::SLT, lhs_int, rhs_int, name)
            }
            IrBinOp::Le => {
                self.builder
                    .build_int_compare(inkwell::IntPredicate::SLE, lhs_int, rhs_int, name)
            }
            IrBinOp::Gt => {
                self.builder
                    .build_int_compare(inkwell::IntPredicate::SGT, lhs_int, rhs_int, name)
            }
            IrBinOp::Ge => {
                self.builder
                    .build_int_compare(inkwell::IntPredicate::SGE, lhs_int, rhs_int, name)
            }
        };

        result
            .map(|v| v.into())
            .map_err(|e| format!("Failed to build binary op: {}", e))
    }

    fn build_float_binop(
        &self,
        op: IrBinOp,
        lhs: inkwell::values::FloatValue<'ctx>,
        rhs: inkwell::values::FloatValue<'ctx>,
        name: &str,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        let result: Result<BasicValueEnum<'ctx>, String> = match op {
            IrBinOp::Add => self.builder.build_float_add(lhs, rhs, name)
                .map(|v| v.into())
                .map_err(|e| format!("Failed to build float add: {}", e)),
            IrBinOp::Sub => self.builder.build_float_sub(lhs, rhs, name)
                .map(|v| v.into())
                .map_err(|e| format!("Failed to build float sub: {}", e)),
            IrBinOp::Mul => self.builder.build_float_mul(lhs, rhs, name)
                .map(|v| v.into())
                .map_err(|e| format!("Failed to build float mul: {}", e)),
            IrBinOp::Div => self.builder.build_float_div(lhs, rhs, name)
                .map(|v| v.into())
                .map_err(|e| format!("Failed to build float div: {}", e)),
            IrBinOp::Rem => self.builder.build_float_rem(lhs, rhs, name)
                .map(|v| v.into())
                .map_err(|e| format!("Failed to build float rem: {}", e)),
            IrBinOp::Eq => self.builder
                .build_float_compare(inkwell::FloatPredicate::OEQ, lhs, rhs, name)
                .map(|v| v.into())
                .map_err(|e| format!("Failed to build float compare: {}", e)),
            IrBinOp::Ne => self.builder
                .build_float_compare(inkwell::FloatPredicate::ONE, lhs, rhs, name)
                .map(|v| v.into())
                .map_err(|e| format!("Failed to build float compare: {}", e)),
            IrBinOp::Lt => self.builder
                .build_float_compare(inkwell::FloatPredicate::OLT, lhs, rhs, name)
                .map(|v| v.into())
                .map_err(|e| format!("Failed to build float compare: {}", e)),
            IrBinOp::Le => self.builder
                .build_float_compare(inkwell::FloatPredicate::OLE, lhs, rhs, name)
                .map(|v| v.into())
                .map_err(|e| format!("Failed to build float compare: {}", e)),
            IrBinOp::Gt => self.builder
                .build_float_compare(inkwell::FloatPredicate::OGT, lhs, rhs, name)
                .map(|v| v.into())
                .map_err(|e| format!("Failed to build float compare: {}", e)),
            IrBinOp::Ge => self.builder
                .build_float_compare(inkwell::FloatPredicate::OGE, lhs, rhs, name)
                .map(|v| v.into())
                .map_err(|e| format!("Failed to build float compare: {}", e)),
            _ => Err(format!("Unsupported float operation: {:?}", op)),
        };
        result
    }

    fn get_llvm_value(&mut self, value: &IrValue) -> Result<BasicValueEnum<'ctx>, String> {
        match value {
            IrValue::ConstInt(n, ty) => {
                let llvm_ty = self.get_llvm_type(ty);
                Ok(llvm_ty.into_int_type().const_int(*n as u64, true).into())
            }
            IrValue::ConstFloat(n, ty) => {
                let llvm_ty = self.get_llvm_type(ty);
                Ok(llvm_ty.into_float_type().const_float(*n).into())
            }
            IrValue::ConstBool(b) => {
                Ok(self.context.bool_type().const_int(*b as u64, false).into())
            }
            IrValue::ConstString(s) => {
                // Create a global string constant and return pointer to it
                let str_ptr = self.create_global_string(s);
                Ok(str_ptr.into())
            }
            IrValue::Var(name) => {
                let ptr = self
                    .variables
                    .get(name)
                    .ok_or_else(|| format!("Variable not found: {}", name))?;
                // Use tracked type if available, otherwise default to i64
                let ty = self.variable_types
                    .get(name)
                    .cloned()
                    .unwrap_or_else(|| self.context.i64_type().into());
                self.builder
                    .build_load(ty, *ptr, name)
                    .map_err(|e| format!("Failed to build load: {}", e))
            }
            IrValue::Unit => {
                // Return a dummy value for unit type
                Ok(self.context.i64_type().const_zero().into())
            }
        }
    }

    fn get_llvm_type(&self, ty: &IrType) -> BasicTypeEnum<'ctx> {
        match ty {
            IrType::I8 | IrType::U8 => self.context.i8_type().into(),
            IrType::I16 | IrType::U16 => self.context.i16_type().into(),
            IrType::I32 | IrType::U32 => self.context.i32_type().into(),
            IrType::I64 | IrType::U64 => self.context.i64_type().into(),
            IrType::F32 => self.context.f32_type().into(),
            IrType::F64 => self.context.f64_type().into(),
            IrType::Bool => self.context.bool_type().into(),
            IrType::Ptr(_inner) => {
                // LLVM 15+ uses opaque pointers - no need for inner type
                self.context
                    .ptr_type(inkwell::AddressSpace::default())
                    .into()
            }
            IrType::Array(inner, size) => {
                let inner_ty = self.get_llvm_type(inner);
                inner_ty.array_type(*size as u32).into()
            }
            IrType::Struct(_name) => {
                // TODO: Look up struct type
                self.context.i64_type().into()
            }
            IrType::Void => self.context.i64_type().into(), // Placeholder
        }
    }

    /// Get or declare the malloc function
    fn get_or_declare_malloc(&mut self) -> Result<FunctionValue<'ctx>, String> {
        if let Some(func) = self.functions.get("malloc") {
            return Ok(*func);
        }

        // Declare: void* malloc(size_t size)
        let ptr_ty = self.context.ptr_type(inkwell::AddressSpace::default());
        let size_ty = self.context.i64_type();
        let malloc_type = ptr_ty.fn_type(&[size_ty.into()], false);
        let malloc_fn = self.module.add_function("malloc", malloc_type, None);
        self.functions.insert("malloc".to_string(), malloc_fn);
        Ok(malloc_fn)
    }

    /// Get or declare the free function
    #[allow(dead_code)]
    fn get_or_declare_free(&mut self) -> Result<FunctionValue<'ctx>, String> {
        if let Some(func) = self.functions.get("free") {
            return Ok(*func);
        }

        // Declare: void free(void* ptr)
        let ptr_ty = self.context.ptr_type(inkwell::AddressSpace::default());
        let free_type = self.context.void_type().fn_type(&[ptr_ty.into()], false);
        let free_fn = self.module.add_function("free", free_type, None);
        self.functions.insert("free".to_string(), free_fn);
        Ok(free_fn)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::lower_program;
    use crate::parser::Parser;

    fn compile_to_ir(source: &str) -> String {
        let mut parser = Parser::new(source);
        let program = parser.parse().expect("Parse error");
        let ir_module = lower_program(&program);

        let context = Context::create();
        let mut codegen = CodeGen::new(&context, "test");
        codegen.compile(&ir_module).expect("Codegen error");
        codegen.get_ir_string()
    }

    #[test]
    fn test_simple_function_codegen() {
        let source = r#"fn main() -> i64
    42
"#;
        let ir = compile_to_ir(source);
        assert!(ir.contains("define i64 @main()"));
        assert!(ir.contains("ret i64"));
    }

    #[test]
    fn test_function_with_params() {
        let source = r#"fn add(a: i64, b: i64) -> i64
    a + b
"#;
        let ir = compile_to_ir(source);
        assert!(ir.contains("define i64 @add(i64"));
        assert!(ir.contains("add i64"));
    }

    #[test]
    fn test_function_call_codegen() {
        let source = r#"fn helper() -> i64
    42

fn main() -> i64
    helper()
"#;
        let ir = compile_to_ir(source);
        assert!(ir.contains("call i64 @helper()"));
    }

    #[test]
    fn test_let_binding_codegen() {
        let source = r#"fn main() -> i64
    let x = 42
    x
"#;
        let ir = compile_to_ir(source);
        assert!(ir.contains("alloca i64"));
        assert!(ir.contains("store i64 42"));
    }

    #[test]
    fn test_binary_operations_with_vars() {
        // Use variables to prevent constant folding
        let source = r#"fn add(a: i64, b: i64) -> i64
    a + b
"#;
        let ir = compile_to_ir(source);
        // Check that addition instruction is generated
        assert!(ir.contains("add i64"));
    }

    #[test]
    fn test_subtraction_codegen() {
        let source = r#"fn sub(a: i64, b: i64) -> i64
    a - b
"#;
        let ir = compile_to_ir(source);
        assert!(ir.contains("sub i64"));
    }

    #[test]
    fn test_multiplication_codegen() {
        let source = r#"fn mul(a: i64, b: i64) -> i64
    a * b
"#;
        let ir = compile_to_ir(source);
        assert!(ir.contains("mul i64"));
    }

    #[test]
    fn test_division_codegen() {
        let source = r#"fn div(a: i64, b: i64) -> i64
    a / b
"#;
        let ir = compile_to_ir(source);
        assert!(ir.contains("sdiv i64"));
    }

    #[test]
    fn test_comparison_codegen() {
        let source = r#"fn less(a: i64, b: i64) -> bool
    a < b
"#;
        let ir = compile_to_ir(source);
        assert!(ir.contains("icmp slt i64"));
    }

    #[test]
    fn test_constant_folding() {
        // Verify that 1 + 2 is constant folded to 3
        let source = r#"fn main() -> i64
    1 + 2
"#;
        let ir = compile_to_ir(source);
        assert!(ir.contains("store i64 3"));
    }

    #[test]
    fn test_multiple_functions() {
        let source = r#"fn foo() -> i64
    1

fn bar() -> i64
    2

fn main() -> i64
    foo()
"#;
        let ir = compile_to_ir(source);
        assert!(ir.contains("define i64 @foo()"));
        assert!(ir.contains("define i64 @bar()"));
        assert!(ir.contains("define i64 @main()"));
    }

    #[test]
    fn test_float_function_codegen() {
        let source = r#"fn add_floats(a: f64, b: f64) -> f64
    a + b
"#;
        let ir = compile_to_ir(source);
        assert!(ir.contains("define double @add_floats(double"));
        assert!(ir.contains("fadd double"));
    }

    #[test]
    fn test_float_literal_codegen() {
        let source = r#"fn get_pi() -> f64
    3.14159
"#;
        let ir = compile_to_ir(source);
        assert!(ir.contains("define double @get_pi()"));
        // Float constant should be present
        assert!(ir.contains("3.14159"));
    }
}
