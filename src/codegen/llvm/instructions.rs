//! Instruction compilation for LLVM code generation.

use super::CodeGen;
use crate::ir::*;
use inkwell::values::BasicMetadataValueEnum;

impl<'ctx> CodeGen<'ctx> {
    pub(super) fn compile_instruction(&mut self, inst: &IrInst) -> Result<(), String> {
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
                let ptr_alloca = *self
                    .variables
                    .get(ptr)
                    .ok_or_else(|| format!("Variable not found: {}", ptr))?;

                let actual_ptr = if self.gep_results.contains(ptr) {
                    let ptr_ty = self.context.ptr_type(inkwell::AddressSpace::default());
                    self.builder
                        .build_load(ptr_ty, ptr_alloca, "actual_ptr")
                        .map_err(|e| format!("Failed to load pointer: {}", e))?
                        .into_pointer_value()
                } else {
                    ptr_alloca
                };

                self.builder
                    .build_store(actual_ptr, llvm_value)
                    .map_err(|e| format!("Failed to build store: {}", e))?;
            }
            IrInst::Load { dest, ptr, ty } => {
                let ptr_alloca = *self
                    .variables
                    .get(ptr)
                    .ok_or_else(|| format!("Variable not found: {}", ptr))?;
                let llvm_ty = self.get_llvm_type(ty);

                // If the pointer is a GEP result, we need to load the actual pointer first
                let actual_ptr = if self.gep_results.contains(ptr) {
                    let ptr_ty = self.context.ptr_type(inkwell::AddressSpace::default());
                    self.builder
                        .build_load(ptr_ty, ptr_alloca, "actual_ptr")
                        .map_err(|e| format!("Failed to load pointer: {}", e))?
                        .into_pointer_value()
                } else {
                    ptr_alloca
                };

                let loaded = self
                    .builder
                    .build_load(llvm_ty, actual_ptr, dest)
                    .map_err(|e| format!("Failed to build load: {}", e))?;

                let alloca = self
                    .builder
                    .build_alloca(llvm_ty, dest)
                    .map_err(|e| format!("Failed to build alloca: {}", e))?;
                self.builder
                    .build_store(alloca, loaded)
                    .map_err(|e| format!("Failed to build store: {}", e))?;
                self.variables.insert(dest.clone(), alloca);
                self.variable_types.insert(dest.clone(), llvm_ty);
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
                let actual_func = match func.as_str() {
                    "println" => "puts",
                    "print" => "printf",
                    // IO functions that need special handling for stderr
                    "eprint" | "eprintln" => "fprintf",
                    // String stdlib functions
                    "String_len" => "strlen",
                    "String_eq" | "String_cmp" => "strcmp",
                    "String_contains" => "strstr",
                    "String_concat" => "strcat",
                    "String_copy" => "strcpy",
                    // File I/O stdlib functions
                    "File_open" => "fopen",
                    "File_close" => "fclose",
                    "File_write" => "fputs",
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
            IrInst::GetElementPtr { dest, ptr, indices, base_type } => {
                let ptr_alloca = *self
                    .variables
                    .get(ptr)
                    .ok_or_else(|| format!("Variable not found: {}", ptr))?;

                // If the pointer is a GEP result or a pointer parameter, we need to load the actual pointer first
                let ptr_val = if self.gep_results.contains(ptr) || self.pointer_params.contains(ptr) {
                    let ptr_ty = self.context.ptr_type(inkwell::AddressSpace::default());
                    self.builder
                        .build_load(ptr_ty, ptr_alloca, "actual_ptr")
                        .map_err(|e| format!("Failed to load pointer: {}", e))?
                        .into_pointer_value()
                } else {
                    ptr_alloca
                };

                let mut llvm_indices = Vec::new();
                for idx in indices {
                    match idx {
                        IrValue::ConstInt(val, _) => {
                            llvm_indices
                                .push(self.context.i32_type().const_int(*val as u64, false));
                        }
                        _ => {
                            return Err("Non-constant GEP index not supported".to_string());
                        }
                    }
                }

                // Use the struct type if provided, otherwise fall back to i64
                let gep = match base_type {
                    Some(IrType::Struct(name)) => {
                        if let Some(struct_ty) = self.struct_types.get(name) {
                            unsafe {
                                self.builder.build_gep(
                                    *struct_ty,
                                    ptr_val,
                                    &llvm_indices.iter().map(|i| (*i).into()).collect::<Vec<_>>(),
                                    dest,
                                )
                            }
                        } else {
                            unsafe {
                                self.builder.build_gep(
                                    self.context.i64_type(),
                                    ptr_val,
                                    &llvm_indices.iter().map(|i| (*i).into()).collect::<Vec<_>>(),
                                    dest,
                                )
                            }
                        }
                    }
                    _ => unsafe {
                        self.builder.build_gep(
                            self.context.i64_type(),
                            ptr_val,
                            &llvm_indices.iter().map(|i| (*i).into()).collect::<Vec<_>>(),
                            dest,
                        )
                    },
                }
                .map_err(|e| format!("Failed to build GEP: {}", e))?;

                let ptr_ty = self.context.ptr_type(inkwell::AddressSpace::default());
                let alloca = self
                    .builder
                    .build_alloca(ptr_ty, &format!("{}_alloca", dest))
                    .map_err(|e| format!("Failed to build alloca: {}", e))?;
                self.builder
                    .build_store(alloca, gep)
                    .map_err(|e| format!("Failed to store: {}", e))?;
                self.variables.insert(dest.clone(), alloca);
                self.gep_results.insert(dest.clone());
            }
            IrInst::HeapAlloc { dest, ty, value } => {
                self.compile_heap_alloc(dest, ty, value)?;
            }
            IrInst::RcAlloc { dest, ty, value } => {
                self.compile_rc_alloc(dest, ty, value)?;
            }
            IrInst::ArcAlloc { dest, ty, value } => {
                self.compile_arc_alloc(dest, ty, value)?;
            }
            IrInst::RcIncRef { ptr } => {
                self.compile_rc_incref(ptr)?;
            }
            IrInst::RcDecRef { ptr } => {
                self.compile_rc_decref(ptr)?;
            }
        }

        Ok(())
    }
}
