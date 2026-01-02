//! Value and type handling for LLVM code generation.

use super::CodeGen;
use crate::ir::*;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicValueEnum, FunctionValue};

impl<'ctx> CodeGen<'ctx> {
    pub(super) fn get_llvm_value(&mut self, value: &IrValue) -> Result<BasicValueEnum<'ctx>, String> {
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
                let str_ptr = self.create_global_string(s);
                Ok(str_ptr.into())
            }
            IrValue::Var(name) => {
                let ptr = self
                    .variables
                    .get(name)
                    .ok_or_else(|| format!("Variable not found: {}", name))?;
                let ty = self
                    .variable_types
                    .get(name)
                    .cloned()
                    .unwrap_or_else(|| self.context.i64_type().into());
                self.builder
                    .build_load(ty, *ptr, name)
                    .map_err(|e| format!("Failed to build load: {}", e))
            }
            IrValue::Unit => {
                Ok(self.context.i64_type().const_zero().into())
            }
        }
    }

    pub(super) fn get_llvm_type(&self, ty: &IrType) -> BasicTypeEnum<'ctx> {
        match ty {
            IrType::I8 | IrType::U8 => self.context.i8_type().into(),
            IrType::I16 | IrType::U16 => self.context.i16_type().into(),
            IrType::I32 | IrType::U32 => self.context.i32_type().into(),
            IrType::I64 | IrType::U64 => self.context.i64_type().into(),
            IrType::F32 => self.context.f32_type().into(),
            IrType::F64 => self.context.f64_type().into(),
            IrType::Bool => self.context.bool_type().into(),
            IrType::Ptr(_inner) => self
                .context
                .ptr_type(inkwell::AddressSpace::default())
                .into(),
            IrType::Array(inner, size) => {
                let inner_ty = self.get_llvm_type(inner);
                inner_ty.array_type(*size as u32).into()
            }
            IrType::Struct(name) => {
                if let Some(struct_type) = self.struct_types.get(name) {
                    (*struct_type).into()
                } else {
                    self.context.i64_type().into()
                }
            }
            IrType::Void => self.context.i64_type().into(),
        }
    }

    pub(super) fn get_or_declare_malloc(&mut self) -> Result<FunctionValue<'ctx>, String> {
        if let Some(func) = self.functions.get("malloc") {
            return Ok(*func);
        }

        let ptr_ty = self.context.ptr_type(inkwell::AddressSpace::default());
        let size_ty = self.context.i64_type();
        let malloc_type = ptr_ty.fn_type(&[size_ty.into()], false);
        let malloc_fn = self.module.add_function("malloc", malloc_type, None);
        self.functions.insert("malloc".to_string(), malloc_fn);
        Ok(malloc_fn)
    }

    #[allow(dead_code)]
    pub(super) fn get_or_declare_free(&mut self) -> Result<FunctionValue<'ctx>, String> {
        if let Some(func) = self.functions.get("free") {
            return Ok(*func);
        }

        let ptr_ty = self.context.ptr_type(inkwell::AddressSpace::default());
        let free_type = self.context.void_type().fn_type(&[ptr_ty.into()], false);
        let free_fn = self.module.add_function("free", free_type, None);
        self.functions.insert("free".to_string(), free_fn);
        Ok(free_fn)
    }
}
