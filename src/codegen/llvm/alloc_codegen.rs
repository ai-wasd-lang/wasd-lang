//! Heap allocation codegen (heap, rc, arc).

use super::CodeGen;
use crate::ir::*;
use inkwell::types::BasicType;

impl<'ctx> CodeGen<'ctx> {
    pub(super) fn compile_heap_alloc(
        &mut self,
        dest: &str,
        ty: &IrType,
        value: &Option<IrValue>,
    ) -> Result<(), String> {
        let malloc_fn = self.get_or_declare_malloc()?;
        let llvm_ty = self.get_llvm_type(ty);

        let size = llvm_ty
            .size_of()
            .ok_or_else(|| "Cannot get size of type".to_string())?;

        let ptr = self
            .builder
            .build_call(malloc_fn, &[size.into()], "heap_alloc")
            .map_err(|e| format!("Failed to call malloc: {}", e))?
            .try_as_basic_value()
            .left()
            .ok_or_else(|| "malloc returned void".to_string())?;

        if let Some(val) = value {
            let llvm_val = self.get_llvm_value(val)?;
            self.builder
                .build_store(ptr.into_pointer_value(), llvm_val)
                .map_err(|e| format!("Failed to store heap value: {}", e))?;
        }

        let ptr_ty = self.context.ptr_type(inkwell::AddressSpace::default());
        let alloca = self
            .builder
            .build_alloca(ptr_ty, dest)
            .map_err(|e| format!("Failed to build alloca: {}", e))?;
        self.builder
            .build_store(alloca, ptr)
            .map_err(|e| format!("Failed to store: {}", e))?;
        self.variables.insert(dest.to_string(), alloca);

        Ok(())
    }

    pub(super) fn compile_rc_alloc(
        &mut self,
        dest: &str,
        ty: &IrType,
        value: &Option<IrValue>,
    ) -> Result<(), String> {
        let malloc_fn = self.get_or_declare_malloc()?;
        let llvm_ty = self.get_llvm_type(ty);

        let refcount_size = self.context.i64_type().size_of();
        let value_size = llvm_ty
            .size_of()
            .ok_or_else(|| "Cannot get size of type".to_string())?;
        let total_size = self
            .builder
            .build_int_add(refcount_size, value_size, "total_size")
            .map_err(|e| format!("Failed to add sizes: {}", e))?;

        let ptr = self
            .builder
            .build_call(malloc_fn, &[total_size.into()], "rc_alloc")
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
                        "value_ptr",
                    )
                    .map_err(|e| format!("Failed to compute value ptr: {}", e))?
            };
            self.builder
                .build_store(value_ptr, llvm_val)
                .map_err(|e| format!("Failed to store rc value: {}", e))?;
        }

        let ptr_ty = self.context.ptr_type(inkwell::AddressSpace::default());
        let alloca = self
            .builder
            .build_alloca(ptr_ty, dest)
            .map_err(|e| format!("Failed to build alloca: {}", e))?;
        self.builder
            .build_store(alloca, ptr)
            .map_err(|e| format!("Failed to store: {}", e))?;
        self.variables.insert(dest.to_string(), alloca);

        Ok(())
    }

    pub(super) fn compile_arc_alloc(
        &mut self,
        dest: &str,
        ty: &IrType,
        value: &Option<IrValue>,
    ) -> Result<(), String> {
        let malloc_fn = self.get_or_declare_malloc()?;
        let llvm_ty = self.get_llvm_type(ty);

        let refcount_size = self.context.i64_type().size_of();
        let value_size = llvm_ty
            .size_of()
            .ok_or_else(|| "Cannot get size of type".to_string())?;
        let total_size = self
            .builder
            .build_int_add(refcount_size, value_size, "total_size")
            .map_err(|e| format!("Failed to add sizes: {}", e))?;

        let ptr = self
            .builder
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
                        "value_ptr",
                    )
                    .map_err(|e| format!("Failed to compute value ptr: {}", e))?
            };
            self.builder
                .build_store(value_ptr, llvm_val)
                .map_err(|e| format!("Failed to store arc value: {}", e))?;
        }

        let ptr_ty = self.context.ptr_type(inkwell::AddressSpace::default());
        let alloca = self
            .builder
            .build_alloca(ptr_ty, dest)
            .map_err(|e| format!("Failed to build alloca: {}", e))?;
        self.builder
            .build_store(alloca, ptr)
            .map_err(|e| format!("Failed to store: {}", e))?;
        self.variables.insert(dest.to_string(), alloca);

        Ok(())
    }

    pub(super) fn compile_rc_incref(&mut self, ptr: &str) -> Result<(), String> {
        let ptr_val = *self
            .variables
            .get(ptr)
            .ok_or_else(|| format!("Variable not found: {}", ptr))?;
        let actual_ptr = self
            .builder
            .build_load(
                self.context.ptr_type(inkwell::AddressSpace::default()),
                ptr_val,
                "rc_ptr",
            )
            .map_err(|e| format!("Failed to load ptr: {}", e))?;
        let count = self
            .builder
            .build_load(
                self.context.i64_type(),
                actual_ptr.into_pointer_value(),
                "count",
            )
            .map_err(|e| format!("Failed to load count: {}", e))?;
        let new_count = self
            .builder
            .build_int_add(
                count.into_int_value(),
                self.context.i64_type().const_int(1, false),
                "new_count",
            )
            .map_err(|e| format!("Failed to increment: {}", e))?;
        self.builder
            .build_store(actual_ptr.into_pointer_value(), new_count)
            .map_err(|e| format!("Failed to store count: {}", e))?;

        Ok(())
    }

    pub(super) fn compile_rc_decref(&mut self, ptr: &str) -> Result<(), String> {
        let ptr_val = *self
            .variables
            .get(ptr)
            .ok_or_else(|| format!("Variable not found: {}", ptr))?;
        let actual_ptr = self
            .builder
            .build_load(
                self.context.ptr_type(inkwell::AddressSpace::default()),
                ptr_val,
                "rc_ptr",
            )
            .map_err(|e| format!("Failed to load ptr: {}", e))?;
        let count = self
            .builder
            .build_load(
                self.context.i64_type(),
                actual_ptr.into_pointer_value(),
                "count",
            )
            .map_err(|e| format!("Failed to load count: {}", e))?;
        let new_count = self
            .builder
            .build_int_sub(
                count.into_int_value(),
                self.context.i64_type().const_int(1, false),
                "new_count",
            )
            .map_err(|e| format!("Failed to decrement: {}", e))?;
        self.builder
            .build_store(actual_ptr.into_pointer_value(), new_count)
            .map_err(|e| format!("Failed to store count: {}", e))?;

        Ok(())
    }
}
