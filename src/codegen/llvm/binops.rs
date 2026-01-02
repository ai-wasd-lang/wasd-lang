//! Binary operation compilation for LLVM code generation.

use super::CodeGen;
use crate::ir::IrBinOp;
use inkwell::values::BasicValueEnum;

impl<'ctx> CodeGen<'ctx> {
    pub(super) fn build_binop(
        &self,
        op: IrBinOp,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
        name: &str,
    ) -> Result<BasicValueEnum<'ctx>, String> {
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
            IrBinOp::Shr => self.builder.build_right_shift(lhs_int, rhs_int, false, name),
            IrBinOp::Eq => self.builder.build_int_compare(
                inkwell::IntPredicate::EQ,
                lhs_int,
                rhs_int,
                name,
            ),
            IrBinOp::Ne => self.builder.build_int_compare(
                inkwell::IntPredicate::NE,
                lhs_int,
                rhs_int,
                name,
            ),
            IrBinOp::Lt => self.builder.build_int_compare(
                inkwell::IntPredicate::SLT,
                lhs_int,
                rhs_int,
                name,
            ),
            IrBinOp::Le => self.builder.build_int_compare(
                inkwell::IntPredicate::SLE,
                lhs_int,
                rhs_int,
                name,
            ),
            IrBinOp::Gt => self.builder.build_int_compare(
                inkwell::IntPredicate::SGT,
                lhs_int,
                rhs_int,
                name,
            ),
            IrBinOp::Ge => self.builder.build_int_compare(
                inkwell::IntPredicate::SGE,
                lhs_int,
                rhs_int,
                name,
            ),
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
        match op {
            IrBinOp::Add => self
                .builder
                .build_float_add(lhs, rhs, name)
                .map(|v| v.into())
                .map_err(|e| format!("Failed to build float add: {}", e)),
            IrBinOp::Sub => self
                .builder
                .build_float_sub(lhs, rhs, name)
                .map(|v| v.into())
                .map_err(|e| format!("Failed to build float sub: {}", e)),
            IrBinOp::Mul => self
                .builder
                .build_float_mul(lhs, rhs, name)
                .map(|v| v.into())
                .map_err(|e| format!("Failed to build float mul: {}", e)),
            IrBinOp::Div => self
                .builder
                .build_float_div(lhs, rhs, name)
                .map(|v| v.into())
                .map_err(|e| format!("Failed to build float div: {}", e)),
            IrBinOp::Rem => self
                .builder
                .build_float_rem(lhs, rhs, name)
                .map(|v| v.into())
                .map_err(|e| format!("Failed to build float rem: {}", e)),
            IrBinOp::Eq => self
                .builder
                .build_float_compare(inkwell::FloatPredicate::OEQ, lhs, rhs, name)
                .map(|v| v.into())
                .map_err(|e| format!("Failed to build float compare: {}", e)),
            IrBinOp::Ne => self
                .builder
                .build_float_compare(inkwell::FloatPredicate::ONE, lhs, rhs, name)
                .map(|v| v.into())
                .map_err(|e| format!("Failed to build float compare: {}", e)),
            IrBinOp::Lt => self
                .builder
                .build_float_compare(inkwell::FloatPredicate::OLT, lhs, rhs, name)
                .map(|v| v.into())
                .map_err(|e| format!("Failed to build float compare: {}", e)),
            IrBinOp::Le => self
                .builder
                .build_float_compare(inkwell::FloatPredicate::OLE, lhs, rhs, name)
                .map(|v| v.into())
                .map_err(|e| format!("Failed to build float compare: {}", e)),
            IrBinOp::Gt => self
                .builder
                .build_float_compare(inkwell::FloatPredicate::OGT, lhs, rhs, name)
                .map(|v| v.into())
                .map_err(|e| format!("Failed to build float compare: {}", e)),
            IrBinOp::Ge => self
                .builder
                .build_float_compare(inkwell::FloatPredicate::OGE, lhs, rhs, name)
                .map(|v| v.into())
                .map_err(|e| format!("Failed to build float compare: {}", e)),
            _ => Err(format!("Unsupported float operation: {:?}", op)),
        }
    }
}
