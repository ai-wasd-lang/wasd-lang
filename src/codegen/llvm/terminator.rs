//! Terminator instruction compilation for LLVM code generation.

use super::CodeGen;
use crate::ir::IrTerminator;

impl<'ctx> CodeGen<'ctx> {
    pub(super) fn compile_terminator(&mut self, term: &IrTerminator) -> Result<(), String> {
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
                let target_bb = self
                    .blocks
                    .get(target)
                    .ok_or_else(|| format!("Branch target not found: {}", target))?;
                self.builder
                    .build_unconditional_branch(*target_bb)
                    .map_err(|e| format!("Failed to build branch: {}", e))?;
            }
            IrTerminator::CondBranch {
                cond,
                true_block,
                false_block,
            } => {
                let cond_value = self.get_llvm_value(cond)?;
                let cond_int = cond_value.into_int_value();

                let true_bb = self
                    .blocks
                    .get(true_block)
                    .ok_or_else(|| format!("True branch target not found: {}", true_block))?;
                let false_bb = self
                    .blocks
                    .get(false_block)
                    .ok_or_else(|| format!("False branch target not found: {}", false_block))?;

                self.builder
                    .build_conditional_branch(cond_int, *true_bb, *false_bb)
                    .map_err(|e| format!("Failed to build conditional branch: {}", e))?;
            }
        }

        Ok(())
    }
}
