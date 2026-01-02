//! Intermediate representation module for Lux.
//!
//! Provides the Lux IR that bridges the AST and LLVM IR.

mod lower;
mod lux_ir;

pub use lower::lower_program;
pub use lux_ir::*;
