//! Intermediate representation module for WASD.
//!
//! Provides the WASD IR that bridges the AST and LLVM IR.

mod lower;
mod wasd_ir;

pub use lower::lower_program;
pub use wasd_ir::*;
