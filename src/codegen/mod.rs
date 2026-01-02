//! Code generation module for Lux.
//!
//! Compiles Lux IR to LLVM IR using inkwell.

mod llvm;

pub use llvm::CodeGen;
