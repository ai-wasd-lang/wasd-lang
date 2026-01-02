//! Code generation module for WASD.
//!
//! Compiles WASD IR to LLVM IR using inkwell.

mod llvm;

pub use llvm::CodeGen;
