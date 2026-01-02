//! Type system module for WASD.
//!
//! Handles type inference, type checking, and generic monomorphization.

pub mod checker;
#[cfg(test)]
mod checker_tests;
mod inference;
#[allow(dead_code)]
pub mod monomorphize;
#[allow(dead_code)]
mod mono_specialize;
#[allow(dead_code)]
mod mono_transform;
#[cfg(test)]
mod monomorphize_tests;
pub mod types;

pub use checker::TypeChecker;
pub use types::WasdType;
