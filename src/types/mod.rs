//! Type system module for WASD.
//!
//! Handles type inference, type checking, and generic monomorphization.

pub mod checker;
#[cfg(test)]
mod checker_tests;
mod inference;
pub mod monomorphize;
mod mono_specialize;
mod mono_transform;
#[cfg(test)]
mod monomorphize_tests;
pub mod types;

pub use checker::TypeChecker;
pub use monomorphize::{monomorphize, Monomorphizer};
pub use types::WasdType;
