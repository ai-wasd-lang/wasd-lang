//! Type system module for Lux.
//!
//! Handles type inference, type checking, and generic monomorphization.

mod checker;
mod types;

pub use checker::TypeChecker;
pub use types::*;
