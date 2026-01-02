//! Type system module for WASD.
//!
//! Handles type inference, type checking, and generic monomorphization.

mod checker;
pub mod monomorphize;
mod types;

pub use checker::TypeChecker;
pub use monomorphize::{monomorphize, Monomorphizer};
pub use types::WasdType;
