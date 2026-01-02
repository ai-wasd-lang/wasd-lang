//! Type system module for WASD.
//!
//! Handles type inference, type checking, and generic monomorphization.

mod checker;
mod types;

pub use checker::TypeChecker;
