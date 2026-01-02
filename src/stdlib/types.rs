//! Standard library type definitions (std.types)
//!
//! Provides type constructors for Option, Result, Vec, String, HashMap.

use crate::types::WasdType;
use std::collections::HashMap;

use super::StdModule;

/// Get the std.types module definition.
///
/// This module provides common type constructors:
/// - Some/None for Option
/// - Ok/Err for Result
pub fn module() -> StdModule {
    let mut functions = HashMap::new();

    // Some: (T) -> Option[T]
    // Type constructor for Option's Some variant
    functions.insert(
        "Some".to_string(),
        WasdType::Function {
            params: vec![WasdType::Unknown], // Generic T
            ret: Box::new(WasdType::Named("Option".to_string())),
            effects: vec![],
        },
    );

    // None: () -> Option[T]
    // Type constructor for Option's None variant
    functions.insert(
        "None".to_string(),
        WasdType::Named("Option".to_string()),
    );

    // Ok: (T) -> Result[T, E]
    // Type constructor for Result's Ok variant
    functions.insert(
        "Ok".to_string(),
        WasdType::Function {
            params: vec![WasdType::Unknown], // Generic T
            ret: Box::new(WasdType::Named("Result".to_string())),
            effects: vec![],
        },
    );

    // Err: (E) -> Result[T, E]
    // Type constructor for Result's Err variant
    functions.insert(
        "Err".to_string(),
        WasdType::Function {
            params: vec![WasdType::Unknown], // Generic E
            ret: Box::new(WasdType::Named("Result".to_string())),
            effects: vec![],
        },
    );

    StdModule::with_functions(functions)
}
