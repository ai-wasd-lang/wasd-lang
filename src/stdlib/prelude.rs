//! Standard prelude module (std.prelude)
//!
//! Items that are commonly needed and can be auto-imported.
//! When a program uses `use std.prelude.*`, these become available.

use crate::types::WasdType;
use std::collections::HashMap;

use super::StdModule;

/// Get the std.prelude module definition.
///
/// The prelude contains commonly used items:
/// - print, println (from std.io)
/// - assert, assert_eq (debugging)
/// - Common types would go here once we have them (Option, Result, Vec, String)
pub fn module() -> StdModule {
    let mut functions = HashMap::new();

    // Re-export print and println from std.io
    functions.insert(
        "print".to_string(),
        WasdType::Function {
            params: vec![WasdType::String],
            ret: Box::new(WasdType::I32),
            effects: vec!["IO".to_string()],
        },
    );

    functions.insert(
        "println".to_string(),
        WasdType::Function {
            params: vec![WasdType::String],
            ret: Box::new(WasdType::I32),
            effects: vec!["IO".to_string()],
        },
    );

    // assert: (bool) -> () with [Panic]
    // Panics if condition is false
    functions.insert(
        "assert".to_string(),
        WasdType::Function {
            params: vec![WasdType::Bool],
            ret: Box::new(WasdType::Unit),
            effects: vec!["Panic".to_string()],
        },
    );

    // dbg: (T) -> T with [IO]
    // Debug print and return value (placeholder for now)
    // In real implementation this would be a macro/generic

    // Re-export type constructors from std.types
    // Some: (T) -> Option[T]
    functions.insert(
        "Some".to_string(),
        WasdType::Function {
            params: vec![WasdType::Unknown],
            ret: Box::new(WasdType::Named("Option".to_string())),
            effects: vec![],
        },
    );

    // None is a value, not a function
    functions.insert("None".to_string(), WasdType::Named("Option".to_string()));

    // Ok: (T) -> Result[T, E]
    functions.insert(
        "Ok".to_string(),
        WasdType::Function {
            params: vec![WasdType::Unknown],
            ret: Box::new(WasdType::Named("Result".to_string())),
            effects: vec![],
        },
    );

    // Err: (E) -> Result[T, E]
    functions.insert(
        "Err".to_string(),
        WasdType::Function {
            params: vec![WasdType::Unknown],
            ret: Box::new(WasdType::Named("Result".to_string())),
            effects: vec![],
        },
    );

    StdModule::with_functions(functions)
}
