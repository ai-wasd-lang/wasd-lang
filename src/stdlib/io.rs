//! Standard IO module (std.io)
//!
//! Provides basic input/output functions.

use crate::types::WasdType;
use std::collections::HashMap;

use super::StdModule;

/// Get the std.io module definition.
pub fn module() -> StdModule {
    let mut functions = HashMap::new();

    // print: (String) -> i32 with [IO]
    // Prints a string to stdout (no newline)
    functions.insert(
        "print".to_string(),
        WasdType::Function {
            params: vec![WasdType::String],
            ret: Box::new(WasdType::I32),
            effects: vec!["IO".to_string()],
        },
    );

    // println: (String) -> i32 with [IO]
    // Prints a string to stdout with a newline
    functions.insert(
        "println".to_string(),
        WasdType::Function {
            params: vec![WasdType::String],
            ret: Box::new(WasdType::I32),
            effects: vec!["IO".to_string()],
        },
    );

    // read_line: () -> String with [IO]
    // Reads a line from stdin
    functions.insert(
        "read_line".to_string(),
        WasdType::Function {
            params: vec![],
            ret: Box::new(WasdType::String),
            effects: vec!["IO".to_string()],
        },
    );

    // eprint: (String) -> i32 with [IO]
    // Prints a string to stderr (no newline)
    functions.insert(
        "eprint".to_string(),
        WasdType::Function {
            params: vec![WasdType::String],
            ret: Box::new(WasdType::I32),
            effects: vec!["IO".to_string()],
        },
    );

    // eprintln: (String) -> i32 with [IO]
    // Prints a string to stderr with a newline
    functions.insert(
        "eprintln".to_string(),
        WasdType::Function {
            params: vec![WasdType::String],
            ret: Box::new(WasdType::I32),
            effects: vec!["IO".to_string()],
        },
    );

    // print_int: (i64) -> i32 with [IO]
    // Prints an integer to stdout
    functions.insert(
        "print_int".to_string(),
        WasdType::Function {
            params: vec![WasdType::I64],
            ret: Box::new(WasdType::I32),
            effects: vec!["IO".to_string()],
        },
    );

    // print_bool: (bool) -> i32 with [IO]
    // Prints a boolean to stdout
    functions.insert(
        "print_bool".to_string(),
        WasdType::Function {
            params: vec![WasdType::Bool],
            ret: Box::new(WasdType::I32),
            effects: vec!["IO".to_string()],
        },
    );

    StdModule::with_functions(functions)
}
