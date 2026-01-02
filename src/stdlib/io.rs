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

    // TODO: Add more IO functions
    // - read_line: () -> String with [IO]
    // - eprint: (String) -> i32 with [IO]
    // - eprintln: (String) -> i32 with [IO]

    StdModule::with_functions(functions)
}
