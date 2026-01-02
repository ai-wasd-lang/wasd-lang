//! Standard file system module (std.fs)
//!
//! Provides file I/O operations.

use crate::types::WasdType;
use std::collections::HashMap;

use super::StdModule;

/// Get the std.fs module definition.
pub fn module() -> StdModule {
    let mut functions = HashMap::new();

    // File_open: (String, String) -> i64 with [IO]
    // Opens a file with mode ("r", "w", "a", etc.), returns file handle
    functions.insert(
        "File_open".to_string(),
        WasdType::Function {
            params: vec![WasdType::String, WasdType::String],
            ret: Box::new(WasdType::I64),
            effects: vec!["IO".to_string()],
        },
    );

    // File_close: (i64) -> i32 with [IO]
    // Closes a file handle
    functions.insert(
        "File_close".to_string(),
        WasdType::Function {
            params: vec![WasdType::I64],
            ret: Box::new(WasdType::I32),
            effects: vec!["IO".to_string()],
        },
    );

    // File_read: (i64, i64) -> String with [IO]
    // Reads up to n bytes from file handle
    functions.insert(
        "File_read".to_string(),
        WasdType::Function {
            params: vec![WasdType::I64, WasdType::I64],
            ret: Box::new(WasdType::String),
            effects: vec!["IO".to_string()],
        },
    );

    // File_write: (i64, String) -> i64 with [IO]
    // Writes string to file handle, returns bytes written
    functions.insert(
        "File_write".to_string(),
        WasdType::Function {
            params: vec![WasdType::I64, WasdType::String],
            ret: Box::new(WasdType::I64),
            effects: vec!["IO".to_string()],
        },
    );

    // File_read_all: (String) -> String with [IO]
    // Reads entire file contents as string
    functions.insert(
        "File_read_all".to_string(),
        WasdType::Function {
            params: vec![WasdType::String],
            ret: Box::new(WasdType::String),
            effects: vec!["IO".to_string()],
        },
    );

    // File_write_all: (String, String) -> i64 with [IO]
    // Writes string to file (creates or overwrites)
    functions.insert(
        "File_write_all".to_string(),
        WasdType::Function {
            params: vec![WasdType::String, WasdType::String],
            ret: Box::new(WasdType::I64),
            effects: vec!["IO".to_string()],
        },
    );

    // File_exists: (String) -> bool with [IO]
    // Check if file exists
    functions.insert(
        "File_exists".to_string(),
        WasdType::Function {
            params: vec![WasdType::String],
            ret: Box::new(WasdType::Bool),
            effects: vec!["IO".to_string()],
        },
    );

    StdModule::with_functions(functions)
}
