//! Standard library string utilities (std.string)
//!
//! Provides String type and string manipulation functions.

use crate::types::WasdType;
use std::collections::HashMap;

use super::StdModule;

/// Get the std.string module definition.
///
/// This module provides String operations.
pub fn module() -> StdModule {
    let mut functions = HashMap::new();

    // String::new: () -> String
    functions.insert(
        "String_new".to_string(),
        WasdType::Function {
            params: vec![],
            ret: Box::new(WasdType::String),
            effects: vec!["Alloc".to_string()],
        },
    );

    // String::from: (&str) -> String
    functions.insert(
        "String_from".to_string(),
        WasdType::Function {
            params: vec![WasdType::String], // String literal
            ret: Box::new(WasdType::String),
            effects: vec!["Alloc".to_string()],
        },
    );

    // String::len: (String) -> i64
    functions.insert(
        "String_len".to_string(),
        WasdType::Function {
            params: vec![WasdType::String],
            ret: Box::new(WasdType::I64),
            effects: vec![],
        },
    );

    // String::is_empty: (String) -> bool
    functions.insert(
        "String_is_empty".to_string(),
        WasdType::Function {
            params: vec![WasdType::String],
            ret: Box::new(WasdType::Bool),
            effects: vec![],
        },
    );

    // String::push: (String, char) -> ()
    functions.insert(
        "String_push".to_string(),
        WasdType::Function {
            params: vec![WasdType::String, WasdType::Char],
            ret: Box::new(WasdType::Unit),
            effects: vec![],
        },
    );

    // String::push_str: (String, String) -> ()
    functions.insert(
        "String_push_str".to_string(),
        WasdType::Function {
            params: vec![WasdType::String, WasdType::String],
            ret: Box::new(WasdType::Unit),
            effects: vec![],
        },
    );

    // String::contains: (String, String) -> bool
    functions.insert(
        "String_contains".to_string(),
        WasdType::Function {
            params: vec![WasdType::String, WasdType::String],
            ret: Box::new(WasdType::Bool),
            effects: vec![],
        },
    );

    // String::starts_with: (String, String) -> bool
    functions.insert(
        "String_starts_with".to_string(),
        WasdType::Function {
            params: vec![WasdType::String, WasdType::String],
            ret: Box::new(WasdType::Bool),
            effects: vec![],
        },
    );

    // String::ends_with: (String, String) -> bool
    functions.insert(
        "String_ends_with".to_string(),
        WasdType::Function {
            params: vec![WasdType::String, WasdType::String],
            ret: Box::new(WasdType::Bool),
            effects: vec![],
        },
    );

    // String::trim: (String) -> String
    functions.insert(
        "String_trim".to_string(),
        WasdType::Function {
            params: vec![WasdType::String],
            ret: Box::new(WasdType::String),
            effects: vec![],
        },
    );

    // String::eq: (String, String) -> bool (actually returns i32 from strcmp)
    functions.insert(
        "String_eq".to_string(),
        WasdType::Function {
            params: vec![WasdType::String, WasdType::String],
            ret: Box::new(WasdType::I32),
            effects: vec![],
        },
    );

    // String::cmp: (String, String) -> i32
    functions.insert(
        "String_cmp".to_string(),
        WasdType::Function {
            params: vec![WasdType::String, WasdType::String],
            ret: Box::new(WasdType::I32),
            effects: vec![],
        },
    );

    // String::to_uppercase: (String) -> String
    functions.insert(
        "String_to_uppercase".to_string(),
        WasdType::Function {
            params: vec![WasdType::String],
            ret: Box::new(WasdType::String),
            effects: vec!["Alloc".to_string()],
        },
    );

    // String::to_lowercase: (String) -> String
    functions.insert(
        "String_to_lowercase".to_string(),
        WasdType::Function {
            params: vec![WasdType::String],
            ret: Box::new(WasdType::String),
            effects: vec!["Alloc".to_string()],
        },
    );

    StdModule::with_functions(functions)
}
