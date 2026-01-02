//! Standard library collections (std.collections)
//!
//! Provides Vec, HashMap, HashSet, and other collection types.

use crate::types::WasdType;
use std::collections::HashMap;

use super::StdModule;

/// Get the std.collections module definition.
///
/// This module provides collection types and their methods.
pub fn module() -> StdModule {
    let mut functions = HashMap::new();

    // Vec::new: () -> Vec[T]
    functions.insert(
        "Vec_new".to_string(),
        WasdType::Function {
            params: vec![],
            ret: Box::new(WasdType::Named("Vec".to_string())),
            effects: vec!["Alloc".to_string()],
        },
    );

    // Vec::push: (Vec[T], T) -> ()
    functions.insert(
        "Vec_push".to_string(),
        WasdType::Function {
            params: vec![
                WasdType::Named("Vec".to_string()),
                WasdType::Unknown, // T
            ],
            ret: Box::new(WasdType::Unit),
            effects: vec![],
        },
    );

    // Vec::pop: (Vec[T]) -> Option[T]
    functions.insert(
        "Vec_pop".to_string(),
        WasdType::Function {
            params: vec![WasdType::Named("Vec".to_string())],
            ret: Box::new(WasdType::Named("Option".to_string())),
            effects: vec![],
        },
    );

    // Vec::len: (Vec[T]) -> i64
    functions.insert(
        "Vec_len".to_string(),
        WasdType::Function {
            params: vec![WasdType::Named("Vec".to_string())],
            ret: Box::new(WasdType::I64),
            effects: vec![],
        },
    );

    // Vec::is_empty: (Vec[T]) -> bool
    functions.insert(
        "Vec_is_empty".to_string(),
        WasdType::Function {
            params: vec![WasdType::Named("Vec".to_string())],
            ret: Box::new(WasdType::Bool),
            effects: vec![],
        },
    );

    // HashMap::new: () -> HashMap[K, V]
    functions.insert(
        "HashMap_new".to_string(),
        WasdType::Function {
            params: vec![],
            ret: Box::new(WasdType::Named("HashMap".to_string())),
            effects: vec!["Alloc".to_string()],
        },
    );

    // HashMap::insert: (HashMap[K, V], K, V) -> Option[V]
    functions.insert(
        "HashMap_insert".to_string(),
        WasdType::Function {
            params: vec![
                WasdType::Named("HashMap".to_string()),
                WasdType::Unknown, // K
                WasdType::Unknown, // V
            ],
            ret: Box::new(WasdType::Named("Option".to_string())),
            effects: vec![],
        },
    );

    // HashMap::get: (HashMap[K, V], K) -> Option[V]
    functions.insert(
        "HashMap_get".to_string(),
        WasdType::Function {
            params: vec![
                WasdType::Named("HashMap".to_string()),
                WasdType::Unknown, // K
            ],
            ret: Box::new(WasdType::Named("Option".to_string())),
            effects: vec![],
        },
    );

    // HashMap::contains_key: (HashMap[K, V], K) -> bool
    functions.insert(
        "HashMap_contains_key".to_string(),
        WasdType::Function {
            params: vec![
                WasdType::Named("HashMap".to_string()),
                WasdType::Unknown, // K
            ],
            ret: Box::new(WasdType::Bool),
            effects: vec![],
        },
    );

    StdModule::with_functions(functions)
}
