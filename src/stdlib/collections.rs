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

    // Vec::get: (Vec[T], i64) -> Option[T]
    functions.insert(
        "Vec_get".to_string(),
        WasdType::Function {
            params: vec![WasdType::Named("Vec".to_string()), WasdType::I64],
            ret: Box::new(WasdType::Named("Option".to_string())),
            effects: vec![],
        },
    );

    // Vec::set: (Vec[T], i64, T) -> ()
    functions.insert(
        "Vec_set".to_string(),
        WasdType::Function {
            params: vec![
                WasdType::Named("Vec".to_string()),
                WasdType::I64,
                WasdType::Unknown, // T
            ],
            ret: Box::new(WasdType::Unit),
            effects: vec![],
        },
    );

    // Vec::first: (Vec[T]) -> Option[T]
    functions.insert(
        "Vec_first".to_string(),
        WasdType::Function {
            params: vec![WasdType::Named("Vec".to_string())],
            ret: Box::new(WasdType::Named("Option".to_string())),
            effects: vec![],
        },
    );

    // Vec::last: (Vec[T]) -> Option[T]
    functions.insert(
        "Vec_last".to_string(),
        WasdType::Function {
            params: vec![WasdType::Named("Vec".to_string())],
            ret: Box::new(WasdType::Named("Option".to_string())),
            effects: vec![],
        },
    );

    // Vec::clear: (Vec[T]) -> ()
    functions.insert(
        "Vec_clear".to_string(),
        WasdType::Function {
            params: vec![WasdType::Named("Vec".to_string())],
            ret: Box::new(WasdType::Unit),
            effects: vec![],
        },
    );

    // Vec::capacity: (Vec[T]) -> i64
    functions.insert(
        "Vec_capacity".to_string(),
        WasdType::Function {
            params: vec![WasdType::Named("Vec".to_string())],
            ret: Box::new(WasdType::I64),
            effects: vec![],
        },
    );

    // Vec::reserve: (Vec[T], i64) -> ()
    functions.insert(
        "Vec_reserve".to_string(),
        WasdType::Function {
            params: vec![WasdType::Named("Vec".to_string()), WasdType::I64],
            ret: Box::new(WasdType::Unit),
            effects: vec![],
        },
    );

    // Vec::insert: (Vec[T], i64, T) -> ()
    functions.insert(
        "Vec_insert".to_string(),
        WasdType::Function {
            params: vec![
                WasdType::Named("Vec".to_string()),
                WasdType::I64,
                WasdType::Unknown, // T
            ],
            ret: Box::new(WasdType::Unit),
            effects: vec![],
        },
    );

    // Vec::remove: (Vec[T], i64) -> T
    functions.insert(
        "Vec_remove".to_string(),
        WasdType::Function {
            params: vec![WasdType::Named("Vec".to_string()), WasdType::I64],
            ret: Box::new(WasdType::Unknown),
            effects: vec![],
        },
    );

    // Vec::swap: (Vec[T], i64, i64) -> ()
    functions.insert(
        "Vec_swap".to_string(),
        WasdType::Function {
            params: vec![
                WasdType::Named("Vec".to_string()),
                WasdType::I64,
                WasdType::I64,
            ],
            ret: Box::new(WasdType::Unit),
            effects: vec![],
        },
    );

    // Vec::reverse: (Vec[T]) -> ()
    functions.insert(
        "Vec_reverse".to_string(),
        WasdType::Function {
            params: vec![WasdType::Named("Vec".to_string())],
            ret: Box::new(WasdType::Unit),
            effects: vec![],
        },
    );

    // Vec::contains: (Vec[T], T) -> bool
    functions.insert(
        "Vec_contains".to_string(),
        WasdType::Function {
            params: vec![WasdType::Named("Vec".to_string()), WasdType::Unknown],
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

    // HashMap::remove: (HashMap[K, V], K) -> Option[V]
    functions.insert(
        "HashMap_remove".to_string(),
        WasdType::Function {
            params: vec![
                WasdType::Named("HashMap".to_string()),
                WasdType::Unknown, // K
            ],
            ret: Box::new(WasdType::Named("Option".to_string())),
            effects: vec![],
        },
    );

    // HashMap::len: (HashMap[K, V]) -> i64
    functions.insert(
        "HashMap_len".to_string(),
        WasdType::Function {
            params: vec![WasdType::Named("HashMap".to_string())],
            ret: Box::new(WasdType::I64),
            effects: vec![],
        },
    );

    // HashMap::is_empty: (HashMap[K, V]) -> bool
    functions.insert(
        "HashMap_is_empty".to_string(),
        WasdType::Function {
            params: vec![WasdType::Named("HashMap".to_string())],
            ret: Box::new(WasdType::Bool),
            effects: vec![],
        },
    );

    // HashMap::clear: (HashMap[K, V]) -> ()
    functions.insert(
        "HashMap_clear".to_string(),
        WasdType::Function {
            params: vec![WasdType::Named("HashMap".to_string())],
            ret: Box::new(WasdType::Unit),
            effects: vec![],
        },
    );

    // HashMap::capacity: (HashMap[K, V]) -> i64
    functions.insert(
        "HashMap_capacity".to_string(),
        WasdType::Function {
            params: vec![WasdType::Named("HashMap".to_string())],
            ret: Box::new(WasdType::I64),
            effects: vec![],
        },
    );

    // HashMap::get_or_default: (HashMap[K, V], K, V) -> V
    functions.insert(
        "HashMap_get_or_default".to_string(),
        WasdType::Function {
            params: vec![
                WasdType::Named("HashMap".to_string()),
                WasdType::Unknown, // K
                WasdType::Unknown, // V (default)
            ],
            ret: Box::new(WasdType::Unknown),
            effects: vec![],
        },
    );

    // HashSet::new: () -> HashSet[T]
    functions.insert(
        "HashSet_new".to_string(),
        WasdType::Function {
            params: vec![],
            ret: Box::new(WasdType::Named("HashSet".to_string())),
            effects: vec!["Alloc".to_string()],
        },
    );

    // HashSet::insert: (HashSet[T], T) -> bool
    functions.insert(
        "HashSet_insert".to_string(),
        WasdType::Function {
            params: vec![
                WasdType::Named("HashSet".to_string()),
                WasdType::Unknown, // T
            ],
            ret: Box::new(WasdType::Bool),
            effects: vec![],
        },
    );

    // HashSet::contains: (HashSet[T], T) -> bool
    functions.insert(
        "HashSet_contains".to_string(),
        WasdType::Function {
            params: vec![
                WasdType::Named("HashSet".to_string()),
                WasdType::Unknown, // T
            ],
            ret: Box::new(WasdType::Bool),
            effects: vec![],
        },
    );

    // HashSet::remove: (HashSet[T], T) -> bool
    functions.insert(
        "HashSet_remove".to_string(),
        WasdType::Function {
            params: vec![
                WasdType::Named("HashSet".to_string()),
                WasdType::Unknown, // T
            ],
            ret: Box::new(WasdType::Bool),
            effects: vec![],
        },
    );

    // HashSet::len: (HashSet[T]) -> i64
    functions.insert(
        "HashSet_len".to_string(),
        WasdType::Function {
            params: vec![WasdType::Named("HashSet".to_string())],
            ret: Box::new(WasdType::I64),
            effects: vec![],
        },
    );

    // HashSet::is_empty: (HashSet[T]) -> bool
    functions.insert(
        "HashSet_is_empty".to_string(),
        WasdType::Function {
            params: vec![WasdType::Named("HashSet".to_string())],
            ret: Box::new(WasdType::Bool),
            effects: vec![],
        },
    );

    // HashSet::clear: (HashSet[T]) -> ()
    functions.insert(
        "HashSet_clear".to_string(),
        WasdType::Function {
            params: vec![WasdType::Named("HashSet".to_string())],
            ret: Box::new(WasdType::Unit),
            effects: vec![],
        },
    );

    StdModule::with_functions(functions)
}
