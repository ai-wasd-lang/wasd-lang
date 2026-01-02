//! WASD Standard Library definitions.
//!
//! This module defines the standard library functions and types
//! that are available to WASD programs.

#![allow(dead_code)]

pub mod io;
pub mod prelude;

use crate::types::WasdType;
use std::collections::HashMap;

/// A standard library module definition.
#[derive(Debug, Clone)]
pub struct StdModule {
    /// Functions provided by this module
    pub functions: HashMap<String, WasdType>,
}

impl StdModule {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
        }
    }

    pub fn with_functions(functions: HashMap<String, WasdType>) -> Self {
        Self { functions }
    }
}

impl Default for StdModule {
    fn default() -> Self {
        Self::new()
    }
}

/// Get all standard library modules.
pub fn get_stdlib() -> HashMap<String, StdModule> {
    let mut modules = HashMap::new();

    modules.insert("std.io".to_string(), io::module());
    modules.insert("std.prelude".to_string(), prelude::module());

    modules
}

/// Get functions from a specific module path.
/// Supports:
/// - "std.io" -> all functions from io module
/// - "std.io.print" -> just the print function
/// - "std.prelude" -> all prelude functions
pub fn resolve_import(path: &str) -> Option<HashMap<String, WasdType>> {
    let parts: Vec<&str> = path.split('.').collect();

    if parts.is_empty() || parts[0] != "std" {
        return None;
    }

    let stdlib = get_stdlib();

    if parts.len() == 2 {
        // Import entire module: "std.io"
        let module_path = format!("std.{}", parts[1]);
        stdlib.get(&module_path).map(|m| m.functions.clone())
    } else if parts.len() == 3 {
        // Import specific item: "std.io.print"
        let module_path = format!("std.{}", parts[1]);
        let item_name = parts[2];
        stdlib.get(&module_path).and_then(|m| {
            m.functions.get(item_name).map(|ty| {
                let mut result = HashMap::new();
                result.insert(item_name.to_string(), ty.clone());
                result
            })
        })
    } else {
        None
    }
}
