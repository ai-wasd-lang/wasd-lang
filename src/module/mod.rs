//! Module system for WASD.
//!
//! This module handles loading and resolving imports from:
//! - Standard library (std.*)
//! - Local files (relative paths)
//! - Package dependencies (when implemented)

use crate::parser::{Item, Parser, Program};
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

/// Resolved module with its AST items.
#[derive(Debug, Clone)]
pub struct ResolvedModule {
    /// The path this module was loaded from
    #[allow(dead_code)]
    pub path: PathBuf,
    /// The items in this module
    pub items: Vec<Item>,
}

/// Module loader that resolves and loads imported modules.
pub struct ModuleLoader {
    /// Base directory for resolving relative imports
    base_dir: PathBuf,
    /// Cache of already loaded modules
    loaded: HashMap<PathBuf, ResolvedModule>,
}

impl ModuleLoader {
    /// Create a new module loader with the given base directory.
    pub fn new(base_dir: PathBuf) -> Self {
        Self {
            base_dir,
            loaded: HashMap::new(),
        }
    }

    /// Load a module from a path (relative to base_dir).
    pub fn load(&mut self, import_path: &[String]) -> Result<&ResolvedModule, String> {
        // Convert import path to file path
        let mut file_path = self.base_dir.clone();
        for part in import_path {
            file_path.push(part);
        }
        file_path.set_extension("wasd");

        // Check if already loaded
        if self.loaded.contains_key(&file_path) {
            return Ok(self.loaded.get(&file_path).unwrap());
        }

        // Load and parse the file
        let source = fs::read_to_string(&file_path)
            .map_err(|e| format!("Failed to load module {:?}: {}", file_path, e))?;

        let mut parser = Parser::new(&source);
        let program = parser.parse()
            .map_err(|e| format!("Failed to parse module {:?}: {}", file_path, e))?;

        let module = ResolvedModule {
            path: file_path.clone(),
            items: program.items,
        };

        self.loaded.insert(file_path.clone(), module);
        Ok(self.loaded.get(&file_path).unwrap())
    }

    /// Resolve all imports in a program and return a combined program.
    /// This includes all the imported items that are public.
    pub fn resolve_imports(&mut self, program: &Program) -> Result<Program, String> {
        let mut all_items = Vec::new();

        // Process use statements
        for item in &program.items {
            if let Item::Use(use_stmt) = item {
                // Skip stdlib imports (handled separately by type checker)
                if use_stmt.path.first().map(|s| s.as_str()) == Some("std") {
                    all_items.push(item.clone());
                    continue;
                }

                // Load the module
                let module = self.load(&use_stmt.path)?;

                // Add public items from the module
                for mod_item in &module.items {
                    if is_public(mod_item) {
                        all_items.push(mod_item.clone());
                    }
                }
            }
        }

        // Add the original items
        all_items.extend(program.items.iter().cloned());

        Ok(Program { items: all_items })
    }
}

/// Check if an item is public.
fn is_public(item: &Item) -> bool {
    use crate::parser::Visibility;
    match item {
        Item::Function(f) => f.visibility == Visibility::Public,
        Item::Struct(s) => s.visibility == Visibility::Public,
        Item::Enum(e) => e.visibility == Visibility::Public,
        Item::Trait(_) => true, // Traits are always public for now
        Item::Impl(_) => true,  // Impls are always public
        Item::Use(_) => false,  // Don't re-export use statements
    }
}

/// Resolve the import path to a file path.
#[allow(dead_code)]
pub fn resolve_import_path(base_dir: &Path, import_path: &[String]) -> PathBuf {
    let mut file_path = base_dir.to_path_buf();
    for part in import_path {
        file_path.push(part);
    }
    file_path.set_extension("wasd");
    file_path
}
