//! Borrow checker implementation for WASD.

use crate::parser::{Expr, Function, Item, Program, Stmt, UnaryOp};
use std::collections::{HashMap, HashSet};

/// Tracks the state of a value for borrow checking.
#[derive(Debug, Clone, PartialEq)]
pub enum ValueState {
    /// Value is owned and usable
    Owned,
    /// Value has been moved
    Moved,
    /// Value is borrowed immutably (count of borrows)
    Borrowed(usize),
    /// Value is borrowed mutably
    BorrowedMut,
}

/// The WASD borrow checker.
pub struct BorrowChecker {
    /// Maps variable names to their borrow state
    states: HashMap<String, ValueState>,
    /// Active borrows: maps borrow name to borrowed variable
    borrows: HashMap<String, String>,
    /// Mutable borrows currently active
    mut_borrows: HashSet<String>,
}

impl BorrowChecker {
    /// Create a new borrow checker.
    pub fn new() -> Self {
        Self {
            states: HashMap::new(),
            borrows: HashMap::new(),
            mut_borrows: HashSet::new(),
        }
    }

    /// Check a complete program for borrow violations.
    pub fn check_program(&mut self, program: &Program) -> Result<(), Vec<String>> {
        let mut errors = Vec::new();

        for item in &program.items {
            match item {
                Item::Use(_) => {} // No borrow checking needed for imports
                Item::Function(f) => {
                    if let Err(mut errs) = self.check_function(f) {
                        errors.append(&mut errs);
                    }
                }
                Item::Struct(_) | Item::Enum(_) => {} // Types don't need borrow checking
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    fn check_function(&mut self, func: &Function) -> Result<(), Vec<String>> {
        // Reset state for new function
        self.states.clear();
        self.borrows.clear();
        self.mut_borrows.clear();

        // Parameters are initially owned
        for param in &func.params {
            self.states.insert(param.name.clone(), ValueState::Owned);
        }

        let mut errors = Vec::new();
        for stmt in &func.body {
            if let Err(e) = self.check_stmt(stmt) {
                errors.push(e);
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    fn check_stmt(&mut self, stmt: &Stmt) -> Result<(), String> {
        match stmt {
            Stmt::Let { name, value, .. } => {
                self.check_expr(value)?;
                self.states.insert(name.clone(), ValueState::Owned);
                Ok(())
            }
            Stmt::Expr(e) => self.check_expr(e),
            Stmt::Return(Some(e), _) => self.check_expr(e),
            Stmt::Return(None, _) => Ok(()),
        }
    }

    fn check_expr(&mut self, expr: &Expr) -> Result<(), String> {
        match expr {
            Expr::Ident(name, _) => self.check_use(name),
            Expr::Binary(left, _, right, _) => {
                self.check_expr(left)?;
                self.check_expr(right)
            }
            Expr::Unary(op, inner, _) => {
                match op {
                    UnaryOp::Ref => {
                        if let Expr::Ident(name, _) = inner.as_ref() {
                            self.add_borrow(name, false)?;
                        }
                    }
                    UnaryOp::RefMut => {
                        if let Expr::Ident(name, _) = inner.as_ref() {
                            self.add_borrow(name, true)?;
                        }
                    }
                    _ => {}
                }
                self.check_expr(inner)
            }
            Expr::Call(callee, args, _) => {
                self.check_expr(callee)?;
                for arg in args {
                    self.check_expr(arg)?;
                }
                Ok(())
            }
            Expr::FieldAccess(base, _, _) => self.check_expr(base),
            Expr::If(cond, then_branch, else_branch, _) => {
                self.check_expr(cond)?;
                for stmt in then_branch {
                    self.check_stmt(stmt)?;
                }
                if let Some(else_stmts) = else_branch {
                    for stmt in else_stmts {
                        self.check_stmt(stmt)?;
                    }
                }
                Ok(())
            }
            Expr::Match(value, arms, _) => {
                self.check_expr(value)?;
                for arm in arms {
                    self.check_expr(&arm.body)?;
                }
                Ok(())
            }
            Expr::Block(stmts, _) => {
                for stmt in stmts {
                    self.check_stmt(stmt)?;
                }
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn check_use(&self, name: &str) -> Result<(), String> {
        match self.states.get(name) {
            Some(ValueState::Moved) => Err(format!("Use of moved value: {}", name)),
            Some(ValueState::BorrowedMut) => {
                Err(format!("Cannot use {} while mutably borrowed", name))
            }
            None => Ok(()), // Might be a function or external
            _ => Ok(()),
        }
    }

    fn add_borrow(&mut self, name: &str, is_mut: bool) -> Result<(), String> {
        let state = self.states.get(name);

        match state {
            Some(ValueState::Moved) => {
                return Err(format!("Cannot borrow moved value: {}", name));
            }
            Some(ValueState::BorrowedMut) => {
                return Err(format!(
                    "Cannot borrow {} while already mutably borrowed",
                    name
                ));
            }
            Some(ValueState::Borrowed(_)) if is_mut => {
                return Err(format!(
                    "Cannot mutably borrow {} while immutably borrowed",
                    name
                ));
            }
            _ => {}
        }

        if is_mut {
            self.states
                .insert(name.to_string(), ValueState::BorrowedMut);
            self.mut_borrows.insert(name.to_string());
        } else {
            let count = match self.states.get(name) {
                Some(ValueState::Borrowed(n)) => n + 1,
                _ => 1,
            };
            self.states
                .insert(name.to_string(), ValueState::Borrowed(count));
        }

        Ok(())
    }

    #[allow(dead_code)]
    fn mark_moved(&mut self, name: &str) -> Result<(), String> {
        match self.states.get(name) {
            Some(ValueState::Moved) => Err(format!("Value already moved: {}", name)),
            Some(ValueState::Borrowed(_)) | Some(ValueState::BorrowedMut) => {
                Err(format!("Cannot move borrowed value: {}", name))
            }
            _ => {
                self.states.insert(name.to_string(), ValueState::Moved);
                Ok(())
            }
        }
    }
}

impl Default for BorrowChecker {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;

    fn check(source: &str) -> Result<(), Vec<String>> {
        let mut parser = Parser::new(source);
        let program = parser.parse().expect("Parse error");
        let mut checker = BorrowChecker::new();
        checker.check_program(&program)
    }

    #[test]
    fn test_simple_ownership() {
        let source = r#"fn main()
    let x = 42
    x
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_multiple_uses() {
        let source = r#"fn main()
    let x = 42
    x
    x
    x
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_parameter_ownership() {
        let source = r#"fn use_val(x: i64) -> i64
    x
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_function_call() {
        let source = r#"fn id(x: i64) -> i64
    x

fn main() -> i64
    let y = 42
    id(y)
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_binary_expression() {
        let source = r#"fn main() -> i64
    let a = 1
    let b = 2
    a + b
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_reassignment() {
        let source = r#"fn main()
    let x = 1
    let y = x
    y
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_if_expression() {
        let source = r#"fn main() -> i64
    let x = 1
    if true
        x
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_return_value() {
        let source = r#"fn get_val() -> i64
    let x = 42
    return x
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_chained_calls() {
        let source = r#"fn add(a: i64, b: i64) -> i64
    a + b

fn main() -> i64
    add(add(1, 2), add(3, 4))
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_nested_let() {
        let source = r#"fn main()
    let x = 1
    let y = 2
    let z = x + y
    z
"#;
        assert!(check(source).is_ok());
    }
}
