//! Simple AST interpreter for REPL evaluation.
//!
//! Evaluates WASD expressions directly without compilation.

use crate::parser::{BinOp, Expr, Program, Stmt, UnaryOp};
use std::collections::HashMap;
use std::fmt;

/// A runtime value in the interpreter.
#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Unit,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{}", n),
            Value::Float(n) => write!(f, "{}", n),
            Value::Bool(b) => write!(f, "{}", b),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Unit => write!(f, "()"),
        }
    }
}

/// The interpreter holds variable bindings.
pub struct Interpreter {
    env: HashMap<String, Value>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            env: HashMap::new(),
        }
    }

    /// Evaluate a program and return the result of the last expression.
    pub fn eval_program(&mut self, program: &Program) -> Result<Value, String> {
        let mut result = Value::Unit;

        for item in &program.items {
            match item {
                crate::parser::Item::Function(func) => {
                    // Only evaluate the __repl__ function
                    if func.name == "__repl__" {
                        for stmt in &func.body {
                            result = self.eval_stmt(stmt)?;
                        }
                    }
                }
                _ => {} // Ignore other items in REPL
            }
        }

        Ok(result)
    }

    fn eval_stmt(&mut self, stmt: &Stmt) -> Result<Value, String> {
        match stmt {
            Stmt::Let { name, value, .. } => {
                let val = self.eval_expr(value)?;
                self.env.insert(name.clone(), val.clone());
                Ok(val)
            }
            Stmt::Assign { target, value, .. } => {
                if let Expr::Ident(name, _) = target {
                    let val = self.eval_expr(value)?;
                    self.env.insert(name.clone(), val);
                    Ok(Value::Unit)
                } else {
                    Err("Can only assign to variables".to_string())
                }
            }
            Stmt::Expr(expr) => self.eval_expr(expr),
            Stmt::Return(Some(expr), _) => self.eval_expr(expr),
            Stmt::Return(None, _) => Ok(Value::Unit),
            Stmt::While { condition, body, .. } => {
                while self.eval_expr(condition)?.as_bool()? {
                    for s in body {
                        self.eval_stmt(s)?;
                    }
                }
                Ok(Value::Unit)
            }
            Stmt::For { var, iterable, body, .. } => {
                // Simple range-based for loop
                if let Expr::Range { start, end, .. } = iterable {
                    let start = self.eval_expr(start)?.as_int()?;
                    let end = self.eval_expr(end)?.as_int()?;
                    for i in start..end {
                        self.env.insert(var.clone(), Value::Int(i));
                        for s in body {
                            self.eval_stmt(s)?;
                        }
                    }
                }
                Ok(Value::Unit)
            }
            Stmt::Break(_) | Stmt::Continue(_) => Ok(Value::Unit),
        }
    }

    fn eval_expr(&mut self, expr: &Expr) -> Result<Value, String> {
        match expr {
            Expr::Int(n, _) => Ok(Value::Int(*n)),
            Expr::Float(n, _) => Ok(Value::Float(*n)),
            Expr::Bool(b, _) => Ok(Value::Bool(*b)),
            Expr::String(s, _) => Ok(Value::String(s.clone())),
            Expr::Ident(name, _) => self
                .env
                .get(name)
                .cloned()
                .ok_or_else(|| format!("Undefined variable: {}", name)),
            Expr::Binary(left, op, right, _) => {
                let l = self.eval_expr(left)?;
                let r = self.eval_expr(right)?;
                self.eval_binary_op(*op, l, r)
            }
            Expr::Unary(op, inner, _) => {
                let val = self.eval_expr(inner)?;
                self.eval_unary_op(*op, val)
            }
            Expr::If(cond, then_branch, else_branch, _) => {
                if self.eval_expr(cond)?.as_bool()? {
                    let mut result = Value::Unit;
                    for stmt in then_branch {
                        result = self.eval_stmt(stmt)?;
                    }
                    Ok(result)
                } else if let Some(else_stmts) = else_branch {
                    let mut result = Value::Unit;
                    for stmt in else_stmts {
                        result = self.eval_stmt(stmt)?;
                    }
                    Ok(result)
                } else {
                    Ok(Value::Unit)
                }
            }
            Expr::Block(stmts, _) => {
                let mut result = Value::Unit;
                for stmt in stmts {
                    result = self.eval_stmt(stmt)?;
                }
                Ok(result)
            }
            Expr::Try(inner, _) => {
                // For REPL, ? is a no-op (just evaluate inner)
                self.eval_expr(inner)
            }
            Expr::Await(inner, _) => {
                // For REPL, await is a no-op (just evaluate inner)
                self.eval_expr(inner)
            }
            _ => Err("Expression not supported in REPL".to_string()),
        }
    }

    fn eval_binary_op(&self, op: BinOp, left: Value, right: Value) -> Result<Value, String> {
        match (left, right) {
            (Value::Int(l), Value::Int(r)) => match op {
                BinOp::Add => Ok(Value::Int(l + r)),
                BinOp::Sub => Ok(Value::Int(l - r)),
                BinOp::Mul => Ok(Value::Int(l * r)),
                BinOp::Div => Ok(Value::Int(l / r)),
                BinOp::Mod => Ok(Value::Int(l % r)),
                BinOp::Eq => Ok(Value::Bool(l == r)),
                BinOp::NotEq => Ok(Value::Bool(l != r)),
                BinOp::Lt => Ok(Value::Bool(l < r)),
                BinOp::LtEq => Ok(Value::Bool(l <= r)),
                BinOp::Gt => Ok(Value::Bool(l > r)),
                BinOp::GtEq => Ok(Value::Bool(l >= r)),
                _ => Err("Invalid operation".to_string()),
            },
            (Value::Float(l), Value::Float(r)) => match op {
                BinOp::Add => Ok(Value::Float(l + r)),
                BinOp::Sub => Ok(Value::Float(l - r)),
                BinOp::Mul => Ok(Value::Float(l * r)),
                BinOp::Div => Ok(Value::Float(l / r)),
                BinOp::Eq => Ok(Value::Bool(l == r)),
                BinOp::Lt => Ok(Value::Bool(l < r)),
                _ => Err("Invalid operation".to_string()),
            },
            (Value::Bool(l), Value::Bool(r)) => match op {
                BinOp::And => Ok(Value::Bool(l && r)),
                BinOp::Or => Ok(Value::Bool(l || r)),
                BinOp::Eq => Ok(Value::Bool(l == r)),
                BinOp::NotEq => Ok(Value::Bool(l != r)),
                _ => Err("Invalid operation on booleans".to_string()),
            },
            _ => Err("Type mismatch in binary operation".to_string()),
        }
    }

    fn eval_unary_op(&self, op: UnaryOp, val: Value) -> Result<Value, String> {
        match (op, val) {
            (UnaryOp::Neg, Value::Int(n)) => Ok(Value::Int(-n)),
            (UnaryOp::Neg, Value::Float(n)) => Ok(Value::Float(-n)),
            (UnaryOp::Not, Value::Bool(b)) => Ok(Value::Bool(!b)),
            _ => Err("Invalid unary operation".to_string()),
        }
    }
}

impl Value {
    fn as_bool(&self) -> Result<bool, String> {
        match self {
            Value::Bool(b) => Ok(*b),
            _ => Err("Expected boolean".to_string()),
        }
    }

    fn as_int(&self) -> Result<i64, String> {
        match self {
            Value::Int(n) => Ok(*n),
            _ => Err("Expected integer".to_string()),
        }
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}
