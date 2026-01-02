//! WASD REPL (Read-Eval-Print Loop)
//!
//! Provides an interactive environment for evaluating WASD expressions.

mod interpreter;

use crate::parser::Parser;
use crate::types::TypeChecker;
use interpreter::Interpreter;
use std::io::{self, Write};

/// The REPL state holds persistent type environment and interpreter state.
pub struct Repl {
    type_checker: TypeChecker,
    interpreter: Interpreter,
}

impl Repl {
    pub fn new() -> Self {
        Self {
            type_checker: TypeChecker::new(),
            interpreter: Interpreter::new(),
        }
    }

    /// Run the REPL loop.
    pub fn run(&mut self) -> Result<(), String> {
        println!("WASD REPL v0.1.0");
        println!("Type expressions to evaluate, or :help for commands");
        println!();

        loop {
            print!("wasd> ");
            io::stdout().flush().unwrap();

            let mut input = String::new();
            match io::stdin().read_line(&mut input) {
                Ok(0) => break, // EOF
                Ok(_) => {}
                Err(e) => {
                    eprintln!("Error reading input: {}", e);
                    continue;
                }
            }

            let input = input.trim();
            if input.is_empty() {
                continue;
            }

            // Handle REPL commands
            if input.starts_with(':') {
                if self.handle_command(input) {
                    break;
                }
                continue;
            }

            // Try to evaluate as expression
            self.eval_input(input);
        }

        println!("\nGoodbye!");
        Ok(())
    }

    /// Handle REPL commands (starting with :)
    fn handle_command(&self, cmd: &str) -> bool {
        match cmd {
            ":quit" | ":q" | ":exit" => {
                return true; // Signal to exit
            }
            ":help" | ":h" | ":?" => {
                println!("WASD REPL Commands:");
                println!("  :help, :h, :?  - Show this help");
                println!("  :quit, :q      - Exit the REPL");
                println!("  :type <expr>   - Show the type of an expression");
                println!();
                println!("Examples:");
                println!("  wasd> 2 + 3");
                println!("  => 5");
                println!("  wasd> let x = 42");
                println!("  wasd> x * 2");
                println!("  => 84");
            }
            _ if cmd.starts_with(":type ") => {
                let expr = &cmd[6..];
                self.show_type(expr);
            }
            _ => {
                println!("Unknown command: {}", cmd);
                println!("Type :help for available commands");
            }
        }
        false
    }

    /// Evaluate input and print result.
    fn eval_input(&mut self, input: &str) {
        // Parse as expression
        let wrapped = format!("fn __repl__() -> i64\n    {}", input);

        let mut parser = Parser::new(&wrapped);
        match parser.parse() {
            Ok(program) => {
                // Type check
                if let Err(errors) = self.type_checker.check_program(&program) {
                    for err in errors {
                        println!("Type error: {}", err);
                    }
                    return;
                }

                // Interpret the expression
                match self.interpreter.eval_program(&program) {
                    Ok(value) => {
                        println!("=> {}", value);
                    }
                    Err(e) => {
                        println!("Runtime error: {}", e);
                    }
                }
            }
            Err(e) => {
                println!("Parse error: {}", e);
            }
        }
    }

    /// Show the type of an expression.
    fn show_type(&self, expr: &str) {
        let wrapped = format!("fn __repl__() -> i64\n    {}", expr);

        let mut parser = Parser::new(&wrapped);
        match parser.parse() {
            Ok(program) => {
                let mut checker = TypeChecker::new();
                match checker.check_program(&program) {
                    Ok(()) => {
                        // For now, just confirm it type checks
                        println!("Expression type checks successfully");
                    }
                    Err(errors) => {
                        for err in errors {
                            println!("Type error: {}", err);
                        }
                    }
                }
            }
            Err(e) => {
                println!("Parse error: {}", e);
            }
        }
    }
}

impl Default for Repl {
    fn default() -> Self {
        Self::new()
    }
}
