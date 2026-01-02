//! WASD Programming Language Compiler
//!
//! A compiler for the WASD language with explicit semantics,
//! zero-cost abstractions, and memory safety.

#![allow(clippy::module_inception)]

use clap::{Parser as ClapParser, Subcommand};
use inkwell::context::Context;
use std::fs;
use std::path::PathBuf;
use std::process::Command;

mod borrow;
mod codegen;
mod errors;
mod ir;
mod lexer;
mod parser;
mod stdlib;
mod types;

use borrow::BorrowChecker;
use codegen::CodeGen;
use errors::{report_error, Diagnostic};
use ir::lower_program;
use parser::Parser;
use types::TypeChecker;

#[derive(ClapParser)]
#[command(name = "wasd")]
#[command(author, version, about = "The WASD programming language compiler", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Compile a WASD source file to a binary
    Build {
        /// Input file path
        file: PathBuf,

        /// Output file path
        #[arg(short, long)]
        output: Option<PathBuf>,
    },

    /// Compile and run a WASD source file
    Run {
        /// Input file path
        file: PathBuf,
    },

    /// Type check a WASD source file without compiling
    Check {
        /// Input file path
        file: PathBuf,
    },

    /// Emit LLVM IR for a WASD source file
    EmitIr {
        /// Input file path
        file: PathBuf,

        /// Output file path
        #[arg(short, long)]
        output: Option<PathBuf>,
    },

    /// Format a WASD source file
    Fmt {
        /// Input file path
        file: PathBuf,

        /// Check formatting without modifying the file
        #[arg(long)]
        check: bool,
    },
}

fn compile(file: &PathBuf, output: Option<&PathBuf>) -> Result<PathBuf, String> {
    let filename = file.to_string_lossy();
    let source = fs::read_to_string(file).map_err(|e| format!("Failed to read file: {}", e))?;

    // Phase 1: Lexing and Parsing
    let mut parser = Parser::new(&source);
    let program = parser.parse().inspect_err(|e| {
        report_error(&filename, &source, &Diagnostic::error(e, (0, 1)));
    })?;

    // Phase 2: Type Checking
    let mut type_checker = TypeChecker::new();
    if let Err(errors) = type_checker.check_program(&program) {
        for err in &errors {
            report_error(&filename, &source, &Diagnostic::error(err, (0, 1)));
        }
        return Err(format!("{} type errors found", errors.len()));
    }

    // Phase 3: Borrow Checking
    let mut borrow_checker = BorrowChecker::new();
    if let Err(errors) = borrow_checker.check_program(&program) {
        for err in &errors {
            report_error(&filename, &source, &Diagnostic::error(err, (0, 1)));
        }
        return Err(format!("{} borrow errors found", errors.len()));
    }

    // Phase 4: Lower to IR
    let ir_module = lower_program(&program);

    // Phase 5: Code Generation
    let context = Context::create();
    let module_name = file
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("module");

    let mut codegen = CodeGen::new(&context, module_name);
    codegen.compile(&ir_module)?;

    // Write object file
    let obj_path = file.with_extension("o");
    codegen.write_object_file(&obj_path)?;

    // Link to executable
    let exe_path = output.cloned().unwrap_or_else(|| file.with_extension(""));

    let status = Command::new("cc")
        .args([obj_path.to_str().unwrap(), "-o", exe_path.to_str().unwrap()])
        .status()
        .map_err(|e| format!("Failed to run linker: {}", e))?;

    if !status.success() {
        return Err("Linking failed".to_string());
    }

    // Clean up object file
    let _ = fs::remove_file(&obj_path);

    Ok(exe_path)
}

fn check(file: &PathBuf) -> Result<(), String> {
    let filename = file.to_string_lossy();
    let source = fs::read_to_string(file).map_err(|e| format!("Failed to read file: {}", e))?;

    // Parse
    let mut parser = Parser::new(&source);
    let program = parser.parse().inspect_err(|e| {
        report_error(&filename, &source, &Diagnostic::error(e, (0, 1)));
    })?;

    // Type check
    let mut type_checker = TypeChecker::new();
    if let Err(errors) = type_checker.check_program(&program) {
        for err in &errors {
            report_error(&filename, &source, &Diagnostic::error(err, (0, 1)));
        }
        return Err(format!("{} type errors found", errors.len()));
    }

    // Borrow check
    let mut borrow_checker = BorrowChecker::new();
    if let Err(errors) = borrow_checker.check_program(&program) {
        for err in &errors {
            report_error(&filename, &source, &Diagnostic::error(err, (0, 1)));
        }
        return Err(format!("{} borrow errors found", errors.len()));
    }

    println!("No errors found.");
    Ok(())
}

fn emit_ir(file: &PathBuf, output: Option<&PathBuf>) -> Result<(), String> {
    let filename = file.to_string_lossy();
    let source = fs::read_to_string(file).map_err(|e| format!("Failed to read file: {}", e))?;

    // Parse
    let mut parser = Parser::new(&source);
    let program = parser.parse().inspect_err(|e| {
        report_error(&filename, &source, &Diagnostic::error(e, (0, 1)));
    })?;

    // Type check
    let mut type_checker = TypeChecker::new();
    if let Err(errors) = type_checker.check_program(&program) {
        for err in &errors {
            report_error(&filename, &source, &Diagnostic::error(err, (0, 1)));
        }
        return Err(format!("{} type errors found", errors.len()));
    }

    // Lower to IR
    let ir_module = lower_program(&program);

    // Generate LLVM IR
    let context = Context::create();
    let module_name = file
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("module");

    let mut codegen = CodeGen::new(&context, module_name);
    codegen.compile(&ir_module)?;

    let ir_string = codegen.get_ir_string();

    if let Some(out_path) = output {
        fs::write(out_path, &ir_string).map_err(|e| format!("Failed to write IR: {}", e))?;
        println!("LLVM IR written to {:?}", out_path);
    } else {
        println!("{}", ir_string);
    }

    Ok(())
}

fn main() {
    let cli = Cli::parse();

    let result = match cli.command {
        Commands::Build { file, output } => match compile(&file, output.as_ref()) {
            Ok(exe_path) => {
                println!("Compiled successfully: {:?}", exe_path);
                Ok(())
            }
            Err(e) => Err(e),
        },
        Commands::Run { file } => match compile(&file, None) {
            Ok(exe_path) => {
                // Use absolute path or prefix with ./ to ensure the executable is found
                let exe_path = if exe_path.is_relative() && !exe_path.starts_with(".") {
                    PathBuf::from(".").join(&exe_path)
                } else {
                    exe_path
                };
                match Command::new(&exe_path).status() {
                    Ok(status) => {
                        // Non-zero exit codes are expected for programs that return values
                        // Only report if the program was killed by a signal
                        #[cfg(unix)]
                        {
                            use std::os::unix::process::ExitStatusExt;
                            if status.signal().is_some() {
                                Err(format!("Program killed by signal: {:?}", status.signal()))
                            } else {
                                Ok(())
                            }
                        }
                        #[cfg(not(unix))]
                        Ok(())
                    }
                    Err(e) => Err(format!("Failed to run: {}", e)),
                }
            }
            Err(e) => Err(e),
        },
        Commands::Check { file } => check(&file),
        Commands::EmitIr { file, output } => emit_ir(&file, output.as_ref()),
        Commands::Fmt { file, check: _ } => {
            // TODO: Implement formatter
            println!("Formatter not yet implemented for {:?}", file);
            Ok(())
        }
    };

    if let Err(e) = result {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}
