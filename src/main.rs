//! WASD Programming Language Compiler
//!
//! A compiler for the WASD language with explicit semantics,
//! zero-cost abstractions, and memory safety.

#![allow(clippy::module_inception)]

use clap::{Parser as ClapParser, Subcommand};
use inkwell::context::Context;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

mod borrow;
mod codegen;
mod errors;
mod fmt;
mod ir;
mod lexer;
mod lsp;
mod module;
mod parser;
mod pkg;
mod repl;
mod stdlib;
mod types;

use borrow::BorrowChecker;
use codegen::CodeGen;
use errors::{report_error, Diagnostic};
use ir::lower_program;
use module::ModuleLoader;
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

    /// Start an interactive REPL session
    Repl,

    /// Start the Language Server Protocol server
    Lsp,

    /// Create a new WASD project
    New {
        /// Project name
        name: String,
    },

    /// Initialize a WASD project in the current directory
    Init,

    /// Add a dependency to the project
    Add {
        /// Dependency specification (e.g., "http", "http@1.2.3", "github:user/repo@1.0.0")
        dependency: String,
    },

    /// Remove a dependency from the project
    Remove {
        /// Package name to remove
        name: String,
    },

    /// Install all dependencies
    Install,

    /// Update dependencies
    Update {
        /// Specific package to update (updates all if not specified)
        package: Option<String>,
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

    // Phase 1.5: Resolve module imports
    let base_dir = file.parent().unwrap_or(Path::new(".")).to_path_buf();
    let mut module_loader = ModuleLoader::new(base_dir);
    let program = module_loader.resolve_imports(&program)?;

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
        .args([
            obj_path.to_str().unwrap(),
            "-o",
            exe_path.to_str().unwrap(),
            "-lm", // Link math library for FFI functions like sqrt, pow, sin, cos
        ])
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

fn format_file(file: &PathBuf, check_only: bool) -> Result<(), String> {
    let source = fs::read_to_string(file).map_err(|e| format!("Failed to read file: {}", e))?;

    let formatted = fmt::format_source(&source)?;

    if check_only {
        if source == formatted {
            println!("File is properly formatted: {:?}", file);
            Ok(())
        } else {
            Err(format!("File needs formatting: {:?}", file))
        }
    } else {
        fs::write(file, &formatted).map_err(|e| format!("Failed to write file: {}", e))?;
        println!("Formatted: {:?}", file);
        Ok(())
    }
}

/// Find the wasd.toml manifest in the current or parent directories
fn find_manifest() -> Result<PathBuf, String> {
    let mut current = std::env::current_dir().map_err(|e| e.to_string())?;

    loop {
        let manifest = current.join("wasd.toml");
        if manifest.exists() {
            return Ok(manifest);
        }

        if !current.pop() {
            return Err("Could not find wasd.toml in current directory or any parent".to_string());
        }
    }
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
        Commands::Fmt { file, check } => format_file(&file, check),
        Commands::Repl => {
            let mut repl = repl::Repl::new();
            repl.run()
        }
        Commands::Lsp => {
            tokio::runtime::Runtime::new()
                .expect("Failed to create Tokio runtime")
                .block_on(lsp::run_server());
            Ok(())
        }
        Commands::New { name } => {
            std::env::current_dir()
                .map_err(|e| e.to_string())
                .and_then(|dir| {
                    pkg::new_project(&name, &dir)
                        .map_err(|e| e.to_string())
                        .map(|_| println!("Created new project: {}", name))
                })
        }
        Commands::Init => {
            std::env::current_dir()
                .map_err(|e| e.to_string())
                .and_then(|dir| {
                    pkg::init_project(&dir, None)
                        .map_err(|e| e.to_string())
                        .map(|_| println!("Initialized project in current directory"))
                })
        }
        Commands::Add { dependency } => {
            find_manifest().and_then(|m| {
                pkg::add_dependency(&m, &dependency).map_err(|e| e.to_string())
            })
        }
        Commands::Remove { name } => {
            find_manifest().and_then(|m| {
                pkg::remove_dependency(&m, &name).map_err(|e| e.to_string())
            })
        }
        Commands::Install => {
            find_manifest().and_then(|m| {
                pkg::install_dependencies(&m).map_err(|e| e.to_string())
            })
        }
        Commands::Update { package } => {
            find_manifest().and_then(|m| {
                pkg::update_dependencies(&m, package.as_deref()).map_err(|e| e.to_string())
            })
        }
    };

    if let Err(e) = result {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}
