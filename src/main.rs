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

/// Find the WASD runtime library for linking.
/// Looks in: 1) next to executable, 2) in runtime/ subdir, 3) in standard install paths
fn find_runtime_library() -> Option<String> {
    // Try to find the library relative to the executable
    if let Ok(exe_path) = std::env::current_exe() {
        if let Some(exe_dir) = exe_path.parent() {
            // Check in the same directory as the executable
            let lib_path = exe_dir.join("libwasd_runtime.a");
            if lib_path.exists() {
                return Some(lib_path.to_string_lossy().to_string());
            }

            // Check in runtime/ subdirectory
            let lib_path = exe_dir.join("runtime").join("libwasd_runtime.a");
            if lib_path.exists() {
                return Some(lib_path.to_string_lossy().to_string());
            }

            // Check parent/runtime for development builds
            if let Some(parent) = exe_dir.parent() {
                // target/debug/../runtime = project_root/runtime (via target/../..)
                if let Some(grandparent) = parent.parent() {
                    let lib_path = grandparent.join("runtime").join("libwasd_runtime.a");
                    if lib_path.exists() {
                        return Some(lib_path.to_string_lossy().to_string());
                    }
                }
            }
        }
    }

    // Check relative to current working directory
    let cwd_lib = Path::new("runtime/libwasd_runtime.a");
    if cwd_lib.exists() {
        return Some(cwd_lib.to_string_lossy().to_string());
    }

    None
}

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

    /// Run tests in a WASD project
    Test {
        /// Path to test file or directory (defaults to current directory)
        #[arg(default_value = ".")]
        path: PathBuf,

        /// Filter tests by name
        #[arg(short, long)]
        filter: Option<String>,
    },

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

    // Find the runtime library directory
    let runtime_lib = find_runtime_library();

    let mut linker_args = vec![
        obj_path.to_str().unwrap().to_string(),
        "-o".to_string(),
        exe_path.to_str().unwrap().to_string(),
        "-lm".to_string(), // Link math library for FFI functions like sqrt, pow, sin, cos
    ];

    // Add runtime library if found
    if let Some(lib_path) = runtime_lib {
        linker_args.push(lib_path);
    }

    let status = Command::new("cc")
        .args(&linker_args)
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

/// Run tests in a WASD project
fn run_tests(path: &PathBuf, filter: Option<&str>) -> Result<(), String> {
    use std::time::Instant;
    use walkdir::WalkDir;

    let start = Instant::now();
    let mut test_files = Vec::new();

    // Collect all .wasd files
    if path.is_file() {
        test_files.push(path.clone());
    } else {
        for entry in WalkDir::new(path)
            .follow_links(true)
            .into_iter()
            .filter_map(|e| e.ok())
        {
            let p = entry.path();
            if p.extension().map_or(false, |e| e == "wasd") {
                test_files.push(p.to_path_buf());
            }
        }
    }

    // Parse all files and collect test functions
    let mut tests = Vec::new();

    for file in &test_files {
        let source = fs::read_to_string(file)
            .map_err(|e| format!("Failed to read {}: {}", file.display(), e))?;

        let mut parser = Parser::new(&source);
        let program = parser
            .parse()
            .map_err(|e| format!("Parse error in {}: {}", file.display(), e))?;

        for item in &program.items {
            if let parser::Item::Function(func) = item {
                // Check if function has #[test] attribute
                let is_test = func.attributes.iter().any(|a| a.name == "test");
                let is_ignored = func.attributes.iter().any(|a| a.name == "ignore");

                if is_test {
                    let name = format!("{}::{}", file.display(), func.name);
                    // Apply filter if provided
                    if let Some(f) = filter {
                        if !name.contains(f) && !func.name.contains(f) {
                            continue;
                        }
                    }
                    tests.push((file.clone(), func.name.clone(), is_ignored));
                }
            }
        }
    }

    if tests.is_empty() {
        println!("No tests found.");
        return Ok(());
    }

    println!("\nRunning {} test(s)...\n", tests.len());

    let mut passed = 0;
    let mut failed = 0;
    let mut ignored = 0;

    for (file, test_name, is_ignored) in &tests {
        print!("test {} ... ", test_name);

        if *is_ignored {
            println!("\x1b[33mignored\x1b[0m");
            ignored += 1;
            continue;
        }

        // Compile and run the test
        match compile_and_run_test(file, test_name) {
            Ok(true) => {
                println!("\x1b[32mok\x1b[0m");
                passed += 1;
            }
            Ok(false) => {
                println!("\x1b[31mFAILED\x1b[0m");
                failed += 1;
            }
            Err(e) => {
                println!("\x1b[31mFAILED\x1b[0m - {}", e);
                failed += 1;
            }
        }
    }

    let elapsed = start.elapsed();
    println!();
    println!(
        "test result: {}. {} passed; {} failed; {} ignored; finished in {:.2}s",
        if failed == 0 { "\x1b[32mok\x1b[0m" } else { "\x1b[31mFAILED\x1b[0m" },
        passed,
        failed,
        ignored,
        elapsed.as_secs_f64()
    );

    if failed > 0 {
        Err(format!("{} test(s) failed", failed))
    } else {
        Ok(())
    }
}

/// Compile and run a single test function, returning Ok(true) for pass, Ok(false) for fail
fn compile_and_run_test(file: &PathBuf, test_name: &str) -> Result<bool, String> {
    let source = fs::read_to_string(file)
        .map_err(|e| format!("Failed to read file: {}", e))?;

    // Parse the file
    let mut parser = Parser::new(&source);
    let mut program = parser.parse()?;

    // Find and wrap the test function into a main function that calls it
    let _test_func = program.items.iter().find_map(|item| {
        if let parser::Item::Function(f) = item {
            if f.name == test_name {
                Some(f.clone())
            } else {
                None
            }
        } else {
            None
        }
    }).ok_or_else(|| format!("Test function '{}' not found", test_name))?;

    // Create a synthetic main that calls the test
    // For now, just compile and run the original - test is expected to return 0 on success
    let has_main = program.items.iter().any(|item| {
        matches!(item, parser::Item::Function(f) if f.name == "main")
    });

    if !has_main {
        // Create a main function that calls the test and returns 0 on success
        let main_source = format!(
            "fn main() -> i64\n    {}()\n    0\n",
            test_name
        );

        // Re-parse with a generated main
        let full_source = format!("{}\n{}", source, main_source);
        let mut parser = Parser::new(&full_source);
        program = parser.parse()?;
    }

    // Type check
    let mut type_checker = TypeChecker::new();
    type_checker.check_program(&program).map_err(|errors| {
        errors.join("; ")
    })?;

    // Borrow check
    let mut borrow_checker = BorrowChecker::new();
    borrow_checker.check_program(&program).map_err(|errors| {
        errors.join("; ")
    })?;

    // Lower to IR
    let ir_module = lower_program(&program);

    // Generate code
    let context = Context::create();
    let mut codegen = CodeGen::new(&context, "test");
    codegen.compile(&ir_module)?;

    // Write and run
    let temp_dir = std::env::temp_dir();
    let obj_path = temp_dir.join(format!("wasd_test_{}.o", std::process::id()));
    let exe_path = temp_dir.join(format!("wasd_test_{}", std::process::id()));

    codegen.write_object_file(&obj_path)?;

    // Link
    let runtime_lib = find_runtime_library();
    let mut linker_args = vec![
        obj_path.to_str().unwrap().to_string(),
        "-o".to_string(),
        exe_path.to_str().unwrap().to_string(),
        "-lm".to_string(),
    ];

    if let Some(lib_path) = runtime_lib {
        linker_args.push(lib_path);
    }

    let status = Command::new("cc")
        .args(&linker_args)
        .status()
        .map_err(|e| format!("Failed to run linker: {}", e))?;

    if !status.success() {
        let _ = fs::remove_file(&obj_path);
        return Err("Linking failed".to_string());
    }

    // Run the test
    let output = Command::new(&exe_path)
        .output()
        .map_err(|e| format!("Failed to run test: {}", e))?;

    // Clean up
    let _ = fs::remove_file(&obj_path);
    let _ = fs::remove_file(&exe_path);

    // Exit code 0 = test passed
    Ok(output.status.success())
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
        Commands::Test { path, filter } => run_tests(&path, filter.as_deref()),
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
