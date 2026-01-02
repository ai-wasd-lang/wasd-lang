//! WASD Programming Language Compiler
//!
//! A compiler for the WASD language with explicit semantics,
//! zero-cost abstractions, and memory safety.

use clap::{Parser, Subcommand};
use std::path::PathBuf;

mod borrow;
mod codegen;
mod errors;
mod ir;
mod lexer;
mod parser;
mod types;

#[derive(Parser)]
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

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Commands::Build { file, output } => {
            println!("Building {:?} -> {:?}", file, output);
            // TODO: Implement build
        }
        Commands::Run { file } => {
            println!("Running {:?}", file);
            // TODO: Implement run
        }
        Commands::Check { file } => {
            println!("Checking {:?}", file);
            // TODO: Implement check
        }
        Commands::EmitIr { file, output } => {
            println!("Emitting IR for {:?} -> {:?}", file, output);
            // TODO: Implement emit-ir
        }
        Commands::Fmt { file, check } => {
            println!("Formatting {:?} (check: {})", file, check);
            // TODO: Implement fmt
        }
    }
}
