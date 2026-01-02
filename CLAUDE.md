# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Lux is a new programming language designed with explicit semantics, zero-cost abstractions, and memory safety. It compiles to native code via LLVM.

**Core philosophy:** Explicit over implicit, if it compiles it's correct, zero-cost abstractions, no hidden behavior.

## Build & Development Commands

```bash
# Build
cargo build                    # Debug build
cargo build --release          # Release build

# Run compiler
cargo run -- build file.lux    # Compile a Lux file
cargo run -- run file.lux      # Compile and execute
cargo run -- check file.lux    # Type check only
cargo run -- emit-ir file.lux  # Output LLVM IR
cargo run -- fmt file.lux      # Format Lux code

# Testing
cargo test                     # Run all tests
cargo test lexer               # Run lexer tests only
cargo test parser              # Run parser tests only
cargo test -- --nocapture      # Show test output

# Development
cargo check                    # Fast type checking
cargo fmt                      # Format Rust code
cargo clippy                   # Lint Rust code
```

## Architecture

The compiler follows a multi-phase pipeline:

```
Source (.lux) → Lexer → Parser → Type Checker → Borrow Checker → Lux IR → LLVM IR → Binary
```

### Directory Structure

```
src/
├── main.rs              # CLI entry (clap)
├── lexer/               # Tokenization
│   ├── token.rs         # Token enum
│   └── lexer.rs         # Lexer with indent tracking
├── parser/              # Syntax analysis
│   ├── ast.rs           # AST node types
│   └── parser.rs        # Recursive descent parser
├── types/               # Type system
│   ├── types.rs         # Type representations
│   └── checker.rs       # Type inference & checking
├── borrow/              # Memory safety
│   └── checker.rs       # Ownership & borrow validation
├── ir/                  # Intermediate representation
│   ├── lux_ir.rs        # Lux IR definition
│   └── lower.rs         # AST → Lux IR
├── codegen/             # Code generation
│   └── llvm.rs          # Lux IR → LLVM IR (via inkwell)
└── errors/              # Diagnostics
    └── diagnostic.rs    # Error reporting (ariadne)
```

## Key Implementation Details

### Indentation-Based Parsing
Lux uses whitespace for blocks (like Python). The lexer tracks indent levels with a stack and emits `Indent`/`Dedent` tokens.

### Generics
Monomorphized at compile time - each generic instantiation generates specialized code.

### Memory Model
- Stack by default
- Explicit heap allocation: `heap T`, `rc T` (ref-counted), `arc T` (atomic ref-counted)
- Borrow checking: many `&T` OR one `&mut T`, never both

### Effects System
Functions declare side effects in their type signature:
```
fn pure(x: i32) -> i32
fn impure() -> String with [IO]
fn async_fetch() -> Data with [Async, IO]
```

## Dependencies

- `inkwell` - Safe LLVM bindings
- `clap` - CLI parsing
- `ariadne` - Pretty error messages
- `thiserror` - Error handling
- `insta` - Snapshot testing (dev)

## Language Syntax Quick Reference

```lux
// Variables
let x = 5              // immutable, type inferred
let mut y: i32 = 5     // mutable, explicit type

// Functions
fn add(a: i32, b: i32) -> i32
    a + b

// Structs
struct Point
    x: f32
    y: f32

// Enums
enum Option[T]
    Some(T)
    None

// Pattern matching
match value
    Pattern1 => expr
    Pattern2(x) => expr
    _ => default

// Error handling (no exceptions)
let content = read(path)?           // propagate
let content = read(path) else "default"  // fallback
```

## Implementation Phases

1. **Lexer/Parser** - Tokenization and AST construction
2. **Type System** - Inference, checking, generics monomorphization
3. **Borrow Checker** - Ownership tracking, reference validation
4. **IR Generation** - AST → Lux IR → LLVM IR
5. **Codegen** - Native binary via LLVM
