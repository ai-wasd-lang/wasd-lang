# WASD Programming Language

A modern systems programming language with explicit semantics, zero-cost abstractions, and memory safety. Compiles to native code via LLVM.

## Philosophy

- **Explicit over implicit** - No hidden behavior or magic
- **If it compiles, it's correct** - Strong static guarantees
- **Zero-cost abstractions** - Pay only for what you use
- **Memory safety without GC** - Ownership and borrowing

## Quick Start

```bash
# Build the compiler
cargo build --release

# Compile and run a program
cargo run -- run examples/hello_world/main.wasd

# Just compile
cargo run -- build examples/fibonacci/main.wasd

# Type check only
cargo run -- check myfile.wasd

# Emit LLVM IR
cargo run -- emit-ir myfile.wasd
```

## Language Syntax

```wasd
// Variables
let x = 5                    // immutable, type inferred
let mut counter: i32 = 0     // mutable, explicit type

// Functions
fn add(a: i32, b: i32) -> i32
    a + b

fn greet(name: String) -> Unit with [IO]
    print("Hello, " + name)

// Structs
struct Point
    x: f32
    y: f32

// Enums (ADTs)
enum Option[T]
    Some(T)
    None

enum Result[T, E]
    Ok(T)
    Err(E)

// Pattern matching
match value
    Some(x) => x + 1
    None => 0

// Control flow
if condition
    do_something()
else
    do_other()

// Error handling (no exceptions)
let content = read(path)?              // propagate error
let content = read(path) else "default" // fallback value

// Memory allocation
let stack_val = Point { x: 1.0, y: 2.0 }   // stack (default)
let heap_val = heap Point { x: 1.0, y: 2.0 } // explicit heap
let shared = rc Point { x: 1.0, y: 2.0 }    // reference counted
let atomic = arc Point { x: 1.0, y: 2.0 }   // atomic ref counted

// References and borrowing
let r = &value        // immutable borrow
let r = &mut value    // mutable borrow
```

## Effects System

Functions declare their side effects in the type signature:

```wasd
fn pure_math(x: i32) -> i32           // no effects, pure
fn read_file() -> String with [IO]     // performs I/O
fn fetch() -> Data with [Async, IO]    // async + I/O
```

## Architecture

```
Source (.wasd) → Lexer → Parser → Type Checker → Borrow Checker → IR → LLVM → Binary
```

```
src/
├── main.rs           # CLI (build, run, check, emit-ir)
├── lexer/            # Tokenization with indent tracking
├── parser/           # Recursive descent parser → AST
├── types/            # Type inference and checking
├── borrow/           # Ownership and borrow validation
├── ir/               # WASD IR representation
├── codegen/          # LLVM code generation
└── errors/           # Diagnostics with ariadne
```

## Implementation Status

| Component | Status | Description |
|-----------|--------|-------------|
| **Lexer** | Complete | All tokens, indentation, escapes |
| **Parser** | Functional | Functions, structs, enums, expressions, match |
| **Type System** | Basic | Inference, primitives, functions, generics (partial) |
| **Borrow Checker** | Basic | Ownership tracking, borrow conflicts |
| **IR Generation** | Functional | Core instructions, needs control flow |
| **LLVM Codegen** | Functional | Integer ops, functions, calls |

### What Works Now

- Integer and float arithmetic
- Function definitions and calls
- Variable bindings with type inference
- Struct and enum definitions
- If/else expressions
- Basic pattern matching
- String literals and `print()`
- Multi-function programs
- Recursive functions
- Basic ownership/borrow checking

### In Progress

- [ ] While/for loops (parsed, not codegen)
- [ ] Enum construction and full pattern matching
- [ ] Generic monomorphization
- [ ] Struct field access codegen
- [ ] Heap allocation (`heap`, `rc`, `arc`)

### Not Yet Implemented

- [ ] Module system / imports
- [ ] Trait system
- [ ] Closures
- [ ] Standard library
- [ ] String operations
- [ ] Collections (arrays, maps)
- [ ] Async/await
- [ ] FFI
- [ ] Code formatter (`fmt` command)

## Development

```bash
# Fast type check
cargo check

# Run tests
cargo test

# Run specific test suite
cargo test lexer
cargo test parser

# Format and lint
cargo fmt && cargo clippy
```

## Examples

See the `examples/` directory:

- `hello_world/` - Basic print
- `functions/` - Function composition
- `arithmetic/` - Math operations
- `fibonacci/` - Recursive functions

## Dependencies

- **inkwell** - Safe LLVM 18 bindings
- **clap** - CLI argument parsing
- **ariadne** - Pretty error diagnostics
- **thiserror** - Error handling

## License

MIT License - see [LICENSE](LICENSE)
