# Lux Features

---

## Core Language Features

### Primitives
- Integers: `i8 i16 i32 i64 i128 u8 u16 u32 u64 u128`
- Floats: `f32 f64`
- Boolean: `bool`
- Character: `char` (UTF-8)
- Unit: `()` (void equivalent)

### Variables
- Immutable by default: `let x = 5`
- Mutable: `let mut x = 5`
- Type inference
- Shadowing allowed

### Functions
- First-class functions
- Closures with capture
- Generic functions
- Default parameters
- Named arguments

### Structs
- Product types
- Methods via `impl`
- Tuple structs
- Unit structs

### Enums
- Sum types
- Associated data
- Pattern matching

### Traits
- Interface definitions
- Default implementations
- Trait bounds
- Derive macros

### Generics
- Type parameters
- Trait constraints
- Monomorphization
- Associated types

---

## Memory Management

### Ownership
- Single owner by default
- Move semantics
- Copy types (primitives)

### Borrowing
- Immutable borrows: `&T`
- Mutable borrows: `&mut T`
- Borrow checker enforcement

### Allocation
- Stack by default
- Explicit heap: `heap T`
- Reference counted: `rc T`
- Atomic reference counted: `arc T`

### RAII
- Automatic cleanup
- `Drop` trait
- `defer` statement

### Arenas (v2)
- Bulk allocation
- Bulk deallocation
- Scoped memory

---

## Error Handling

### Result Type
- `Ok(T)` or `Err(E)`
- Propagation: `?`
- Pattern matching

### Option Type
- `Some(T)` or `None`
- Safe null handling
- Propagation: `?`

### Panics
- Unrecoverable errors
- Stack unwinding
- Abort option

### Error Chaining
- `.context("message")`
- Stack traces
- Source location

---

## Effects System

### Effect Declarations
```
fn pure() -> i32
fn impure() -> i32 with [IO]
fn async_io() -> i32 with [Async, IO]
```

### Built-in Effects
- `IO` — file, network, stdout
- `Async` — async operations
- `Alloc` — heap allocation
- `Panic` — can panic

### Effect Constraints
- `with [NoAlloc]` — no allocation allowed
- `with [NoIO]` — no IO allowed
- Compile-time enforcement

---

## Concurrency

### Async/Await
- Native async functions
- `.await` syntax
- No colored functions

### Structured Concurrency
```
concurrent
    a = task1()
    b = task2()
```
- Automatic cancellation
- No orphan tasks

### Channels
- Typed channels
- Buffered/unbuffered
- Select statement

### Parallelism
- `par_iter()` — parallel iteration
- Work stealing
- Thread pools

### Atomics
- `Atomic[T]` types
- Memory ordering
- Lock-free primitives

### Synchronization
- `Mutex[T]`
- `RwLock[T]`
- `Semaphore`
- `Barrier`

---

## Pattern Matching

### Patterns
- Literals
- Variables
- Wildcards: `_`
- Destructuring
- Guards: `if condition`
- Or patterns: `A | B`
- Range patterns: `1..10`

### Match Expressions
- Exhaustiveness checking
- Compiler warnings

### Let Patterns
```
let Point(x, y) = point
let Some(value) = opt else return
```

---

## Type System

### Inference
- Local type inference
- Bidirectional inference
- Minimal annotations

### Type Aliases
```
type UserId = u64
type Handler = fn(Request) -> Response
```

### Newtypes
```
newtype UserId(u64)
```
- Distinct from underlying type
- Zero-cost wrapper

### Refinement Types (v2)
```
type Positive = i32 where self > 0
type Percentage = f64 where 0.0 <= self <= 100.0
```

### Const Generics
```
struct Array[T, const N: usize]
    data: [T; N]
```

---

## Compile-Time Features

### Comptime
```
comptime fn factorial(n: u64) -> u64
    if n <= 1 then 1 else n * factorial(n - 1)

const FACT_10 = factorial(10)
```

### Const Evaluation
- Compile-time function execution
- Const variables
- Static assertions

### Conditional Compilation
```
#[cfg(target = "linux")]
fn platform_specific()

#[cfg(debug)]
fn debug_only()
```

---

## Macros (v2)

### Declarative Macros
```
macro vec!($($elem:expr),*) -> Vec
    ...
```

### Procedural Macros
- Derive macros
- Attribute macros
- Function-like macros

### Hygiene
- No accidental capture
- Scoped identifiers

---

## Standard Library

### Collections
- `Vec[T]` — dynamic array
- `HashMap[K, V]` — hash map
- `HashSet[T]` — hash set
- `BTreeMap[K, V]` — sorted map
- `BTreeSet[T]` — sorted set
- `LinkedList[T]` — linked list
- `VecDeque[T]` — double-ended queue

### Strings
- `String` — owned UTF-8
- `&str` — string slice
- String formatting
- Regex (external)

### IO
- `File` — file handle
- `stdin/stdout/stderr`
- Buffered IO
- Path handling

### Networking
- TCP/UDP sockets
- HTTP client (external)
- Async IO

### Time
- `Instant` — monotonic time
- `Duration` — time span
- `DateTime` — calendar time

### Random
- `Random` — RNG
- Distributions
- Cryptographic RNG

### Math
- Trig functions
- Min/max
- Abs/sign
- Rounding

---

## Build System

### Project File: `lux.toml`
```toml
[package]
name = "myproject"
version = "1.0.0"
edition = "2024"

[dependencies]
http = "1.2.3"
json = { git = "https://github.com/lux/json" }
local = { path = "../local" }

[dev-dependencies]
test-utils = "0.5"

[build]
opt-level = 3
lto = true

[target.linux]
features = ["epoll"]

[target.windows]
features = ["iocp"]
```

### CLI Commands
```bash
lux new project           # create project
lux init                  # init in current dir
lux build                 # compile
lux build --release       # optimized build
lux run                   # compile and run
lux run --release         # run optimized
lux test                  # run tests
lux test --filter name    # filter tests
lux bench                 # run benchmarks
lux check                 # type check only
lux clean                 # remove build artifacts
lux fmt                   # format code
lux lint                  # run linter
lux doc                   # generate docs
lux doc --open            # generate and open docs
lux publish               # publish to registry
lux update                # update dependencies
```

### Workspaces
```toml
[workspace]
members = ["core", "cli", "web"]
```
- Shared dependencies
- Unified builds

---

## Package Manager

### Registry
- Central package registry
- Semantic versioning
- Dependency resolution

### Features
```toml
[features]
default = ["std"]
std = []
async = ["runtime"]
```
- Conditional compilation
- Optional dependencies

### Lock File
- `lux.lock` — exact versions
- Reproducible builds

### Private Registries
- Corporate registries
- Auth tokens

---

## Testing

### Unit Tests
```
#[test]
fn test_addition()
    assert 2 + 2 == 4

#[test]
fn test_with_setup()
    let data = setup()
    assert data.valid()
```

### Test Attributes
```
#[test]
#[ignore]                    # skip by default
#[should_panic]              # expect panic
#[timeout(1000)]             # timeout in ms
```

### Assertions
```
assert condition
assert condition, "message"
assert_eq a, b
assert_ne a, b
assert_lt a, b
assert_gt a, b
```

### Property Testing
```
#[property]
fn addition_commutative(a: i32, b: i32)
    assert a + b == b + a
```
- Random input generation
- Shrinking on failure

### Fixtures
```
#[fixture]
fn database() -> Database
    Database.connect("test")

#[test]
fn test_query(db: Database)
    assert db.query("SELECT 1").ok()
```

### Mocking
```
#[mock]
trait HttpClient
    fn get(url: &str) -> Response

#[test]
fn test_fetch()
    let mock = MockHttpClient.new()
    mock.expect_get("/api").returns(Response.ok("data"))
    assert fetch(mock).ok()
```

### Coverage
```bash
lux test --coverage
lux test --coverage --html
```
- Line coverage
- Branch coverage
- HTML reports

---

## Benchmarking

### Benchmarks
```
#[bench]
fn bench_sort(b: &mut Bencher)
    let data = random_vec(10000)
    b.iter(||
        sort(data.clone())
    )
```

### Output
```
bench_sort        ... 1,234 ns/iter (+/- 56)
bench_sort_large  ... 45,678 ns/iter (+/- 1,234)
```

### Comparison
```bash
lux bench --save baseline
# make changes
lux bench --compare baseline
```

---

## Debugging

### Debug Builds
```bash
lux build           # debug by default
lux build --release # optimized
```
- No optimization
- Debug symbols
- Assertions enabled
- Overflow checks

### GDB/LLDB Support
```bash
gdb ./target/debug/myapp
lldb ./target/debug/myapp
```
- Full symbol info
- Source mapping
- Breakpoints
- Step debugging

### Debug Macros
```
dbg!(expression)           // print and return
dbg!(a, b, c)              // multiple values
```
Output:
```
[src/main.lux:42] expression = value
```

### Breakpoints in Code
```
breakpoint!()              // trigger debugger
breakpoint!(condition)     // conditional
```

### Print Debugging
```
print("value: {x}")
print("debug: {x:?}")      // debug format
print("pretty: {x:#?}")    // pretty debug
```

### Stack Traces
- Automatic on panic
- `backtrace()` function
- Symbol demangling

---

## Profiling

### CPU Profiling
```bash
lux run --profile cpu
```
- Sampling profiler
- Flame graphs
- Hot function detection

### Memory Profiling
```bash
lux run --profile memory
```
- Allocation tracking
- Leak detection
- Heap snapshots

### Built-in Instrumentation
```
#[profile]
fn hot_function()
    // automatically instrumented
```

### Tracing
```
span "request_handler"
    let user = span "auth"
        authenticate(token)
    let data = span "fetch"
        database.query(user.id)
    respond(data)
```
Output:
```
request_handler: 45ms
├── auth: 12ms
├── fetch: 28ms
└── respond: 5ms
```

---

## Error Monitoring (Sentry-like)

### Panic Reporting
```toml
[monitoring]
sentry = "https://key@sentry.io/project"
```

Automatic panic capture:
- Stack trace
- Local variables
- OS/arch info
- Release version

### Manual Capture
```
capture_error(error)
capture_message("something happened")
```

### Context
```
set_context("user", user.id)
set_tag("environment", "production")

with_scope(|scope|
    scope.set_user(user)
    dangerous_operation()
)
```

### Breadcrumbs
```
add_breadcrumb("user clicked button")
add_breadcrumb("api call started")
// if crash happens, breadcrumbs are sent
```

---

## Logging

### Log Levels
```
log.trace("detailed")
log.debug("debugging")
log.info("information")
log.warn("warning")
log.error("error")
```

### Structured Logging
```
log.info("user logged in", 
    user_id: user.id,
    ip: request.ip,
    duration: elapsed
)
```

### Configuration
```toml
[logging]
level = "info"
format = "json"
output = "stdout"

[logging.file]
path = "app.log"
rotation = "daily"
```

### Filters
```toml
[logging.filters]
"myapp::db" = "debug"
"myapp::http" = "info"
"noisy_lib" = "warn"
```

---

## Documentation

### Doc Comments
```
/// Adds two numbers together.
///
/// # Examples
/// ```
/// let result = add(2, 3)
/// assert result == 5
/// ```
///
/// # Panics
/// Never panics.
fn add(a: i32, b: i32) -> i32
    a + b
```

### Module Docs
```
//! This module provides math utilities.
//!
//! # Overview
//! ...
```

### Doc Generation
```bash
lux doc              # generate
lux doc --open       # generate and open browser
lux doc --json       # machine-readable
```

### Doc Tests
```bash
lux test --doc       # run examples in docs
```

---

## Formatting

### Formatter
```bash
lux fmt              # format all
lux fmt --check      # check only
lux fmt src/main.lux # specific file
```

### Configuration: `luxfmt.toml`
```toml
indent = 4
max_line_length = 100
trailing_commas = true
```

### Editor Integration
- Format on save
- Range formatting

---

## Linting

### Linter
```bash
lux lint             # run linter
lux lint --fix       # auto-fix
```

### Built-in Lints
- Unused variables
- Unused imports
- Dead code
- Shadowing warnings
- Style issues

### Lint Configuration
```toml
[lints]
unused_variables = "warn"
dead_code = "deny"
shadowing = "allow"
```

### Inline Control
```
#[allow(unused)]
let x = 5

#[deny(dead_code)]
mod important
```

---

## LSP (Language Server Protocol)

### Features
- Syntax highlighting
- Error diagnostics
- Autocompletion
- Go to definition
- Find references
- Rename symbol
- Hover information
- Code actions
- Formatting
- Inlay hints

### Editor Support
- VS Code extension
- Neovim plugin
- Emacs mode
- JetBrains plugin

---

## REPL (v2)

### Interactive Mode
```bash
lux repl
```
```
>>> let x = 5
>>> x + 3
8
>>> fn double(n: i32) -> i32
...     n * 2
>>> double(x)
10
```

### Features
- Expression evaluation
- Definition persistence
- History
- Tab completion

---

## FFI (Foreign Function Interface)

### C Interop
```
extern "C"
    fn printf(fmt: *const u8, ...) -> i32
    fn malloc(size: usize) -> *mut u8
    fn free(ptr: *mut u8)

fn main()
    unsafe
        let ptr = malloc(100)
        free(ptr)
```

### Exporting
```
#[export]
fn lux_function(x: i32) -> i32
    x * 2
```

### Bindgen (v2)
```bash
lux bindgen header.h -o bindings.lux
```

---

## Cross Compilation

### Targets
```bash
lux build --target x86_64-linux
lux build --target aarch64-linux
lux build --target x86_64-windows
lux build --target wasm32
```

### Target Configuration
```toml
[target.wasm32]
opt-level = "z"  # size optimization

[target.aarch64-linux]
linker = "aarch64-linux-gnu-gcc"
```

---

## WebAssembly

### WASM Target
```bash
lux build --target wasm32
```

### WASM Features
- No std option
- Size optimization
- JS interop

### Bindings
```
#[wasm_export]
fn add(a: i32, b: i32) -> i32
    a + b

#[wasm_import]
fn console_log(msg: &str)
```

---

## Embedded (v2)

### No-Std
```toml
[package]
std = false
```

### Features
- No heap option
- Custom allocators
- Inline assembly
- Memory-mapped IO

---

## Version Roadmap

### v0.1 — Foundation
- Lexer/Parser
- Basic types
- Functions
- Structs/Enums
- Pattern matching
- LLVM codegen

### v0.2 — Safety
- Borrow checker
- Generics
- Traits
- Error handling

### v0.3 — Tooling
- Package manager
- Testing framework
- Documentation
- LSP

### v0.4 — Concurrency
- Async/await
- Channels
- Parallelism

### v0.5 — Effects
- Effect system
- Effect handlers

### v1.0 — Stable
- Stable language
- Stable stdlib
- Production ready

---

**Lux: Simple. Fast. Safe. Explicit.**