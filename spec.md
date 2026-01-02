I'm honored. Let me name it and specify it concisely.

---

# **WASD**

*Latin for "light" — clear, fast, illuminating.*

---

## Core Philosophy

1. Explicit over implicit
2. If it compiles, it's correct
3. Zero-cost abstractions
4. No hidden behavior

---

## Compilation Target

WASD → LLVM IR → native binary

---

## Primitives

```
i8 i16 i32 i64 i128
u8 u16 u32 u64 u128
f32 f64
bool
char (UTF-8)
```

---

## Variables

```
let x: i32 = 5          // immutable
let mut y: i32 = 5      // mutable
```

Type inference allowed: `let x = 5` infers `i32`

---

## Functions

```
fn name(param: Type, param2: Type) -> ReturnType
    body
    last_expression_is_return
```

Whitespace-based blocks, no braces.

---

## Structs

```
struct Point
    x: f32
    y: f32
```

Instantiation: `Point(x: 1.0, y: 2.0)` or `Point(1.0, 2.0)`

---

## Enums (Sum Types)

```
enum Option[T]
    Some(T)
    None

enum Result[T, E]
    Ok(T)
    Err(E)
```

---

## Pattern Matching

```
match value
    Pattern1 => expression
    Pattern2(x) => expression
    Pattern3 if guard => expression
    _ => default
```

Must be exhaustive.

---

## Control Flow

```
if condition
    body
else if condition
    body
else
    body

for item in iterable
    body

while condition
    body

loop
    body
    if done
        break
```

---

## Generics

```
fn identity[T](x: T) -> T
    x

struct Container[T]
    value: T
```

Monomorphized at compile time.

---

## Traits

```
trait Add[Rhs, Output]
    fn add(self, rhs: Rhs) -> Output

impl Add[Point, Point] for Point
    fn add(self, rhs) -> Point
        Point(self.x + rhs.x, self.y + rhs.y)
```

---

## Memory Model

**Stack by default:**
```
let x = Point(1, 2)     // stack
```

**Heap explicit:**
```
let x = heap Point(1, 2)    // heap, owned
let y = rc Point(1, 2)      // reference counted
let z = arc Point(1, 2)     // atomic reference counted
```

---

## Borrowing

```
&T      // immutable borrow
&mut T  // mutable borrow
```

Rules:
- Many `&T` OR one `&mut T`, never both
- References cannot outlive owner

---

## RAII

Types can have `drop`:
```
impl Drop for FileHandle
    fn drop(self)
        close(self.fd)
```

Called automatically when scope ends.

---

## Error Handling

No exceptions. Errors are values.

```
fn read(path: String) -> String | IoError
    ...

// Propagate
let content = read(path)?

// Handle inline
let content = read(path) else "default"

// Match
match read(path)
    Ok(s) => use(s)
    Err(e) => handle(e)
```

---

## Optionals

No null.

```
let x: i32? = some(5)
let y: i32? = none

// Safe access
x?.method()

// Unwrap with default
x else 0
```

---

## Effects System

Functions declare side effects:
```
fn pure(x: i32) -> i32
    x + 1

fn reads_file() -> String with [IO]
    ...

fn async_fetch() -> Data with [Async, IO]
    ...

fn allocates() -> Vec[i32] with [Alloc]
    heap Vec.new()
```

Effect list is part of type signature.

---

## Async

```
fn fetch(url: String) -> Response with [Async, IO]
    let data = http.get(url).await
    parse(data)

// Concurrent execution
concurrent
    a = fetch(url1)
    b = fetch(url2)
// waits for both
```

---

## Arrays and Slices

```
let arr: [i32; 5] = [1, 2, 3, 4, 5]  // fixed size, stack
let slice: &[i32] = &arr[1..3]       // view into array
```

---

## Vec (Dynamic Array)

```
let v = heap Vec[i32].new()
v.push(1)
v.push(2)
let x = v[0]
```

---

## Strings

```
let s: String = "hello"         // owned, heap
let slice: &str = &s[0..3]      // view
```

UTF-8 by default.

---

## Comments

```
// single line

/* 
   multi
   line
*/
```

---

## Modules

```
// file: math.wasd
pub fn add(a: i32, b: i32) -> i32
    a + b

// file: main.wasd
import math

fn main()
    math.add(1, 2)
```

`pub` for public, private by default.

---

## Entry Point

```
fn main()
    // program starts here
```

Or with args:
```
fn main(args: &[String]) -> i32
    0  // exit code
```

---

## Operators

Standard: `+ - * / %`
Comparison: `== != < > <= >=`
Logical: `and or not`
Bitwise: `& | ^ ~ << >>`

No operator overloading except via traits (Add, Sub, etc).

---

## Type Aliases

```
type UserId = u64
type Callback = fn(i32) -> i32
```

---

## Comptime

Compile-time execution:
```
comptime fn factorial(n: u64) -> u64
    if n <= 1 then 1 else n * factorial(n - 1)

const FACT_10 = factorial(10)  // computed at compile time
```

---

## Attributes

```
#[inline]
fn small() -> i32
    42

#[align(64)]
struct CacheLine
    data: [u8; 64]
```

---

## Defer

```
fn example()
    let f = open("file.txt")
    defer f.close()
    
    // use f
// close called here, even if early return
```

---

## Assertions

```
assert condition
assert condition, "message"
debug_assert expensive_check()  // only in debug builds
```

---

## Priority Implementation Order

1. Lexer/Parser
2. Primitives, variables, functions
3. Structs, enums
4. Pattern matching
5. Generics (monomorphization)
6. Borrow checker (basic)
7. LLVM IR generation
8. Traits
9. Effects system
10. Standard library basics

---

## Non-Goals (For Now)

- Macros (later)
- GC regions (later)
- Full dependent types (later)
- Async runtime (basic version first)

---

**This is WASD. Make it real.**