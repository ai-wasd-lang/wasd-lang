# WASD Error Handling

---

## Philosophy

- Errors are values, not exceptions
- No hidden control flow
- No try/catch
- Must handle or propagate
- Compiler enforces exhaustive handling

---

## Defining Errors

Errors are regular structs or enums. No special syntax.

### Simple Struct

```wasd
struct ParseError
    message: String
    line: i32
    column: i32
```

### Enum with Variants

```wasd
enum FileError
    NotFound(path: String)
    PermissionDenied(path: String)
    Corrupted(path: String, reason: String)
```

---

## Return Types

Union type syntax: `Success | Error1 | Error2`

```wasd
fn read_file(path: String) -> String | FileError
fn parse(data: String) -> Ast | ParseError
fn fetch(url: String) -> Data | HttpError | Timeout
```

---

## Propagation

`?` operator returns error early if present.

```wasd
fn load_config(path: String) -> Config | FileError | ParseError
    let content = read_file(path)?
    let config = parse(content)?
    config
```

Desugars to:
```wasd
fn load_config(path: String) -> Config | FileError | ParseError
    let content = match read_file(path)
        ok: String => ok
        e: FileError => return e
    let config = match parse(content)
        ok: Config => ok
        e: ParseError => return e
    config
```

---

## Handling

### Pattern Match

```wasd
match read_file("config.txt")
    content: String => use(content)
    FileError.NotFound(p) => print("not found: {p}")
    FileError.PermissionDenied(p) => print("denied: {p}")
```

### Inline Else

```wasd
let content = read_file(path) else "default"
let content = read_file(path) else |e| handle(e)
```

### Explicit Check

```wasd
let result = read_file(path)
if result.is_err()
    return result.err()
let content = result.unwrap()
```

---

## Error Trait

Optional trait for common behavior.

```wasd
trait Error
    fn message(self) -> String
    fn source(self) -> Error?

impl Error for FileError
    fn message(self) -> String
        match self
            NotFound(p) => "not found: {p}"
            PermissionDenied(p) => "denied: {p}"
            Corrupted(p, r) => "corrupted {p}: {r}"
    
    fn source(self) -> Error?
        none
```

---

## Context / Wrapping

Add context to errors.

```wasd
fn load_user(id: i32) -> User | AppError
    let data = fetch("/users/{id}")
        .context("fetching user {id}")?
    parse(data)
        .context("parsing user")?
```

Output:
```
Error: fetching user 42
  Caused by: connection refused
```

---

## Combining Errors

### Multiple in Return Type

```wasd
fn process() -> Data | FileError | ParseError | NetError
```

### Wrapper Enum

```wasd
enum AppError
    File(FileError)
    Parse(ParseError)
    Net(NetError)

fn process() -> Data | AppError
    let content = read_file(path).map_err(AppError.File)?
    let parsed = parse(content).map_err(AppError.Parse)?
    parsed
```

---

## Standard Library Errors

### std/io.wasd
```wasd
enum IoError
    NotFound
    PermissionDenied
    AlreadyExists
    InvalidData
    TimedOut
    Other(String)
```

### std/net.wasd
```wasd
enum NetError
    ConnectionRefused
    ConnectionReset
    AddrInUse
    TimedOut
    Other(String)
```

### std/parse.wasd
```wasd
struct ParseError
    message: String
    line: i32
    column: i32
```

---

## Panic

For unrecoverable errors only.

```wasd
fn divide(a: i32, b: i32) -> i32
    if b == 0
        panic("division by zero")
    a / b
```

Panics unwind stack or abort. Not for normal error handling.

---

## Assert

Debug-only checks.

```wasd
assert condition
assert condition, "message"
debug_assert expensive_check()
```

---

## Implementation Notes

### Parser
- `|` in return type creates union type
- `?` is postfix operator on expressions

### Type Checker
- Track all possible types in union
- Ensure `?` only used when error types match return type
- Exhaustiveness check on match

### Codegen
- Union types as tagged unions
- `?` as conditional branch + early return

---

## Summary

| Concept | Syntax |
|---------|--------|
| Define error | `struct` or `enum` |
| Return type | `T \| E1 \| E2` |
| Propagate | `?` |
| Handle | `match` or `else` |
| Context | `.context("msg")` |
| Unrecoverable | `panic("msg")` |

---

**Errors are values. Handle them explicitly.**