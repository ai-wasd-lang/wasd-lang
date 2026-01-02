# WASD Implementation Guide

---

## Toolchain

| Component | Tool |
|-----------|------|
| Implementation language | Rust |
| Lexer | Hand-written (not generators) |
| Parser | Hand-written recursive descent |
| AST | Rust enums and structs |
| Type checking | Custom implementation |
| IR | Custom WASD IR → LLVM IR |
| LLVM bindings | `inkwell` crate (safe LLVM wrapper) |
| Testing | Rust's built-in `#[test]` |
| CLI | `clap` crate |

---

## Project Structure

```
wasd/
├── Cargo.toml
├── src/
│   ├── main.rs           # CLI entry point
│   ├── lexer/
│   │   ├── mod.rs
│   │   ├── token.rs      # Token enum
│   │   └── lexer.rs      # Lexer implementation
│   ├── parser/
│   │   ├── mod.rs
│   │   ├── ast.rs        # AST node types
│   │   └── parser.rs     # Parser implementation
│   ├── types/
│   │   ├── mod.rs
│   │   ├── types.rs      # Type representations
│   │   └── checker.rs    # Type checker
│   ├── borrow/
│   │   ├── mod.rs
│   │   └── checker.rs    # Borrow checker
│   ├── ir/
│   │   ├── mod.rs
│   │   ├── wasd_ir.rs     # WASD intermediate representation
│   │   └── lower.rs      # AST → WASD IR
│   ├── codegen/
│   │   ├── mod.rs
│   │   └── llvm.rs       # WASD IR → LLVM IR
│   └── errors/
│       ├── mod.rs
│       └── diagnostic.rs # Error reporting
└── tests/
    ├── lexer_tests.rs
    ├── parser_tests.rs
    ├── type_tests.rs
    └── codegen_tests.rs
```

---

## Phase 1: Lexer

**Token types to implement:**
```rust
enum Token {
    // Literals
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    
    // Identifiers and keywords
    Ident(String),
    Fn, Let, Mut, If, Else, Match, For, While, Loop,
    Struct, Enum, Impl, Trait, Pub, Import,
    Return, Break, Continue,
    And, Or, Not,
    Heap, Rc, Arc,
    
    // Types
    I8, I16, I32, I64, I128,
    U8, U16, U32, U64, U128,
    F32, F64, Bool, Char,
    
    // Symbols
    Plus, Minus, Star, Slash, Percent,
    Eq, EqEq, NotEq, Lt, Gt, LtEq, GtEq,
    Arrow,      // ->
    FatArrow,   // =>
    Question,   // ?
    Ampersand,  // &
    Pipe,       // |
    Colon,
    Comma,
    Dot,
    LParen, RParen,
    LBracket, RBracket,
    
    // Whitespace
    Newline,
    Indent,
    Dedent,
    
    EOF,
}
```

**Key challenge:** Indentation-based parsing. Track indent levels with a stack.

---

## Phase 2: Parser

**AST nodes:**
```rust
enum Expr {
    Literal(Literal),
    Ident(String),
    Binary(Box<Expr>, BinOp, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    FieldAccess(Box<Expr>, String),
    Index(Box<Expr>, Box<Expr>),
    If(Box<Expr>, Block, Option<Block>),
    Match(Box<Expr>, Vec<MatchArm>),
    Block(Block),
    Lambda(Vec<Param>, Box<Expr>),
}

enum Stmt {
    Let(String, bool, Option<Type>, Expr),  // name, mutable, type, value
    Expr(Expr),
    Return(Option<Expr>),
    Break,
    Continue,
}

enum Item {
    Function(FnDef),
    Struct(StructDef),
    Enum(EnumDef),
    Impl(ImplDef),
    Trait(TraitDef),
    Import(ImportDef),
}
```

**Parser approach:** Recursive descent, one function per grammar rule.

---

## Phase 3: Type System

**Type representation:**
```rust
enum Type {
    Primitive(Primitive),
    Struct(String, Vec<Type>),      // name, generic args
    Enum(String, Vec<Type>),
    Function(Vec<Type>, Box<Type>), // params, return
    Reference(Box<Type>, bool),     // type, mutable
    Array(Box<Type>, usize),        // type, size
    Slice(Box<Type>),
    Option(Box<Type>),
    Result(Box<Type>, Box<Type>),
    Generic(String),                // type parameter
    Infer,                          // to be inferred
}
```

**Type checker tasks:**
1. Build symbol table
2. Resolve types
3. Infer types where needed
4. Check type compatibility
5. Monomorphize generics

---

## Phase 4: Borrow Checker

**Simplified model for v1:**
1. Track ownership of each variable
2. Track active borrows
3. Ensure: many `&T` OR one `&mut T`
4. Ensure references don't outlive owner

**Data structures:**
```rust
struct BorrowChecker {
    scopes: Vec<Scope>,
    loans: HashMap<VarId, Vec<Loan>>,
}

struct Loan {
    kind: LoanKind,  // Shared or Mutable
    region: Region,
}
```

Start simple. Full lifetime inference is complex — defer edge cases.

---

## Phase 5: WASD IR

**Intermediate representation between AST and LLVM:**
```rust
enum WASDIR {
    // Values
    Const(Constant),
    Load(Place),
    
    // Operations
    BinOp(BinOp, Operand, Operand),
    UnaryOp(UnaryOp, Operand),
    Call(FnId, Vec<Operand>),
    
    // Memory
    Alloca(Type),
    Store(Place, Operand),
    HeapAlloc(Type),
    HeapFree(Place),
    
    // Control flow
    Branch(Operand, BlockId, BlockId),
    Jump(BlockId),
    Return(Option<Operand>),
}
```

Why WASD IR before LLVM IR:
- Easier to optimize
- Easier to debug
- Cleaner separation

---

## Phase 6: LLVM Codegen

**Using `inkwell` crate:**
```rust
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::builder::Builder;

struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    variables: HashMap<String, PointerValue<'ctx>>,
}
```

**Mapping:**
| WASD | LLVM |
|-----|------|
| `i32` | `context.i32_type()` |
| `struct` | `context.struct_type(&fields, false)` |
| `fn` | `module.add_function(name, fn_type, None)` |
| `let x = 5` | `builder.build_alloca()` + `builder.build_store()` |
| `x + y` | `builder.build_int_add()` |
| `if` | `builder.build_conditional_branch()` |

---

## Testing Strategy

**Unit tests per phase:**
```rust
#[test]
fn lexer_basic() {
    let tokens = lex("let x = 5");
    assert_eq!(tokens, vec![Let, Ident("x"), Eq, Int(5)]);
}

#[test]
fn parser_function() {
    let ast = parse("fn add(a: i32, b: i32) -> i32\n    a + b");
    // assert AST structure
}

#[test]
fn codegen_add() {
    let ir = compile("fn add(a: i32, b: i32) -> i32\n    a + b");
    let result = execute(ir, vec![2, 3]);
    assert_eq!(result, 5);
}
```

**Integration tests:** Full programs that compile and run.

---

## CLI Interface

```bash
wasd build file.wasd           # compile to binary
wasd build file.wasd -o out    # specify output
wasd run file.wasd             # compile and run
wasd check file.wasd           # type check only
wasd emit-ir file.wasd         # output LLVM IR
wasd fmt file.wasd             # format code
```

---

## Dependencies (Cargo.toml)

```toml
[dependencies]
inkwell = { version = "0.2", features = ["llvm15-0"] }
clap = { version = "4", features = ["derive"] }
thiserror = "1"
ariadne = "0.3"   # pretty error messages

[dev-dependencies]
insta = "1"       # snapshot testing
```

---

## Implementation Order

```
Week 1:
├── Project setup
├── Token types
├── Lexer (basic)
├── Lexer (indentation)
└── Lexer tests

Week 2:
├── AST types
├── Parser (expressions)
├── Parser (statements)
├── Parser (items)
└── Parser tests

Week 3:
├── Type representation
├── Symbol table
├── Type inference (basic)
├── Type checking
└── Type tests

Week 4:
├── Borrow checker (basic)
├── Ownership tracking
├── Reference validation
└── Borrow tests

Week 5:
├── WASD IR design
├── AST → WASD IR lowering
├── IR optimization (basic)
└── IR tests

Week 6:
├── LLVM setup
├── Codegen primitives
├── Codegen functions
├── Codegen structs
└── Codegen tests

Week 7:
├── Codegen enums
├── Codegen pattern matching
├── Codegen generics
├── CLI
├── End-to-end tests
└── Documentation
```

---

## Key Files to Create First

1. `src/lexer/token.rs` — Token enum
2. `src/lexer/lexer.rs` — Lexer struct and `next_token()`
3. `src/parser/ast.rs` — AST node enums
4. `src/parser/parser.rs` — Parser struct and `parse()`
5. `src/types/types.rs` — Type enum
6. `src/types/checker.rs` — TypeChecker struct
7. `src/codegen/llvm.rs` — CodeGen struct

---

## Error Handling

Use `ariadne` for pretty errors:
```
Error: Type mismatch
   ┌─ src/main.wasd:5:12
   │
 5 │     let x: i32 = "hello"
   │            ───   ^^^^^^^ expected i32, found String
   │            │
   │            type declared here
```

---

## Notes for AI Agent

1. Start with simplest working version
2. Add features incrementally
3. Write tests before implementation
4. Keep each module focused
5. Use Rust idioms (Result, Option, iterators)
6. Commit after each working feature
7. Don't optimize prematurely
8. When stuck on borrow checker, simplify rules

---

**Build WASD. Make it compile. Make it run.**