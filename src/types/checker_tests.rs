//! Tests for the WASD type checker.

#[cfg(test)]
mod tests {
    use crate::parser::Parser;
    use crate::types::checker::TypeChecker;
    use crate::types::types::WasdType;

    fn check(source: &str) -> Result<(), Vec<String>> {
        let mut parser = Parser::new(source);
        let program = parser.parse().expect("Parse error");
        let mut checker = TypeChecker::new();
        checker.check_program(&program)
    }

    #[test]
    fn test_simple_function() {
        let source = r#"fn add(a: i64, b: i64) -> i64
    a + b
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_function_call() {
        let source = r#"fn add(a: i64, b: i64) -> i64
    a + b

fn main() -> i64
    add(1, 2)
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_undefined_variable() {
        let source = r#"fn main()
    undefined_var
"#;
        let result = check(source);
        assert!(result.is_err());
        assert!(result.unwrap_err()[0].contains("Undefined variable"));
    }

    #[test]
    fn test_undefined_function() {
        let source = r#"fn main()
    undefined_func()
"#;
        let result = check(source);
        assert!(result.is_err());
    }

    #[test]
    fn test_let_binding() {
        let source = r#"fn main()
    let x = 42
    x
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_type_annotation() {
        let source = r#"fn main()
    let x: i64 = 42
    x
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_binary_op_type() {
        let source = r#"fn main() -> i64
    1 + 2
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_comparison_op() {
        let source = r#"fn main() -> bool
    1 < 2
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_logical_op() {
        let source = r#"fn main() -> bool
    true and false
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_struct_type_registered() {
        let source = r#"struct Point
    x: f64
    y: f64

fn main()
    let x = 1
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_enum_type_registered() {
        let source = r#"enum Option[T]
    Some(T)
    None

fn main()
    let x = 1
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_effects_annotation() {
        let source = r#"fn read_file() -> String with [IO]
    file_contents
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();
        let mut checker = TypeChecker::new();
        let _ = checker.check_program(&program);

        if let Some(WasdType::Function { effects, .. }) = checker.env.get("read_file") {
            assert_eq!(effects, &vec!["IO".to_string()]);
        } else {
            panic!("Expected function with effects");
        }
    }

    #[test]
    fn test_unary_negation() {
        let source = r#"fn main() -> i64
    -42
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_unary_not() {
        let source = r#"fn main() -> bool
    not true
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_multiple_functions() {
        let source = r#"fn helper(x: i64) -> i64
    x + 1

fn main() -> i64
    helper(41)
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_use_stdlib_io() {
        let source = r#"use std.io

fn main()
    print("hello")
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_use_stdlib_prelude() {
        let source = r#"use std.prelude.*

fn main()
    println("hello")
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_use_specific_function() {
        let source = r#"use std.io.println

fn main()
    println("world")
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_use_unknown_stdlib_module() {
        // Unknown stdlib modules should still error
        let source = r#"use std.unknown.module

fn main()
    something()
"#;
        let result = check(source);
        assert!(result.is_err());
        assert!(result.unwrap_err()[0].contains("Unknown import path"));
    }

    #[test]
    fn test_use_local_module_passes_typechecker() {
        // Non-stdlib imports are handled by module loader, so type checker passes them through
        // But undefined functions should still error
        let source = r#"use local.module

fn main()
    something()
"#;
        let result = check(source);
        assert!(result.is_err());
        // Should error on undefined function, not on import
        assert!(result.unwrap_err()[0].contains("Undefined"));
    }

    #[test]
    fn test_use_stdlib_types() {
        let source = r#"use std.types

fn main()
    let x = None
"#;
        let result = check(source);
        if let Err(e) = &result {
            eprintln!("Errors: {:?}", e);
        }
        assert!(result.is_ok());
    }

    #[test]
    fn test_use_stdlib_collections() {
        let source = r#"use std.collections

fn main()
    let v = Vec_new()
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_use_stdlib_string() {
        let source = r#"use std.string

fn main()
    let s = String_new()
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_exhaustive_match() {
        let source = r#"enum Option[T]
    Some(T)
    None

fn main() -> i64
    let x = Option::Some(42)
    match x
        Option::Some(val) => val
        Option::None => 0
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_non_exhaustive_match() {
        let source = r#"enum Option[T]
    Some(T)
    None

fn main() -> i64
    let x = Option::Some(42)
    match x
        Option::Some(val) => val
"#;
        let result = check(source);
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert!(errors[0].contains("Non-exhaustive"));
        assert!(errors[0].contains("Option::None"));
    }

    #[test]
    fn test_wildcard_makes_match_exhaustive() {
        let source = r#"enum Option[T]
    Some(T)
    None

fn main() -> i64
    let x = Option::Some(42)
    match x
        Option::Some(val) => val
        _ => 0
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_effects_pure_calling_pure() {
        // Pure function can call pure function
        let source = r#"fn pure_add(a: i64, b: i64) -> i64
    a + b

fn main() -> i64
    pure_add(1, 2)
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_effects_io_calling_io() {
        // IO function can call IO function
        let source = r#"fn io_read() -> String with [IO]
    "data"

fn main() -> String with [IO]
    io_read()
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_effects_pure_cannot_call_io() {
        // Pure function cannot call IO function
        let source = r#"fn io_read() -> String with [IO]
    "data"

fn main() -> String
    io_read()
"#;
        let result = check(source);
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert!(errors[0].contains("effect"));
        assert!(errors[0].contains("IO"));
    }

    #[test]
    fn test_effects_io_can_call_pure() {
        // IO function can call pure function
        let source = r#"fn pure_add(a: i64, b: i64) -> i64
    a + b

fn main() -> i64 with [IO]
    pure_add(1, 2)
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_effects_multiple_effects() {
        // Function with multiple effects
        let source = r#"fn async_io() -> String with [IO, Async]
    "data"

fn main() -> String with [IO, Async]
    async_io()
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_effects_missing_one_of_multiple() {
        // Caller has IO but callee also needs Async
        let source = r#"fn async_io() -> String with [IO, Async]
    "data"

fn main() -> String with [IO]
    async_io()
"#;
        let result = check(source);
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert!(errors[0].contains("Async"));
    }

    #[test]
    fn test_async_function_has_async_effect() {
        // async fn automatically gets [Async] effect
        let source = r#"async fn fetch() -> String
    "data"

fn main() -> String with [Async]
    fetch()
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_await_in_async_function() {
        // await is valid in async functions
        let source = r#"async fn fetch() -> String
    "data"

async fn main() -> String
    await fetch()
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_await_in_non_async_function_error() {
        // await is not valid in non-async functions
        let source = r#"async fn fetch() -> String
    "data"

fn main() -> String
    await fetch()
"#;
        let result = check(source);
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert!(errors[0].contains("await"));
    }

    #[test]
    fn test_async_with_explicit_effects() {
        // async fn with explicit effects still works
        let source = r#"async fn io_fetch() -> String with [IO]
    "data"

fn main() -> String with [IO, Async]
    io_fetch()
"#;
        assert!(check(source).is_ok());
    }
}
