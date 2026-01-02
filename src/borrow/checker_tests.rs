//! Tests for the WASD borrow checker.

#[cfg(test)]
mod tests {
    use crate::borrow::checker::BorrowChecker;
    use crate::parser::Parser;

    fn check(source: &str) -> Result<(), Vec<String>> {
        let mut parser = Parser::new(source);
        let program = parser.parse().expect("Parse error");
        let mut checker = BorrowChecker::new();
        checker.check_program(&program)
    }

    #[test]
    fn test_simple_ownership() {
        let source = r#"fn main()
    let x = 42
    x
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_multiple_uses() {
        let source = r#"fn main()
    let x = 42
    x
    x
    x
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_parameter_ownership() {
        let source = r#"fn use_val(x: i64) -> i64
    x
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_function_call() {
        let source = r#"fn id(x: i64) -> i64
    x

fn main() -> i64
    let y = 42
    id(y)
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_binary_expression() {
        let source = r#"fn main() -> i64
    let a = 1
    let b = 2
    a + b
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_reassignment() {
        let source = r#"fn main()
    let x = 1
    let y = x
    y
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_if_expression() {
        let source = r#"fn main() -> i64
    let x = 1
    if true
        x
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_return_value() {
        let source = r#"fn get_val() -> i64
    let x = 42
    return x
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_chained_calls() {
        let source = r#"fn add(a: i64, b: i64) -> i64
    a + b

fn main() -> i64
    add(add(1, 2), add(3, 4))
"#;
        assert!(check(source).is_ok());
    }

    #[test]
    fn test_nested_let() {
        let source = r#"fn main()
    let x = 1
    let y = 2
    let z = x + y
    z
"#;
        assert!(check(source).is_ok());
    }
}
