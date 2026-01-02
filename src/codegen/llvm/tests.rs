//! Tests for LLVM code generation.

#[cfg(test)]
mod tests {
    use crate::codegen::CodeGen;
    use crate::ir::lower_program;
    use crate::parser::Parser;
    use inkwell::context::Context;

    fn compile_to_ir(source: &str) -> String {
        let mut parser = Parser::new(source);
        let program = parser.parse().expect("Parse error");
        let ir_module = lower_program(&program);

        let context = Context::create();
        let mut codegen = CodeGen::new(&context, "test");
        codegen.compile(&ir_module).expect("Codegen error");
        codegen.get_ir_string()
    }

    #[test]
    fn test_simple_function_codegen() {
        let source = r#"fn main() -> i64
    42
"#;
        let ir = compile_to_ir(source);
        assert!(ir.contains("define i64 @main()"));
        assert!(ir.contains("ret i64"));
    }

    #[test]
    fn test_function_with_params() {
        let source = r#"fn add(a: i64, b: i64) -> i64
    a + b
"#;
        let ir = compile_to_ir(source);
        assert!(ir.contains("define i64 @add(i64"));
        assert!(ir.contains("add i64"));
    }

    #[test]
    fn test_function_call_codegen() {
        let source = r#"fn helper() -> i64
    42

fn main() -> i64
    helper()
"#;
        let ir = compile_to_ir(source);
        assert!(ir.contains("call i64 @helper()"));
    }

    #[test]
    fn test_let_binding_codegen() {
        let source = r#"fn main() -> i64
    let x = 42
    x
"#;
        let ir = compile_to_ir(source);
        assert!(ir.contains("alloca i64"));
        assert!(ir.contains("store i64 42"));
    }

    #[test]
    fn test_binary_operations_with_vars() {
        let source = r#"fn add(a: i64, b: i64) -> i64
    a + b
"#;
        let ir = compile_to_ir(source);
        assert!(ir.contains("add i64"));
    }

    #[test]
    fn test_subtraction_codegen() {
        let source = r#"fn sub(a: i64, b: i64) -> i64
    a - b
"#;
        let ir = compile_to_ir(source);
        assert!(ir.contains("sub i64"));
    }

    #[test]
    fn test_multiplication_codegen() {
        let source = r#"fn mul(a: i64, b: i64) -> i64
    a * b
"#;
        let ir = compile_to_ir(source);
        assert!(ir.contains("mul i64"));
    }

    #[test]
    fn test_division_codegen() {
        let source = r#"fn div(a: i64, b: i64) -> i64
    a / b
"#;
        let ir = compile_to_ir(source);
        assert!(ir.contains("sdiv i64"));
    }

    #[test]
    fn test_comparison_codegen() {
        let source = r#"fn less(a: i64, b: i64) -> bool
    a < b
"#;
        let ir = compile_to_ir(source);
        assert!(ir.contains("icmp slt i64"));
    }

    #[test]
    fn test_constant_folding() {
        let source = r#"fn main() -> i64
    1 + 2
"#;
        let ir = compile_to_ir(source);
        assert!(ir.contains("store i64 3"));
    }

    #[test]
    fn test_multiple_functions() {
        let source = r#"fn foo() -> i64
    1

fn bar() -> i64
    2

fn main() -> i64
    foo()
"#;
        let ir = compile_to_ir(source);
        assert!(ir.contains("define i64 @foo()"));
        assert!(ir.contains("define i64 @bar()"));
        assert!(ir.contains("define i64 @main()"));
    }

    #[test]
    fn test_float_function_codegen() {
        let source = r#"fn add_floats(a: f64, b: f64) -> f64
    a + b
"#;
        let ir = compile_to_ir(source);
        assert!(ir.contains("define double @add_floats(double"));
        assert!(ir.contains("fadd double"));
    }

    #[test]
    fn test_float_literal_codegen() {
        let source = r#"fn get_pi() -> f64
    3.14159
"#;
        let ir = compile_to_ir(source);
        assert!(ir.contains("define double @get_pi()"));
        assert!(ir.contains("3.14159"));
    }
}
