//! Tests for AST to WASD IR lowering.

#[cfg(test)]
mod tests {
    use crate::ir::{lower_program, IrBinOp, IrInst, IrTerminator, IrType};
    use crate::parser::Parser;

    fn lower(source: &str) -> crate::ir::IrModule {
        let mut parser = Parser::new(source);
        let program = parser.parse().expect("Parse error");
        lower_program(&program)
    }

    #[test]
    fn test_simple_function_lowering() {
        let source = r#"fn main() -> i64
    42
"#;
        let module = lower(source);
        assert_eq!(module.functions.len(), 1);
        assert_eq!(module.functions[0].name, "main");
        assert_eq!(module.functions[0].return_type, IrType::I64);
    }

    #[test]
    fn test_function_with_params() {
        let source = r#"fn add(a: i64, b: i64) -> i64
    a + b
"#;
        let module = lower(source);
        assert_eq!(module.functions[0].params.len(), 2);
        assert_eq!(module.functions[0].params[0].0, "a");
        assert_eq!(module.functions[0].params[1].0, "b");
    }

    #[test]
    fn test_let_binding_generates_alloca() {
        let source = r#"fn main()
    let x = 42
"#;
        let module = lower(source);
        let block = &module.functions[0].blocks[0];

        let has_alloca = block
            .instructions
            .iter()
            .any(|inst| matches!(inst, IrInst::Alloca { dest, .. } if dest == "x"));
        assert!(has_alloca);
    }

    #[test]
    fn test_let_binding_generates_store() {
        let source = r#"fn main()
    let x = 42
"#;
        let module = lower(source);
        let block = &module.functions[0].blocks[0];

        let has_store = block
            .instructions
            .iter()
            .any(|inst| matches!(inst, IrInst::Store { ptr, .. } if ptr == "x"));
        assert!(has_store);
    }

    #[test]
    fn test_binary_op_generates_binop() {
        let source = r#"fn main() -> i64
    1 + 2
"#;
        let module = lower(source);
        let block = &module.functions[0].blocks[0];

        let has_binop = block.instructions.iter().any(|inst| {
            matches!(
                inst,
                IrInst::BinOp {
                    op: IrBinOp::Add,
                    ..
                }
            )
        });
        assert!(has_binop);
    }

    #[test]
    fn test_function_call_generates_call() {
        let source = r#"fn helper() -> i64
    42

fn main() -> i64
    helper()
"#;
        let module = lower(source);
        let main_func = module.functions.iter().find(|f| f.name == "main").unwrap();
        let block = &main_func.blocks[0];

        let has_call = block
            .instructions
            .iter()
            .any(|inst| matches!(inst, IrInst::Call { func, .. } if func == "helper"));
        assert!(has_call);
    }

    #[test]
    fn test_struct_lowering() {
        let source = r#"struct Point
    x: f64
    y: f64

fn main()
    42
"#;
        let module = lower(source);
        assert_eq!(module.structs.len(), 1);
        assert_eq!(module.structs[0].name, "Point");
        assert_eq!(module.structs[0].fields.len(), 2);
    }

    #[test]
    fn test_return_terminator() {
        let source = r#"fn main() -> i64
    42
"#;
        let module = lower(source);
        let block = &module.functions[0].blocks[0];

        assert!(matches!(block.terminator, IrTerminator::Return(Some(_))));
    }

    #[test]
    fn test_void_return() {
        let source = r#"fn main()
    let x = 1
"#;
        let module = lower(source);
        assert_eq!(module.functions[0].return_type, IrType::Void);
    }

    #[test]
    fn test_type_lowering() {
        let source = r#"fn test(a: i32, b: f64, c: bool) -> i64
    a
"#;
        let module = lower(source);
        let params = &module.functions[0].params;
        assert_eq!(params[0].1, IrType::I32);
        assert_eq!(params[1].1, IrType::F64);
        assert_eq!(params[2].1, IrType::Bool);
    }

    #[test]
    fn test_multiple_statements() {
        let source = r#"fn main()
    let x = 1
    let y = 2
    let z = x + y
"#;
        let module = lower(source);
        let block = &module.functions[0].blocks[0];

        let alloca_count = block
            .instructions
            .iter()
            .filter(|i| matches!(i, IrInst::Alloca { .. }))
            .count();
        assert_eq!(alloca_count, 3);
    }

    #[test]
    fn test_match_lowering() {
        let source = r#"fn test(x: i64) -> i64
    match x
        1 => 10
        2 => 20
        _ => 0
"#;
        let module = lower(source);
        assert_eq!(module.functions.len(), 1);
        assert!(module.functions[0].blocks.len() >= 3);
    }

    #[test]
    fn test_if_lowering() {
        let source = r#"fn test(x: bool) -> i64
    if x
        1
    else
        0
"#;
        let module = lower(source);
        assert_eq!(module.functions.len(), 1);
        assert!(module.functions[0].blocks.len() >= 3);
    }

    #[test]
    fn test_enum_lowering() {
        let source = r#"enum Option
    Some(i64)
    None

fn main()
    let x = Option::Some(42)
"#;
        let module = lower(source);
        assert_eq!(module.enums.len(), 1);
        assert_eq!(module.enums[0].name, "Option");
        assert_eq!(module.enums[0].variants.len(), 2);
    }
}
