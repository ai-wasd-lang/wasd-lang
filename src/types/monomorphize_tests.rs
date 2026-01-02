//! Tests for the monomorphization pass.

#[cfg(test)]
mod tests {
    use crate::parser::{Item, Parser, Type};
    use crate::types::monomorphize::{monomorphize, MonoKey};

    #[test]
    fn test_mangled_name() {
        let key = MonoKey::new(
            "Option".to_string(),
            vec![Type::Named("i64".to_string())],
        );
        assert_eq!(key.mangled_name(), "Option_i64");
    }

    #[test]
    fn test_mangled_name_multiple_args() {
        let key = MonoKey::new(
            "Map".to_string(),
            vec![
                Type::Named("String".to_string()),
                Type::Named("i32".to_string()),
            ],
        );
        assert_eq!(key.mangled_name(), "Map_String_i32");
    }

    #[test]
    fn test_monomorphize_generic_struct() {
        let source = r#"struct Box[T]
    value: T

fn main()
    let x: Box[i64] = Box(value: 42)
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        let mono_program = monomorphize(&program);

        // Should have specialized Box_i64 struct
        let struct_names: Vec<_> = mono_program
            .items
            .iter()
            .filter_map(|item| {
                if let Item::Struct(s) = item {
                    Some(s.name.clone())
                } else {
                    None
                }
            })
            .collect();

        assert!(struct_names.contains(&"Box_i64".to_string()));
    }
}
