//! Tests for the WASD parser.

#[cfg(test)]
mod tests {
    use crate::parser::ast::*;
    use crate::parser::Parser;

    #[test]
    fn test_parse_simple_function() {
        let source = r#"fn add(a: i64, b: i64) -> i64
    a + b
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();
        assert_eq!(program.items.len(), 1);

        if let Item::Function(f) = &program.items[0] {
            assert_eq!(f.name, "add");
            assert_eq!(f.params.len(), 2);
            assert_eq!(f.params[0].name, "a");
            assert_eq!(f.params[1].name, "b");
            assert!(f.return_type.is_some());
        } else {
            panic!("Expected function");
        }
    }

    #[test]
    fn test_parse_struct() {
        let source = r#"struct Point
    x: f64
    y: f64
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();
        assert_eq!(program.items.len(), 1);

        if let Item::Struct(s) = &program.items[0] {
            assert_eq!(s.name, "Point");
            assert_eq!(s.fields.len(), 2);
            assert_eq!(s.fields[0].name, "x");
            assert_eq!(s.fields[1].name, "y");
        } else {
            panic!("Expected struct");
        }
    }

    #[test]
    fn test_parse_generic_struct() {
        let source = r#"struct Container[T]
    value: T
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Struct(s) = &program.items[0] {
            assert_eq!(s.name, "Container");
            assert_eq!(s.generics, vec!["T"]);
        } else {
            panic!("Expected struct");
        }
    }

    #[test]
    fn test_parse_enum() {
        let source = r#"enum Option[T]
    Some(T)
    None
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Enum(e) = &program.items[0] {
            assert_eq!(e.name, "Option");
            assert_eq!(e.generics, vec!["T"]);
            assert_eq!(e.variants.len(), 2);
            assert_eq!(e.variants[0].name, "Some");
            assert_eq!(e.variants[0].fields.len(), 1);
            assert_eq!(e.variants[1].name, "None");
            assert_eq!(e.variants[1].fields.len(), 0);
        } else {
            panic!("Expected enum");
        }
    }

    #[test]
    fn test_parse_let_statement() {
        let source = r#"fn main()
    let x = 42
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Function(f) = &program.items[0] {
            assert_eq!(f.body.len(), 1);
            if let Stmt::Let {
                name,
                mutable,
                value,
                ..
            } = &f.body[0]
            {
                assert_eq!(name, "x");
                assert!(!mutable);
                if let Expr::Int(n, _) = value {
                    assert_eq!(*n, 42);
                } else {
                    panic!("Expected int literal");
                }
            } else {
                panic!("Expected let statement");
            }
        }
    }

    #[test]
    fn test_parse_mutable_let() {
        let source = r#"fn main()
    let mut counter = 0
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Function(f) = &program.items[0] {
            if let Stmt::Let { mutable, .. } = &f.body[0] {
                assert!(mutable);
            } else {
                panic!("Expected let statement");
            }
        }
    }

    #[test]
    fn test_parse_binary_expressions() {
        let source = r#"fn main()
    1 + 2 * 3
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Function(f) = &program.items[0] {
            if let Stmt::Expr(Expr::Binary(left, BinOp::Add, right, _)) = &f.body[0] {
                if let Expr::Int(1, _) = left.as_ref() {
                } else {
                    panic!("Expected 1 on left");
                }
                if let Expr::Binary(_, BinOp::Mul, _, _) = right.as_ref() {
                } else {
                    panic!("Expected multiplication on right");
                }
            } else {
                panic!("Expected binary expression");
            }
        }
    }

    #[test]
    fn test_parse_comparison_operators() {
        let source = r#"fn main()
    x < 10
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Function(f) = &program.items[0] {
            if let Stmt::Expr(Expr::Binary(_, BinOp::Lt, _, _)) = &f.body[0] {
            } else {
                panic!("Expected comparison expression");
            }
        }
    }

    #[test]
    fn test_parse_function_call() {
        let source = r#"fn main()
    add(1, 2)
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Function(f) = &program.items[0] {
            if let Stmt::Expr(Expr::Call(callee, _type_args, args, _)) = &f.body[0] {
                if let Expr::Ident(name, _) = callee.as_ref() {
                    assert_eq!(name, "add");
                }
                assert_eq!(args.len(), 2);
            } else {
                panic!("Expected function call");
            }
        }
    }

    #[test]
    fn test_parse_return_statement() {
        let source = r#"fn add(a: i64, b: i64) -> i64
    return a + b
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Function(f) = &program.items[0] {
            if let Stmt::Return(Some(_), _) = &f.body[0] {
            } else {
                panic!("Expected return statement");
            }
        }
    }

    #[test]
    fn test_parse_effects() {
        let source = r#"fn read_file() -> String with [IO]
    file_contents
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Function(f) = &program.items[0] {
            assert_eq!(f.effects, vec!["IO"]);
        } else {
            panic!("Expected function");
        }
    }

    #[test]
    fn test_parse_reference_types() {
        let source = r#"fn borrow(x: &i64) -> i64
    x
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Function(f) = &program.items[0] {
            if let Type::Reference(inner, is_mut) = &f.params[0].ty {
                assert!(!is_mut);
                if let Type::Named(name) = inner.as_ref() {
                    assert_eq!(name, "i64");
                }
            } else {
                panic!("Expected reference type");
            }
        }
    }

    #[test]
    fn test_parse_mutable_reference() {
        let source = r#"fn mutate(x: &mut i64)
    x
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Function(f) = &program.items[0] {
            if let Type::Reference(_, is_mut) = &f.params[0].ty {
                assert!(is_mut);
            } else {
                panic!("Expected mutable reference type");
            }
        }
    }

    #[test]
    fn test_parse_unary_operators() {
        let source = r#"fn main()
    -x
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Function(f) = &program.items[0] {
            if let Stmt::Expr(Expr::Unary(UnaryOp::Neg, _, _)) = &f.body[0] {
            } else {
                panic!("Expected unary negation");
            }
        }
    }

    #[test]
    fn test_parse_boolean_literal() {
        let source = r#"fn main()
    let flag = true
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Function(f) = &program.items[0] {
            if let Stmt::Let {
                value: Expr::Bool(true, _),
                ..
            } = &f.body[0]
            {
            } else {
                panic!("Expected boolean true");
            }
        }
    }

    #[test]
    fn test_parse_float_literal() {
        let source = r#"fn main()
    let pi = 3.14
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Function(f) = &program.items[0] {
            if let Stmt::Let {
                value: Expr::Float(n, _),
                ..
            } = &f.body[0]
            {
                assert!((*n - 3.14).abs() < 0.001);
            } else {
                panic!("Expected float literal");
            }
        }
    }

    #[test]
    fn test_parse_logical_operators() {
        let source = r#"fn main()
    a and b or c
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Function(f) = &program.items[0] {
            if let Stmt::Expr(Expr::Binary(_, BinOp::Or, _, _)) = &f.body[0] {
            } else {
                panic!("Expected logical expression with Or as outer operator");
            }
        }
    }

    #[test]
    fn test_parse_field_access() {
        let source = r#"fn main()
    point.x
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Function(f) = &program.items[0] {
            if let Stmt::Expr(Expr::FieldAccess(obj, field, _)) = &f.body[0] {
                if let Expr::Ident(name, _) = obj.as_ref() {
                    assert_eq!(name, "point");
                }
                assert_eq!(field, "x");
            } else {
                panic!("Expected field access");
            }
        }
    }

    #[test]
    fn test_parse_heap_type() {
        let source = r#"fn alloc() -> heap i64
    x
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Function(f) = &program.items[0] {
            if let Some(Type::Heap(inner)) = &f.return_type {
                if let Type::Named(name) = inner.as_ref() {
                    assert_eq!(name, "i64");
                }
            } else {
                panic!("Expected heap type");
            }
        }
    }

    #[test]
    fn test_parse_while_loop() {
        let source = r#"fn main()
    while true
        1
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Function(f) = &program.items[0] {
            assert_eq!(f.body.len(), 1);
            if let Stmt::While { condition, body, .. } = &f.body[0] {
                assert!(matches!(condition, Expr::Bool(true, _)));
                assert_eq!(body.len(), 1);
            } else {
                panic!("Expected while statement");
            }
        }
    }

    #[test]
    fn test_parse_for_loop() {
        let source = r#"fn main()
    for i in 10
        i
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Function(f) = &program.items[0] {
            if let Stmt::For { var, .. } = &f.body[0] {
                assert_eq!(var, "i");
            } else {
                panic!("Expected for statement");
            }
        }
    }

    #[test]
    fn test_parse_break_continue() {
        let source = r#"fn main()
    while true
        break
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Function(f) = &program.items[0] {
            if let Stmt::While { body, .. } = &f.body[0] {
                assert!(matches!(body[0], Stmt::Break(_)));
            } else {
                panic!("Expected while statement with break");
            }
        }
    }

    #[test]
    fn test_parse_assignment() {
        let source = r#"fn main()
    let mut x = 0
    x = 5
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Function(f) = &program.items[0] {
            assert_eq!(f.body.len(), 2);
            if let Stmt::Assign { target, value, .. } = &f.body[1] {
                if let Expr::Ident(name, _) = target {
                    assert_eq!(name, "x");
                } else {
                    panic!("Expected identifier as target");
                }
                if let Expr::Int(n, _) = value {
                    assert_eq!(*n, 5);
                } else {
                    panic!("Expected int literal as value");
                }
            } else {
                panic!("Expected assignment statement");
            }
        }
    }

    #[test]
    fn test_parse_struct_construction() {
        let source = r#"fn main()
    let p = Point(x: 1, y: 2)
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Function(f) = &program.items[0] {
            if let Stmt::Let { value, .. } = &f.body[0] {
                if let Expr::StructConstruct { name, fields, .. } = value {
                    assert_eq!(name, "Point");
                    assert_eq!(fields.len(), 2);
                    assert_eq!(fields[0].0, "x");
                    assert_eq!(fields[1].0, "y");
                } else {
                    panic!("Expected struct construction");
                }
            }
        }
    }

    #[test]
    fn test_parse_enum_construction() {
        let source = r#"fn main()
    let x = Option::Some(42)
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Function(f) = &program.items[0] {
            if let Stmt::Let { value, .. } = &f.body[0] {
                if let Expr::EnumConstruct {
                    enum_name,
                    variant,
                    value: inner,
                    ..
                } = value
                {
                    assert_eq!(enum_name, &Some("Option".to_string()));
                    assert_eq!(variant, "Some");
                    assert!(inner.is_some());
                } else {
                    panic!("Expected enum construction");
                }
            }
        }
    }

    #[test]
    fn test_parse_enum_none_variant() {
        let source = r#"fn main()
    let x = Option::None
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Function(f) = &program.items[0] {
            if let Stmt::Let { value, .. } = &f.body[0] {
                if let Expr::EnumConstruct {
                    enum_name,
                    variant,
                    value: inner,
                    ..
                } = value
                {
                    assert_eq!(enum_name, &Some("Option".to_string()));
                    assert_eq!(variant, "None");
                    assert!(inner.is_none());
                } else {
                    panic!("Expected enum construction");
                }
            }
        }
    }

    #[test]
    fn test_parse_heap_alloc() {
        let source = r#"fn main()
    let x = heap 42
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Function(f) = &program.items[0] {
            if let Stmt::Let { value, .. } = &f.body[0] {
                if let Expr::HeapAlloc(inner, _) = value {
                    assert!(matches!(inner.as_ref(), Expr::Int(42, _)));
                } else {
                    panic!("Expected heap allocation");
                }
            }
        }
    }

    #[test]
    fn test_parse_rc_alloc() {
        let source = r#"fn main()
    let x = rc 100
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Function(f) = &program.items[0] {
            if let Stmt::Let { value, .. } = &f.body[0] {
                if let Expr::RcAlloc(inner, _) = value {
                    assert!(matches!(inner.as_ref(), Expr::Int(100, _)));
                } else {
                    panic!("Expected rc allocation");
                }
            }
        }
    }

    #[test]
    fn test_parse_arc_alloc() {
        let source = r#"fn main()
    let x = arc 200
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Function(f) = &program.items[0] {
            if let Stmt::Let { value, .. } = &f.body[0] {
                if let Expr::ArcAlloc(inner, _) = value {
                    assert!(matches!(inner.as_ref(), Expr::Int(200, _)));
                } else {
                    panic!("Expected arc allocation");
                }
            }
        }
    }

    #[test]
    fn test_parse_trait() {
        let source = r#"trait Add[T]
    fn add(self, other: T) -> T
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Trait(t) = &program.items[0] {
            assert_eq!(t.name, "Add");
            assert_eq!(t.generics, vec!["T"]);
            assert_eq!(t.methods.len(), 1);
            assert_eq!(t.methods[0].name, "add");
        } else {
            panic!("Expected trait definition");
        }
    }

    #[test]
    fn test_parse_impl() {
        let source = r#"impl Add[Point] for Point
    fn add(self, other: Point) -> Point
        Point(x: self.x + other.x, y: self.y + other.y)
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Impl(impl_def) = &program.items[0] {
            assert_eq!(impl_def.trait_name, Some("Add".to_string()));
            assert_eq!(impl_def.trait_generics.len(), 1);
            assert!(matches!(&impl_def.target_type, Type::Named(n) if n == "Point"));
            assert_eq!(impl_def.methods.len(), 1);
            assert_eq!(impl_def.methods[0].name, "add");
        } else {
            panic!("Expected impl definition");
        }
    }

    #[test]
    fn test_parse_inherent_impl() {
        let source = r#"impl Point
    fn new(x: i64, y: i64) -> Point
        Point(x: x, y: y)
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Impl(impl_def) = &program.items[0] {
            assert_eq!(impl_def.trait_name, None);
            assert!(matches!(&impl_def.target_type, Type::Named(n) if n == "Point"));
            assert_eq!(impl_def.methods.len(), 1);
            assert_eq!(impl_def.methods[0].name, "new");
        } else {
            panic!("Expected impl definition");
        }
    }

    #[test]
    fn test_parse_use_simple() {
        let source = "use std.io\n";
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Use(u) = &program.items[0] {
            assert_eq!(u.path, vec!["std", "io"]);
            assert!(!u.wildcard);
            assert_eq!(u.alias, None);
        } else {
            panic!("Expected use statement");
        }
    }

    #[test]
    fn test_parse_use_specific_item() {
        let source = "use std.io.print\n";
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Use(u) = &program.items[0] {
            assert_eq!(u.path, vec!["std", "io", "print"]);
            assert!(!u.wildcard);
            assert_eq!(u.alias, None);
        } else {
            panic!("Expected use statement");
        }
    }

    #[test]
    fn test_parse_use_wildcard() {
        let source = "use std.io.*\n";
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Use(u) = &program.items[0] {
            assert_eq!(u.path, vec!["std", "io"]);
            assert!(u.wildcard);
            assert_eq!(u.alias, None);
        } else {
            panic!("Expected use statement");
        }
    }

    #[test]
    fn test_parse_use_alias() {
        let source = "use std.io.print as p\n";
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Use(u) = &program.items[0] {
            assert_eq!(u.path, vec!["std", "io", "print"]);
            assert!(!u.wildcard);
            assert_eq!(u.alias, Some("p".to_string()));
        } else {
            panic!("Expected use statement");
        }
    }

    #[test]
    fn test_parse_closure_no_params() {
        let source = r#"fn main()
    let f = || 42
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Function(f) = &program.items[0] {
            if let Stmt::Let { value, .. } = &f.body[0] {
                if let Expr::Lambda { params, body, .. } = value {
                    assert!(params.is_empty());
                    if let Expr::Int(n, _) = body.as_ref() {
                        assert_eq!(*n, 42);
                    } else {
                        panic!("Expected int literal in body");
                    }
                } else {
                    panic!("Expected lambda expression");
                }
            }
        }
    }

    #[test]
    fn test_parse_closure_with_params() {
        let source = r#"fn main()
    let add = |x, y| x + y
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Function(f) = &program.items[0] {
            if let Stmt::Let { value, .. } = &f.body[0] {
                if let Expr::Lambda { params, body, .. } = value {
                    assert_eq!(params.len(), 2);
                    assert_eq!(params[0].name, "x");
                    assert_eq!(params[1].name, "y");
                    if let Expr::Binary(_, _, _, _) = body.as_ref() {
                    } else {
                        panic!("Expected binary expression in body");
                    }
                } else {
                    panic!("Expected lambda expression");
                }
            }
        }
    }

    #[test]
    fn test_parse_closure_with_typed_params() {
        let source = r#"fn main()
    let add = |x: i64, y: i64| x + y
"#;
        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        if let Item::Function(f) = &program.items[0] {
            if let Stmt::Let { value, .. } = &f.body[0] {
                if let Expr::Lambda { params, .. } = value {
                    assert_eq!(params.len(), 2);
                    assert!(matches!(&params[0].ty, Type::Named(n) if n == "i64"));
                    assert!(matches!(&params[1].ty, Type::Named(n) if n == "i64"));
                } else {
                    panic!("Expected lambda expression");
                }
            }
        }
    }
}
