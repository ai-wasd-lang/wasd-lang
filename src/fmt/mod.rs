//! WASD code formatter.
//!
//! Formats WASD source code to a consistent style.

use crate::parser::{
    BinOp, EnumDef, Expr, ExternFn, Function, ImplDef, Item, MatchArm, Param, Pattern, Program,
    Stmt, StructDef, TraitDef, Type, UnaryOp, UseStmt,
};

/// Configuration for the formatter.
#[derive(Debug, Clone)]
pub struct FormatConfig {
    /// Number of spaces per indentation level.
    pub indent_size: usize,
}

impl Default for FormatConfig {
    fn default() -> Self {
        Self { indent_size: 4 }
    }
}

/// The formatter takes a parsed program and produces formatted source code.
pub struct Formatter {
    config: FormatConfig,
    output: String,
    indent_level: usize,
}

impl Formatter {
    pub fn new(config: FormatConfig) -> Self {
        Self {
            config,
            output: String::new(),
            indent_level: 0,
        }
    }

    /// Format a program and return the formatted source code.
    pub fn format(&mut self, program: &Program) -> String {
        self.output.clear();
        self.indent_level = 0;

        for (i, item) in program.items.iter().enumerate() {
            if i > 0 {
                self.output.push('\n');
            }
            self.format_item(item);
        }

        self.output.clone()
    }

    fn format_item(&mut self, item: &Item) {
        match item {
            Item::Function(func) => self.format_function(func),
            Item::Struct(s) => self.format_struct(s),
            Item::Enum(e) => self.format_enum(e),
            Item::Trait(t) => self.format_trait(t),
            Item::Impl(i) => self.format_impl(i),
            Item::ExternFn(e) => self.format_extern_fn(e),
            Item::Use(u) => self.format_use(u),
        }
    }

    fn format_function(&mut self, func: &Function) {
        // Visibility
        if func.visibility == crate::parser::Visibility::Public {
            self.output.push_str("pub ");
        }

        // Async modifier
        if func.is_async {
            self.output.push_str("async ");
        }

        // fn name
        self.output.push_str("fn ");
        self.output.push_str(&func.name);

        // Generics
        if !func.generics.is_empty() {
            self.output.push('[');
            for (i, g) in func.generics.iter().enumerate() {
                if i > 0 {
                    self.output.push_str(", ");
                }
                self.output.push_str(g);
            }
            self.output.push(']');
        }

        // Parameters
        self.output.push('(');
        for (i, param) in func.params.iter().enumerate() {
            if i > 0 {
                self.output.push_str(", ");
            }
            self.format_param(param);
        }
        self.output.push(')');

        // Return type
        if let Some(ret) = &func.return_type {
            self.output.push_str(" -> ");
            self.format_type(ret);
        }

        // Effects
        if !func.effects.is_empty() {
            self.output.push_str(" with [");
            for (i, effect) in func.effects.iter().enumerate() {
                if i > 0 {
                    self.output.push_str(", ");
                }
                self.output.push_str(effect);
            }
            self.output.push(']');
        }

        self.output.push('\n');

        // Body
        self.indent_level += 1;
        for stmt in &func.body {
            self.write_indent();
            self.format_stmt(stmt);
            self.output.push('\n');
        }
        self.indent_level -= 1;
    }

    fn format_param(&mut self, param: &Param) {
        self.output.push_str(&param.name);
        self.output.push_str(": ");
        self.format_type(&param.ty);
    }

    fn format_type(&mut self, ty: &Type) {
        match ty {
            Type::Named(name) => self.output.push_str(name),
            Type::Generic(name, args) => {
                self.output.push_str(name);
                self.output.push('[');
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        self.output.push_str(", ");
                    }
                    self.format_type(arg);
                }
                self.output.push(']');
            }
            Type::Reference(inner, is_mut) => {
                self.output.push('&');
                if *is_mut {
                    self.output.push_str("mut ");
                }
                self.format_type(inner);
            }
            Type::Heap(inner) => {
                self.output.push_str("heap ");
                self.format_type(inner);
            }
            Type::Rc(inner) => {
                self.output.push_str("rc ");
                self.format_type(inner);
            }
            Type::Arc(inner) => {
                self.output.push_str("arc ");
                self.format_type(inner);
            }
            Type::Function(params, ret) => {
                self.output.push_str("fn(");
                for (i, p) in params.iter().enumerate() {
                    if i > 0 {
                        self.output.push_str(", ");
                    }
                    self.format_type(p);
                }
                self.output.push_str(") -> ");
                self.format_type(ret);
            }
            Type::Unit => self.output.push_str("()"),
        }
    }

    fn format_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Let {
                name,
                ty,
                mutable,
                value,
                ..
            } => {
                self.output.push_str("let ");
                if *mutable {
                    self.output.push_str("mut ");
                }
                self.output.push_str(name);
                if let Some(t) = ty {
                    self.output.push_str(": ");
                    self.format_type(t);
                }
                self.output.push_str(" = ");
                self.format_expr(value);
            }
            Stmt::Assign { target, value, .. } => {
                self.format_expr(target);
                self.output.push_str(" = ");
                self.format_expr(value);
            }
            Stmt::Expr(expr) => self.format_expr(expr),
            Stmt::Return(Some(expr), _) => {
                self.output.push_str("return ");
                self.format_expr(expr);
            }
            Stmt::Return(None, _) => self.output.push_str("return"),
            Stmt::While {
                condition, body, ..
            } => {
                self.output.push_str("while ");
                self.format_expr(condition);
                self.output.push('\n');
                self.indent_level += 1;
                for s in body {
                    self.write_indent();
                    self.format_stmt(s);
                    self.output.push('\n');
                }
                self.indent_level -= 1;
            }
            Stmt::For {
                var,
                iterable,
                body,
                ..
            } => {
                self.output.push_str("for ");
                self.output.push_str(var);
                self.output.push_str(" in ");
                self.format_expr(iterable);
                self.output.push('\n');
                self.indent_level += 1;
                for s in body {
                    self.write_indent();
                    self.format_stmt(s);
                    self.output.push('\n');
                }
                self.indent_level -= 1;
            }
            Stmt::Break(_) => self.output.push_str("break"),
            Stmt::Continue(_) => self.output.push_str("continue"),
        }
    }

    fn format_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Int(n, _) => self.output.push_str(&n.to_string()),
            Expr::Float(n, _) => self.output.push_str(&n.to_string()),
            Expr::Bool(b, _) => self.output.push_str(if *b { "true" } else { "false" }),
            Expr::String(s, _) => {
                self.output.push('"');
                self.output.push_str(s);
                self.output.push('"');
            }
            Expr::Ident(name, _) => self.output.push_str(name),
            Expr::Binary(left, op, right, _) => {
                self.format_expr(left);
                self.output.push(' ');
                self.format_binop(*op);
                self.output.push(' ');
                self.format_expr(right);
            }
            Expr::Unary(op, inner, _) => {
                self.format_unaryop(*op);
                self.format_expr(inner);
            }
            Expr::Call(callee, type_args, args, _) => {
                self.format_expr(callee);
                if !type_args.is_empty() {
                    self.output.push('[');
                    for (i, t) in type_args.iter().enumerate() {
                        if i > 0 {
                            self.output.push_str(", ");
                        }
                        self.format_type(t);
                    }
                    self.output.push(']');
                }
                self.output.push('(');
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        self.output.push_str(", ");
                    }
                    self.format_expr(arg);
                }
                self.output.push(')');
            }
            Expr::FieldAccess(base, field, _) => {
                self.format_expr(base);
                self.output.push('.');
                self.output.push_str(field);
            }
            Expr::StructConstruct { name, fields, .. } => {
                self.output.push_str(name);
                self.output.push('(');
                for (i, (fname, fval)) in fields.iter().enumerate() {
                    if i > 0 {
                        self.output.push_str(", ");
                    }
                    self.output.push_str(fname);
                    self.output.push_str(": ");
                    self.format_expr(fval);
                }
                self.output.push(')');
            }
            Expr::EnumConstruct {
                enum_name,
                variant,
                value,
                ..
            } => {
                if let Some(name) = enum_name {
                    self.output.push_str(name);
                    self.output.push_str("::");
                }
                self.output.push_str(variant);
                if let Some(v) = value {
                    self.output.push('(');
                    self.format_expr(v);
                    self.output.push(')');
                }
            }
            Expr::If(cond, then_branch, else_branch, _) => {
                self.output.push_str("if ");
                self.format_expr(cond);
                self.output.push('\n');
                self.indent_level += 1;
                for s in then_branch {
                    self.write_indent();
                    self.format_stmt(s);
                    self.output.push('\n');
                }
                self.indent_level -= 1;
                if let Some(else_stmts) = else_branch {
                    self.write_indent();
                    self.output.push_str("else\n");
                    self.indent_level += 1;
                    for s in else_stmts {
                        self.write_indent();
                        self.format_stmt(s);
                        self.output.push('\n');
                    }
                    self.indent_level -= 1;
                }
            }
            Expr::Match(value, arms, _) => {
                self.output.push_str("match ");
                self.format_expr(value);
                self.output.push('\n');
                self.indent_level += 1;
                for arm in arms {
                    self.write_indent();
                    self.format_match_arm(arm);
                    self.output.push('\n');
                }
                self.indent_level -= 1;
            }
            Expr::Block(stmts, _) => {
                for s in stmts {
                    self.format_stmt(s);
                }
            }
            Expr::HeapAlloc(inner, _) => {
                self.output.push_str("heap ");
                self.format_expr(inner);
            }
            Expr::RcAlloc(inner, _) => {
                self.output.push_str("rc ");
                self.format_expr(inner);
            }
            Expr::ArcAlloc(inner, _) => {
                self.output.push_str("arc ");
                self.format_expr(inner);
            }
            Expr::Lambda { params, body, .. } => {
                self.output.push('|');
                for (i, p) in params.iter().enumerate() {
                    if i > 0 {
                        self.output.push_str(", ");
                    }
                    self.format_param(p);
                }
                self.output.push_str("| ");
                self.format_expr(body);
            }
            Expr::Range { start, end, .. } => {
                self.format_expr(start);
                self.output.push_str("..");
                self.format_expr(end);
            }
            Expr::Await(inner, _) => {
                self.output.push_str("await ");
                self.format_expr(inner);
            }
            Expr::Try(inner, _) => {
                self.format_expr(inner);
                self.output.push('?');
            }
        }
    }

    fn format_match_arm(&mut self, arm: &MatchArm) {
        self.format_pattern(&arm.pattern);
        self.output.push_str(" => ");
        self.format_expr(&arm.body);
    }

    fn format_pattern(&mut self, pattern: &Pattern) {
        match pattern {
            Pattern::Wildcard(_) => self.output.push('_'),
            Pattern::Ident(name, _) => self.output.push_str(name),
            Pattern::Literal(expr) => self.format_expr(expr),
            Pattern::Constructor(name, patterns, _) => {
                self.output.push_str(name);
                if !patterns.is_empty() {
                    self.output.push('(');
                    for (i, p) in patterns.iter().enumerate() {
                        if i > 0 {
                            self.output.push_str(", ");
                        }
                        self.format_pattern(p);
                    }
                    self.output.push(')');
                }
            }
        }
    }

    fn format_binop(&mut self, op: BinOp) {
        let s = match op {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Mod => "%",
            BinOp::Eq => "==",
            BinOp::NotEq => "!=",
            BinOp::Lt => "<",
            BinOp::LtEq => "<=",
            BinOp::Gt => ">",
            BinOp::GtEq => ">=",
            BinOp::And => "and",
            BinOp::Or => "or",
        };
        self.output.push_str(s);
    }

    fn format_unaryop(&mut self, op: UnaryOp) {
        let s = match op {
            UnaryOp::Neg => "-",
            UnaryOp::Not => "not ",
            UnaryOp::Ref => "&",
            UnaryOp::RefMut => "&mut ",
            UnaryOp::Deref => "*",
        };
        self.output.push_str(s);
    }

    fn format_struct(&mut self, s: &StructDef) {
        if s.visibility == crate::parser::Visibility::Public {
            self.output.push_str("pub ");
        }
        self.output.push_str("struct ");
        self.output.push_str(&s.name);
        if !s.generics.is_empty() {
            self.output.push('[');
            for (i, g) in s.generics.iter().enumerate() {
                if i > 0 {
                    self.output.push_str(", ");
                }
                self.output.push_str(g);
            }
            self.output.push(']');
        }
        self.output.push('\n');
        self.indent_level += 1;
        for field in &s.fields {
            self.write_indent();
            self.output.push_str(&field.name);
            self.output.push_str(": ");
            self.format_type(&field.ty);
            self.output.push('\n');
        }
        self.indent_level -= 1;
    }

    fn format_enum(&mut self, e: &EnumDef) {
        if e.visibility == crate::parser::Visibility::Public {
            self.output.push_str("pub ");
        }
        self.output.push_str("enum ");
        self.output.push_str(&e.name);
        if !e.generics.is_empty() {
            self.output.push('[');
            for (i, g) in e.generics.iter().enumerate() {
                if i > 0 {
                    self.output.push_str(", ");
                }
                self.output.push_str(g);
            }
            self.output.push(']');
        }
        self.output.push('\n');
        self.indent_level += 1;
        for variant in &e.variants {
            self.write_indent();
            self.output.push_str(&variant.name);
            if !variant.fields.is_empty() {
                self.output.push('(');
                for (i, ty) in variant.fields.iter().enumerate() {
                    if i > 0 {
                        self.output.push_str(", ");
                    }
                    self.format_type(ty);
                }
                self.output.push(')');
            }
            self.output.push('\n');
        }
        self.indent_level -= 1;
    }

    fn format_trait(&mut self, t: &TraitDef) {
        self.output.push_str("trait ");
        self.output.push_str(&t.name);
        self.output.push('\n');
        // TODO: format trait methods
    }

    fn format_impl(&mut self, i: &ImplDef) {
        self.output.push_str("impl ");
        if let Some(trait_name) = &i.trait_name {
            self.output.push_str(trait_name);
            self.output.push_str(" for ");
        }
        self.format_type(&i.target_type);
        self.output.push('\n');
        self.indent_level += 1;
        for func in &i.methods {
            self.write_indent();
            self.format_function(func);
        }
        self.indent_level -= 1;
    }

    fn format_extern_fn(&mut self, e: &ExternFn) {
        self.output.push_str("extern fn ");
        self.output.push_str(&e.name);
        self.output.push('(');
        for (i, param) in e.params.iter().enumerate() {
            if i > 0 {
                self.output.push_str(", ");
            }
            self.format_param(param);
        }
        self.output.push(')');
        if let Some(ret) = &e.return_type {
            self.output.push_str(" -> ");
            self.format_type(ret);
        }
        self.output.push('\n');
    }

    fn format_use(&mut self, u: &UseStmt) {
        self.output.push_str("use ");
        self.output.push_str(&u.path.join("."));
        if u.wildcard {
            self.output.push_str(".*");
        }
        if let Some(alias) = &u.alias {
            self.output.push_str(" as ");
            self.output.push_str(alias);
        }
        self.output.push('\n');
    }

    fn write_indent(&mut self) {
        for _ in 0..(self.indent_level * self.config.indent_size) {
            self.output.push(' ');
        }
    }
}

/// Format WASD source code.
pub fn format_source(source: &str) -> Result<String, String> {
    let mut parser = crate::parser::Parser::new(source);
    let program = parser.parse()?;

    let mut formatter = Formatter::new(FormatConfig::default());
    Ok(formatter.format(&program))
}
