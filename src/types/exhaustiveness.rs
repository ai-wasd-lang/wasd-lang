//! Pattern matching exhaustiveness checker.
//!
//! Ensures that match expressions cover all possible cases.

use crate::parser::{EnumDef, MatchArm, Pattern, Variant};
use std::collections::{HashMap, HashSet};

/// Exhaustiveness checker for pattern matching.
pub struct ExhaustivenessChecker {
    /// Registered enum definitions: name -> variants
    enums: HashMap<String, Vec<Variant>>,
}

impl ExhaustivenessChecker {
    /// Create a new exhaustiveness checker.
    pub fn new() -> Self {
        Self {
            enums: HashMap::new(),
        }
    }

    /// Register an enum definition.
    pub fn register_enum(&mut self, enum_def: &EnumDef) {
        self.enums
            .insert(enum_def.name.clone(), enum_def.variants.clone());
    }

    /// Check if match arms are exhaustive for a given enum type.
    /// Returns Ok(()) if exhaustive, or Err with missing variants.
    pub fn check_match(
        &self,
        enum_name: &str,
        arms: &[MatchArm],
    ) -> Result<(), Vec<String>> {
        // Get the enum definition
        let variants = match self.enums.get(enum_name) {
            Some(v) => v,
            None => return Ok(()), // Unknown enum, skip check
        };

        // Check for wildcard or catch-all pattern
        for arm in arms {
            if is_catch_all(&arm.pattern) {
                return Ok(());
            }
        }

        // Collect covered variants
        let mut covered: HashSet<String> = HashSet::new();
        for arm in arms {
            if let Some(variant_name) = get_variant_name(&arm.pattern, enum_name) {
                covered.insert(variant_name);
            }
        }

        // Find missing variants
        let mut missing: Vec<String> = Vec::new();
        for variant in variants {
            if !covered.contains(&variant.name) {
                missing.push(format!("{}::{}", enum_name, variant.name));
            }
        }

        if missing.is_empty() {
            Ok(())
        } else {
            Err(missing)
        }
    }
}

impl Default for ExhaustivenessChecker {
    fn default() -> Self {
        Self::new()
    }
}

/// Check if a pattern is a catch-all (wildcard or simple identifier).
fn is_catch_all(pattern: &Pattern) -> bool {
    match pattern {
        Pattern::Wildcard(_) => true,
        Pattern::Ident(name, _) => !name.contains("::"), // Simple identifier binding
        _ => false,
    }
}

/// Extract the variant name from a constructor pattern.
fn get_variant_name(pattern: &Pattern, enum_name: &str) -> Option<String> {
    match pattern {
        Pattern::Constructor(name, _, _) => {
            // Handle both "EnumName::Variant" and just "Variant" patterns
            if name.contains("::") {
                // Qualified pattern like "Option::Some"
                let parts: Vec<&str> = name.split("::").collect();
                if parts.len() == 2 && parts[0] == enum_name {
                    Some(parts[1].to_string())
                } else {
                    None
                }
            } else {
                // Unqualified pattern - assume it matches this enum
                Some(name.clone())
            }
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Span;

    fn make_span() -> Span {
        Span { start: 0, end: 1 }
    }

    fn make_variant(name: &str, fields: Vec<crate::parser::Type>) -> Variant {
        Variant {
            name: name.to_string(),
            fields,
            span: make_span(),
        }
    }

    fn make_enum(name: &str, variants: Vec<Variant>) -> EnumDef {
        EnumDef {
            visibility: crate::parser::Visibility::Public,
            name: name.to_string(),
            generics: vec![],
            variants,
            span: make_span(),
        }
    }

    fn make_arm(pattern: Pattern) -> MatchArm {
        MatchArm {
            pattern,
            body: crate::parser::Expr::Int(0, make_span()),
            span: make_span(),
        }
    }

    #[test]
    fn test_exhaustive_match() {
        let mut checker = ExhaustivenessChecker::new();

        let option_enum = make_enum(
            "Option",
            vec![
                make_variant("Some", vec![crate::parser::Type::Named("i64".to_string())]),
                make_variant("None", vec![]),
            ],
        );
        checker.register_enum(&option_enum);

        let arms = vec![
            make_arm(Pattern::Constructor(
                "Option::Some".to_string(),
                vec![Pattern::Ident("x".to_string(), make_span())],
                make_span(),
            )),
            make_arm(Pattern::Constructor(
                "Option::None".to_string(),
                vec![],
                make_span(),
            )),
        ];

        assert!(checker.check_match("Option", &arms).is_ok());
    }

    #[test]
    fn test_non_exhaustive_match() {
        let mut checker = ExhaustivenessChecker::new();

        let option_enum = make_enum(
            "Option",
            vec![
                make_variant("Some", vec![crate::parser::Type::Named("i64".to_string())]),
                make_variant("None", vec![]),
            ],
        );
        checker.register_enum(&option_enum);

        // Only cover Some, missing None
        let arms = vec![make_arm(Pattern::Constructor(
            "Option::Some".to_string(),
            vec![Pattern::Ident("x".to_string(), make_span())],
            make_span(),
        ))];

        let result = checker.check_match("Option", &arms);
        assert!(result.is_err());
        let missing = result.unwrap_err();
        assert_eq!(missing, vec!["Option::None"]);
    }

    #[test]
    fn test_wildcard_makes_exhaustive() {
        let mut checker = ExhaustivenessChecker::new();

        let option_enum = make_enum(
            "Option",
            vec![
                make_variant("Some", vec![crate::parser::Type::Named("i64".to_string())]),
                make_variant("None", vec![]),
            ],
        );
        checker.register_enum(&option_enum);

        // Only wildcard pattern
        let arms = vec![make_arm(Pattern::Wildcard(make_span()))];

        assert!(checker.check_match("Option", &arms).is_ok());
    }

    #[test]
    fn test_identifier_makes_exhaustive() {
        let mut checker = ExhaustivenessChecker::new();

        let option_enum = make_enum(
            "Option",
            vec![
                make_variant("Some", vec![crate::parser::Type::Named("i64".to_string())]),
                make_variant("None", vec![]),
            ],
        );
        checker.register_enum(&option_enum);

        // Identifier binding catches all
        let arms = vec![make_arm(Pattern::Ident("x".to_string(), make_span()))];

        assert!(checker.check_match("Option", &arms).is_ok());
    }
}
