use super::Type;
use std::fmt;

#[derive(Debug, Clone)]
pub struct TypeSyntaxConfig {
    pub empty_kw: &'static str,
    pub universe_kw: &'static str,
    pub arrow: Vec<&'static str>,
    pub negation: Vec<&'static str>,
}

// variable syntax
// I like types
impl Default for TypeSyntaxConfig {
    fn default() -> Self {
        Self {
            empty_kw: "∅",
            universe_kw: "⊤",
            arrow: vec!["->", "→"],
            negation: vec!["¬", "!"],
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Atom(s) => write!(f, "{}", s),
            Type::Raw(s) => write!(f, "'{}'", s),
            Type::Meta(s) => write!(f, "?{}", s),
            Type::Arrow(l, r) => write!(f, "{} → {}", l, r),
            Type::Tuple(t) => write!(f, "({}...)", t),
            Type::Not(t) => write!(f, "¬{}", t),
            Type::ContextCall(ctx, var) => write!(f, "{}({})", ctx, var),
            Type::Universe => write!(f, "⊤"),
            Type::Empty => write!(f, "∅"),
        }
    }
}

impl TypeSyntaxConfig {
    pub fn allowed_chars(&self) -> String {
        let mut chars = String::from(
            "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_λτ₁₂₃₄₅₆₇₈₉₀ ()[],.;",
        );
        for token in self.arrow.iter().chain(self.negation.iter()) {
            for c in token.chars() {
                if !chars.contains(c) {
                    chars.push(c);
                }
            }
        }

        chars
    }
}

impl Type {
    pub fn parse(type_str: &str) -> Result<Self, String> {
        Self::parse_with_config(type_str, &TypeSyntaxConfig::default())
    }

    pub fn parse_with_config(type_str: &str, cfg: &TypeSyntaxConfig) -> Result<Self, String> {
        let s = type_str.trim();
        if s.is_empty() {
            return Err("Type expression cannot be empty".into());
        }
        if s == cfg.universe_kw {
            return Ok(Type::Universe);
        }
        if s == cfg.empty_kw {
            return Ok(Type::Empty);
        }

        if s.starts_with('\'') && s.ends_with('\'') && s.len() > 2 {
            let raw_type = &s[1..s.len() - 1]; // Remove quotes
            return Ok(Type::Raw(raw_type.to_string()));
        }

        // Parse tuple types (meta types, cool)
        if s.starts_with('(') && s.ends_with("...)") && s.len() > 5 {
            let inner = s[1..s.len() - 4].trim();
            // Check if it's a simple identifier (meta type / tuple)
            if inner
                .chars()
                .all(|c| c.is_alphanumeric() || c == '_' || "λτ₁₂₃₄₅₆₇₈₉₀".contains(c))
                && !inner.is_empty()
            {
                return Ok(Type::Tuple(inner.to_string()));
            }
        }

        // Parse regular parenthesized expressions
        if s.starts_with('(') && s.ends_with(')') && is_outer_paren_pair(s) {
            return Self::parse_with_config(&s[1..s.len() - 1], cfg);
        }

        // Arrow types are RIGHT-associative: A -> B -> C  ==  A -> (B -> C)
        // So we split on the FIRST arrow outside parens
        if let Some((pos, tok_len)) = find_first_outside_parens(s, &cfg.arrow) {
            return Ok(Type::Arrow(
                Box::new(Self::parse_with_config(&s[..pos], cfg)?),
                Box::new(Self::parse_with_config(&s[pos + tok_len..], cfg)?),
            ));
        }

        if let Some(tok) = cfg.negation.iter().find(|t| s.starts_with(**t)) {
            return Ok(Type::Not(Box::new(Self::parse_with_config(
                &s[tok.len()..],
                cfg,
            )?)));
        }

        // Parse context calls "Γ(x)", "Delta(y)"
        if let Some(paren_start) = s.find('(') {
            if let Some(paren_end) = s.find(')') {
                if paren_end > paren_start && paren_end == s.len() - 1 {
                    let context = s[..paren_start].trim();
                    let var = s[paren_start + 1..paren_end].trim();
                    if !context.is_empty() && !var.is_empty() {
                        // Validate context name contains only valid characters
                        if context.chars().all(|c| {
                            c.is_alphanumeric()
                                || c == '_'
                                || "ΓΔΘΛΣΦΨΩΞΠΡΤΥΧδγτλσφψωξπρυχ₁₂₃₄₅₆₇₈₉₀".contains(c)
                        }) {
                            return Ok(Type::ContextCall(context.to_string(), var.to_string()));
                        }
                    }
                }
            }
        }

        if s.chars()
            .all(|c| c.is_alphanumeric() || c == '_' || c == '?')
        {
            // Meta variables start with '?' (e.g., ?A, ?B, ?Result)
            if s.starts_with('?') && s.len() > 1 {
                return Ok(Type::Meta(s[1..].to_string()));
            }
            return Ok(Type::Atom(s.to_string()));
        }
        Err(format!("Invalid type expression: {}", s))
    }
}

const TYPE_CHARS: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_λτ→₁₂₃₄₅₆₇₈₉₀()!¬[] where,.;''?";
pub fn validate_type_expr(expr: &str) -> bool {
    !expr.is_empty() && expr.chars().all(|c| TYPE_CHARS.contains(c))
}

fn is_outer_paren_pair(s: &str) -> bool {
    if !s.starts_with('(') || !s.ends_with(')') {
        return false;
    }
    let mut depth = 0;
    for (i, c) in s.chars().enumerate() {
        match c {
            '(' => depth += 1,
            ')' => {
                depth -= 1;
                if depth == 0 && i != s.len() - 1 {
                    return false;
                }
            }
            _ => {}
        }
    }
    depth == 0
}

fn find_first_outside_parens(s: &str, tokens: &[&str]) -> Option<(usize, usize)> {
    let mut depth = 0;
    for (i, c) in s.char_indices() {
        match c {
            '(' => depth += 1,
            ')' if depth > 0 => depth -= 1,
            _ if depth == 0 => {
                for tok in tokens {
                    if s[i..].starts_with(tok) {
                        return Some((i, tok.len()));
                    }
                }
            }
            _ => {}
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use crate::logic::grammar::Grammar;
    use crate::logic::partial::parse::Parser;
    use crate::logic::typing::Type;
    use crate::logic::typing::core::{Context, TreeStatus};
    use crate::logic::typing::eval::check_tree_with_context;

    #[test]
    fn arrow_associativity() {
        // Arrow types should be RIGHT-associative:
        // A -> B -> C  should parse as  A -> (B -> C)
        let t = Type::parse("A->B->C").unwrap();
        println!("Parsed A->B->C as: {:?}", t);

        // If right-associative: Arrow(A, Arrow(B, C))
        // If left-associative: Arrow(Arrow(A, B), C)
        match &t {
            Type::Arrow(left, right) => {
                println!("  Left:  {:?}", left);
                println!("  Right: {:?}", right);

                // Right side should be Arrow(B, C) for right-associativity
                match right.as_ref() {
                    Type::Arrow(_, _) => println!("  => RIGHT-associative (correct)"),
                    _ => println!("  => LEFT-associative (WRONG!)"),
                }

                // For right-associativity: left = A, right = B->C
                assert!(
                    matches!(right.as_ref(), Type::Arrow(_, _)),
                    "A->B->C should be right-associative: A -> (B -> C), but got: {:?}",
                    t
                );
            }
            _ => panic!("Expected Arrow type"),
        }
    }

    #[test]
    fn curried_function_application_types() {
        // When f : A -> B -> C and x : A
        // Then (f x) should have type B -> C

        let f_type = Type::parse("A->B->C").unwrap();
        println!("f : {:?}", f_type);

        match &f_type {
            Type::Arrow(domain, codomain) => {
                println!("Domain (should be A): {:?}", domain);
                println!("Codomain (should be B->C): {:?}", codomain);

                // For curried application to work, domain must be simple A
                assert_eq!(
                    **domain,
                    Type::Atom("A".into()),
                    "For curried functions, domain should be A, not {:?}",
                    domain
                );
            }
            _ => panic!("Expected arrow type"),
        }
    }

    #[test]
    fn curried_application_prefix_f_x() {
        // Test the exact failing case: prefix 'λf:A->B->C.λx:A.λy:B.f x'
        // This is the application (f x) where f : A->B->C and x : A
        // Result type should be B->C

        let spec = include_str!("../../../examples/xtlc.spec");
        let grammar = Grammar::load(spec).expect("load xtlc grammar");
        let mut parser = Parser::new(grammar.clone());

        // Parse just the application part with context
        let input = "f x";
        let ctx = Context::new()
            .extend("f".into(), Type::parse("A->B->C").unwrap())
            .extend("x".into(), Type::Atom("A".into()));

        println!("Testing: '{}' with context f:A->B->C, x:A", input);

        let ast = parser.partial(input).expect("should parse");
        println!("Parse trees: {}", ast.roots.len());

        for (i, root) in ast.roots.iter().enumerate() {
            let status = check_tree_with_context(root, &grammar, &ctx);
            println!("  Tree {}: {:?} -> {:?}", i, root.name, status);
        }

        // At least one tree should type-check
        let has_valid = ast
            .roots
            .iter()
            .any(|r| check_tree_with_context(r, &grammar, &ctx).is_ok());

        assert!(has_valid, "f x should type-check when f:A->B->C and x:A");
    }

    #[test]
    fn full_curried_double_apply() {
        // Test: λf:A->B->C.λx:A.λy:B.f x y
        // This requires parsing f x y as ((f x) y)

        let spec = include_str!("../../../examples/xtlc.spec");
        let grammar = Grammar::load(spec).expect("load xtlc grammar");
        let mut parser = Parser::new(grammar.clone());

        let input = "λf:A->B->C.λx:A.λy:B.(f x) y";
        println!("Testing complete: '{}'", input);

        let ast = parser.partial(input).expect("should parse");
        println!("Parse trees: {}", ast.roots.len());

        for (i, root) in ast.roots.iter().enumerate() {
            let status = check_tree_with_context(root, &grammar, &Context::new());
            println!(
                "  Tree {}: complete={} -> {:?}",
                i,
                root.is_complete(),
                status
            );
        }

        let has_valid = ast
            .roots
            .iter()
            .any(|r| check_tree_with_context(r, &grammar, &Context::new()).is_ok());

        assert!(has_valid, "λf:A->B->C.λx:A.λy:B.(f x) y should type-check");
    }

    #[test]
    fn failing_prefix_f_x_in_lambda() {
        // Test the EXACT failing prefix: 'λf:A->B->C.λx:A.λy:B.f x'
        // The full thing would be: λf:A->B->C.λx:A.λy:B.f x y

        let spec = include_str!("../../../examples/xtlc.spec");
        let grammar = Grammar::load(spec).expect("load xtlc grammar");
        let mut parser = Parser::new(grammar.clone());

        let input = "λf:A->B->C.λx:A.λy:B.f x";
        println!("\n=== Testing exact failing prefix ===");
        println!("Input: '{}'", input);

        let ast = parser.partial(input).expect("should parse");
        println!("Parse trees: {}", ast.roots.len());

        let mut valid_count = 0;
        let mut partial_count = 0;
        let mut malformed_count = 0;

        for (i, root) in ast.roots.iter().enumerate() {
            let status = check_tree_with_context(root, &grammar, &Context::new());
            match &status {
                crate::logic::typing::core::TreeStatus::Valid(_) => valid_count += 1,
                crate::logic::typing::core::TreeStatus::Partial(_) => partial_count += 1,
                crate::logic::typing::core::TreeStatus::Malformed => malformed_count += 1,
                _ => {}
            }
            if i < 10 || status.is_ok() {
                println!("  Tree {}: {} -> {:?}", i, root.name, status);
            }
        }

        println!(
            "\nSummary: {} valid, {} partial, {} malformed",
            valid_count, partial_count, malformed_count
        );

        let has_valid = ast
            .roots
            .iter()
            .any(|r| check_tree_with_context(r, &grammar, &Context::new()).is_ok());

        assert!(
            has_valid,
            "Prefix 'λf:A->B->C.λx:A.λy:B.f x' should have at least one well-typed tree"
        );
    }

    #[test]
    fn test_chained_application_f_x_y() {
        // Test that f x y parses and type-checks as (f x) y
        let spec = include_str!("../../../examples/xtlc.spec");
        let grammar = Grammar::load(spec).expect("load xtlc grammar");
        let mut parser = Parser::new(grammar.clone());

        // Test with explicit context: f:A->B->C, x:A, y:B
        let input = "f x y";
        let ctx = Context::new()
            .extend("f".into(), Type::parse("A->B->C").unwrap())
            .extend("x".into(), Type::Atom("A".into()))
            .extend("y".into(), Type::Atom("B".into()));

        println!("\n=== Testing chained application f x y ===");
        println!("Input: '{}' with f:A->B->C, x:A, y:B", input);

        let ast = parser.partial(input).expect("should parse");
        println!("Parse trees: {}", ast.roots.len());

        for (i, root) in ast.roots.iter().enumerate() {
            let status = check_tree_with_context(root, &grammar, &ctx);
            if status.is_ok() {
                println!(
                    "  Tree {}: {} complete={} -> {:?}",
                    i,
                    root.name,
                    root.is_complete(),
                    status
                );
            }
        }

        let has_valid = ast
            .roots
            .iter()
            .any(|r| check_tree_with_context(r, &grammar, &ctx).is_ok());

        assert!(
            has_valid,
            "f x y should type-check when f:A->B->C, x:A, y:B"
        );
    }

    #[test]
    fn test_double_apply_in_lambda() {
        // Test: λf:A->B->C.λx:A.λy:B.f x y
        // This is the exact failing test case
        let spec = include_str!("../../../examples/xtlc.spec");
        let grammar = Grammar::load(spec).expect("load xtlc grammar");

        println!("\n=== Testing progressively ===");

        // Test 1: Simple identity
        let mut p1 = Parser::new(grammar.clone());
        let ast1 = p1.partial("λf:A->B.f").expect("parse");
        let valid1 = ast1
            .roots
            .iter()
            .any(|r| check_tree_with_context(r, &grammar, &Context::new()).is_ok());
        println!("'λf:A->B.f': {}", if valid1 { "✓" } else { "✗" });

        // Test 2: Simple application
        let mut p2 = Parser::new(grammar.clone());
        let ast2 = p2.partial("λf:A->B.λx:A.f x").expect("parse");
        let valid2 = ast2
            .roots
            .iter()
            .any(|r| check_tree_with_context(r, &grammar, &Context::new()).is_ok());
        println!("'λf:A->B.λx:A.f x': {}", if valid2 { "✓" } else { "✗" });

        // Test 3: Inner lambda with body that uses outer vars
        // λy:B.f x y  where f:A->B->C, x:A are in context
        let ctx3 = Context::new()
            .extend("f".into(), Type::parse("A->B->C").unwrap())
            .extend("x".into(), Type::Atom("A".into()));
        let mut p3 = Parser::new(grammar.clone());
        let ast3 = p3.partial("λy:B.f x y").expect("parse");
        let valid3 = ast3
            .roots
            .iter()
            .any(|r| check_tree_with_context(r, &grammar, &ctx3).is_ok());
        println!(
            "'λy:B.f x y' with ctx {{f:A->B->C, x:A}}: {}",
            if valid3 { "✓" } else { "✗" }
        );

        // Test 4: Just the body f x y with full context
        let ctx4 = Context::new()
            .extend("f".into(), Type::parse("A->B->C").unwrap())
            .extend("x".into(), Type::Atom("A".into()))
            .extend("y".into(), Type::Atom("B".into()));
        let mut p4 = Parser::new(grammar.clone());
        let ast4 = p4.partial("f x y").expect("parse");
        let valid4 = ast4
            .roots
            .iter()
            .any(|r| check_tree_with_context(r, &grammar, &ctx4).is_ok());
        println!(
            "'f x y' with ctx {{f:A->B->C, x:A, y:B}}: {}",
            if valid4 { "✓" } else { "✗" }
        );

        // Test 5: Full expression
        let mut p5 = Parser::new(grammar.clone());
        let ast5 = p5.partial("λf:A->B->C.λx:A.λy:B.f x y").expect("parse");
        let valid5 = ast5
            .roots
            .iter()
            .filter(|r| check_tree_with_context(r, &grammar, &Context::new()).is_ok())
            .count();
        println!("'λf:A->B->C.λx:A.λy:B.f x y': {} valid trees", valid5);

        assert!(valid3, "λy:B.f x y with context should type-check");
    }
}
