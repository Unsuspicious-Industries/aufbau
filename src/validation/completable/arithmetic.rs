//! Arithmetic Expression Tests
//!
//! Tests typed completion for simple arithmetic expressions:
//! - Numbers and identifiers
//! - Binary operators (+, -, *, /)
//! - Parenthesized expressions

#![allow(dead_code)]

use super::*;

// ============================================================================
// Suite Definitions (used by validate binary)
// ============================================================================

pub fn suites() -> Vec<(&'static str, Grammar, Vec<TypedCompletionTestCase>)> {
    let g = arithmetic_grammar();
    vec![("arithmetic::completable", g, completable_cases())]
}

use TypedCompletionTestCase as T;

fn completable_cases() -> Vec<TypedCompletionTestCase> {
    vec![
        T::ok("empty", "", 3),
        T::ok("single digit", "1", 1),
        T::ok("multi digit", "42", 1),
        T::ok("large number", "9999", 1),
        T::ok("simple var", "x", 1),
        T::ok("longer var", "abc", 1),
        T::ok("var with digits", "x1", 1),
        T::ok("add prefix", "1 +", 2),
        T::ok("sub prefix", "x -", 2),
        T::ok("mul prefix", "2 *", 2),
        T::ok("div prefix", "y /", 2),
        T::ok("simple add", "1 + 2", 1),
        T::ok("chain ops", "1 + 2 * 3", 1),
        T::ok("open paren", "(", 3),
        T::ok("paren number", "(42", 2),
        T::ok("closed paren", "(42)", 1),
        T::ok("nested parens", "((1))", 2),
        T::ok("complex paren", "(x + y) * z", 2),
    ]
}

// ============================================================================
// Grammar
// ============================================================================

/// Simple arithmetic grammar - no typing rules
const ARITHMETIC_GRAMMAR: &str = r#"
    Number ::= /[0-9]+/
    Identifier ::= /[a-z][a-zA-Z0-9]*/
    Literal ::= Number
    Variable ::= Identifier
    Operator ::= '+' | '-' | '*' | '/'
    Primary ::= Literal | Variable | '(' Expression ')'
    Expression ::= Primary | Primary Operator Expression
"#;

fn arithmetic_grammar() -> Grammar {
    load_inline_grammar(ARITHMETIC_GRAMMAR)
}

// ============================================================================
// Batch Test Cases
// ============================================================================

#[test]
fn check_completable() {
    let grammar = arithmetic_grammar();
    let res = run_test_batch(&grammar, &completable_cases());
    res.assert_all_passed();
}

#[test]
#[ignore = "debug probe"]
fn debug_arith_completions() {
    use crate::logic::partial::MetaParser;
    use crate::logic::typing::Context;
    use crate::validation::completability::complete;
    let grammar = arithmetic_grammar();
    let input = "1 +";
    let ctx = Context::new();

    let mut meta = MetaParser::new(grammar.clone());

    // First parse
    let (partial1, depth1) = meta.partial_with_depth(input).unwrap();
    println!(
        "First parse: depth={} roots={} last_used={:?}",
        depth1,
        partial1.roots().len(),
        meta.last_used_depth()
    );

    let typed1 = partial1.typed_ctx(&grammar, &ctx).unwrap();
    let comps1 = typed1.completions(&grammar);
    println!(
        "Completions1: {:?}",
        comps1.iter().map(|t| t.to_string()).collect::<Vec<_>>()
    );

    // Simulate Synthesizer.depth_local_window(depth1)
    let floor = depth1.max(1);
    let ceil = depth1 + 2;
    println!("Second parse with bounds [{}, {}]:", floor, ceil);

    // Second parse (what parse_with_hint does with a hint)
    match meta.partial_with_bounds(input, floor, ceil) {
        Ok((partial2, depth2)) => {
            println!(
                "Second parse OK: depth={} roots={}",
                depth2,
                partial2.roots().len()
            );
            match partial2.typed_ctx(&grammar, &ctx) {
                Ok(typed2) => {
                    let comps2 = typed2.completions(&grammar);
                    println!(
                        "Completions2: {:?}",
                        comps2.iter().map(|t| t.to_string()).collect::<Vec<_>>()
                    );
                }
                Err(e) => println!("typed_ctx2 FAILED: {}", e),
            }
        }
        Err(e) => {
            println!("Second parse FAILED: {}", e);
            for d in floor..=ceil {
                match meta.partial_with_bounds(input, d, d) {
                    Ok((p, dep)) => println!("  depth={} OK: roots={}", dep, p.roots().len()),
                    Err(e2) => println!("  depth={} FAILED: {}", d, e2),
                }
            }
        }
    }

    let result = complete(&grammar, input, 4, Some(ctx));
    println!("Complete result: {:?}", result);
}

#[test]
#[ignore = "debug probe 2 - cache poisoning"]
fn debug_arith_cache_poisoning() {
    use crate::logic::partial::MetaParser;
    let grammar = arithmetic_grammar();
    let input = "1 +";

    // Test 1: fresh meta, single parse at depth 5
    {
        let mut meta = MetaParser::new(grammar.clone());
        let r = meta.partial_with_bounds(input, 5, 5);
        println!(
            "Fresh meta, bounds [5,5]: {}",
            match &r {
                Ok((a, d)) => format!("OK roots={} depth={}", a.roots().len(), d),
                Err(e) => format!("FAIL: {}", e),
            }
        );
    }

    // Test 2: parse once unconstrained, then try [5,5]
    {
        let mut meta = MetaParser::new(grammar.clone());
        let r1 = meta.partial_with_depth(input);
        println!(
            "After unconstrained parse (depth={:?}), bounds [5,5]:",
            r1.as_ref().map(|(_, d)| *d).ok()
        );
        let r2 = meta.partial_with_bounds(input, 5, 5);
        println!(
            "  result: {}",
            match &r2 {
                Ok((a, d)) => format!("OK roots={} depth={}", a.roots().len(), d),
                Err(e) => format!("FAIL: {}", e),
            }
        );
    }

    // Test 3: fresh meta, clear cache between calls
    {
        let mut meta = MetaParser::new(grammar.clone());
        let _ = meta.partial_with_depth(input);
        meta.clear_cache();
        let r2 = meta.partial_with_bounds(input, 5, 5);
        println!(
            "After clear_cache, bounds [5,5]: {}",
            match &r2 {
                Ok((a, d)) => format!("OK roots={} depth={}", a.roots().len(), d),
                Err(e) => format!("FAIL: {}", e),
            }
        );
    }
}
