// Typing tests for the Fun language (let-bindings, conditionals, lambdas)
//
// ## Parser depth bounds
//
// MetaParser defaults to max_depth=256, which causes OOM when tests run in
// parallel.  Each test caps depth with `valid_depth_for(input)` (10 +
// byte_len/2), which was calibrated for Fun and appears sufficient here.
//
// HACKY: no formal proof this bound is tight for all Fun inputs; if tests
// start failing with "No parse results after trying depths …", raise the cap.
use crate::logic::grammar::Grammar;
use crate::logic::partial::MetaParser;
use crate::logic::typing::core::TreeStatus;
use crate::logic::typing::eval::check_tree;
use crate::logic::Parser;
use crate::validation::parseable::valid_depth_for;
use crate::{add_module_filter, set_debug_level};

fn load_grammar() -> Grammar {
    let spec = include_str!("../../../../examples/fun.auf");
    Grammar::load(spec).expect("Failed to load Fun grammar")
}

/// Assert that at least one complete parse is well-typed.
///
/// Reuses a single `MetaParser` instance across checks.
fn assert_well_typed_with_level(p: &mut MetaParser, input: &str, level: crate::DebugLevel) {
    set_debug_level(level);
    let ast = p
        .partial(input)
        .expect(&format!("Failed to parse '{}'", input));
    let completes = ast.completes();
    println!(
        "\n=== Checking '{}': {} roots, {} complete ===\n",
        input,
        ast.roots().len(),
        completes.len()
    );
    assert!(!completes.is_empty(), "No complete parse for '{}'", input);

    // Borrow the grammar after `p.partial()` so mutable/immutable borrows don't overlap
    let g = &p.parser().grammar;

    let any_ok = completes.iter().any(|tree| {
        println!("Checking tree: {}", tree);
        let status = check_tree(tree, g);
        println!("  '{}' -> {:?}", input, status);
        status.is_ok()
    });
    assert!(any_ok, "Expected '{}' to be well-typed", input);
}

fn assert_well_typed(p: &mut MetaParser, input: &str) {
    assert_well_typed_with_level(p, input, crate::DebugLevel::Info);
}

/// Assert that all complete parses are malformed.
fn assert_malformed(g: &Grammar, input: &str) {
    set_debug_level(crate::DebugLevel::Info);
    let mut parser = Parser::new(g.clone()).with_max_recursion(15);
    let ast = parser
        .partial(input)
        .into_result()
        .expect(&format!("Failed to parse '{}'", input));
    let completes = ast.completes();
    assert!(
        !completes.is_empty(),
        "No complete parse for '{}' (need completes to check malformed)",
        input
    );

    let all_malformed = completes.iter().all(|tree| {
        let status = check_tree(tree, g);
        println!("  '{}' -> {:?}", input, status);
        matches!(status, TreeStatus::Malformed)
    });
    assert!(
        all_malformed,
        "Expected all trees for '{}' to be Malformed",
        input
    );
}

// ============================================================================
// Debug: prefix parsing
// ============================================================================

#[test]
fn test_debug_lambda_prefixes() {
    set_debug_level(crate::DebugLevel::Info);
    let g = load_grammar();
    println!("\n=== Debug Lambda Prefixes ===");
    let mut p = MetaParser::new(g.clone()).with_max_depth(valid_depth_for("1.0 +. 2.7"));
    for prefix in &["1.0 +. 2.7"] {
        match p.partial(prefix) {
            Ok(ast) => {
                println!(
                    "'{}' -> OK ({} roots, {} complete)",
                    prefix,
                    ast.roots().len(),
                    ast.completes().len()
                );
                let complete = ast.typed_complete(&g).unwrap();
                // typecheck
                println!("{}", complete);
            }
            Err(e) => println!("'{}' -> ERR: {}", prefix, e),
        }
    }
}

#[test]
fn test_float_operator_prefix_behavior() {
    let g = load_grammar();
    let mut p = MetaParser::new(g.clone()).with_max_depth(41);

    // Structural parsing should succeed on these prefixes.
    for prefix in ["1.0 +", "10.0 /"] {
        let ast = p
            .partial(prefix)
            .unwrap_or_else(|e| panic!("Expected structural parse for '{}': {}", prefix, e));
        assert!(
            !ast.is_empty(),
            "Expected at least one root for '{}'",
            prefix
        );
    }

    // Typed partial parsing should remain available on these arithmetic-prefix
    // states to support completion.
    for prefix in ["1.0 +", "10.0 /"] {
        let typed = p.partial_typed(prefix);
        assert!(
            typed.is_ok(),
            "Expected typed partial to succeed for '{}', got {:?}",
            prefix,
            typed.err()
        );
    }
}

// ============================================================================
// Integer and Boolean literals
// ============================================================================

#[test]
fn test_integer_literal() {
    let g = load_grammar();
    println!("\n=== Integer Literal ===");
    let mut p = MetaParser::new(g.clone()).with_max_depth(valid_depth_for("42"));
    assert_well_typed(&mut p, "42");
}

#[test]
fn test_boolean_true() {
    let g = load_grammar();
    println!("\n=== Boolean True ===");
    let mut p = MetaParser::new(g.clone()).with_max_depth(valid_depth_for("true"));
    assert_well_typed(&mut p, "true");
}

#[test]
fn test_boolean_false() {
    let g = load_grammar();
    println!("\n=== Boolean False ===");
    let mut p = MetaParser::new(g.clone()).with_max_depth(valid_depth_for("false"));
    assert_well_typed(&mut p, "false");
}

#[test]
fn test_float_addition_operator() {
    let g = load_grammar();
    let input = "1.0 +. 2.5";
    let mut p = MetaParser::new(g.clone()).with_max_depth(valid_depth_for(input));
    assert_well_typed(&mut p, input);
}

#[test]
fn test_float_division_operator() {
    let g = load_grammar();
    let input = "10.0 /. 2.0";
    let mut p = MetaParser::new(g.clone()).with_max_depth(valid_depth_for(input));
    assert_well_typed(&mut p, input);
}

// ============================================================================
// Lambda abstractions
// ============================================================================

#[test]
fn test_identity_lambda() {
    let g = load_grammar();
    println!("\n=== Identity Lambda ===");
    let mut p = MetaParser::new(g.clone()).with_max_depth(valid_depth_for("(x: Int) => x"));
    assert_well_typed(&mut p, "(x: Int) => x");
}

#[test]
fn test_lambda_constant() {
    let g = load_grammar();
    println!("\n=== Lambda Constant Body ===");
    let mut p = MetaParser::new(g.clone()).with_max_depth(valid_depth_for("(x: Int) => 42"));
    assert_well_typed(&mut p, "(x: Int) => 42");
}

#[test]
fn test_lambda_bool_body() {
    let g = load_grammar();
    println!("\n=== Lambda Bool Body ===");
    let mut p = MetaParser::new(g.clone()).with_max_depth(valid_depth_for("(x: Int) => true"));
    assert_well_typed(&mut p, "(x: Int) => true");
}

#[test]
fn test_nested_lambda() {
    let g = load_grammar();
    println!("\n=== Nested Lambda ===");
    let mut p =
        MetaParser::new(g.clone()).with_max_depth(valid_depth_for("(x: Int) => (y: Bool) => x"));
    assert_well_typed(&mut p, "(x: Int) => (y: Bool) => x");
}

#[test]
fn test_lambda_unbound_body() {
    let g = load_grammar();
    println!("\n=== Lambda Unbound Body ===");
    assert_malformed(&g, "(x: Int) => z");
}

// ============================================================================
// Let bindings
// ============================================================================

#[test]
fn test_let_int() {
    let g = load_grammar();
    println!("\n=== Let Int ===");
    let mut p = MetaParser::new(g.clone()).with_max_depth(valid_depth_for("let x: Int = 42; x"));
    assert_well_typed(&mut p, "let x: Int = 42; x");
}

#[test]
fn test_let_bool() {
    let g = load_grammar();
    println!("\n=== Let Bool ===");
    let mut p = MetaParser::new(g.clone()).with_max_depth(valid_depth_for("let b: Bool = true; b"));
    assert_well_typed(&mut p, "let b: Bool = true; b");
}

#[test]
fn test_nested_let() {
    let g = load_grammar();
    println!("\n=== Nested Let ===");
    let mut p = MetaParser::new(g.clone())
        .with_max_depth(valid_depth_for("let x: Int = 1; let y: Int = 2; x"));
    assert_well_typed(&mut p, "let x: Int = 1; let y: Int = 2; x");
}

#[test]
fn test_let_type_mismatch() {
    let g = load_grammar();
    println!("\n=== Let Type Mismatch ===");
    assert_malformed(&g, "let x: Int = true; x");
}

#[test]
fn test_let_unbound_in_body() {
    let g = load_grammar();
    println!("\n=== Let Unbound In Body ===");
    assert_malformed(&g, "let x: Int = 42; z");
}

// ============================================================================
// Unbound variables
// ============================================================================

#[test]
fn test_bare_unbound_variable() {
    let g = load_grammar();
    println!("\n=== Bare Unbound Variable ===");
    assert_malformed(&g, "x");
}

// ============================================================================
// Application
// ============================================================================

#[test]
fn test_fun_applied_to_arg() {
    let g = load_grammar();
    println!("\n=== fun Applied to Arg ===");
    let input = "(f: Int -> Int) => ((x: Int) => f(x))";
    let mut p = MetaParser::new(g.clone()).with_max_depth(valid_depth_for(input));
    add_module_filter("meta");
    add_module_filter("parser2");
    add_module_filter("typing");
    assert_well_typed_with_level(&mut p, input, crate::DebugLevel::Info);
}
