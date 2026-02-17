use crate::logic::Parser;
// Typing tests for the Fun language (let-bindings, conditionals, lambdas)
use crate::logic::grammar::Grammar;
use crate::logic::partial::MetaParser;
use crate::logic::typing::core::TreeStatus;
use crate::logic::typing::eval::check_tree;
use crate::{add_module_filter, set_debug_level};

fn load_grammar() -> Grammar {
    let spec = include_str!("../../../../examples/fun.auf");
    Grammar::load(spec).expect("Failed to load Fun grammar")
}

/// Helper: parse input, get complete trees, check that at least one is well-typed
///
/// Accept a shared `MetaParser` so the grammar start-depth cache (`gscache`)
/// can be reused across multiple calls.
fn assert_well_typed_with_level(p: &mut MetaParser, input: &str, level: crate::DebugLevel) {
    set_debug_level(level);
    let ast = p
        .partial(input)
        .expect(&format!("Failed to parse '{}'", input));
    let completes = ast.completes();
    println!(
        "\n=== Checking '{}': {} roots, {} complete ===\n",
        input,
        ast.roots.len(),
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

/// Helper: parse input, get complete trees, check that ALL are malformed
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
    let mut p = MetaParser::new(g.clone());
    for prefix in &["1.0 +. 2.7"] {
        match p.partial(prefix) {
            Ok(ast) => {
                println!(
                    "'{}' -> OK ({} roots, {} complete)",
                    prefix,
                    ast.roots.len(),
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

// ============================================================================
// Integer and Boolean literals
// ============================================================================

#[test]
fn test_integer_literal() {
    let g = load_grammar();
    println!("\n=== Integer Literal ===");
    let mut p = MetaParser::new(g.clone());
    assert_well_typed(&mut p, "42");
}

#[test]
fn test_boolean_true() {
    let g = load_grammar();
    println!("\n=== Boolean True ===");
    let mut p = MetaParser::new(g.clone());
    assert_well_typed(&mut p, "true");
}

#[test]
fn test_boolean_false() {
    let g = load_grammar();
    println!("\n=== Boolean False ===");
    let mut p = MetaParser::new(g.clone());
    assert_well_typed(&mut p, "false");
}

// ============================================================================
// Lambda abstractions
// ============================================================================

#[test]
fn test_identity_lambda() {
    let g = load_grammar();
    println!("\n=== Identity Lambda ===");
    let mut p = MetaParser::new(g.clone());
    assert_well_typed(&mut p, "(x: Int) => x");
}

#[test]
fn test_lambda_constant() {
    let g = load_grammar();
    println!("\n=== Lambda Constant Body ===");
    let mut p = MetaParser::new(g.clone());
    assert_well_typed(&mut p, "(x: Int) => 42");
}

#[test]
fn test_lambda_bool_body() {
    let g = load_grammar();
    println!("\n=== Lambda Bool Body ===");
    let mut p = MetaParser::new(g.clone());
    assert_well_typed(&mut p, "(x: Int) => true");
}

#[test]
fn test_nested_lambda() {
    let g = load_grammar();
    println!("\n=== Nested Lambda ===");
    let mut p = MetaParser::new(g.clone());
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
    let mut p = MetaParser::new(g.clone());
    assert_well_typed(&mut p, "let x: Int = 42; x");
}

#[test]
fn test_let_bool() {
    let g = load_grammar();
    println!("\n=== Let Bool ===");
    let mut p = MetaParser::new(g.clone());
    assert_well_typed(&mut p, "let b: Bool = true; b");
}

#[test]
fn test_nested_let() {
    let g = load_grammar();
    println!("\n=== Nested Let ===");
    let mut p = MetaParser::new(g.clone());
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
    let mut p = MetaParser::new(g.clone());
    add_module_filter("meta");
    add_module_filter("parser2");
    add_module_filter("typing");
    assert_well_typed_with_level(
        &mut p,
        "(f: Int -> Int) => ((x: Int) => f(x))",
        crate::DebugLevel::Info,
    );
}
