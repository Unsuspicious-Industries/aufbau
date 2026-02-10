// Typing tests for the Fun language (let-bindings, conditionals, lambdas)

use anstream::println;

use crate::logic::grammar::Grammar;
use crate::logic::partial::parse::Parser;
use crate::logic::typing::core::TreeStatus;
use crate::logic::typing::eval::check_tree;

fn load_grammar() -> Grammar {
    let spec = include_str!("../../../../examples/fun.spec");
    Grammar::load(spec).expect("Failed to load Fun grammar")
}

/// Helper: parse input, get complete trees, check that at least one is well-typed
fn assert_well_typed(g: &Grammar, input: &str) {
    let mut parser = Parser::new(g.clone());
    let ast = parser
        .partial(input)
        .expect(&format!("Failed to parse '{}'", input));
    let completes = ast.completes();
    assert!(!completes.is_empty(), "No complete parse for '{}'", input);

    let any_ok = completes.iter().any(|tree| {
        let status = check_tree(tree, g);
        println!("  '{}' -> {:?}", input, status);
        status.is_ok()
    });
    assert!(any_ok, "Expected '{}' to be well-typed", input);
}

/// Helper: parse input, get complete trees, check that ALL are malformed
fn assert_malformed(g: &Grammar, input: &str) {
    let mut parser = Parser::new(g.clone());
    let ast = parser
        .partial(input)
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
    let g = load_grammar();
    println!("\n=== Debug Lambda Prefixes ===");
    for prefix in &["1.0 +. 2.7"] {
        let mut p = Parser::new(g.clone());
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
    assert_well_typed(&g, "42");
}

#[test]
fn test_boolean_true() {
    let g = load_grammar();
    println!("\n=== Boolean True ===");
    assert_well_typed(&g, "true");
}

#[test]
fn test_boolean_false() {
    let g = load_grammar();
    println!("\n=== Boolean False ===");
    assert_well_typed(&g, "false");
}

// ============================================================================
// Lambda abstractions
// ============================================================================

#[test]
fn test_identity_lambda() {
    let g = load_grammar();
    println!("\n=== Identity Lambda ===");
    assert_well_typed(&g, "λx:Int.x");
}

#[test]
fn test_lambda_constant() {
    let g = load_grammar();
    println!("\n=== Lambda Constant Body ===");
    assert_well_typed(&g, "λx:Int.42");
}

#[test]
fn test_lambda_bool_body() {
    let g = load_grammar();
    println!("\n=== Lambda Bool Body ===");
    assert_well_typed(&g, "λx:Int.true");
}

#[test]
fn test_nested_lambda() {
    let g = load_grammar();
    println!("\n=== Nested Lambda ===");
    assert_well_typed(&g, "λx:Int.λy:Bool.x");
}

#[test]
fn test_lambda_unbound_body() {
    let g = load_grammar();
    println!("\n=== Lambda Unbound Body ===");
    assert_malformed(&g, "λx:Int.z");
}

// ============================================================================
// Let bindings
// ============================================================================

#[test]
fn test_let_int() {
    let g = load_grammar();
    println!("\n=== Let Int ===");
    assert_well_typed(&g, "let x:Int=42 in x");
}

#[test]
fn test_let_bool() {
    let g = load_grammar();
    println!("\n=== Let Bool ===");
    assert_well_typed(&g, "let b:Bool=true in b");
}

#[test]
fn test_nested_let() {
    let g = load_grammar();
    println!("\n=== Nested Let ===");
    assert_well_typed(&g, "let x:Int=1 in let y:Int=2 in x");
}

#[test]
fn test_let_type_mismatch() {
    let g = load_grammar();
    println!("\n=== Let Type Mismatch ===");
    assert_malformed(&g, "let x:Int=true in x");
}

#[test]
fn test_let_unbound_in_body() {
    let g = load_grammar();
    println!("\n=== Let Unbound In Body ===");
    assert_malformed(&g, "let x:Int=42 in z");
}

// ============================================================================
// If-then-else
// ============================================================================

#[test]
fn test_if_int_branches() {
    let g = load_grammar();
    println!("\n=== If Int Branches ===");
    assert_well_typed(&g, "if true then 1 else 2");
}

#[test]
fn test_if_bool_branches() {
    let g = load_grammar();
    println!("\n=== If Bool Branches ===");
    assert_well_typed(&g, "if true then true else false");
}

#[test]
fn test_if_non_bool_condition() {
    let g = load_grammar();
    println!("\n=== If Non-Bool Condition ===");
    assert_malformed(&g, "if 42 then 1 else 2");
}

#[test]
fn test_if_branch_mismatch() {
    let g = load_grammar();
    println!("\n=== If Branch Type Mismatch ===");
    assert_malformed(&g, "if true then 1 else false");
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
fn test_lambda_applied_to_arg() {
    let g = load_grammar();
    println!("\n=== Lambda Applied to Arg ===");
    assert_well_typed(&g, "λf:Int->Int.λx:Int.f x");
}
