#![allow(dead_code)]
#![allow(unused_imports)]

use crate::logic::grammar::Grammar;
use crate::logic::partial::parse::Parser;
use crate::validation::complexity::{ComplexityData, determine_complexity_exponent};
use std::time::Instant;

fn stlc_grammar() -> Grammar {
    use std::path::Path;
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let path = Path::new(manifest_dir).join("examples").join("stlc.auf");
    let content = std::fs::read_to_string(&path).expect("Failed to read stlc.auf");
    Grammar::load(&content).expect("Failed to load STLC grammar")
}

/// Generate a left-associative application chain of length n.
///
/// Uses conventional lambda-calculus-ish variable names to keep the generated
/// programs readable while still stressing the left-recursive `Application` rule.
///
/// Example (n = 4):
/// `apply a b c d`
fn generate_app_chain(n: usize) -> String {
    if n == 0 {
        return "apply".to_string();
    }

    // Cycle through a small set of natural variable names.
    const NAMES: &[&str] = &[
        "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "m", "n",
    ];

    let mut parts = Vec::with_capacity(n + 1);
    parts.push("apply".to_string());
    for i in 0..n {
        let name = NAMES[i % NAMES.len()];
        // Ensure uniqueness after wrapping the base list.
        let suffix = i / NAMES.len();
        if suffix == 0 {
            parts.push(name.to_string());
        } else {
            parts.push(format!("{}{}", name, suffix));
        }
    }

    parts.join(" ")
}

/// Generate a nested lambda term whose body is an application chain.
///
/// Example (n = 3):
/// `λf:A->A.λx:A.λy:A.f x y`
///
/// This grows both the binder stack and the body size with n, which is a nice
/// stress test for nested constructs.
fn generate_nested_lambda_with_app(n: usize) -> String {
    if n == 0 {
        return "λx:A.x".to_string();
    }

    const VARS: &[&str] = &["x", "y", "z", "u", "v", "w", "p", "q", "r", "s", "t"];

    // First binder is a function we can apply in the body.
    let mut result = String::from("λf:A->A.");

    let mut args = Vec::with_capacity(n);
    for i in 0..n {
        let v = VARS[i % VARS.len()];
        let suffix = i / VARS.len();
        let name = if suffix == 0 {
            v.to_string()
        } else {
            format!("{}{}", v, suffix)
        };
        result.push_str(&format!("λ{}:A.", name));
        args.push(name);
    }

    // Body: left-associative application chain starting with f.
    if args.is_empty() {
        result.push_str("f");
    } else {
        result.push_str(&format!("f {}", args.join(" ")));
    }

    result
}

/// Measure parse time for a single input
fn measure_parse_time(grammar: &Grammar, input: &str) -> std::time::Duration {
    let mut parser = Parser::new(grammar.clone());
    let start = Instant::now();
    let _ = parser.partial(input);
    start.elapsed()
}

/// Run complexity test and return data points
fn run_complexity_test(
    grammar: &Grammar,
    generator: fn(usize) -> String,
    name: &str,
    max_n: usize,
    tries: usize,
    jobs: Option<usize>,
) -> Vec<ComplexityData> {
    println!("\n=== {} Complexity Test ===", name);
    println!("Testing input sizes from 1 to {}", max_n);

    assert!(tries >= max_n * 5);

    let results = super::run_complexity_experiment(grammar, generator, name, max_n, tries, jobs);

    for r in &results {
        println!("n={:2}: len={} -> {:?}", r.n, r.input.len(), r.time);
    }

    results
}

/// Export STLC experiments
pub fn experiments(jobs: Option<usize>) -> Vec<(String, Vec<ComplexityData>)> {
    let grammar = stlc_grammar();
    vec![
        (
            "STLC App Chain".to_string(),
            run_complexity_test(
                &grammar,
                generate_app_chain,
                "STLC App Chain",
                50,
                500,
                jobs,
            ),
        ),
        (
            "STLC Nested Lambda".to_string(),
            run_complexity_test(
                &grammar,
                generate_nested_lambda_with_app,
                "STLC Nested Lambda",
                20,
                100,
                jobs,
            ),
        ),
    ]
}

#[test]
fn stlc_app_chain_complexity() {
    let grammar = stlc_grammar();
    let data = run_complexity_test(
        &grammar,
        generate_app_chain,
        "STLC App Chain",
        50,
        500,
        None,
    );

    // Determine complexity exponent
    let k = determine_complexity_exponent(&data);

    println!("\nEmpirical complexity: O(n^{:.2})", k);
    println!("Expected: O(n^2) for left-recursive grammar with memoization");
    println!("Actual: k = {:.2} (closer to 1.0 is better)", k);

    // With memoization, we should get better than exponential (k < 3)
    // The exact value depends on implementation
    assert!(
        k < 3.0,
        "Complexity should be better than O(n^3) with memoization"
    );
    assert!(
        k > 0.01,
        "Complexity should be worse than O(1) for non-trivial inputs"
    );
}

#[test]
fn stlc_nested_lambda_complexity() {
    let grammar = stlc_grammar();
    let data = run_complexity_test(
        &grammar,
        generate_nested_lambda_with_app,
        "STLC Nested Lambda",
        20,
        100,
        None,
    );

    // Determine complexity exponent
    let k = determine_complexity_exponent(&data);

    println!("\nEmpirical complexity: O(n^{:.2})", k);
    println!("Nested structures test the parser's handling of complex expressions");
    println!("k = {:.2}", k);

    // Nested structures may have higher complexity
    assert!(
        k < 4.0,
        "Complexity should be reasonable even for nested structures"
    );
}
