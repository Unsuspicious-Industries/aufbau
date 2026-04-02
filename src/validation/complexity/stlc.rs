#![allow(dead_code)]
#![allow(unused_imports)]

use crate::logic::grammar::Grammar;
use crate::validation::complexity::{
    ComplexityData, determine_complexity_exponent, determine_height_complexity_exponent,
};
fn stlc_grammar() -> Grammar {
    super::load_example_grammar("stlc")
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
        result.push('f');
    } else {
        result.push_str(&format!("f {}", args.join(" ")));
    }

    result
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
    assert!(tries >= max_n * 2);
    let _ = name;
    super::run_parse_experiment(grammar, generator, max_n, tries, jobs)
}

/// Export STLC experiments
pub fn experiments(jobs: Option<usize>) -> Vec<(String, Vec<ComplexityData>)> {
    let grammar = stlc_grammar();
    vec![
        (
            "STLC App Chain".to_string(),
            run_complexity_test(&grammar, generate_app_chain, "STLC App Chain", 8, 24, jobs),
        ),
        (
            "STLC Nested Lambda".to_string(),
            run_complexity_test(
                &grammar,
                generate_nested_lambda_with_app,
                "STLC Nested Lambda",
                6,
                18,
                jobs,
            ),
        ),
    ]
}

#[test]
fn stlc_app_chain_complexity() {
    let grammar = stlc_grammar();
    let data = run_complexity_test(&grammar, generate_app_chain, "STLC App Chain", 12, 36, None);

    // Determine complexity exponent
    let k = determine_complexity_exponent(&data);
    let kh = super::maybe_height_complexity_exponent(&data).unwrap_or(1.0);

    super::print_complexity_summary(
        "STLC app chain",
        k,
        kh,
        "Expected: near-polynomial under memoized left-recursive parsing.",
    );

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
        8,
        24,
        None,
    );

    // Determine complexity exponent
    let k = determine_complexity_exponent(&data);
    let kh = super::maybe_height_complexity_exponent(&data).unwrap_or(1.0);

    super::print_complexity_summary(
        "STLC nested lambda",
        k,
        kh,
        "Nested terms stress binder and application structure.",
    );

    // Nested structures may have higher complexity
    assert!(
        k < 4.0,
        "Complexity should be reasonable even for nested structures"
    );
}
