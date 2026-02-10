#![allow(dead_code)]
#![allow(unused_imports)]

use crate::logic::grammar::Grammar;
use crate::logic::partial::parse::Parser;
use crate::validation::complexity::{ComplexityData, determine_complexity_exponent};
use rand::{Rng, SeedableRng, rngs::StdRng};
use std::time::Instant;

fn fun_grammar() -> Grammar {
    use std::path::Path;
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let path = Path::new(manifest_dir).join("examples").join("fun.spec");
    let content = std::fs::read_to_string(&path).expect("Failed to read fun.spec");
    Grammar::load(&content).expect("Failed to load fun grammar")
}

/// Generate nested parenthesized literals:
/// `(((1)))`
fn generate_parenthesized_literal(n: usize) -> String {
    let mut out = "1".to_string();
    for _ in 0..n {
        out = format!("({})", out);
    }
    out
}

/// Generate linear let chains with literal RHS:
/// `let x0: Int = 1; let x1: Int = 1; ...; xN`
fn generate_let_literal_chain(n: usize) -> String {
    if n == 0 {
        return "1".to_string();
    }

    let mut out = String::new();
    for i in 0..n {
        out.push_str(&format!("let x{}: Int = 1; ", i));
    }

    out.push_str(&format!("x{}", n - 1));
    out
}

/// Generate deterministic pseudo-random "weird" fun-like inputs.
///
/// Intentionally mixes valid fragments and incomplete tails to exercise
/// parser behavior on noisy real-world prefixes.
fn generate_weird_random_fun(n: usize) -> String {
    let mut rng = StdRng::seed_from_u64(0xC0FFEE_u64.wrapping_mul((n as u64) + 1));
    let atoms = ["1", "0", "true", "false", "x", "y", "(1)", "(true)"];
    let odd_tails = ["(", ")", "->", "-", "=>", ";", ".", ""];

    let mut out = atoms[rng.gen_range(0..atoms.len())].to_string();

    for i in 0..=n {
        match rng.gen_range(0..6) {
            0 => out = format!("({})", out),
            1 => out = format!("{} {}", out, atoms[rng.gen_range(0..atoms.len())]),
            2 => out = format!("let x{}: Int = 1; {}", i % 4, out),
            3 => out = format!("(x: Int) => {}", out),
            4 => out.push_str(&format!(
                " {}",
                odd_tails[rng.gen_range(0..odd_tails.len())]
            )),
            _ => out = format!("{} {}", atoms[rng.gen_range(0..atoms.len())], out),
        }
    }

    out
}

fn measure_parse_time(grammar: &Grammar, input: &str) -> std::time::Duration {
    // Keep a bounded recursion budget so complexity tests stay practical in CI.
    let mut parser = Parser::new(grammar.clone()).with_max_recursion(16);
    let start = Instant::now();
    let _ = parser.partial(input);
    start.elapsed()
}

fn run_complexity_test(
    grammar: &Grammar,
    generator: fn(usize) -> String,
    name: &str,
    max_n: usize,
    tries: usize,
) -> Vec<ComplexityData> {
    let mut results = Vec::new();

    println!("\n=== {} Complexity Test ===", name);
    println!("Testing input sizes from 1 to {}", max_n);

    assert!(tries >= max_n * 2);

    for i in 0..=tries {
        let n = (i % max_n) + 1;
        let input = generator(n);
        let time = measure_parse_time(grammar, &input);
        println!("n={:2}: len={} -> {:?}", n, input.len(), time);
        results.push(ComplexityData::new(n, time, input));
    }

    results
}

#[test]
fn fun_parenthesized_literal_complexity() {
    let grammar = fun_grammar();
    let data = run_complexity_test(
        &grammar,
        generate_parenthesized_literal,
        "Fun Parenthesized Literal",
        4,
        8,
    );

    let k = determine_complexity_exponent(&data);

    println!("\nEmpirical complexity: O(n^{:.2})", k);
    println!("Expected: near-polynomial with parser memoization");

    assert!(
        k < 5.0,
        "Fun parenthesized-literal parsing should remain below ~O(n^5), got O(n^{:.2})",
        k
    );
    assert!(
        k > 0.01,
        "Complexity exponent should be > 0 for non-trivial inputs"
    );
}

#[test]
fn fun_let_literal_chain_complexity() {
    let grammar = fun_grammar();
    let data = run_complexity_test(
        &grammar,
        generate_let_literal_chain,
        "Fun Let Literal Chain",
        4,
        8,
    );

    let k = determine_complexity_exponent(&data);

    println!("\nEmpirical complexity: O(n^{:.2})", k);
    println!("Linear let-chains stress sequential grammar growth and bindings.");

    assert!(
        k < 5.0,
        "Fun let-literal-chain parsing should stay below ~O(n^5), got O(n^{:.2})",
        k
    );
}

#[test]
fn fun_weird_random_complexity() {
    let grammar = fun_grammar();
    let data = run_complexity_test(
        &grammar,
        generate_weird_random_fun,
        "Fun Weird Random",
        8,
        16,
    );

    let k = determine_complexity_exponent(&data);

    println!("\nEmpirical complexity: O(n^{:.2})", k);
    println!("Weird/random prefixes simulate noisy, partially malformed edits.");

    assert!(
        k < 6.0,
        "Fun weird-random parsing should stay below ~O(n^6), got O(n^{:.2})",
        k
    );
}
