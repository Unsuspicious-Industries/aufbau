#![allow(dead_code)]

const DEEP_NESTING: &str = r#"
    Atom ::= 'x'
    L1 ::= '(' L2 ')' | Atom
    L2 ::= '(' L3 ')' | L1
    L3 ::= '(' L4 ')' | L2
    L4 ::= '(' L5 ')' | L3
    L5 ::= '(' Atom ')' | L4
    start ::= L5
"#;

use crate::logic::Parser;
use crate::logic::grammar::Grammar;

use super::*;

fn basic_grammar() -> Grammar {
    Grammar::load(DEEP_NESTING).expect("Failed to load basic grammar")
}

/// Generate deeply nested parentheses of depth n
fn generate_deep_nesting(n: usize) -> String {
    let mut result = String::new();
    for _ in 0..n {
        result.push('(');
    }
    result.push_str("x");
    for _ in 0..n {
        result.push(')');
    }
    result
}

fn generate_random_string(n: usize) -> String {
    // make sure we dont have more closeing parens than opening
    let mut result = String::new();
    let mut open_count = 0;
    let mut close_count = 0;
    use rand::Rng;
    let mut rng = rand::thread_rng();
    for _ in 0..n {
        let choice: u8 = rng.gen_range(0..3);
        match choice {
            0 => {
                result.push('(');
                open_count += 1;
            }
            1 => {
                if open_count > close_count {
                    result.push(')');
                    close_count += 1;
                } else {
                    result.push('(');
                    open_count += 1;
                }
            }
            _ => {
                result.push('x');
            }
        }
    }
    // Ensure balanced parentheses
    while open_count > close_count {
        result.push(')');
        close_count += 1;
    }
    result
}

use std::time::Instant;

fn measure_parse_time(grammar: &Grammar, input: &str) -> std::time::Duration {
    let mut parser = Parser::new(grammar.clone());
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
    jobs: Option<usize>,
) -> Vec<ComplexityData> {
    println!("\n=== {} Complexity Test ===", name);
    println!("Testing input sizes from 1 to {}", max_n);

    // ensure tries >> n
    assert!(tries >= max_n * 5);

    let results = super::run_complexity_experiment(grammar, generator, name, max_n, tries, jobs);

    // Print compact summary
    for r in &results {
        println!("n={:2}: {} -> {:?}", r.n, r.input, r.time);
    }

    results
}

/// Export experiments for the basic complexity module
pub fn experiments(jobs: Option<usize>) -> Vec<(String, Vec<ComplexityData>)> {
    let grammar = basic_grammar();
    let mut out = Vec::new();
    out.push((
        "Deep Nesting".to_string(),
        run_complexity_test(&grammar, generate_deep_nesting, "Deep Nesting", 10, 100, jobs),
    ));
    out.push((
        "Random String".to_string(),
        run_complexity_test(&grammar, generate_random_string, "Random String", 100, 1000, jobs),
    ));
    out
}

#[test]
fn basic_deep_nesting_complexity() {
    let grammar = basic_grammar();
    let data = run_complexity_test(&grammar, generate_deep_nesting, "Deep Nesting", 10, 100, None);

    // Determine complexity exponent
    let k = determine_complexity_exponent(&data);

    println!("\nEmpirical complexity: O(n^{:.2})", k);
    println!("Expected: O(n) for linear nesting");
    println!("Actual: k = {:.2} (closer to 1.0 is better)", k);

    // For linear nesting, complexity should be close to O(n)
    assert!(
        k < 4.0,
        "Complexity should be better than O(n^4.0) for linear nesting (Parser is bad for stuff like that)"
    );
    assert!(
        k > 0.01,
        "Complexity should be worse than O(1) for non-trivial inputs"
    );
}

#[test]
fn basic_random_string_complexity() {
    let grammar = basic_grammar();
    let data = run_complexity_test(&grammar, generate_random_string, "Random String", 100, 1000, None);

    // Determine complexity exponent
    let k = determine_complexity_exponent(&data);

    println!("\nEmpirical complexity: O(n^{:.2})", k);
    println!("Random strings test the parser's handling of arbitrary inputs");
    println!("k = {:.2}", k);

    // Random strings may have higher complexity due to backtracking
    assert!(
        k < 4.0,
        "Complexity should be reasonable even for random inputs"
    );
}
