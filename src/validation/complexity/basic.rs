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
    result.push('x');
    for _ in 0..n {
        result.push(')');
    }
    result
}

fn generate_random_string(n: usize) -> String {
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
            1 => match open_count > close_count {
                true => {
                    result.push(')');
                    close_count += 1;
                }
                false => {
                    result.push('(');
                    open_count += 1;
                }
            },
            _ => result.push('x'),
        }
    }

    while open_count > close_count {
        result.push(')');
        close_count += 1;
    }
    result
}

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

/// Export experiments for the basic complexity module
pub fn experiments(jobs: Option<usize>) -> Vec<(String, Vec<ComplexityData>)> {
    let grammar = basic_grammar();
    vec![
        (
            "Deep Nesting".to_string(),
            run_complexity_test(&grammar, generate_deep_nesting, "Deep Nesting", 5, 15, jobs),
        ),
        (
            "Random String".to_string(),
            run_complexity_test(
                &grammar,
                generate_random_string,
                "Random String",
                10,
                30,
                jobs,
            ),
        ),
    ]
}

#[test]
fn basic_deep_nesting_complexity() {
    let grammar = basic_grammar();
    let data = run_complexity_test(&grammar, generate_deep_nesting, "Deep Nesting", 6, 18, None);

    let k = determine_complexity_exponent(&data);
    let kh = super::maybe_height_complexity_exponent(&data).unwrap_or(1.0);

    super::print_complexity_summary(
        "Deep nesting",
        k,
        kh,
        "Expected: near-linear in nesting height.",
    );

    // For linear nesting, complexity should be close to O(n)
    assert!(
        k < 4.0,
        "Complexity should be better than O(n^4.0) for linear nesting (Parser is bad for stuff like that)"
    );
    assert!(
        k > 0.01,
        "Complexity should be worse than O(1) for non-trivial inputs"
    );
    assert!(kh > 0.01, "Height complexity should be non-trivial");
}

#[test]
fn basic_random_string_complexity() {
    let grammar = basic_grammar();
    let data = run_complexity_test(
        &grammar,
        generate_random_string,
        "Random String",
        16,
        48,
        None,
    );

    let k = determine_complexity_exponent(&data);
    let kh = super::maybe_height_complexity_exponent(&data).unwrap_or(1.0);

    super::print_complexity_summary(
        "Random strings",
        k,
        kh,
        "Random strings probe arbitrary-input parser behavior.",
    );

    // Random strings may have higher complexity due to backtracking
    assert!(
        k < 4.0,
        "Complexity should be reasonable even for random inputs"
    );
}
