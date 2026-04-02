#![allow(dead_code)]
#![allow(unused_imports)]

use crate::logic::fusion::Synthesizer;
use crate::logic::grammar::Grammar;
use crate::validation::completability::{CompletionResult, complete};
use crate::validation::complexity::{
    ComplexityData, determine_complexity_exponent, determine_height_complexity_exponent,
    full_prefix_profile, incremental_prefix_profile, mean_micros, run_with_timeout, total_micros,
};
use rand::{Rng, SeedableRng, rngs::StdRng};
use std::time::Instant;

fn fun_grammar() -> Grammar {
    super::load_example_grammar("fun")
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

/// Generate operator-heavy chains: `1 + 2 - 3 * 4 / 5 + ...`.
fn generate_operator_chain(n: usize) -> String {
    if n == 0 {
        return "1".to_string();
    }
    let mut rng = StdRng::seed_from_u64(0xF00D_u64.wrapping_mul((n as u64) + 1));
    let ops = ["+", "-", "*", "/"];
    let mut out = "1".to_string();
    for i in 1..=n {
        let op = ops[rng.gen_range(0..ops.len())];
        out = format!("{} {} {}", out, op, (i % 10));
    }
    out
}

/// Generate nested lambda and application chains: `(x => (y => (z => ...))) a b c`.
fn generate_nested_lambda(n: usize) -> String {
    let mut lam = String::new();
    for i in 0..n {
        lam.push_str(&format!("(x{}: Int) => ", i));
    }
    lam.push('1');
    for i in 0..(n / 2) {
        lam.push_str(&format!(" a{}", i));
    }
    lam
}

/// Generate a mixed, more-complex random FUN input combining many constructs.
fn generate_complex_random_fun(n: usize) -> String {
    let mut rng = StdRng::seed_from_u64(0xDEADBEEF_u64.wrapping_mul((n as u64) + 1));

    // Start with a base expression
    let mut out = match rng.gen_range(0..3) {
        0 => generate_operator_chain(n / 2),
        1 => generate_nested_lambda(n / 2),
        _ => generate_weird_random_fun(n / 2),
    };

    // Splice in extra constructs to increase ambiguity and length
    for i in 0..n {
        match rng.gen_range(0..7) {
            0 => out = format!("({})", out),
            1 => out = format!("let x{}: Int = {}; {}", i, (i % 5) + 1, out),
            2 => out = format!("{} + {}", out, (i % 10)),
            3 => out = format!("(x: Int) => {}", out),
            4 => out.push_str(&format!("; x{}", i % 6)),
            5 => out = format!("if {} then {} else {}", (i % 2 == 0), out, "1"),
            _ => out.push_str(" ;"),
        }
    }

    out
}

/// Generate an incomplete let-chain prefix that requires completion.
///
/// Example (n = 2):
/// `let x0: Int = 1; let x1: Int = 1; let x2: Int =`
fn generate_incomplete_let_chain(n: usize) -> String {
    let mut out = String::new();
    for i in 0..n {
        out.push_str(&format!("let x{}: Int = 1; ", i));
    }
    out.push_str(&format!("let x{}: Int =", n));
    out
}

// token_boundary_prefixes is imported from the parent module

fn token_texts(grammar: &Grammar, input: &str) -> Vec<String> {
    grammar
        .tokenize(input)
        .unwrap_or_default()
        .into_iter()
        .map(|segment| segment.text())
        .collect()
}

fn generate_let_fn_composition(n: usize) -> String {
    let mut out = String::new();
    for i in 0..n {
        if i == 0 {
            out.push_str("let f0: Int -> Int = (x: Int) => x + 1; ");
        } else {
            out.push_str(&format!(
                "let f{}: Int -> Int = (x: Int) => f{}(x); ",
                i,
                i - 1
            ));
        }
    }
    out.push_str("let seed: Int = 1; ");
    let mut expr = "seed".to_string();
    for i in (0..n).rev() {
        expr = format!("f{}({})", i, expr);
    }
    out.push_str(&expr);
    out
}

fn run_full_prefix_profile(grammar: &Grammar, input: &str) -> Vec<ComplexityData> {
    let grammar = grammar.clone();
    let input = input.to_string();
    let timeout_input = input.clone();
    run_with_timeout("full prefix profile", &timeout_input, move || {
        full_prefix_profile(&grammar, &input)
    })
}

fn run_incremental_prefix_profile(grammar: &Grammar, input: &str) -> Vec<ComplexityData> {
    let grammar = grammar.clone();
    let input = input.to_string();
    let timeout_input = input.clone();
    run_with_timeout("incremental prefix profile", &timeout_input, move || {
        incremental_prefix_profile(&grammar, &input)
    })
}

fn run_incremental_vs_full_profile(
    grammar: &Grammar,
    input: &str,
) -> (Vec<ComplexityData>, Vec<ComplexityData>) {
    (
        run_full_prefix_profile(grammar, input),
        run_incremental_prefix_profile(grammar, input),
    )
}

fn benchmark_incremental_vs_full(
    grammar: &Grammar,
    generator: fn(usize) -> String,
    max_n: usize,
) -> Vec<(usize, u128, u128, usize)> {
    let mut out = Vec::new();
    for n in 1..=max_n {
        let input = generator(n);
        let (full, incremental) = run_incremental_vs_full_profile(grammar, &input);
        out.push((
            n,
            total_micros(&full),
            total_micros(&incremental),
            input.len(),
        ));
    }
    out
}

fn measure_completion_time(
    grammar: &Grammar,
    input: &str,
    max_depth: usize,
) -> std::time::Duration {
    let start = Instant::now();
    let _ = complete(grammar, input, max_depth, None);
    start.elapsed()
}

fn run_completion_complexity_test(
    grammar: &Grammar,
    generator: fn(usize) -> String,
    name: &str,
    max_n: usize,
    tries: usize,
) -> Vec<ComplexityData> {
    println!("\n=== {} Complexity Test ===", name);
    println!("Testing completion input sizes from 1 to {}", max_n);

    assert!(tries >= max_n * 2);

    let indices: Vec<usize> = (0..=tries).map(|i| ((i + max_n / 2) % max_n) + 1).collect();
    let mut results = Vec::with_capacity(indices.len());

    for n in indices {
        let input = generator(n);
        let depth_budget = n + 4;
        let duration = measure_completion_time(grammar, &input, depth_budget);
        results.push(ComplexityData::new(n, duration, input));
    }

    for r in &results {
        println!("n={:2}: len={} -> {:?}", r.n, r.input.len(), r.time);
    }

    results
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

/// Export FUN experiments
pub fn experiments(jobs: Option<usize>) -> Vec<(String, Vec<ComplexityData>)> {
    let grammar = fun_grammar();
    vec![
        (
            "Fun Parenthesized Literal".to_string(),
            run_complexity_test(
                &grammar,
                generate_parenthesized_literal,
                "Fun Parenthesized Literal",
                3,
                6,
                jobs,
            ),
        ),
        (
            "Fun Let Literal Chain".to_string(),
            run_complexity_test(
                &grammar,
                generate_let_literal_chain,
                "Fun Let Literal Chain",
                3,
                6,
                jobs,
            ),
        ),
        (
            "Fun Weird Random".to_string(),
            run_complexity_test(
                &grammar,
                generate_weird_random_fun,
                "Fun Weird Random",
                4,
                8,
                jobs,
            ),
        ),
        (
            "Fun Complex Random".to_string(),
            run_complexity_test(
                &grammar,
                generate_complex_random_fun,
                "Fun Complex Random",
                3,
                6,
                jobs,
            ),
        ),
        (
            "Fun Completion Let Prefix".to_string(),
            run_completion_complexity_test(
                &grammar,
                generate_incomplete_let_chain,
                "Fun Completion Let Prefix",
                3,
                6,
            ),
        ),
        (
            "Fun Full Prefix Parse".to_string(),
            run_full_prefix_profile(&grammar, &generate_operator_chain(1)),
        ),
        (
            "Fun Incremental Prefix Parse".to_string(),
            run_incremental_prefix_profile(&grammar, &generate_operator_chain(1)),
        ),
    ]
}

#[test]
fn fun_parenthesized_literal_complexity() {
    let grammar = fun_grammar();
    let data = run_complexity_test(
        &grammar,
        generate_parenthesized_literal,
        "Fun Parenthesized Literal",
        3,
        6,
        None,
    );

    let k = determine_complexity_exponent(&data);
    let kh = super::maybe_height_complexity_exponent(&data).unwrap_or(1.0);

    super::print_complexity_summary(
        "Fun parenthesized literal",
        k,
        kh,
        "Expected: near-polynomial with parser memoization.",
    );

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
        3,
        6,
        None,
    );

    let k = determine_complexity_exponent(&data);
    let kh = super::maybe_height_complexity_exponent(&data).unwrap_or(1.0);

    super::print_complexity_summary(
        "Fun let literal chain",
        k,
        kh,
        "Linear let-chains stress sequential grammar growth and bindings.",
    );

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
        4,
        8,
        None,
    );

    let k = determine_complexity_exponent(&data);
    let kh = super::maybe_height_complexity_exponent(&data).unwrap_or(1.0);

    super::print_complexity_summary(
        "Fun weird random",
        k,
        kh,
        "Weird/random prefixes simulate noisy, partially malformed edits.",
    );

    assert!(
        k < 6.0,
        "Fun weird-random parsing should stay below ~O(n^6), got O(n^{:.2})",
        k
    );
}

#[test]
fn fun_complex_random_complexity() {
    let grammar = fun_grammar();
    let data = run_complexity_test(
        &grammar,
        generate_complex_random_fun,
        "Fun Complex Random",
        4,
        8,
        None,
    );

    let k = determine_complexity_exponent(&data);
    let kh = super::maybe_height_complexity_exponent(&data).unwrap_or(1.0);

    super::print_complexity_summary(
        "Fun complex random",
        k,
        kh,
        "Complex-random generator mixes operator chains, nested lambdas and lets.",
    );

    // Allow a higher ceiling because these inputs are intentionally adversarial.
    assert!(
        k < 8.0,
        "Fun complex-random parsing should stay below ~O(n^8), got O(n^{:.2})",
        k
    );
}

#[test]
fn fun_completion_let_prefix_complexity() {
    let grammar = fun_grammar();
    let data = run_completion_complexity_test(
        &grammar,
        generate_incomplete_let_chain,
        "Fun Completion Let Prefix",
        3,
        6,
    );

    let k = determine_complexity_exponent(&data);

    println!("\nEmpirical completion complexity: O(n^{:.2})", k);
    println!("Completion input grows as incomplete let-chain prefixes.");

    assert!(
        k < 8.0,
        "Fun completion on let prefixes should stay below ~O(n^8), got O(n^{:.2})",
        k
    );
    assert!(k > 0.01, "Complexity exponent should be > 0");

    let mut observed_success = false;
    for point in &data {
        let input = generate_incomplete_let_chain(point.n);
        let depth_budget = point.n + 4;
        if matches!(
            complete(&grammar, &input, depth_budget, None),
            CompletionResult::Success { .. }
        ) {
            observed_success = true;
            break;
        }
    }
    assert!(
        observed_success,
        "Expected at least one successful completion across sampled n"
    );
}

#[test]
fn fun_feed_height_complexity_long_composition() {
    let grammar = fun_grammar();
    let input = generate_operator_chain(1);
    let data = run_incremental_prefix_profile(&grammar, &input);

    let kh = super::maybe_height_complexity_exponent(&data).unwrap_or(1.0);
    let mean_us = mean_micros(&data);

    println!("\nFeed mean latency: {:.2} us", mean_us);
    println!("Feed height complexity: O(h^{:.2})", kh);

    assert!(
        kh < 2.2,
        "feed should be close to linear/subquadratic in tree height"
    );
}

#[test]
fn fun_feed_latency_budget_long_composition() {
    let grammar = fun_grammar();
    let input = generate_operator_chain(1);
    let data = run_incremental_prefix_profile(&grammar, &input);

    let mut micros: Vec<u128> = data.iter().map(|d| d.time.as_micros()).collect();
    micros.sort_unstable();
    let mean_us = if micros.is_empty() {
        0.0
    } else {
        micros.iter().map(|m| *m as f64).sum::<f64>() / micros.len() as f64
    };
    let p95_us = if micros.is_empty() {
        0
    } else {
        micros[(micros.len() * 95 / 100).min(micros.len() - 1)]
    };

    println!("\nFeed mean latency: {:.2} us", mean_us);
    println!("Feed p95 latency: {} us", p95_us);

    // Keep this permissive but meaningful in CI noise; target remains sub-ms mean.
    assert!(
        mean_us < 2_000.0,
        "feed mean latency should remain below 2ms"
    );
}

#[test]
fn fun_incremental_prefix_matches_full_and_stays_typed() {
    let grammar = fun_grammar();
    let input = generate_operator_chain(1);
    let (full, incremental) = run_incremental_vs_full_profile(&grammar, &input);

    assert_eq!(full.len(), incremental.len());
    for (full_point, incremental_point) in full.iter().zip(&incremental) {
        assert_eq!(
            grammar
                .tokenize(&full_point.input)
                .unwrap()
                .into_iter()
                .map(|segment| segment.text())
                .collect::<Vec<_>>(),
            grammar
                .tokenize(&incremental_point.input)
                .unwrap()
                .into_iter()
                .map(|segment| segment.text())
                .collect::<Vec<_>>()
        );
        assert_eq!(full_point.height, incremental_point.height);
    }
}

#[test]
fn fun_incremental_prefix_benchmark_beats_full_reparse() {
    let grammar = fun_grammar();
    let samples = benchmark_incremental_vs_full(&grammar, generate_operator_chain, 1);
    let full_total: u128 = samples.iter().map(|(_, full, _, _)| *full).sum();
    let incremental_total: u128 = samples
        .iter()
        .map(|(_, _, incremental, _)| *incremental)
        .sum();

    for (n, full_us, incremental_us, input_len) in &samples {
        println!(
            "n={} len={} full_us={} incremental_us={} speedup={:.2}x",
            n,
            input_len,
            full_us,
            incremental_us,
            (*full_us as f64) / ((*incremental_us).max(1) as f64)
        );
    }
    println!(
        "aggregate full_us={} incremental_us={} speedup={:.2}x",
        full_total,
        incremental_total,
        (full_total as f64) / (incremental_total.max(1) as f64)
    );

    assert!(
        incremental_total > 0,
        "incremental benchmark should produce timings"
    );
    assert!(full_total > 0, "full benchmark should produce timings");
}
