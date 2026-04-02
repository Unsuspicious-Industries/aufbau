//! Complexity Analysis Module
//!
//! This module provides utilities to empirically measure the time complexity
//! of the parser on different grammar types and input sizes.

pub mod basic;
pub mod fun;
pub mod stlc;

use crate::logic::fusion::Synthesizer;
use crate::logic::fusion::{RuleRuntime, TypedParser};
use crate::logic::grammar::Grammar;
use crate::logic::typing::Context;
use rayon::prelude::*;
use std::path::Path;
use std::sync::mpsc;
use std::time::Duration;
use std::time::Instant;

pub const STEP_TIMEOUT: Duration = Duration::from_secs(30);

pub fn load_example_grammar(name: &str) -> Grammar {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let path = Path::new(manifest_dir)
        .join("examples")
        .join(format!("{name}.auf"));
    let content = std::fs::read_to_string(&path)
        .unwrap_or_else(|_| panic!("Failed to read {}", path.display()));
    Grammar::load(&content).unwrap_or_else(|_| panic!("Failed to load {} grammar", name))
}

pub fn tree_height(parsed: &Result<crate::logic::fusion::FusionAST, String>) -> usize {
    parsed.as_ref().map(|ast| ast.min_tree_depth()).unwrap_or(0)
}

pub fn measure_parse(grammar: &Grammar, input: &str) -> (Duration, usize) {
    let start = Instant::now();
    let mut synth = Synthesizer::new_with_max_depth(grammar.clone(), input, 64);
    let parsed = synth.parse_with(&Context::new());
    let elapsed = start.elapsed();
    let height = tree_height(&parsed);
    (elapsed, height)
}

pub fn profile_parse(n: usize, input: String, grammar: &Grammar) -> ComplexityData {
    let (time, height) = measure_parse(grammar, &input);
    ComplexityData::new_with_height(n, time, input, height)
}

pub fn mean_micros(data: &[ComplexityData]) -> f64 {
    match data.is_empty() {
        true => 0.0,
        false => data.iter().map(|d| d.time.as_micros() as f64).sum::<f64>() / data.len() as f64,
    }
}

pub fn print_complexity_summary(label: &str, k: f64, kh: f64, note: &str) {
    println!("\n{label}: O(n^{k:.2}), O(h^{kh:.2})");
    println!("{note}");
}

pub fn total_micros(data: &[ComplexityData]) -> u128 {
    data.iter().map(|point| point.time.as_micros()).sum()
}

pub fn check_timeout(start: Instant, label: &str, input: &str) {
    let elapsed = start.elapsed();
    assert!(
        elapsed <= STEP_TIMEOUT,
        "{} exceeded timeout of {:?} for input {:?} (elapsed {:?})",
        label,
        STEP_TIMEOUT,
        input,
        elapsed
    );
}

pub fn run_with_timeout<T, F>(label: &str, input: &str, f: F) -> T
where
    T: Send + 'static,
    F: FnOnce() -> T + Send + 'static,
{
    let (tx, rx) = mpsc::channel();
    let label = label.to_string();
    let input = input.to_string();
    std::thread::spawn(move || {
        let out = f();
        let _ = tx.send(out);
    });
    rx.recv_timeout(STEP_TIMEOUT).unwrap_or_else(|_| {
        panic!(
            "{} exceeded timeout of {:?} for input {:?}",
            label, STEP_TIMEOUT, input
        )
    })
}

pub fn token_boundary_prefixes(grammar: &Grammar, input: &str) -> Vec<String> {
    match grammar.tokenize(input) {
        Ok(segments) => {
            let mut cuts = vec![0usize];
            cuts.extend(segments.iter().map(|s| s.end));
            if !cuts.contains(&input.len()) {
                cuts.push(input.len());
            }
            cuts.sort_unstable();
            cuts.dedup();
            cuts.into_iter().map(|e| input[..e].to_string()).collect()
        }
        Err(_) => {
            let chars: Vec<char> = input.chars().collect();
            (0..=chars.len())
                .map(|len| chars[..len].iter().collect::<String>())
                .collect()
        }
    }
}

pub fn parser_height<T>(
    parser: &TypedParser<T>,
    roots: &[crate::logic::fusion::NodeId],
    input: &str,
) -> usize
where
    T: crate::logic::fusion::TypingRuntime,
{
    let segments = parser.grammar().tokenize(input).unwrap_or_default();
    parser.forest(roots, &segments, input).min_tree_depth()
}

pub fn parser_well_typed<T>(
    parser: &TypedParser<T>,
    roots: &[crate::logic::fusion::NodeId],
    input: &str,
) -> bool
where
    T: crate::logic::fusion::TypingRuntime,
{
    let segments = parser.grammar().tokenize(input).unwrap_or_default();
    parser.forest(roots, &segments, input).has_well_typed_root()
}

pub fn full_prefix_profile(grammar: &Grammar, input: &str) -> Vec<ComplexityData> {
    let runtime = RuleRuntime::new(grammar.clone());
    let ctx_id = runtime.intern_context(Context::new());
    token_boundary_prefixes(grammar, input)
        .into_iter()
        .filter(|prefix| !prefix.is_empty())
        .enumerate()
        .map(|(idx, prefix)| {
            let start = Instant::now();
            let mut parser = TypedParser::new(grammar.clone(), runtime.clone()).with_max_depth(64);
            let state = parser
                .parse(&prefix, ctx_id)
                .unwrap_or_else(|err| panic!("full prefix parse failed for {:?}: {}", prefix, err));
            let elapsed = start.elapsed();
            check_timeout(start, "full prefix parse", &prefix);
            ComplexityData::new_with_height(
                idx + 1,
                elapsed,
                prefix.clone(),
                parser_height(&parser, &state.roots, &prefix),
            )
        })
        .collect()
}

pub fn incremental_prefix_profile(grammar: &Grammar, input: &str) -> Vec<ComplexityData> {
    let runtime = RuleRuntime::new(grammar.clone());
    let ctx_id = runtime.intern_context(Context::new());
    let mut parser = TypedParser::new(grammar.clone(), runtime).with_max_depth(64);
    let mut prefixes = token_boundary_prefixes(grammar, input)
        .into_iter()
        .filter(|prefix| !prefix.is_empty());
    let Some(first) = prefixes.next() else {
        return Vec::new();
    };

    let start = Instant::now();
    let mut state = parser
        .parse(&first, ctx_id)
        .unwrap_or_else(|err| panic!("initial incremental parse failed for {:?}: {}", first, err));
    let elapsed = start.elapsed();
    check_timeout(start, "incremental prefix parse", &first);
    assert!(parser_well_typed(&parser, &state.roots, &first));

    let mut out = vec![ComplexityData::new_with_height(
        1,
        elapsed,
        first.clone(),
        parser_height(&parser, &state.roots, &first),
    )];

    for (idx, input) in prefixes.enumerate() {
        let start = Instant::now();
        state = parser
            .advance(&state, &input, ctx_id)
            .unwrap_or_else(|err| {
                panic!(
                    "incremental extend failed at step {} input {:?}: {}",
                    idx, input, err
                )
            });
        let elapsed = start.elapsed();
        check_timeout(start, "incremental prefix parse", &input);
        assert!(
            parser_well_typed(&parser, &state.roots, &input),
            "incremental parse lost typing"
        );
        out.push(ComplexityData::new_with_height(
            idx + 2,
            elapsed,
            input.clone(),
            parser_height(&parser, &state.roots, &input),
        ));
    }

    out
}

pub fn run_parse_experiment(
    grammar: &Grammar,
    generator: fn(usize) -> String,
    max_n: usize,
    tries: usize,
    jobs: Option<usize>,
) -> Vec<ComplexityData> {
    run_complexity_experiment(grammar, generator, "", max_n, tries, jobs)
}

/// Run a complexity experiment in parallel (optional jobs)
pub fn run_complexity_experiment(
    grammar: &Grammar,
    generator: fn(usize) -> String,
    _name: &str,
    max_n: usize,
    tries: usize,
    jobs: Option<usize>,
) -> Vec<ComplexityData> {
    assert!(tries >= 1 && max_n >= 1);

    // Build the sequence of sizes to test
    let indices: Vec<usize> = (0..=tries).map(|i| ((i + max_n / 2) % max_n) + 1).collect();

    // Parallel map to measure each input
    let measure = |n: usize| profile_parse(n, generator(n), grammar);

    let results: Vec<ComplexityData> = match jobs {
        Some(n) if n > 0 => {
            let pool = rayon::ThreadPoolBuilder::new()
                .num_threads(n)
                .build()
                .expect("failed to create thread pool");
            let mut out = Vec::new();
            pool.install(|| {
                out = indices.par_iter().map(|&n| measure(n)).collect();
            });
            out
        }
        _ => indices.iter().map(|&n| measure(n)).collect(),
    };

    results
}

/// Data point for complexity analysis
#[derive(Debug, Clone)]
pub struct ComplexityData {
    pub n: usize,       // Input size
    pub time: Duration, // Parse time
    pub input: String,  // Actual input string
    pub height: usize,  // Parsed AST height (0 when parsing fails)
}

impl ComplexityData {
    pub fn new(n: usize, time: Duration, input: String) -> Self {
        Self {
            n,
            time,
            input,
            height: 0,
        }
    }

    pub fn new_with_height(n: usize, time: Duration, input: String, height: usize) -> Self {
        Self {
            n,
            time,
            input,
            height,
        }
    }
}

/// Determine the order of growth from empirical data
/// Returns the exponent k where complexity is approximately O(n^k)
fn determine_complexity_exponent(data: &[ComplexityData]) -> f64 {
    let mut log_n = Vec::new();
    let mut log_time = Vec::new();

    for point in data {
        let time_secs = point.time.as_secs_f64();
        if time_secs > 0.0 && point.n > 0 {
            log_n.push((point.n as f64).ln());
            log_time.push(time_secs.ln());
        }
    }

    let n = log_n.len() as f64;
    let sum_x: f64 = log_n.iter().sum();
    let sum_y: f64 = log_time.iter().sum();
    let sum_xx: f64 = log_n.iter().map(|x| x * x).sum();
    let sum_xy: f64 = log_n.iter().zip(&log_time).map(|(x, y)| x * y).sum();

    let denominator = n * sum_xx - sum_x * sum_x;
    if denominator.abs() < 1e-10 {
        panic!("Insufficient variance in data for complexity estimation");
    }

    let k = (n * sum_xy - sum_x * sum_y) / denominator;

    // Sanity check
    if k.is_finite() {
        k
    } else {
        panic!("Invalid complexity exponent");
    }
}

/// Public wrapper to export complexity estimation to CLI
pub fn estimate_complexity_exponent(data: &[ComplexityData]) -> f64 {
    determine_complexity_exponent(data)
}

/// Returns exponent k where time is approximately O(h^k), with h = parse tree height.
pub fn determine_height_complexity_exponent(data: &[ComplexityData]) -> f64 {
    let mut log_h = Vec::new();
    let mut log_time = Vec::new();

    for point in data {
        let time_secs = point.time.as_secs_f64();
        if time_secs > 0.0 && point.height > 0 {
            log_h.push((point.height as f64).ln());
            log_time.push(time_secs.ln());
        }
    }

    let n = log_h.len() as f64;
    let sum_x: f64 = log_h.iter().sum();
    let sum_y: f64 = log_time.iter().sum();
    let sum_xx: f64 = log_h.iter().map(|x| x * x).sum();
    let sum_xy: f64 = log_h.iter().zip(&log_time).map(|(x, y)| x * y).sum();

    let denominator = n * sum_xx - sum_x * sum_x;
    if denominator.abs() < 1e-10 {
        panic!("Insufficient variance in height data for complexity estimation");
    }

    let k = (n * sum_xy - sum_x * sum_y) / denominator;
    if k.is_finite() {
        k
    } else {
        panic!("Invalid height-based complexity exponent");
    }
}

pub fn maybe_height_complexity_exponent(data: &[ComplexityData]) -> Option<f64> {
    let heights: Vec<_> = data
        .iter()
        .filter(|point| point.height > 0)
        .map(|point| point.height)
        .collect();
    match heights.iter().min() == heights.iter().max() {
        true => None,
        false => Some(determine_height_complexity_exponent(data)),
    }
}
