//! Complexity Analysis Module
//!
//! This module provides utilities to empirically measure the time complexity
//! of the parser on different grammar types and input sizes.

pub mod basic;
pub mod fun;
pub mod stlc;

use crate::logic::grammar::Grammar;
use rayon::prelude::*;
use std::time::Duration;
use std::time::Instant;

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
    let measure = |n: usize| {
        let input = generator(n);
        let start = Instant::now();
        // create a parser and measure
        let mut parser = crate::logic::partial::parse::Parser::new(grammar.clone());
        let _ = parser.partial(&input);
        let duration = start.elapsed();
        ComplexityData::new(n, duration, input)
    };

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
}

impl ComplexityData {
    pub fn new(n: usize, time: Duration, input: String) -> Self {
        Self { n, time, input }
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
