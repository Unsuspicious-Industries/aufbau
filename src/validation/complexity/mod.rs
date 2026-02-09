//! Complexity Analysis Module
//!
//! This module provides utilities to empirically measure the time complexity
//! of the parser on different grammar types and input sizes.

pub mod stlc;
pub mod basic;
pub mod fun;

use std::time::Duration;

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
