//! Fast Parseability Test Suite
//!
//! This module provides fast validation tests that check if expressions
//! and all their prefixes can be partially parsed. Unlike the completable
//! tests which do full BFS completion search, these tests only verify
//! that the parser accepts the input - much faster for large test suites.
//!
//! ## Test Categories
//!
//! - **Valid expressions**: Complete expressions that should parse fully,
//!   and all their prefixes should parse partially
//! - **Invalid expressions**: Syntax errors that should fail to parse
//! - **Type errors (xfails)**: Syntactically valid but semantically invalid
//!   (e.g., unbound variables, type mismatches)
//!
//! ## Performance
//!
//! These tests run in O(n²) time for an input of length n (checking all prefixes),
//! but each prefix check is just a single parse - no BFS exploration.

pub mod arithmetic;
pub mod fun;
pub mod imp;
pub mod stlc;
pub mod toy;
pub mod weird;
// pub mod clike;

use crate::logic::synth::Synthesizer;
use crate::logic::grammar::Grammar;
use crate::logic::typing::core::Context;
use crate::logic::typing::Type;
use rayon::prelude::*;
use std::time::{Duration, Instant};

// ============================================================================
// Test Framework
// ============================================================================

/// Result of a parseability test
#[derive(Debug)]
pub enum ParseResult {
    /// All prefixes parsed successfully
    Pass {
        /// Time taken for all prefix checks
        duration: Duration,
        /// Number of prefixes checked
        prefix_count: usize,
    },
    /// A prefix failed to parse
    Fail {
        /// The failing prefix
        failing_prefix: String,
        /// Error message from parser
        error: String,
        /// Index of the failing prefix (0 = empty, n = full input)
        prefix_index: usize,
    },
}

impl ParseResult {
    pub fn is_pass(&self) -> bool {
        matches!(self, ParseResult::Pass { .. })
    }
}

/// A test case for parseability verification
#[derive(Debug, Clone)]
pub struct ParseTestCase {
    /// Human-readable description
    pub description: &'static str,
    /// The input to test
    pub input: &'static str,
    /// Whether this test is expected to fail (xfail)
    pub xfail: bool,
    /// Initial typing context for typed parsing
    pub context: Vec<(&'static str, &'static str)>,
}

impl ParseTestCase {
    /// Create a new test case expecting success (all prefixes parseable).
    pub fn valid(desc: &'static str, input: &'static str) -> Self {
        Self {
            description: desc,
            input,
            xfail: false,
            context: vec![],
        }
    }

    /// Create a new test case expecting parse failure (syntax error).
    pub fn invalid(desc: &'static str, input: &'static str) -> Self {
        Self {
            description: desc,
            input,
            xfail: true,
            context: vec![],
        }
    }

    /// Create a new test case expecting type error (syntactically valid but
    /// type-invalid — xfail because no complete well-typed tree should exist).
    pub fn type_error(desc: &'static str, input: &'static str) -> Self {
        Self {
            description: desc,
            input,
            xfail: true,
            context: vec![],
        }
    }

    /// Add typing context.
    pub fn with_context(mut self, ctx: Vec<(&'static str, &'static str)>) -> Self {
        self.context = ctx;
        self
    }
}

/// Check if all prefixes of an input can be partially parsed
///
/// Parallelized using Rayon by checking each prefix independently and then
/// re-assembling results in order to preserve the original failure semantics.
pub fn check_all_prefixes_parseable(
    grammar: &mut Grammar,
    input: &str,
    ctx: &Context,
) -> ParseResult {
    let start = Instant::now();
    // Prefer token-boundary prefixes when tokenization succeeds. This keeps
    // parseability checks representative while avoiding quadratic character-level
    // blowups on long/ambiguous inputs.
    let prefixes: Vec<(usize, String)> = match grammar.tokenize(input) {
        Ok(segments) => {
            let mut cuts = vec![0usize];
            cuts.extend(segments.iter().map(|s| s.end));
            if !cuts.contains(&input.len()) {
                cuts.push(input.len());
            }
            cuts.sort_unstable();
            cuts.dedup();
            cuts.into_iter()
                .map(|byte_end| {
                    let p = input[..byte_end].to_string();
                    (p.chars().count(), p)
                })
                .filter(|(len, prefix)| *len == 0 || !prefix.trim().is_empty())
                .collect()
        }
        Err(_) => {
            let chars: Vec<char> = input.chars().collect();
            (0..=chars.len())
                .map(|len| (len, chars[..len].iter().collect::<String>()))
                .filter(|(len, prefix)| *len == 0 || !prefix.trim().is_empty())
                .collect()
        }
    };

    let parse_prefix = |prefix: &str| {
        let mut synth = Synthesizer::new(grammar.clone(), prefix);
        match synth.parse_with(ctx) {
            Ok(_) => None,
            Err(e) => Some(e),
        }
    };

    let results: Vec<Option<String>> = prefixes
        .par_iter()
        .map(|(_, prefix)| parse_prefix(prefix))
        .collect();

    let prefix_count = prefixes.len();

    for ((len, prefix), opt_err) in prefixes.into_iter().zip(results.into_iter()) {
        if let Some(e) = opt_err {
            return ParseResult::Fail {
                failing_prefix: prefix,
                error: e,
                prefix_index: len,
            };
        }
    }

    ParseResult::Pass {
        duration: start.elapsed(),
        prefix_count,
    }
}

/// Check if input fails to produce a complete well-typed parse (for xfail tests)
pub fn check_parse_fails(
    grammar: &Grammar,
    input: &str,
    ctx: &Context,
) -> ParseResult {
    let start = Instant::now();
    let mut synth = Synthesizer::new(grammar.clone(), input);
    match synth.parse_with(ctx) {
        Ok(ast) => {
            if ast.is_complete() {
                ParseResult::Fail {
                    failing_prefix: input.to_string(),
                    error: "Expected failure but found a complete well-typed tree".to_string(),
                    prefix_index: input.chars().count(),
                }
            } else {
                ParseResult::Pass {
                    duration: start.elapsed(),
                    prefix_count: 1,
                }
            }
        }
        Err(_) => ParseResult::Pass {
            duration: start.elapsed(),
            prefix_count: 1,
        },
    }
}

/// Build a Context from the test case's context field
fn build_context(pairs: &[(&str, &str)]) -> Context {
    let mut ctx = Context::new();
    for (name, ty_str) in pairs {
        let ty = Type::parse_raw(ty_str)
            .unwrap_or_else(|e| panic!("Failed to parse type '{}' in test context: {}", ty_str, e));
        ctx.add(name.to_string(), ty);
    }
    ctx
}

/// Run a single parseability test case
pub fn run_parse_test(grammar: &mut Grammar, case: &ParseTestCase) -> ParseResult {
    let ctx = build_context(&case.context);
    if case.xfail {
        check_parse_fails(grammar, case.input, &ctx)
    } else {
        check_all_prefixes_parseable(grammar, case.input, &ctx)
    }
}

/// Batch test result summary
#[derive(Debug)]
pub struct BatchResult {
    pub passed: usize,
    pub failed: usize,
    pub failures: Vec<(String, ParseResult)>,
    pub total_duration: Duration,
    pub avg_duration: Duration,
}

impl BatchResult {
    /// Format a detailed error message for failed test cases
    pub fn format_failures(&self) -> String {
        if self.failures.is_empty() {
            return String::new();
        }

        let mut msg = format!("\n\n{} test(s) failed:\n", self.failures.len());
        msg.push_str("=".repeat(60).as_str());
        msg.push('\n');

        for (i, (desc, result)) in self.failures.iter().enumerate() {
            msg.push_str(&format!("\n[{}] {}\n", i + 1, desc));
            msg.push_str("-".repeat(60).as_str());
            msg.push('\n');

            match result {
                ParseResult::Fail {
                    failing_prefix,
                    error,
                    prefix_index,
                } => {
                    msg.push_str(&format!("  Failing prefix: '{}'\n", failing_prefix));
                    msg.push_str(&format!("  Prefix index:   {}\n", prefix_index));
                    msg.push_str(&format!("  Error:          {}\n", error));
                }
                ParseResult::Pass { .. } => {
                    msg.push_str("  (unexpected pass - should not be in failures list)\n");
                }
            }
        }

        msg.push('\n');
        msg.push_str("=".repeat(60).as_str());
        msg
    }
}

/// Run a batch of parseability test cases
pub fn run_parse_batch(
    grammar: &mut Grammar,
    cases: &[ParseTestCase],
) -> (BatchResult, Vec<serde_json::Value>) {
    let start = Instant::now();
    let mut passed = 0;
    let mut failed = 0;
    let mut failures = Vec::new();

    // Collect per-case JSON records so callers can consume profiling info
    let mut case_records: Vec<serde_json::Value> = Vec::with_capacity(cases.len());

    for case in cases {
        let start = Instant::now();
        // actuall running
        let result = run_parse_test(grammar, case);
        let elapsed = start.elapsed();

        // Emit a per-case JSON record for optional profiling (do not print it here)
        {
            use serde_json::json;
            let (passed_flag, prefix_count, failing_prefix, error, prefix_index) = match &result {
                ParseResult::Pass { prefix_count, .. } => (
                    true,
                    Some(*prefix_count),
                    None::<String>,
                    None::<String>,
                    None::<usize>,
                ),
                ParseResult::Fail {
                    failing_prefix,
                    error,
                    prefix_index,
                } => (
                    false,
                    None,
                    Some(failing_prefix.clone()),
                    Some(error.clone()),
                    Some(*prefix_index),
                ),
            };
            let case_obj = json!({
                "module": "parseable",
                "desc": case.description,
                "input": case.input,
                "xfail": case.xfail,
                "passed": passed_flag,
                "time_ms": elapsed.as_millis(),
                "time_us": elapsed.as_micros(),
                "prefix_count": prefix_count,
                "failing_prefix": failing_prefix,
                "error": error,
                "prefix_index": prefix_index,
            });

            // Keep the record for optional profile file generation (silent)
            case_records.push(case_obj.clone());
        }

        match &result {
            ParseResult::Pass { .. } => {
                passed += 1;
            }
            ParseResult::Fail { .. } => {
                failures.push((case.description.to_string(), result));
                failed += 1;
            }
        }
    }

    let total_duration = start.elapsed();
    let avg_duration = if cases.is_empty() {
        Duration::ZERO
    } else {
        total_duration / cases.len() as u32
    };
    (
        BatchResult {
            passed,
            failed,
            failures,
            total_duration,
            avg_duration,
        },
        case_records,
    )
}

// ============================================================================
// Suite Registry
// ============================================================================

/// Collect all parseable test suites.
///
/// Each entry is `(suite_name, grammar, valid_cases, invalid_cases)`.
pub fn all_suites() -> Vec<(
    &'static str,
    Grammar,
    Vec<ParseTestCase>,
    Vec<ParseTestCase>,
)> {
    let mut modules: Vec<(&str, Grammar, Vec<ParseTestCase>, Vec<ParseTestCase>)> = vec![
        (
            "arithmetic",
            arithmetic::arithmetic_grammar(),
            arithmetic::valid_expressions_cases(),
            arithmetic::invalid_expressions_cases(),
        ),
        (
            "fun",
            load_example_grammar("fun"),
            fun::valid_expressions_cases(),
            fun::invalid_expressions_cases(),
        ),
        (
            "imp",
            load_example_grammar("imp"),
            imp::valid_expressions_cases(),
            imp::invalid_expressions_cases(),
        ),
        (
            "stlc",
            load_example_grammar("stlc"),
            stlc::valid_expressions_cases(),
            stlc::invalid_expressions_cases(),
        ),
        (
            "toy",
            load_example_grammar("toy"),
            toy::valid_expressions_cases(),
            toy::invalid_expressions_cases(),
        ),
    ];
    modules.extend(weird::suites());
    modules
}

// ============================================================================
// Grammar Loading Utilities
// ============================================================================

/// Load a grammar from the examples directory
pub fn load_example_grammar(name: &str) -> Grammar {
    use std::path::Path;
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let path = Path::new(manifest_dir)
        .join("examples")
        .join(format!("{}.auf", name));
    let content = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("Failed to read {}: {}", path.display(), e));
    Grammar::load(&content).unwrap_or_else(|e| panic!("Failed to load {}: {}", name, e))
}
