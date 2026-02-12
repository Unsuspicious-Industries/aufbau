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

pub mod fun;
pub mod imp;
pub mod stlc;
pub mod weird;
// pub mod clike;

use crate::logic::grammar::Grammar;
use crate::logic::partial::parse::Parser;
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
    /// Whether to check typing (use partial_typed vs partial)
    pub check_typing: bool,
    /// Initial typing context for typed parsing
    pub context: Vec<(&'static str, &'static str)>,
}

impl ParseTestCase {
    /// Create a new test case expecting success
    pub fn valid(desc: &'static str, input: &'static str) -> Self {
        Self {
            description: desc,
            input,
            xfail: false,
            check_typing: false, // partial type checking unsupported
            context: vec![],
        }
    }

    /// Create a new test case expecting structural parse success (no type checking)
    pub fn structural(desc: &'static str, input: &'static str) -> Self {
        Self {
            description: desc,
            input,
            xfail: false,
            check_typing: false,
            context: vec![],
        }
    }

    /// Create a new test case expecting parse failure (syntax error)
    pub fn invalid(desc: &'static str, input: &'static str) -> Self {
        Self {
            description: desc,
            input,
            xfail: true,
            check_typing: false,
            context: vec![],
        }
    }

    /// Create a new test case expecting type error (syntactically valid but type-invalid)
    pub fn type_error(desc: &'static str, input: &'static str) -> Self {
        Self {
            description: desc,
            input,
            xfail: true,
            check_typing: true,
            context: vec![],
        }
    }

    /// Enable type checking for this test
    pub fn with_typing(mut self) -> Self {
        self.check_typing = true;
        self
    }

    /// Add typing context
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
    grammar: &Grammar,
    input: &str,
    check_typing: bool,
) -> ParseResult {
    let start = Instant::now();
    let chars: Vec<char> = input.chars().collect();

    // Determine the range of prefix lengths to check (exclude full input here)
    let max_prefix = if chars.is_empty() { 0 } else { chars.len() - 1 };

    // Collect prefixes to check, skipping whitespace-only prefixes after the first
    let prefixes: Vec<(usize, String)> = (0..=max_prefix)
        .map(|len| (len, chars[..len].iter().collect::<String>()))
        .filter(|(len, prefix)| *len == 0 || !prefix.trim().is_empty())
        .collect();

    // Check prefixes in parallel. Each item returns `Option<String>` where `Some(err)`
    // indicates a parse error for that prefix, while `None` indicates success.
    let results: Vec<Option<String>> = prefixes
        .par_iter()
        .map(|(_len, prefix)| {
            let mut parser = Parser::new(grammar.clone());
            let res = if check_typing {
                parser.partial_typed(prefix)
            } else {
                parser.partial(prefix)
            };
            match res {
                Ok(_) => None,
                Err(e) => Some(e),
            }
        })
        .collect();

    // Re-assemble results in original order and return the first failing prefix, if any
    for ((len, prefix), opt_err) in prefixes.into_iter().zip(results.into_iter()) {
        if let Some(e) = opt_err {
            return ParseResult::Fail {
                failing_prefix: prefix,
                error: e,
                prefix_index: len,
            };
        }
    }

    // Check the full input. For typed checks, require at least one complete
    // well-typed tree (not just partial typed branches).
    let mut parser = Parser::new(grammar.clone());
    if check_typing {
        match parser.partial(input) {
            Ok(ast) => {
                if let Err(e) = ast.typed_complete(grammar) {
                    return ParseResult::Fail {
                        failing_prefix: input.to_string(),
                        error: format!("Expected complete well-typed tree, got: {}", e),
                        prefix_index: input.chars().count(),
                    };
                }
            }
            Err(e) => {
                return ParseResult::Fail {
                    failing_prefix: input.to_string(),
                    error: e,
                    prefix_index: input.chars().count(),
                };
            }
        }
    } else if let Err(e) = parser.partial(input) {
        return ParseResult::Fail {
            failing_prefix: input.to_string(),
            error: e,
            prefix_index: input.chars().count(),
        };
    }

    ParseResult::Pass {
        duration: start.elapsed(),
        prefix_count: chars.len() + 1,
    }
}

/// Check if input fails to parse (for xfail tests)
pub fn check_parse_fails(grammar: &Grammar, input: &str, check_typing: bool) -> ParseResult {
    let start = Instant::now();
    let mut parser = Parser::new(grammar.clone());

    if check_typing {
        // For type errors, syntax may still parse. We only fail this check if a
        // complete well-typed tree exists.
        match parser.partial(input) {
            Ok(ast) => {
                if ast.typed_complete(grammar).is_ok() {
                    ParseResult::Fail {
                        failing_prefix: input.to_string(),
                        error: "Expected type failure but found a complete well-typed tree"
                            .to_string(),
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
    } else {
        match parser.partial(input) {
            Ok(t) => ParseResult::Fail {
                failing_prefix: input.to_string(),
                error: format!("Expected parse/type failure but succeeded with {}", t).to_string(),
                prefix_index: input.chars().count(),
            },
            Err(_) => ParseResult::Pass {
                duration: start.elapsed(),
                prefix_count: 1,
            },
        }
    }
}

/// Run a single parseability test case
pub fn run_parse_test(grammar: &Grammar, case: &ParseTestCase) -> ParseResult {
    if case.xfail {
        // For xfail cases, we expect the full input to fail parsing
        check_parse_fails(grammar, case.input, case.check_typing)
    } else {
        // For valid cases, all prefixes should be parseable
        check_all_prefixes_parseable(grammar, case.input, case.check_typing)
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

        msg.push_str("\n");
        msg.push_str("=".repeat(60).as_str());
        msg
    }
}

/// Run a batch of parseability test cases
pub fn run_parse_batch(grammar: &Grammar, cases: &[ParseTestCase]) -> (BatchResult, Vec<serde_json::Value>) {
    let start = Instant::now();
    let mut passed = 0;
    let mut failed = 0;
    let mut failures = Vec::new();

    // Collect per-case JSON records so callers can consume profiling info
    let mut case_records: Vec<serde_json::Value> = Vec::with_capacity(cases.len());

    for case in cases {
        let start = Instant::now();
        let result = run_parse_test(grammar, case);
        let elapsed = start.elapsed();

        // Emit a per-case JSON record for optional profiling (do not print it here)
        {
            use serde_json::json;
            let (passed_flag, prefix_count, failing_prefix, error, prefix_index) = match &result {
                ParseResult::Pass { prefix_count, .. } => (true, Some(*prefix_count as usize), None::<String>, None::<String>, None::<usize>),
                ParseResult::Fail { failing_prefix, error, prefix_index } => (false, None, Some(failing_prefix.clone()), Some(error.clone()), Some(*prefix_index)),
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
// Grammar Loading Utilities
// ============================================================================

/// Load a grammar from the examples directory
pub fn load_example_grammar(name: &str) -> Grammar {
    use std::path::Path;
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let path = Path::new(manifest_dir)
        .join("examples")
        .join(format!("{}.spec", name));
    let content = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("Failed to read {}: {}", path.display(), e));
    Grammar::load(&content).unwrap_or_else(|e| panic!("Failed to load {}: {}", name, e))
}
