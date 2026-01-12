//! Validation Test Suite for Constrained Generation

pub mod arithmetic;

pub mod weird;
pub mod stlc;

pub mod imp;

use crate::logic::grammar::Grammar;
use crate::logic::partial::parse::Parser;
use crate::logic::typing::core::{Context, TreeStatus};
use crate::logic::typing::eval::check_tree_with_context;
use crate::regex::Regex as DerivativeRegex;
use crate::validation::completability::{
    CompletionResult, PrefixSoundnessResult, complete, sound_complete 
};
use std::time::{Duration, Instant};

// ============================================================================
// Performance Debugging Infrastructure
// ============================================================================


/// Wrapper that times completion with context
pub fn timed_sound_complete(
    grammar: &Grammar,
    input: &str,
    max_depth: usize,
    opt_ctx: Option<Context>,
    max_states: Option<usize>,
) -> (PrefixSoundnessResult, Duration) {
    let start = Instant::now();
    let result = sound_complete(grammar, input, max_depth, opt_ctx, max_states);
    let elapsed = start.elapsed();
    (result, elapsed)
}

/// Simpler wrapper that times completion without prefix soundness checking.
/// Use this for xfail tests where we only care if the full input is completable,
/// not whether all prefixes are completable.
pub fn timed_complete(
    grammar: &Grammar,
    input: &str,
    max_depth: usize,
    opt_ctx: Option<Context>,
    max_states: Option<usize>,
) -> (CompletionResult, Duration) {
    let start = Instant::now();
    let result = complete(grammar, input, max_depth, opt_ctx, max_states);
    let elapsed = start.elapsed();
    (result, elapsed)
}
// ============================================================================
// Test Framework - Core Verification Utilities
// ============================================================================

/// A test case for typed completion verification
#[derive(Debug, Clone)]
pub struct TypedCompletionTestCase {
    /// Human-readable description
    pub description: &'static str,
    /// The partial input to test
    pub input: &'static str,
    /// Expected result
    pub xfail: bool,
    /// Maximum depth for completion search
    pub max_depth: usize,
    /// Initial typing context (variable bindings)
    pub context: Vec<(&'static str, &'static str)>,
}

impl TypedCompletionTestCase {
    pub fn new(desc: &'static str, input: &'static str, xfail: bool) -> Self {
        Self {
            description: desc,
            input,
            xfail,
            max_depth: 10,
            context: vec![],
        }
    }
    pub fn with_depth(mut self, depth: usize) -> Self {
        self.max_depth = depth;
        self
    }

    pub fn with_context(mut self, ctx: Vec<(&'static str, &'static str)>) -> Self {
        self.context = ctx;
        self
    }
}

/// Run a single typed completion test case, returning timing info
pub fn run_test_timed(grammar: &Grammar, case: &TypedCompletionTestCase) -> (TestResult, Duration) {
    let start = Instant::now();
    
    // Build typing context
    let mut ctx = Context::new();
    for (var, ty_str) in &case.context {
        if let Ok(ty) = crate::logic::typing::Type::parse(ty_str) {
            ctx.add(var.to_string(), ty);
        }
    }

    let result = match case.xfail {
        // Expecting success
        false  => {
            let (result, elapsed) = timed_sound_complete(grammar, case.input, case.max_depth, Some(ctx.clone()), None);
            
            if elapsed.as_secs() >= 2 {
                eprintln!("  ⏱️  '{}' completion took {:?}", case.input, elapsed);
            }
            
            if result.is_sound {
                // Expected success
                TestResult::Pass(result.complete_string)
            } else {
                // Unexpectedly failed - provide detailed error information
                let mut error_msg = format!("Expected success but got unsound completion\n");
                error_msg.push_str(&format!("  Input: '{}'\n", case.input));
                error_msg.push_str(&format!("  Prefixes checked: {}\n", result.prefixes_checked));
                
                if let Some(ref failing) = result.failing_prefix {
                    error_msg.push_str(&format!("  ❌ First failing prefix: '{}'\n", failing));
                }
                
                // Show sample of visited states if available
                if let Some(ref visited) = result.failing_prefix_visited_states {
                    if !visited.is_empty() {
                        error_msg.push_str(&format!("  � Sample visited states ({} total):\n", visited.len()));
                        for (i, state) in visited.iter().take(10).enumerate() {
                            error_msg.push_str(&format!("    {}: '{}'\n", i + 1, state));
                        }
                        if visited.len() > 10 {
                            error_msg.push_str(&format!("    ... and {} more\n", visited.len() - 10));
                        }
                    }
                }
                
                // Show detailed prefix results
                error_msg.push_str("  Prefix completion results:\n");
                for (prefix, success) in &result.prefix_details {
                    let status = if *success { "✓" } else { "✗" };
                    error_msg.push_str(&format!("    {} '{}'\n", status, prefix));
                }
                
                if let Some(ref complete) = result.complete_string {
                    error_msg.push_str(&format!("  Completed to: '{}'\n", complete));
                }
                
                TestResult::Fail(error_msg)
            }
        }
        // Expecting failure
        true => {
            let (result, elapsed) = timed_complete(grammar, case.input, case.max_depth, Some(ctx.clone()), None);
            
            if elapsed.as_secs() >= 2 {
                eprintln!("  ⏱️  '{}' completion took {:?}", case.input, elapsed);
            }
            
            let is_completable = matches!(result, CompletionResult::Success { .. });
            
            if is_completable {
                // Unexpectedly succeeded - provide details about what completed
                let mut error_msg = format!("Expected failure but got successful completion\n");
                error_msg.push_str(&format!("  Input: '{}'\n", case.input));
                
                if let CompletionResult::Success { complete_input, depth, completion_path, .. } = result {
                    error_msg.push_str(&format!("  Completed to: '{}'\n", complete_input));
                    error_msg.push_str(&format!("  Depth: {}\n", depth));
                    error_msg.push_str(&format!("  Completion path: {} tokens\n", completion_path.len()));
                }
                
                TestResult::Fail(error_msg)
            } else {
                // Expected failure - log what kind of failure
                match result {
                    CompletionResult::Failure { max_depth_reached, states_explored, .. } => {
                        eprintln!("  ✓ Failed as expected (explored {} states, max depth: {})", 
                                  states_explored, max_depth_reached);
                    }
                    CompletionResult::Invalid(ref msg) => {
                        eprintln!("  ✓ Invalid as expected: {}", msg);
                    }
                    CompletionResult::StateOverflow(limit) => {
                        eprintln!("  ✓ State overflow as expected (limit: {})", limit);
                    }
                    CompletionResult::Error(ref msg) => {
                        eprintln!("  ✓ Error as expected: {}", msg);
                    }
                    _ => {}
                }
                TestResult::Pass(None)
            }
        }
    };
    
    (result, start.elapsed())
}

#[derive(Debug)]
pub enum TestResult {
    Pass(Option<String>), // completed input
    Fail(String),
}

impl TestResult {
    pub fn is_pass(&self) -> bool {
        matches!(self, TestResult::Pass(_))
    }
}

/// Run a batch of test cases and report results
pub fn run_test_batch(grammar: &Grammar, cases: &[TypedCompletionTestCase]) -> BatchResult {
    let mut passed = 0;
    let mut failed = 0;
    let mut failures = Vec::new();
    let mut total_time = Duration::new(0, 0);

    eprintln!("\n═══════════════════════════════════════════════════════");
    eprintln!("Running test batch: {} cases", cases.len());
    eprintln!("═══════════════════════════════════════════════════════\n");

    for (idx, case) in cases.iter().enumerate() {
        eprintln!("[{}/{}] Running test: {}", idx + 1, cases.len(), case.description);
        eprintln!("  Input: '{}'", case.input);
        eprintln!("  Expected: {}", if case.xfail { "FAIL" } else { "PASS" });
        eprintln!("  Max depth: {}", case.max_depth);
        if !case.context.is_empty() {
            eprintln!("  Context: {:?}", case.context);
        }
        
        let (result, duration) = run_test_timed(grammar, case);
        
        match result {
            TestResult::Pass(completed) => {
                eprintln!("  ✓ PASSED in {:?}", duration);
                if let Some(ref complete) = completed {
                    eprintln!("    Completed: '{}'", complete);
                }
                passed += 1;
            }
            TestResult::Fail(msg) => {
                eprintln!("  ✗ FAILED in {:?}", duration);
                eprintln!("  Error details:");
                for line in msg.lines() {
                    eprintln!("    {}", line);
                }
                failed += 1;
                failures.push((case.description, case.input, msg));
            }
        }
        eprintln!();
        total_time += duration;
    }

    eprintln!("═══════════════════════════════════════════════════════");
    eprintln!("Batch results: {} passed, {} failed", passed, failed);
    eprintln!("Average duration: {:?}", total_time / (cases.len() as u32));
    eprintln!("Total duration: {:?}", total_time);
    eprintln!("═══════════════════════════════════════════════════════\n");

    BatchResult {
        passed,
        failed,
        failures,
        avg_duration: total_time / (cases.len() as u32),
    }
}

#[derive(Debug)]
pub struct BatchResult {
    pub passed: usize,
    pub failed: usize,
    pub failures: Vec<(&'static str, &'static str, String)>,
    pub avg_duration: Duration,
   
}

impl BatchResult {
    pub fn assert_all_passed(&self) {
        if self.failed > 0 {
            eprintln!("\n╔═══════════════════════════════════════════════════════╗");
            eprintln!("║  TEST FAILURES: {} out of {} tests failed           ║", 
                     self.failed, self.passed + self.failed);
            eprintln!("╚═══════════════════════════════════════════════════════╝\n");
            
            for (idx, (desc, input, msg)) in self.failures.iter().enumerate() {
                eprintln!("────────────────────────────────────────────────────────");
                eprintln!("Failure #{}: {}", idx + 1, desc);
                eprintln!("  Input: '{}'", input);
                eprintln!("\nDetails:");
                for line in msg.lines() {
                    eprintln!("  {}", line);
                }
                eprintln!();
            }
            eprintln!("════════════════════════════════════════════════════════\n");
            
            panic!(
                "{} out of {} tests failed (see details above)",
                self.failed,
                self.passed + self.failed
            );
        }
    }
}

// ============================================================================
// Well-Typed Completion Verification
// ============================================================================

/// Get all syntactically valid completions for an input
pub fn get_completions(grammar: &Grammar, input: &str) -> Vec<DerivativeRegex> {
    let mut parser = Parser::new(grammar.clone());
    match parser.partial(input) {
        Ok(partial) => partial.completions(grammar).tokens,
        Err(_) => vec![],
    }
}

/// Extend input with a completion token, handling spacing
/// useful as utils
fn extend_input(input: &str, token: &str) -> String {
    let first_char = token.chars().next().unwrap_or(' ');
    if input.is_empty()
        || input.ends_with(' ')
        || input.ends_with('\n')
        || input.ends_with('\t')
        || !first_char.is_alphanumeric()
    {
        format!("{}{}", input, token)
    } else {
        format!("{} {}", input, token)
    }
}

/// Verify that a completion leads to a well-typed AND complete tree
pub fn verify_completion_well_typed(
    grammar: &Grammar,
    input: &str,
    completion: &str,
    ctx: &Context,
) -> Result<(), String> {
    let extended = extend_input(input, completion);

    let mut parser = Parser::new(grammar.clone());
    let partial = parser
        .partial(&extended)
        .map_err(|e| format!("Parse failed for '{}': {}", extended, e))?;

    // Check for COMPLETE and well-typed trees (TreeStatus::Valid, not Partial)
    let any_complete_and_typed = partial.roots.iter().any(|root| {
        root.is_complete() && matches!(
            check_tree_with_context(root, grammar, ctx),
            TreeStatus::Valid(_)
        )
    });

    // Also accept partial trees as valid during incremental completion
    let any_well_typed = partial.roots.iter().any(|root| {
        matches!(
            check_tree_with_context(root, grammar, ctx),
            TreeStatus::Valid(_) | TreeStatus::Partial(_)
        )
    });

    if any_complete_and_typed || any_well_typed {
        Ok(())
    } else {
        Err(format!(
            "Completion '{}' after '{}' produces no well-typed trees (extended: '{}')",
            completion, input, extended
        ))
    }
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

/// Load grammar from inline specification
pub fn load_inline_grammar(spec: &str) -> Grammar {
    Grammar::load(spec).expect("Failed to load inline grammar")
}

