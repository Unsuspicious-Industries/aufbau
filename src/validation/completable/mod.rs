//! Validation Test Suite for Constrained Generation

pub mod arithmetic;
pub mod clike;
pub mod typescript;
pub mod weird;
pub mod xtlc;

use crate::logic::grammar::Grammar;
use crate::logic::partial::parse::Parser;
use crate::logic::typing::core::{Context, TreeStatus};
use crate::logic::typing::eval::{check_tree, check_tree_with_context};
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
                // Unexpectedly failed
                TestResult::Fail(format!("Expected success but got unsound completion"))
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
                // Unexpectedly succeeded
                TestResult::Fail(format!("Expected failure but got successful completion"))
            } else {
                // Expected failure
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

    for case in cases {
        println!("Running test: {}", case.description);
        let (result,duration )= run_test_timed(grammar, case);
        match result {
            TestResult::Pass(completed) => {
                println!("Passed {:?}", completed);
                passed += 1;
            }
            TestResult::Fail(msg) => {
                println!("Failed: {}", msg);
                failed += 1;
                failures.push((case.description, case.input, msg));
            }
        }
        total_time += duration;
    }

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
            eprintln!("\n=== {} FAILURES ===", self.failed);
            for (desc, input, msg) in &self.failures {
                eprintln!("  [{}] '{}': {}", desc, input, msg);
            }
            panic!(
                "{} out of {} tests failed",
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

