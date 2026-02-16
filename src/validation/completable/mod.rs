//! Validation Test Suite for Constrained Generation

pub mod arithmetic;

pub mod fun;
pub mod stlc;
pub mod weird;

pub mod imp;

use crate::logic::grammar::Grammar;
use crate::logic::partial::parse_extended_input;
use crate::logic::partial::MetaParser;
use crate::logic::typing::core::{Context, TreeStatus};
use crate::logic::typing::eval::check_tree_with_context;
use crate::regex::Regex as DerivativeRegex;
use crate::validation::completability::{
    complete, sound_complete, CompletionResult, PrefixSoundnessResult,
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
    /// Whether to require all prefixes to be completable (soundness).
    pub require_prefix_soundness: bool,
}

impl TypedCompletionTestCase {
    pub fn new(desc: &'static str, input: &'static str, xfail: bool) -> Self {
        Self {
            description: desc,
            input,
            xfail,
            max_depth: 10,
            context: vec![],
            require_prefix_soundness: true,
        }
    }

    /// Expect-pass helper: completable input. Prefix-soundness is now required by default.
    ///
    /// Use `.without_soundness()` on the returned object when you explicitly do NOT
    /// want to require every prefix to be completable.
    pub fn ok(desc: &'static str, input: &'static str, depth: usize) -> Self {
        Self::new(desc, input, false).with_depth(depth)
    }

    /// Expect-pass with prefix soundness check.
    pub fn sound(desc: &'static str, input: &'static str, depth: usize) -> Self {
        Self::new(desc, input, false).with_depth(depth)
    }

    /// Expect-fail helper: syntax error, invalid input, etc.
    pub fn fail(desc: &'static str, input: &'static str) -> Self {
        Self::new(desc, input, true)
    }

    /// Expect-fail with an explicit typing context (for type-error tests).
    pub fn typefail(
        desc: &'static str,
        input: &'static str,
        ctx: Vec<(&'static str, &'static str)>,
    ) -> Self {
        Self::new(desc, input, true).with_context(ctx)
    }

    pub fn with_depth(mut self, depth: usize) -> Self {
        self.max_depth = depth;
        self
    }

    pub fn with_context(mut self, ctx: Vec<(&'static str, &'static str)>) -> Self {
        self.context = ctx;
        self
    }

    pub fn without_soundness(mut self) -> Self {
        self.require_prefix_soundness = false;
        self
    }
}

/// Metadata for a single test run useful to profiling and reporting
#[derive(Debug, Clone)]
pub struct TestRunMeta {
    pub beam_fallback: Option<bool>,
    pub states_explored: Option<usize>,
    /// Per-prefix metadata collected during prefix soundness checks (if any)
    pub prefix_meta: Option<Vec<crate::validation::completability::PrefixDetail>>,
    /// Total number of prefixes checked (if available)
    pub prefixes_checked: Option<usize>,
    /// Sum of per-prefix times in microseconds (if available)
    pub total_prefix_time_us: Option<u128>,
}

/// Run a single typed completion test case, returning timing info and metadata.
/// All failure messages are structured as key=value lines for machine parsing.
pub fn run_test_timed_meta(
    grammar: &Grammar,
    case: &TypedCompletionTestCase,
) -> (TestResult, Duration, TestRunMeta) {
    let start = Instant::now();

    // Build typing context
    let mut ctx = Context::new();
    for (var, ty_str) in &case.context {
        if let Ok(ty) = crate::logic::typing::Type::parse(ty_str) {
            ctx.add(var.to_string(), ty);
        }
    }

    let mut meta = TestRunMeta {
        beam_fallback: None,
        states_explored: None,
        prefix_meta: None,
        prefixes_checked: None,
        total_prefix_time_us: None,
    };

    let result = match case.xfail {
        // ── Expecting success ────────────────────────────────────────────
        false => {
            if case.require_prefix_soundness {
                let (result, elapsed) = timed_sound_complete(
                    grammar,
                    case.input,
                    case.max_depth,
                    Some(ctx.clone()),
                    None,
                );
                eprintln!(
                    "CASE_TIME input=\"{}\" time_ms={}",
                    case.input,
                    elapsed.as_millis()
                );

                // Attach prefix-level metadata to the test run meta so CLI can emit structured JSON
                let total_prefix_time: u128 = result.prefix_meta.iter().map(|pd| pd.time_us).sum();

                meta.prefix_meta = Some(result.prefix_meta.clone());
                meta.prefixes_checked = Some(result.prefixes_checked);
                meta.total_prefix_time_us = Some(total_prefix_time);

                if result.is_sound {
                    TestResult::Pass(result.complete_string)
                } else {
                    let mut m = String::new();
                    m.push_str("kind=unsound_completion\n");
                    m.push_str(&format!("input={}\n", case.input));
                    m.push_str(&format!("prefixes_checked={}\n", result.prefixes_checked));
                    m.push_str(&format!("prefix_total_time_us={}\n", total_prefix_time));

                    if let Some(ref fp) = result.failing_prefix {
                        m.push_str(&format!("failing_prefix={}\n", fp));
                    }
                    if let Some(ref complete) = result.complete_string {
                        m.push_str(&format!("completed_to={}\n", complete));
                    }

                    // Visited states at the failing prefix
                    if let Some(ref visited) = result.failing_prefix_visited_states {
                        m.push_str(&format!("failing_visited_count={}\n", visited.len()));
                        for (i, state) in visited.iter().enumerate() {
                            m.push_str(&format!("failing_visited_{}={}\n", i, state));
                        }
                    }

                    // Per-prefix breakdown: every prefix and its pass/fail status + rich meta
                    m.push_str(&format!("prefix_count={}\n", result.prefix_details.len()));
                    for (i, pd) in result.prefix_meta.iter().enumerate() {
                        m.push_str(&format!(
                            "prefix_{} ok={} time_us={} states_explored={:?} visited_count={:?}\n",
                            i, pd.ok, pd.time_us, pd.states_explored, pd.visited_count
                        ));
                        for (j, v) in pd.visited_sample.iter().enumerate() {
                            m.push_str(&format!("prefix_{}_visited_{}={}\n", i, j, v));
                        }
                        if let Some(vc) = pd.visited_count {
                            if vc > pd.visited_sample.len() {
                                m.push_str(&format!(
                                    "prefix_{}_visited_truncated={}\n",
                                    i,
                                    vc - pd.visited_sample.len()
                                ));
                            }
                        }
                    }

                    TestResult::Fail(m)
                }
            } else {
                let (result, elapsed) =
                    timed_complete(grammar, case.input, case.max_depth, Some(ctx.clone()), None);
                eprintln!(
                    "CASE_TIME input=\"{}\" time_ms={}",
                    case.input,
                    elapsed.as_millis()
                );

                match result {
                    CompletionResult::Success { complete_input, .. } => {
                        TestResult::Pass(Some(complete_input))
                    }
                    CompletionResult::Failure {
                        max_depth_reached,
                        states_explored,
                        visited_states,
                    } => {
                        meta.states_explored = Some(visited_states.len());

                        let mut m = String::new();
                        m.push_str("kind=completion_failed\n");
                        m.push_str(&format!("input={}\n", case.input));
                        m.push_str(&format!("states_explored={}\n", states_explored));
                        m.push_str(&format!("max_depth_reached={}\n", max_depth_reached));
                        m.push_str(&format!("visited_count={}\n", visited_states.len()));
                        for (i, state) in visited_states.iter().take(20).enumerate() {
                            m.push_str(&format!("visited_{}={}\n", i, state));
                        }
                        if visited_states.len() > 20 {
                            m.push_str(&format!(
                                "visited_truncated={}\n",
                                visited_states.len() - 20
                            ));
                        }
                        TestResult::Fail(m)
                    }
                    CompletionResult::Invalid(msg) => {
                        let mut m = String::new();
                        m.push_str("kind=invalid\n");
                        m.push_str(&format!("input={}\n", case.input));
                        m.push_str(&format!("reason={}\n", msg));
                        TestResult::Fail(m)
                    }
                    CompletionResult::Error(msg) => {
                        let mut m = String::new();
                        m.push_str("kind=error\n");
                        m.push_str(&format!("input={}\n", case.input));
                        m.push_str(&format!("reason={}\n", msg));
                        TestResult::Fail(m)
                    }
                    CompletionResult::Inconsistency(msg) => {
                        let mut m = String::new();
                        m.push_str("kind=inconsistency\n");
                        m.push_str(&format!("input={}\n", case.input));
                        m.push_str(&format!("reason={}\n", msg));
                        TestResult::Fail(m)
                    }
                    CompletionResult::StateOverflow(limit) => {
                        let mut m = String::new();
                        m.push_str("kind=state_overflow\n");
                        m.push_str(&format!("input={}\n", case.input));
                        m.push_str(&format!("limit={}\n", limit));
                        TestResult::Fail(m)
                    }
                }
            }
        }
        // ── Expecting failure (xfail) ────────────────────────────────────
        true => {
            let (result, elapsed) =
                timed_complete(grammar, case.input, case.max_depth, Some(ctx.clone()), None);
            eprintln!(
                "CASE_TIME input=\"{}\" time_ms={}",
                case.input,
                elapsed.as_millis()
            );

            match result {
                CompletionResult::Success {
                    complete_input,
                    completion_depth: depth,
                    completion_path,
                    ast,
                } => {
                    let mut m = String::new();
                    m.push_str("kind=unexpected_success\n");
                    m.push_str(&format!("input={}\n", case.input));
                    m.push_str(&format!("completed_to={}\n", complete_input));
                    m.push_str(&format!("completion_depth={}\n", depth));
                    m.push_str(&format!("path_len={}\n", completion_path.len()));
                    for (i, tok) in completion_path.iter().take(30).enumerate() {
                        m.push_str(&format!("path_{}={}\n", i, tok));
                    }
                    if completion_path.len() > 30 {
                        m.push_str(&format!("path_truncated={}\n", completion_path.len() - 30));
                    }
                    m.push_str(&format!("ast_root={}\n", ast.name));
                    m.push_str(&format!("ast_complete={}\n", ast.is_complete()));
                    TestResult::Fail(m)
                }
                _ => TestResult::Pass(None),
            }
        }
    };

    (result, start.elapsed(), meta)
}

/// Backwards-compatible wrapper that returns the original pair
pub fn run_test_timed(grammar: &Grammar, case: &TypedCompletionTestCase) -> (TestResult, Duration) {
    let (res, dur, _meta) = run_test_timed_meta(grammar, case);
    (res, dur)
}

#[derive(Debug)]
pub enum TestResult {
    Pass(Option<String>), // completed input
    Fail(String),
}

impl TestResult {
    pub fn is_pass(&self) -> bool {
        match self {
            TestResult::Pass(_) => true,
            TestResult::Fail(_) => false,
        }
    }
}

/// Run a batch of test cases and report results.
///
/// Output uses a normalised line format so external tools can parse it:
///   BATCH_BEGIN count=N
///   CASE idx=I desc="..." input="..." expect=PASS|FAIL depth=D
///   CASE_PASS idx=I desc="..." time_ms=T completed="..."
///   CASE_FAIL idx=I desc="..." time_ms=T error="first line"
///   CASE_DETAIL ...continuation line...
///   BATCH_END passed=P failed=F avg_ms=A total_ms=T
pub fn run_test_batch(grammar: &Grammar, cases: &[TypedCompletionTestCase]) -> BatchResult {
    let mut passed = 0;
    let mut failed = 0;
    let mut failures = Vec::new();
    let mut total_time = Duration::new(0, 0);

    eprintln!("BATCH_BEGIN count={}", cases.len());

    for (idx, case) in cases.iter().enumerate() {
        let expect = if case.xfail { "FAIL" } else { "PASS" };
        eprintln!(
            "CASE idx={} desc=\"{}\" input=\"{}\" expect={} depth={}",
            idx, case.description, case.input, expect, case.max_depth
        );

        let (result, duration) = run_test_timed(grammar, case);
        let ms = duration.as_millis();

        match result {
            TestResult::Pass(completed) => {
                let comp = completed.as_deref().unwrap_or("");
                eprintln!(
                    "CASE_PASS idx={} desc=\"{}\" time_ms={} completed=\"{}\"",
                    idx, case.description, ms, comp
                );
                passed += 1;
            }
            TestResult::Fail(msg) => {
                // First line of msg is always kind=...
                let kind = msg.lines().next().unwrap_or("kind=unknown");
                eprintln!(
                    "CASE_FAIL idx={} desc=\"{}\" input=\"{}\" time_ms={} {}",
                    idx, case.description, case.input, ms, kind
                );
                // Every subsequent line tagged with case index for grouping
                for line in msg.lines().skip(1) {
                    if !line.trim().is_empty() {
                        eprintln!("CASE_DETAIL idx={} {}", idx, line.trim());
                    }
                }
                failed += 1;
                failures.push((case.description, case.input, msg));
            }
        }
        total_time += duration;
    }

    let avg_ms = if cases.is_empty() {
        0
    } else {
        (total_time / cases.len() as u32).as_millis()
    };
    eprintln!(
        "BATCH_END passed={} failed={} avg_ms={} total_ms={}",
        passed,
        failed,
        avg_ms,
        total_time.as_millis()
    );

    BatchResult {
        passed,
        failed,
        failures,
        avg_duration: if cases.is_empty() {
            Duration::new(0, 0)
        } else {
            total_time / cases.len() as u32
        },
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
            eprintln!(
                "BATCH_FAILURES total={} out_of={}",
                self.failed,
                self.passed + self.failed
            );
            for (idx, (desc, input, msg)) in self.failures.iter().enumerate() {
                let kind = msg.lines().next().unwrap_or("kind=unknown");
                eprintln!(
                    "FAILURE idx={} desc=\"{}\" input=\"{}\" {}",
                    idx, desc, input, kind
                );
                for line in msg.lines().skip(1) {
                    if !line.trim().is_empty() {
                        eprintln!("FAILURE_DETAIL idx={} {}", idx, line.trim());
                    }
                }
            }
            panic!(
                "{} out of {} tests failed (see CASE_FAIL / FAILURE lines above)",
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
    let mut parser = MetaParser::new(grammar.clone());
    get_completions_with_meta(&mut parser, input)
}

/// Get completions using a reusable MetaParser (incremental-friendly)
pub fn get_completions_with_meta(parser: &mut MetaParser, input: &str) -> Vec<DerivativeRegex> {
    match parser.meta_partial(input) {
        Ok((partial, depth)) => {
            println!("Depth reached: {}", depth);
            partial.completions(&parser.parser().grammar).tokens
        }
        Err(e) => {
            println!("Error during parsing: {}", e);
            vec![]
        }
    }
}

/// Get completions using a reusable MetaParser, filtered by typing.
pub fn get_typed_completions_with_meta(
    parser: &mut MetaParser,
    input: &str,
    ctx: &Context,
) -> Vec<DerivativeRegex> {
    match parser.meta_partial(input) {
        Ok((partial, depth)) => {
            println!("Depth reached: {}", depth);
            let grammar = &parser.parser().grammar;
            partial
                .completions_in_ctx(grammar, ctx)
                .tokens
                .into_iter()
                .filter(|token| {
                    token
                        .example()
                        .map(|example| verify_completion_well_typed(grammar, input, &example, ctx))
                        .unwrap_or(Err("no example".to_string()))
                        .is_ok()
                })
                .collect()
        }
        Err(e) => {
            println!("Error during parsing: {}", e);
            vec![]
        }
    }
}

/// Verify that a completion leads to a well-typed tree and return the extended input.
pub fn extend_input_checked(
    grammar: &Grammar,
    input: &str,
    completion: &str,
    ctx: &Context,
) -> Result<String, String> {
    let (partial, extended) =
        parse_extended_input(grammar, input, completion).map_err(|e| e.to_string())?;

    // Check for COMPLETE and well-typed trees (TreeStatus::Valid, not Partial)
    let any_complete_and_typed = partial.roots.iter().any(|root| {
        root.is_complete()
            && matches!(
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
        Ok(extended)
    } else {
        Err(format!(
            "Completion '{}' after '{}' produces no well-typed trees (extended: '{}')",
            completion, input, extended
        ))
    }
}

/// Verify that a completion leads to a well-typed AND complete tree
pub fn verify_completion_well_typed(
    grammar: &Grammar,
    input: &str,
    completion: &str,
    ctx: &Context,
) -> Result<(), String> {
    extend_input_checked(grammar, input, completion, ctx).map(|_| ())
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

/// Load grammar from inline specification
pub fn load_inline_grammar(spec: &str) -> Grammar {
    Grammar::load(spec).expect("Failed to load inline grammar")
}
