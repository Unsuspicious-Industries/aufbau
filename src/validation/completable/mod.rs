//! Validation Test Suite for Constrained Generation

pub mod arithmetic;

pub mod fun;
pub mod stlc;
pub mod toy;
pub mod weird;

pub mod imp;

use crate::logic::grammar::Grammar;
use crate::logic::typing::core::Context;

use crate::validation::completability::{
    complete, sound_complete, CompletionResult, PrefixSoundnessResult,
};
use rayon::prelude::*;
use rayon::ThreadPoolBuilder;
use serde_json::json;
use std::time::{Duration, Instant};

fn batch_worker_count(cases_len: usize) -> usize {
    if cases_len == 0 {
        return 1;
    }

    let env_jobs = std::env::var("AUFBAU_VALIDATION_JOBS")
        .ok()
        .and_then(|s| s.parse::<usize>().ok())
        .filter(|n| *n > 0);

    // Default to a small worker pool to avoid severe oversubscription when
    // many validation tests run concurrently under `cargo test`.
    let base = env_jobs.unwrap_or(2);

    base.min(cases_len).max(1)
}

// ============================================================================
// Suite Registry
// ============================================================================

/// Collect all completable test suites.
pub fn all_suites() -> Vec<(&'static str, Grammar, Vec<TypedCompletionTestCase>)> {
    let mut out = Vec::new();
    out.extend(arithmetic::suites());
    out.extend(stlc::suites());
    out.extend(toy::suites());
    out.extend(fun::suites());
    out.extend(imp::suites());
    out.extend(weird::suites());
    out
}

// ============================================================================
// Performance Debugging Infrastructure
// ============================================================================

/// Wrapper that times completion with context
pub fn timed_sound_complete(
    grammar: &Grammar,
    input: &str,
    max_depth: usize,
    opt_ctx: Option<Context>,
) -> (PrefixSoundnessResult, Duration) {
    let start = Instant::now();
    let result = sound_complete(grammar, input, max_depth, opt_ctx);
    let elapsed = start.elapsed();
    (result, elapsed)
}

/// Simpler wrapper that times completion without prefix soundness checking.
/// Use this for cases where we only care if the full input is completable,
/// not whether all prefixes are completable.
pub fn timed_complete(
    grammar: &Grammar,
    input: &str,
    max_depth: usize,
    opt_ctx: Option<Context>,
) -> (CompletionResult, Duration) {
    let start = Instant::now();
    let result = complete(grammar, input, max_depth, opt_ctx);
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
    /// Maximum depth for completion search
    pub max_depth: usize,
    /// Initial typing context (variable bindings)
    pub context: Vec<(&'static str, &'static str)>,
    /// Whether to require all prefixes to be completable (soundness).
    pub require_prefix_soundness: bool,
    /// Timeout in seconds for the test (default: 180 = 3 minutes)
    pub timeout_secs: u64,
}

impl TypedCompletionTestCase {
    pub fn new(desc: &'static str, input: &'static str) -> Self {
        Self {
            description: desc,
            input,
            max_depth: 10,
            context: vec![],
            require_prefix_soundness: true,
            timeout_secs: 300,
        }
    }

    /// Expect-pass helper: completable input. Prefix-soundness is now required by default.
    ///
    /// Use `.without_soundness()` on the returned object when you explicitly do NOT
    /// want to require every prefix to be completable.
    pub fn ok(desc: &'static str, input: &'static str, depth: usize) -> Self {
        Self::new(desc, input).with_depth(depth)
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

    pub fn with_timeout_secs(mut self, secs: u64) -> Self {
        self.timeout_secs = secs;
        self
    }
}

/// Metadata for a single test run useful to profiling and reporting
#[derive(Debug, Clone)]
pub struct TestRunMeta {
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
    let result = run_test_inner(grammar, case);
    let out = (result.0, start.elapsed(), result.1);
    out
}

fn run_test_inner(grammar: &Grammar, case: &TypedCompletionTestCase) -> (TestResult, TestRunMeta) {
    let mut ctx = Context::new();
    for (var, ty_str) in &case.context {
        if let Ok(ty) = crate::logic::typing::Type::parse_raw(ty_str) {
            ctx.add(var.to_string(), ty);
        }
    }

    let mut meta = TestRunMeta {
        states_explored: None,
        prefix_meta: None,
        prefixes_checked: None,
        total_prefix_time_us: None,
    };

    let result = if case.require_prefix_soundness {
        let (result, _elapsed) =
            timed_sound_complete(grammar, case.input, case.max_depth, Some(ctx.clone()));
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

            if let Some(ref visited) = result.failing_prefix_visited_states {
                m.push_str(&format!("failing_visited_count={}\n", visited.len()));
                for (i, state) in visited.iter().enumerate() {
                    m.push_str(&format!("failing_visited_{}={}\n", i, state));
                }
            }

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
        let (result, _elapsed) =
            timed_complete(grammar, case.input, case.max_depth, Some(ctx.clone()));
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
        }
    };
    (result, meta)
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
/// Output emits JSON lines so external tools can parse deterministically.
pub fn run_test_batch(grammar: &Grammar, cases: &[TypedCompletionTestCase]) -> BatchResult {
    #[derive(Debug)]
    struct CaseOutcome {
        idx: usize,
        result: TestResult,
        duration: Duration,
    }

    let mut passed = 0;
    let mut failed = 0;
    let mut failures = Vec::new();
    let mut total_time = Duration::new(0, 0);

    eprintln!(
        "{}",
        json!({
            "event": "BATCH_BEGIN",
            "count": cases.len()
        })
    );

    let workers = batch_worker_count(cases.len());
    println!(
        "Launching batch with {} worker threads ({} cases,  AUFBAU_VALIDATION_JOBS={:?})",
        workers,
        cases.len(),
        std::env::var("AUFBAU_VALIDATION_JOBS")
            .ok()
            .and_then(|s| s.parse::<usize>().ok())
            .filter(|n| *n > 0)
    );
    let pool = ThreadPoolBuilder::new()
        .num_threads(workers)
        .build()
        .expect("failed to build completable thread pool");

    let mut outcomes: Vec<CaseOutcome> = pool.install(|| {
        cases
            .par_iter()
            .enumerate()
            .map(|(idx, case)| {
                let (result, duration) = run_test_timed(grammar, case);
                CaseOutcome {
                    idx,
                    result,
                    duration,
                }
            })
            .collect()
    });

    outcomes.sort_by_key(|o| o.idx);

    for out in outcomes {
        let idx = out.idx;
        let case = &cases[idx];
        let expect = "PASS";
        eprintln!(
            "{}",
            json!({
                "event": "CASE",
                "idx": idx,
                "desc": case.description,
                "input": case.input,
                "expect": expect,
                "depth": case.max_depth,
            })
        );

        let ms = out.duration.as_millis();

        match out.result {
            TestResult::Pass(completed) => {
                let comp = completed.as_deref().unwrap_or("");
                eprintln!(
                    "{}",
                    json!({
                        "event": "CASE_PASS",
                        "idx": idx,
                        "desc": case.description,
                        "time_ms": ms,
                        "completed": comp,
                    })
                );
                passed += 1;
            }
            TestResult::Fail(msg) => {
                // First line of msg is always kind=...
                let kind = msg.lines().next().unwrap_or("kind=unknown");
                eprintln!(
                    "{}",
                    json!({
                        "event": "CASE_FAIL",
                        "idx": idx,
                        "desc": case.description,
                        "input": case.input,
                        "time_ms": ms,
                        "kind": kind,
                    })
                );
                // Every subsequent line tagged with case index for grouping
                for line in msg.lines().skip(1) {
                    if !line.trim().is_empty() {
                        eprintln!(
                            "{}",
                            json!({
                                "event": "CASE_DETAIL",
                                "idx": idx,
                                "detail": line.trim(),
                            })
                        );
                    }
                }
                failed += 1;
                failures.push((case.description, case.input, msg));
            }
        }
        total_time += out.duration;
    }

    let avg_ms = if cases.is_empty() {
        0
    } else {
        (total_time / cases.len() as u32).as_millis()
    };
    eprintln!(
        "{}",
        json!({
            "event": "BATCH_END",
            "passed": passed,
            "failed": failed,
            "avg_ms": avg_ms,
            "total_ms": total_time.as_millis(),
        })
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
                "{}",
                json!({
                    "event": "BATCH_FAILURES",
                    "total": self.failed,
                    "out_of": self.passed + self.failed,
                })
            );
            for (idx, (desc, input, msg)) in self.failures.iter().enumerate() {
                let kind = msg.lines().next().unwrap_or("kind=unknown");
                eprintln!(
                    "{}",
                    json!({
                        "event": "FAILURE",
                        "idx": idx,
                        "desc": desc,
                        "input": input,
                        "kind": kind,
                    })
                );
                for line in msg.lines().skip(1) {
                    if !line.trim().is_empty() {
                        eprintln!(
                            "{}",
                            json!({
                                "event": "FAILURE_DETAIL",
                                "idx": idx,
                                "detail": line.trim(),
                            })
                        );
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
