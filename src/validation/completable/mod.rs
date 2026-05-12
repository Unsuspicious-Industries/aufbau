//! Slower empirical suites built on feed replay.

pub mod arithmetic;

pub mod fun;
pub mod stlc;
pub mod toy;
pub mod typescript;
pub mod weird;

pub mod imp;

use crate::logic::grammar::Grammar;
use crate::logic::typing::Context;

use crate::validation::completability::check_incremental_feed_replay;
use rayon::ThreadPoolBuilder;
use rayon::prelude::*;
use serde_json::json;
use std::sync::mpsc;
use std::thread;
use std::time::{Duration, Instant};

fn effective_case_timeout_secs(base_secs: u64) -> u64 {
    let base = base_secs.max(1);
    let workers = rayon::current_num_threads().max(1) as u64;
    let scaled = if workers > 1 {
        base.saturating_mul(workers)
    } else {
        base
    };
    scaled.min(3600)
}

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

/// Collect all replay-soundness suites.
pub fn all_suites() -> Vec<(&'static str, Grammar, Vec<TypedCompletionTestCase>)> {
    let mut out = Vec::new();
    out.extend(arithmetic::suites());
    out.extend(stlc::suites());
    out.extend(typescript::suites());
    out.extend(toy::suites());
    out.extend(fun::suites());
    out.extend(imp::suites());
    out.extend(weird::suites());
    out
}

// ============================================================================
// Test Framework - Core Verification Utilities
// ============================================================================

/// A test case for typed feed-replay verification.
#[derive(Debug, Clone)]
pub struct TypedCompletionTestCase {
    /// Human-readable description
    pub description: &'static str,
    /// The partial input to test
    pub input: &'static str,
    /// Initial typing context (variable bindings)
    pub context: Vec<(&'static str, &'static str)>,
    /// Timeout in seconds for the test (default: 180 = 3 minutes)
    pub timeout_secs: u64,
}

impl TypedCompletionTestCase {
    pub fn new(desc: &'static str, input: &'static str) -> Self {
        Self {
            description: desc,
            input,
            context: vec![],
            timeout_secs: 10,
        }
    }

    pub fn ok(desc: &'static str, input: &'static str, _budget: usize) -> Self {
        Self::new(desc, input)
    }

    pub fn with_context(mut self, ctx: Vec<(&'static str, &'static str)>) -> Self {
        self.context = ctx;
        self
    }

    pub fn with_timeout_secs(mut self, secs: u64) -> Self {
        self.timeout_secs = secs.max(1);
        self
    }
}

/// Metadata for a single test run useful to profiling and reporting
#[derive(Debug, Clone)]
pub struct TestRunMeta {
    pub prefixes_checked: Option<usize>,
}

/// Run a single typed replay test case, returning timing info and metadata.
/// All failure messages are structured as key=value lines for machine parsing.
pub fn run_test_timed_meta(
    grammar: &Grammar,
    case: &TypedCompletionTestCase,
) -> (TestResult, Duration, TestRunMeta) {
    let start = Instant::now();
    let timeout_secs = effective_case_timeout_secs(case.timeout_secs);
    let timeout = Duration::from_secs(timeout_secs);
    let grammar_cloned = grammar.clone();
    let case_cloned = case.clone();
    let (tx, rx) = mpsc::channel();

    thread::spawn(move || {
        let mut grammar_cloned = grammar_cloned;
        let out = run_test_inner(&mut grammar_cloned, &case_cloned);
        let _ = tx.send(out);
    });

    match rx.recv_timeout(timeout) {
        Ok((res, meta)) => (res, start.elapsed(), meta),
        Err(_) => {
            let mut m = String::new();
            m.push_str("kind=timeout\n");
            m.push_str(&format!("input={}\n", case.input));
            m.push_str(&format!("timeout_secs={}\n", timeout_secs));
            let meta = TestRunMeta {
                prefixes_checked: None,
            };
            (TestResult::Fail(m), start.elapsed(), meta)
        }
    }
}

fn run_test_inner(
    grammar: &mut Grammar,
    case: &TypedCompletionTestCase,
) -> (TestResult, TestRunMeta) {
    let mut ctx = Context::new();
    for (var, ty_str) in &case.context {
        if let Ok(ty) = crate::logic::typing::Type::parse_raw(ty_str) {
            ctx.add(var.to_string(), ty);
        }
    }

    let mut meta = TestRunMeta {
        prefixes_checked: None,
    };

    let start = Instant::now();
    let result = check_incremental_feed_replay(grammar, case.input, Some(ctx.clone()));
    let _elapsed = start.elapsed();
    meta.prefixes_checked = Some(result.prefixes_checked);

    let result = if result.is_sound {
        TestResult::Pass(Some(result.accepted_input))
    } else {
        let mut m = String::new();
        m.push_str("kind=feed_replay_failed\n");
        m.push_str(&format!("input={}\n", case.input));
        m.push_str(&format!("prefixes_checked={}\n", result.prefixes_checked));

        if let Some(ref fp) = result.failing_prefix {
            m.push_str(&format!("failing_prefix={}\n", fp));
        }
        if !result.accepted_input.is_empty() {
            m.push_str(&format!("accepted_prefix={}\n", result.accepted_input));
        }
        if let Some(ref failure) = result.failure {
            m.push_str(&format!("failure={}\n", failure));
        }

        TestResult::Fail(m)
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
    let fail_fast = std::env::var("AUFBAU_FAIL_FAST")
        .ok()
        .map(|value| value != "0")
        .unwrap_or(true);
    eprintln!(
        "{}",
        serde_json::json!({
            "event": "BATCH_CONFIG",
            "workers": workers,
            "cases": cases.len(),
            "fail_fast": fail_fast,
        })
    );
    let mut outcomes: Vec<CaseOutcome> = if fail_fast {
        let mut out = Vec::new();
        for (idx, case) in cases.iter().enumerate() {
            let (result, duration) = run_test_timed(grammar, case);
            let stop = matches!(result, TestResult::Fail(_));
            out.push(CaseOutcome {
                idx,
                result,
                duration,
            });
            if stop {
                break;
            }
        }
        out
    } else {
        let pool = ThreadPoolBuilder::new()
            .num_threads(workers)
            .stack_size(32 * 1024 * 1024)
            .build()
            .expect("failed to build completable thread pool");

        pool.install(|| {
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
        })
    };

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
