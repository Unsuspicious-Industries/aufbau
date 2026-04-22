use clap::Args;
use std::fs;
use std::io::Write as _;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use indicatif::{MultiProgress, ProgressBar, ProgressStyle};
use rayon::prelude::*;
use serde_json::json;

pub mod completable;

pub mod parseable;

#[derive(clap::ValueEnum, Clone, Debug)]
pub enum ValidationModule {
    /// Run feed-acceptance validation tests
    Completable,
    /// Run parseable validation tests (fast prefix parsing)
    Parseable,

}

#[derive(Args, Debug, Clone)]
pub struct ValidateCmd {
    /// Validation module to run. If omitted, run all modules.
    #[arg(short = 'm', long = "module")]
    pub module: Option<ValidationModule>,

    /// Filter suites by name substring (e.g. "stlc", "fun::lambda")
    #[arg(short = 'f', long = "filter")]
    pub filter: Option<String>,

    /// Optional profile output file (Chrome trace format). If provided, the
    /// validation run will emit a trace JSON that can be opened with
    /// Chrome tracing or Firefox profiler ("Load profile" -> "Import").
    #[arg(long = "profile", value_name = "FILE")]
    pub profile: Option<std::path::PathBuf>,

    /// Optional number of worker threads to use for parallel test execution.
    /// If omitted, Rayon will use its default thread pool size.
    #[arg(long = "jobs", short = 'j', value_name = "N")]
    pub jobs: Option<usize>,

    /// Override per-case feed-suite timeout in seconds.
    #[arg(long = "completable-timeout-secs", value_name = "N")]
    pub completable_timeout_secs: Option<u64>,
}

pub fn run(args: &ValidateCmd) {
    match &args.module {
        Some(ValidationModule::Completable) => completable::run(args),
        Some(ValidationModule::Parseable) => parseable::run(args),
        None => {
            completable::run(args);
            parseable::run(args);
        }
    }
}

// ============================================================================
// Shared validation runner infrastructure
// ============================================================================

/// Uniform result for a single test case, used by both parseable and completable.
pub struct CaseResult {
    pub suite: String,
    pub desc: String,
    pub input: String,
    pub passed: bool,
    pub duration: Duration,
    pub detail: String,
}

/// A suite of test cases ready to run. Each case is a closure that returns
/// `(passed, detail_on_failure)` along with its description and input.
pub struct RunnableSuite {
    pub name: &'static str,
    pub cases: Vec<RunnableCase>,
}

pub struct RunnableCase {
    pub desc: String,
    pub input: String,
    pub run: Box<dyn Fn() -> (bool, String) + Send + Sync>,
}

/// Run validation suites with progress bars, parallel execution, reporting,
/// and optional profiling. This is the shared entry point for parseable and
/// completable CLI modules.
pub fn run_suites(module_name: &str, suites: Vec<RunnableSuite>, args: &ValidateCmd) {
    let suites: Vec<_> = match &args.filter {
        Some(f) => suites
            .into_iter()
            .filter(|s| s.name.contains(f.as_str()))
            .collect(),
        None => suites,
    };

    let total_suites = suites.len();
    let total_cases: usize = suites.iter().map(|s| s.cases.len()).sum();

    eprintln!("aufbau validation runner - {}", module_name);
    eprintln!("  suites: {}", total_suites);
    eprintln!("  cases:  {}", total_cases);
    if let Some(ref f) = args.filter {
        eprintln!("  filter: {}", f);
    }
    eprintln!();

    // Progress bars
    let mp = MultiProgress::new();
    let suite_style = ProgressStyle::with_template(
        "{prefix:.bold} [{bar:30.cyan/dim}] {pos}/{len} suites  {msg}",
    )
    .unwrap()
    .progress_chars("=> ");
    let case_style = ProgressStyle::with_template(
        "  {prefix:.dim} [{bar:25.green/dim}] {pos}/{len}  {elapsed_precise} eta {eta_precise}  {msg}",
    )
    .unwrap()
    .progress_chars("-> ");

    let suite_pb = mp.add(ProgressBar::new(total_suites as u64));
    suite_pb.set_style(suite_style);
    suite_pb.set_prefix("suites");

    // Build thread pool if requested
    if let Some(n) = args.jobs
        && n > 0
        && let Err(e) = rayon::ThreadPoolBuilder::new()
            .num_threads(n)
            .stack_size(32 * 1024 * 1024)
            .build_global()
    {
        eprintln!("Warning: failed to set rayon thread pool: {}", e);
    }

    let run_start = Instant::now();
    let mut results: Vec<CaseResult> = Vec::with_capacity(total_cases);
    let mut profile_records: Vec<serde_json::Value> = Vec::new();

    for suite in &suites {
        suite_pb.set_message(suite.name.to_string());

        let case_pb = mp.insert_after(&suite_pb, ProgressBar::new(suite.cases.len() as u64));
        case_pb.set_style(case_style.clone());
        case_pb.set_prefix(suite.name.to_string());

        let mut par_results: Vec<(usize, bool, String, Duration)> =
            if module_name == "completable" {
                suite
                    .cases
                    .iter()
                    .enumerate()
                    .map(|(idx, case)| {
                        let start = Instant::now();
                        let (passed, detail) = (case.run)();
                        (idx, passed, detail, start.elapsed())
                    })
                    .collect()
            } else {
                let indexed: Vec<(usize, &RunnableCase)> = suite.cases.iter().enumerate().collect();
                let mut results: Vec<(usize, bool, String, Duration)> = indexed
                    .par_iter()
                    .map(|(idx, case)| {
                        let start = Instant::now();
                        let (passed, detail) = (case.run)();
                        (*idx, passed, detail, start.elapsed())
                    })
                    .collect();
                results.sort_by_key(|(idx, _, _, _)| *idx);
                results
            };

        par_results.sort_by_key(|(idx, _, _, _)| *idx);

        for (idx, passed, detail, duration) in par_results {
            let case = &suite.cases[idx];
            case_pb.set_message(format!("\"{}\"", case.input));

            if passed {
                println!("PASS ({:?}) {} '{}'", duration, case.desc, case.input);
            } else {
                println!("FAIL {} '{}'", case.desc, case.input);
                if !detail.is_empty() {
                    println!("    {}", detail.lines().next().unwrap_or(""));
                }
            }

            profile_records.push(json!({
                "module": module_name,
                "suite": suite.name,
                "desc": case.desc,
                "input": case.input,
                "passed": passed,
                "time_us": duration.as_micros() as u64,
                "time_ms": duration.as_millis() as u64,
                "detail": if passed { "" } else { &detail },
            }));

            results.push(CaseResult {
                suite: suite.name.to_string(),
                desc: case.desc.clone(),
                input: case.input.clone(),
                passed,
                duration,
                detail,
            });

            case_pb.inc(1);
        }

        case_pb.finish_and_clear();
        suite_pb.inc(1);
    }

    let total_duration = run_start.elapsed();
    suite_pb.finish_with_message("done");

    let passed = results.iter().filter(|r| r.passed).count();
    let failed = results.iter().filter(|r| !r.passed).count();

    // Write profile files
    if let Some(profile_path) = &args.profile {
        write_profiles(
            module_name,
            profile_path,
            &profile_records,
            total_cases,
            passed,
            failed,
        );
    }

    // Write reports
    write_reports(
        module_name,
        &results,
        &args.filter,
        total_suites,
        total_cases,
        passed,
        failed,
        total_duration,
    );

    // Terminal summary
    eprintln!();
    eprintln!("----------------------------------------------");
    if failed == 0 {
        eprintln!(
            "  ALL {} CASES PASSED  ({}ms)",
            total_cases,
            total_duration.as_millis()
        );
    } else {
        eprintln!(
            "  {} FAILED / {} total  ({}ms)",
            failed,
            total_cases,
            total_duration.as_millis()
        );
        eprintln!();
        for r in &results {
            if !r.passed {
                let kind = r.detail.lines().next().unwrap_or("");
                eprintln!("  FAIL  {} / {}  {}", r.suite, r.desc, kind);
            }
        }
    }
    eprintln!("----------------------------------------------");
    eprintln!();

    if failed > 0 {
        std::process::exit(1);
    }
}

// ============================================================================
// Report & profile helpers
// ============================================================================

fn write_profiles(
    module_name: &str,
    profile_path: &std::path::Path,
    records: &[serde_json::Value],
    total: usize,
    passed: usize,
    failed: usize,
) {
    if let Some(parent) = profile_path.parent() {
        fs::create_dir_all(parent).ok();
    }
    let stem = profile_path.file_stem().unwrap().to_string_lossy();
    let dir = profile_path.parent().unwrap_or(std::path::Path::new("."));

    let perf_path = dir.join(format!("{}-perf.json", stem));
    let fail_path = dir.join(format!("{}-failures.json", stem));

    let ts = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_secs();
    let summary = json!({"cases": total, "passed": passed, "failed": failed});

    let perf_obj = json!({
        "generated_by": format!("aufbau validate {}", module_name),
        "variant": "performance",
        "timestamp": ts,
        "summary": summary,
        "cases": records,
    });
    let fperf = fs::File::create(&perf_path).expect("failed to create perf profile");
    serde_json::to_writer_pretty(fperf, &perf_obj).expect("failed to write perf profile");
    eprintln!("WROTE_PROFILE {}", perf_path.display());

    let failures: Vec<_> = records
        .iter()
        .filter(|c| !c.get("passed").and_then(|v| v.as_bool()).unwrap_or(false))
        .collect();
    let fail_obj = json!({
        "generated_by": format!("aufbau validate {}", module_name),
        "variant": "failures",
        "timestamp": ts,
        "summary": summary,
        "cases": failures,
    });
    let ffails = fs::File::create(&fail_path).expect("failed to create failures profile");
    serde_json::to_writer_pretty(ffails, &fail_obj).expect("failed to write failures profile");
    eprintln!("WROTE_PROFILE {}", fail_path.display());
}

#[allow(clippy::too_many_arguments)]
fn write_reports(
    module_name: &str,
    results: &[CaseResult],
    filter: &Option<String>,
    total_suites: usize,
    total_cases: usize,
    passed: usize,
    failed: usize,
    total_duration: Duration,
) {
    let timestamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_secs();
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let reports_dir = std::path::Path::new(manifest_dir)
        .join("src")
        .join("validation")
        .join("reports");
    fs::create_dir_all(&reports_dir).expect("failed to create reports dir");

    let report_path = reports_dir.join(format!("{}-{}.txt", module_name, timestamp));
    let mut f = fs::File::create(&report_path).expect("failed to create report");
    let sep = "=".repeat(79);
    let thin = "-".repeat(79);

    writeln!(f, "{}", sep).ok();
    writeln!(f, "  {} VALIDATION REPORT", module_name.to_uppercase()).ok();
    writeln!(f, "  timestamp={}", timestamp).ok();
    if let Some(flt) = filter {
        writeln!(f, "  filter={}", flt).ok();
    }
    writeln!(
        f,
        "  suites={} cases={} passed={} failed={} time={}ms",
        total_suites,
        total_cases,
        passed,
        failed,
        total_duration.as_millis()
    )
    .ok();
    writeln!(f, "{}", sep).ok();
    writeln!(f).ok();

    if failed == 0 {
        writeln!(f, "RESULT: ALL {} CASES PASSED", total_cases).ok();
    } else {
        writeln!(f, "RESULT: {} FAILED out of {} cases", failed, total_cases).ok();
    }
    writeln!(f).ok();

    // Passing
    writeln!(f, "{}", thin).ok();
    writeln!(f, "  PASSING ({}/{})", passed, total_cases).ok();
    writeln!(f, "{}", thin).ok();
    let mut cur = "";
    for r in results {
        if r.passed {
            if r.suite != cur {
                writeln!(f).ok();
                writeln!(f, "  [{}]", r.suite).ok();
                cur = &r.suite;
            }
            writeln!(
                f,
                "    ok  {:30} {:>8}us  \"{}\"",
                r.desc,
                r.duration.as_micros(),
                r.input
            )
            .ok();
        }
    }
    writeln!(f).ok();

    // Failures
    writeln!(f, "{}", sep).ok();
    writeln!(f, "  FAILURES ({}/{})", failed, total_cases).ok();
    writeln!(f, "{}", sep).ok();
    if failed == 0 {
        writeln!(f, "  (none)").ok();
    } else {
        let mut cur = "";
        for r in results {
            if !r.passed {
                if r.suite != cur {
                    writeln!(f).ok();
                    writeln!(f, "  [{}]", r.suite).ok();
                    cur = &r.suite;
                }
                writeln!(f).ok();
                writeln!(
                    f,
                    "    FAIL  {}  (time={}us)",
                    r.desc,
                    r.duration.as_micros()
                )
                .ok();
                writeln!(f, "    input=\"{}\"", r.input).ok();
                for line in r.detail.lines() {
                    let trimmed = line.trim();
                    if !trimmed.is_empty() {
                        writeln!(f, "      {}", trimmed).ok();
                    }
                }
            }
        }
    }
    writeln!(f).ok();

    // Per-suite summary
    writeln!(f, "{}", thin).ok();
    writeln!(f, "  PER-SUITE SUMMARY").ok();
    writeln!(f, "{}", thin).ok();
    let mut suite_names: Vec<&str> = results.iter().map(|r| r.suite.as_str()).collect();
    suite_names.sort();
    suite_names.dedup();
    for sn in suite_names {
        let sr: Vec<_> = results.iter().filter(|r| r.suite == sn).collect();
        let sp = sr.iter().filter(|r| r.passed).count();
        let sf = sr.iter().filter(|r| !r.passed).count();
        let st: Duration = sr.iter().map(|r| r.duration).sum();
        let status = if sf == 0 { "ok" } else { "FAIL" };
        writeln!(
            f,
            "    {:4}  {:40} passed={} failed={} time={}us",
            status,
            sn,
            sp,
            sf,
            st.as_micros()
        )
        .ok();
    }
    writeln!(f).ok();
    writeln!(f, "{}", sep).ok();
    writeln!(f, "  END OF REPORT").ok();
    writeln!(f, "{}", sep).ok();

    eprintln!("WROTE_REPORT {}", report_path.display());
}
