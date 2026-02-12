use clap::Args;
use std::fs;
use std::io::Write as _;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use indicatif::{MultiProgress, ProgressBar, ProgressStyle};
use p7::logic::grammar::Grammar;
use p7::validation::completable::{self, run_test_timed, TestResult, TypedCompletionTestCase};
use rayon::prelude::*;

use crate::cli::validate::ValidateCmd;

/// Suite registry for completable tests
fn all_suites() -> Vec<(&'static str, Grammar, Vec<TypedCompletionTestCase>)> {
    let mut out = Vec::new();
    out.extend(completable::arithmetic::suites());
    out.extend(completable::stlc::suites());
    out.extend(completable::fun::suites());
    out.extend(completable::imp::suites());
    out.extend(completable::weird::suites());
    out
}

/// Per-case result (reporting)
struct CaseResult {
    suite: &'static str,
    desc: String,
    input: String,
    expect: String,
    passed: bool,
    duration: Duration,
    detail: String,
    beam_fallback: Option<bool>,
    states_explored: Option<usize>,
}

pub fn run(args: &ValidateCmd) {
    let all = all_suites();
    let suites: Vec<_> = match &args.filter {
        Some(f) => all
            .into_iter()
            .filter(|(name, _, _)| name.contains(f.as_str()))
            .collect(),
        None => all,
    };

    let total_cases: usize = suites.iter().map(|(_, _, c)| c.len()).sum();
    let total_suites = suites.len();

    eprintln!("p7 validation runner - completable");
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

    // Run
    let mut results: Vec<CaseResult> = Vec::with_capacity(total_cases);
    let mut passed = 0usize;
    let mut failed = 0usize;
    let run_start = Instant::now();

    // Optional in-memory profile collection when --profile is specified
    let mut profile_cases: Option<Vec<serde_json::Value>> = if args.profile.is_some() {
        Some(Vec::new())
    } else {
        None
    };

    // If requested, attempt to set Rayon global thread pool size. build_global()
    // can only be called once per process; if it fails we proceed with the
    // default global pool and emit a warning.
    if let Some(n) = args.jobs {
        if n > 0 {
            if let Err(e) = rayon::ThreadPoolBuilder::new().num_threads(n).build_global() {
                eprintln!("Warning: failed to set global rayon thread pool: {}", e);
            }
        }
    }

    for (suite_name, grammar, cases) in &suites {
        suite_pb.set_message(suite_name.to_string());

        let case_pb = mp.insert_after(&suite_pb, ProgressBar::new(cases.len() as u64));
        case_pb.set_style(case_style.clone());
        case_pb.set_prefix(suite_name.to_string());

        // Prepare indexed cases for deterministic ordering
        let indexed_cases: Vec<(usize, &TypedCompletionTestCase)> = cases.iter().enumerate().collect();

        // Run cases in parallel using Rayon global pool
        let mut par_results: Vec<(usize, serde_json::Value, TestResult, Duration, p7::validation::completable::TestRunMeta)> =
            indexed_cases
                .par_iter()
                .map(|(idx, case)| {
                    let idx = *idx;
                    let case = *case;
                    let start_ts = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_micros();
                    let (result, duration, meta) = completable::run_test_timed_meta(grammar, case);

                    use serde_json::json;
                    let prefix_meta_json = meta.prefix_meta.as_ref().map(|v| {
                        v.iter()
                            .map(|pd| {
                                json!({
                                    "prefix": pd.prefix,
                                    "ok": pd.ok,
                                    "time_us": pd.time_us,
                                    "states_explored": pd.states_explored,
                                    "visited_count": pd.visited_count,
                                    "visited_sample": pd.visited_sample,
                                    "beam_fallback": pd.beam_fallback,
                                })
                            })
                            .collect::<Vec<_>>()
                    });

                    let is_pass = result.is_pass();
                    let detail = match &result {
                        TestResult::Pass(_) => String::new(),
                        TestResult::Fail(msg) => msg.clone(),
                    };

                    let case_obj = json!({
                        "suite": suite_name,
                        "desc": case.description,
                        "input": case.input,
                        "expect": if case.xfail { "FAIL" } else { "PASS" },
                        "passed": is_pass,
                        "time_ms": duration.as_millis(),
                        "time_us": duration.as_micros(),
                        "beam_fallback": meta.beam_fallback,
                        "states_explored": meta.states_explored,
                        "prefixes_checked": meta.prefixes_checked,
                        "prefix_total_time_us": meta.total_prefix_time_us,
                        "detail": detail,
                        "prefix_meta": prefix_meta_json,
                        "timestamp_us": start_ts,
                    });

                    (idx, case_obj, result, duration, meta)
                })
                .collect();

        // Sort results by original index to restore input order
        par_results.sort_by_key(|(idx, _, _, _, _)| *idx);

        for (_idx, case_obj, result, duration, meta) in par_results {
            let input = case_obj.get("input").and_then(serde_json::Value::as_str).unwrap_or("").to_string();
            case_pb.set_message(format!("\"{}\"", input));

            let desc = case_obj.get("desc").and_then(serde_json::Value::as_str).unwrap_or("").to_string();
            let detail = case_obj.get("detail").and_then(serde_json::Value::as_str).unwrap_or("").to_string();
            let expect = case_obj.get("expect").and_then(serde_json::Value::as_str).unwrap_or("").to_string();

            // Human readable output
            match &result {
                TestResult::Pass(_) => {
                    println!("✓ ({:?}) {} '{}'", duration, desc, input);
                }
                TestResult::Fail(_) => {
                    println!("✗ {} '{}'", desc, input);
                    if !detail.is_empty() {
                        // Print the first line of the structured failure message for readability
                        let first_line = detail.lines().next().unwrap_or("");
                        println!("    Error: {}", first_line);
                    }
                }
            }

            let is_pass = result.is_pass();

            if is_pass {
                passed += 1;
            } else {
                failed += 1;
            }

            results.push(CaseResult {
                suite: suite_name,
                desc: desc.clone(),
                input: input.clone(),
                expect: if expect == "FAIL" { "FAIL" } else { "PASS" }.to_string(),
                passed: is_pass,
                duration,
                detail: detail.clone(),
                beam_fallback: meta.beam_fallback,
                states_explored: meta.states_explored,
            });

            if let Some(ref mut v) = profile_cases {
                v.push(case_obj);
            }

            case_pb.inc(1);
        }

        case_pb.finish_and_clear();
        suite_pb.inc(1);
    }

    let total_duration = run_start.elapsed();
    suite_pb.finish_with_message("done");

    // If profiling was requested, write performance and failures JSON profiles
    if let Some(profile_path) = &args.profile {
        use serde_json::json;
        if let Some(cases) = profile_cases {
            // Build a performance-focused profile with trimmed fields for analytics
            let perf_cases: Vec<serde_json::Value> = cases
                .iter()
                .map(|c| {
                    json!({
                        "suite": c.get("suite").cloned().unwrap_or(json!(null)),
                        "desc": c.get("desc").cloned().unwrap_or(json!(null)),
                        "input": c.get("input").cloned().unwrap_or(json!(null)),
                        "passed": c.get("passed").cloned().unwrap_or(json!(false)),
                        "time_ms": c.get("time_ms").cloned().unwrap_or(json!(0)),
                        "time_us": c.get("time_us").cloned().unwrap_or(json!(0)),
                        "prefixes_checked": c.get("prefixes_checked").cloned().unwrap_or(json!(null)),
                        "prefix_total_time_us": c.get("prefix_total_time_us").cloned().unwrap_or(json!(null)),
                        "states_explored": c.get("states_explored").cloned().unwrap_or(json!(null)),
                        "beam_fallback": c.get("beam_fallback").cloned().unwrap_or(json!(null)),
                    })
                })
                .collect();

            // Failures profile: keep full records for failed cases for troubleshooting
            let failure_cases: Vec<serde_json::Value> = cases
                .iter()
                .filter(|c| !c.get("passed").and_then(|v| v.as_bool()).unwrap_or(false))
                .cloned()
                .collect();

            // Compose profile objects
            let perf_obj = json!({
                "generated_by": "p7 validate completable",
                "variant": "performance",
                "timestamp": SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs(),
                "summary": {"suites": total_suites, "cases": total_cases, "passed": passed, "failed": failed, "total_time_ms": total_duration.as_millis()},
                "cases": perf_cases,
            });
            let fail_obj = json!({
                "generated_by": "p7 validate completable",
                "variant": "failures",
                "timestamp": SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs(),
                "summary": {"suites": total_suites, "cases": total_cases, "passed": passed, "failed": failed, "total_time_ms": total_duration.as_millis()},
                "cases": failure_cases,
            });

            if let Some(parent) = profile_path.parent() {
                std::fs::create_dir_all(parent).ok();
            }

            let stem = profile_path.file_stem().unwrap().to_string_lossy();
            let perf_path = profile_path.parent().unwrap_or(std::path::Path::new(".")).join(format!("{}-perf.json", stem));
            let fail_path = profile_path.parent().unwrap_or(std::path::Path::new(".")).join(format!("{}-failures.json", stem));

            let fperf = fs::File::create(&perf_path).expect("failed to create perf profile file");
            serde_json::to_writer_pretty(fperf, &perf_obj).expect("failed to write perf profile JSON");
            eprintln!("WROTE_PROFILE {}", perf_path.display());

            let ffails = fs::File::create(&fail_path).expect("failed to create failures profile file");
            serde_json::to_writer_pretty(ffails, &fail_obj).expect("failed to write failures profile JSON");
            eprintln!("WROTE_PROFILE {}", fail_path.display());
        } else {
            eprintln!("PROFILE flag provided but no cases recorded");
        }
    }

    // Generate report
    let timestamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_secs();

    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let reports_dir = std::path::Path::new(manifest_dir)
        .join("validation")
        .join("reports");
    fs::create_dir_all(&reports_dir).expect("failed to create reports dir");

    let report_path = reports_dir.join(format!("{}.txt", timestamp));
    let mut f = fs::File::create(&report_path).expect("failed to create report file");

    // Header
    writeln!(f, "===============================================================================").ok();
    writeln!(f, "  VALIDATION REPORT").ok();
    writeln!(f, "  timestamp={}", timestamp).ok();
    if let Some(ref flt) = args.filter {
        writeln!(f, "  filter={}", flt).ok();
    }
    writeln!(
        f,
        "  suites={} cases={} passed={} failed={}",
        total_suites, total_cases, passed, failed
    )
    .ok();
    writeln!(f, "  total_time_ms={}", total_duration.as_millis()).ok();
    writeln!(f, "===============================================================================").ok();
    writeln!(f).ok();

    if failed == 0 {
        writeln!(f, "RESULT: ALL {} CASES PASSED", total_cases).ok();
    } else {
        writeln!(f, "RESULT: {} FAILED out of {} cases", failed, total_cases).ok();
    }
    writeln!(f).ok();

    // Passing by suite
    writeln!(f, "-------------------------------------------------------------------------------").ok();
    writeln!(f, "  PASSING ({}/{})", passed, total_cases).ok();
    writeln!(f, "-------------------------------------------------------------------------------").ok();

    let mut current_suite = "";
    for r in &results {
        if r.passed {
            if r.suite != current_suite {
                writeln!(f).ok();
                writeln!(f, "  [{}]", r.suite).ok();
                current_suite = r.suite;
            }
            writeln!(
                f,
                "    ok  {:30} {:>8}ms  \"{}\"{}",
                r.desc,
                r.duration.as_millis(),
                r.input,
                if r.beam_fallback.unwrap_or(false) { " bf" } else { "" }
            )
            .ok();
        }
    }
    writeln!(f).ok();

    // Failures — full detail
    writeln!(f, "===============================================================================").ok();
    writeln!(f, "  FAILURES ({}/{})", failed, total_cases).ok();
    writeln!(f, "===============================================================================").ok();

    if failed == 0 {
        writeln!(f, "  (none)").ok();
    } else {
        let mut current_suite = "";
        for r in &results {
            if !r.passed {
                if r.suite != current_suite {
                    writeln!(f).ok();
                    writeln!(f, "  [{}]", r.suite).ok();
                    current_suite = r.suite;
                }
                writeln!(f).ok();
                writeln!(
                    f,
                    "    FAIL  {}  (expect={}, time={}ms)",
                    r.desc,
                    r.expect,
                    r.duration.as_millis()
                )
                .ok();
                writeln!(f, "    input=\"{}\"", r.input).ok();
                if let Some(bf) = r.beam_fallback {
                    writeln!(f, "    beam_fallback={}", bf).ok();
                }
                if let Some(se) = r.states_explored {
                    writeln!(f, "    states_explored={}", se).ok();
                }
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

    // Slow cases
    let mut slow: Vec<&CaseResult> = results
        .iter()
        .filter(|r| r.duration.as_millis() >= 1000)
        .collect();
    slow.sort_by(|a, b| b.duration.cmp(&a.duration));

    if !slow.is_empty() {
        writeln!(f, "-------------------------------------------------------------------------------").ok();
        writeln!(f, "  SLOW CASES (>= 1s): {}", slow.len()).ok();
        writeln!(f, "-------------------------------------------------------------------------------").ok();
        for r in &slow {
            writeln!(
                f,
                "    {}ms  {} / {}  \"{}\"",
                r.duration.as_millis(),
                r.suite,
                r.desc,
                r.input
            )
            .ok();
        }
        writeln!(f).ok();
    }

    // Per-suite summary
    writeln!(f, "-------------------------------------------------------------------------------").ok();
    writeln!(f, "  PER-SUITE SUMMARY").ok();
    writeln!(f, "-------------------------------------------------------------------------------").ok();

    for (suite_name, _, _) in &suites {
        let suite_results: Vec<&CaseResult> =
            results.iter().filter(|r| r.suite == *suite_name).collect();
        let sp = suite_results.iter().filter(|r| r.passed).count();
        let sf = suite_results.iter().filter(|r| !r.passed).count();
        let s_bf = suite_results.iter().filter(|r| r.beam_fallback.unwrap_or(false)).count();
        let st: Duration = suite_results.iter().map(|r| r.duration).sum();
        let status = if sf == 0 { "ok" } else { "FAIL" };
        writeln!(
            f,
            "    {:4}  {:40} passed={} failed={} beam_fallbacks={} time={}ms",
            status, suite_name, sp, sf, s_bf, st.as_millis()
        )
        .ok();
    }
    writeln!(f).ok();

    writeln!(f, "===============================================================================").ok();
    writeln!(f, "  END OF REPORT").ok();
    writeln!(f, "===============================================================================").ok();

    drop(f);

    // Create separate reports: successes and failures for easier consumption
    let successes_path = reports_dir.join(format!("successes-{}.txt", timestamp));
    let mut sf = fs::File::create(&successes_path).expect("failed to create successes file");
    writeln!(sf, "==============================================================================").ok();
    writeln!(sf, "  PASSING REPORT").ok();
    writeln!(sf, "  timestamp={}", timestamp).ok();
    writeln!(sf, "  suites={} cases={} passed={} failed={}", total_suites, total_cases, passed, failed).ok();
    writeln!(sf, "  total_time_ms={}", total_duration.as_millis()).ok();
    writeln!(sf, "==============================================================================").ok();
    writeln!(sf).ok();

    let mut current_suite = "";
    for r in &results {
        if r.passed {
            if r.suite != current_suite {
                writeln!(sf).ok();
                writeln!(sf, "  [{}]", r.suite).ok();
                current_suite = r.suite;
            }
            writeln!(sf, "    ok  {:30} {:>8}ms  \"{}\"{}", r.desc, r.duration.as_millis(), r.input, if r.beam_fallback.unwrap_or(false) { " bf" } else { "" }).ok();
        }
    }

    eprintln!("WROTE_REPORT {}", successes_path.display());

    let failures_path = reports_dir.join(format!("failures-{}.txt", timestamp));
    let mut ff = fs::File::create(&failures_path).expect("failed to create failures file");
    writeln!(ff, "==============================================================================").ok();
    writeln!(ff, "  FAILURES REPORT").ok();
    writeln!(ff, "  timestamp={}", timestamp).ok();
    writeln!(ff, "  suites={} cases={} passed={} failed={}", total_suites, total_cases, passed, failed).ok();
    writeln!(ff, "  total_time_ms={}", total_duration.as_millis()).ok();
    writeln!(ff, "==============================================================================").ok();
    writeln!(ff).ok();

    if failed == 0 {
        writeln!(ff, "  (none)").ok();
    } else {
        let mut current_suite = "";
        for r in &results {
            if !r.passed {
                if r.suite != current_suite {
                    writeln!(ff).ok();
                    writeln!(ff, "  [{}]", r.suite).ok();
                    current_suite = r.suite;
                }
                writeln!(ff).ok();
                writeln!(ff, "    FAIL  {}  (expect={}, time={}ms)", r.desc, r.expect, r.duration.as_millis()).ok();
                writeln!(ff, "    input=\"{}\"", r.input).ok();
                if let Some(bf) = r.beam_fallback {
                    writeln!(ff, "    beam_fallback={}", bf).ok();
                }
                if let Some(se) = r.states_explored {
                    writeln!(ff, "    states_explored={}", se).ok();
                }
                for line in r.detail.lines() {
                    let trimmed = line.trim();
                    if !trimmed.is_empty() {
                        writeln!(ff, "      {}", trimmed).ok();
                    }
                }
            }
        }
    }

    eprintln!("WROTE_REPORT {}", failures_path.display());

    // Terminal summary
    eprintln!();
    eprintln!("----------------------------------------------");
    let total_bf = results.iter().filter(|r| r.beam_fallback.unwrap_or(false)).count();
    if failed == 0 {
        eprintln!(
            "  ALL {} CASES PASSED  ({}ms)  beam_fallbacks={} ",
            total_cases,
            total_duration.as_millis(),
            total_bf
        );
    } else {
        eprintln!(
            "  {} FAILED / {} total  ({}ms)  beam_fallbacks={}",
            failed,
            total_cases,
            total_duration.as_millis(),
            total_bf
        );
        eprintln!();
        for r in &results {
            if !r.passed {
                let kind = r.detail.lines().next().unwrap_or("");
                eprintln!("  FAIL  {} / {}  {}  bf={}", r.suite, r.desc, kind, r.beam_fallback.unwrap_or(false));
            }
        }
    }
    eprintln!("----------------------------------------------");
    eprintln!("  Report: {}", report_path.display());
    eprintln!();

    if failed > 0 {
        std::process::exit(1);
    }
}