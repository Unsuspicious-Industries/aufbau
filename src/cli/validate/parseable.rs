use crate::cli::validate::ValidateCmd;
use aufbau::logic::grammar::Grammar;
use aufbau::validation::parseable::{self, ParseTestCase};
use rayon::prelude::*;
use rayon::ThreadPoolBuilder;
use serde_json::json;
use std::fs;
use std::io::Write as _;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

struct CaseResult {
    suite: &'static str,
    desc: String,
    input: String,
    expect: String,
    passed: bool,
    duration: Duration,
    detail: String,
    _prefix_count: Option<usize>,
    _prefix_index: Option<usize>,
}

pub fn run(args: &ValidateCmd) {
    eprintln!("aufbau validation runner - parseable");

    let mut total_cases = 0usize;
    let mut results: Vec<CaseResult> = Vec::new();

    // Collect all per-case JSON records for optional profiling output
    let mut profile_cases: Vec<serde_json::Value> = Vec::new();

    let mut modules: Vec<(&str, Grammar, Vec<ParseTestCase>, Vec<ParseTestCase>)> = vec![
        (
            "fun",
            parseable::load_example_grammar("fun"),
            parseable::fun::valid_expressions_cases(),
            parseable::fun::invalid_expressions_cases(),
        ),
        (
            "imp",
            parseable::load_example_grammar("imp"),
            parseable::imp::valid_expressions_cases(),
            parseable::imp::invalid_expressions_cases(),
        ),
        (
            "stlc",
            parseable::load_example_grammar("stlc"),
            parseable::stlc::valid_expressions_cases(),
            parseable::stlc::invalid_expressions_cases(),
        ),
    ];

    // Expand per-grammar weird suites (they use inline grammars and provide their
    // own case lists for valids and invalids).
    modules.extend(parseable::weird::suites());

    // Multi-progress UI
    let mp = indicatif::MultiProgress::new();
    let suite_style = indicatif::ProgressStyle::with_template(
        "{prefix:.bold} [{bar:30.cyan/dim}] {pos}/{len} suites  {msg}",
    )
    .unwrap()
    .progress_chars("=> ");
    let case_style = indicatif::ProgressStyle::with_template(
        "  {prefix:.dim} [{bar:25.green/dim}] {pos}/{len}  {elapsed_precise} eta {eta_precise}  {msg}",
    )
    .unwrap()
    .progress_chars("-> ");

    let suite_pb = mp.add(indicatif::ProgressBar::new(modules.len() as u64));
    suite_pb.set_style(suite_style);
    suite_pb.set_prefix("suites".to_string());

    for (name, grammar, valids, invalids) in modules {
        if let Some(f) = &args.filter {
            if !name.contains(f.as_str()) {
                suite_pb.inc(1);
                continue;
            }
        }

        eprintln!(
            "Running parseable '{}' ({} valid + {} invalid)",
            name,
            valids.len(),
            invalids.len()
        );

        // Build a combined cases list so one progress bar covers them
        let mut all_cases: Vec<ParseTestCase> = Vec::with_capacity(valids.len() + invalids.len());
        all_cases.extend(valids.into_iter());
        all_cases.extend(invalids.into_iter());

        total_cases += all_cases.len();

        let case_pb = mp.insert_after(
            &suite_pb,
            indicatif::ProgressBar::new(all_cases.len() as u64),
        );
        case_pb.set_style(case_style.clone());
        case_pb.set_prefix(name.to_string());

        // Run cases in parallel using Rayon ThreadPool (if provided) or global pool
        let indexed_cases: Vec<(usize, ParseTestCase)> =
            all_cases.into_iter().enumerate().collect();

        // Build worker pool if requested
        let thread_pool = match args.jobs {
            Some(n) if n > 0 => Some(
                ThreadPoolBuilder::new()
                    .num_threads(n)
                    .build()
                    .expect("failed to create thread pool"),
            ),
            _ => None,
        };

        let mut par_results: Vec<(
            usize,
            serde_json::Value,
            parseable::ParseResult,
            std::time::Duration,
            u128,
        )> = if let Some(pool) = &thread_pool {
            let mut v = Vec::new();
            pool.install(|| {
                v = indexed_cases
                    .par_iter()
                    .map(|(idx, case)| {
                        let start_ts = SystemTime::now()
                            .duration_since(UNIX_EPOCH)
                            .unwrap()
                            .as_micros();
                        let start = std::time::Instant::now();
                        let result = parseable::run_parse_test(&grammar, case);
                        let duration = start.elapsed();

                        let (passed_flag, prefix_count, failing_prefix, error, prefix_index) =
                            match &result {
                                parseable::ParseResult::Pass { prefix_count, .. } => (
                                    true,
                                    Some(*prefix_count as usize),
                                    None::<String>,
                                    None::<String>,
                                    None::<usize>,
                                ),
                                parseable::ParseResult::Fail {
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
                            "time_ms": duration.as_millis(),
                            "time_us": duration.as_micros(),
                            "prefix_count": prefix_count,
                            "failing_prefix": failing_prefix,
                            "error": error,
                            "prefix_index": prefix_index,
                            "timestamp_us": start_ts,
                        });

                        (*idx, case_obj, result, duration, start_ts)
                    })
                    .collect();
            });
            v
        } else {
            indexed_cases
                .par_iter()
                .map(|(idx, case)| {
                    let start_ts = SystemTime::now()
                        .duration_since(UNIX_EPOCH)
                        .unwrap()
                        .as_micros();
                    let start = std::time::Instant::now();
                    let result = parseable::run_parse_test(&grammar, case);
                    let duration = start.elapsed();

                    let (passed_flag, prefix_count, failing_prefix, error, prefix_index) =
                        match &result {
                            parseable::ParseResult::Pass { prefix_count, .. } => (
                                true,
                                Some(*prefix_count as usize),
                                None::<String>,
                                None::<String>,
                                None::<usize>,
                            ),
                            parseable::ParseResult::Fail {
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
                        "time_ms": duration.as_millis(),
                        "time_us": duration.as_micros(),
                        "prefix_count": prefix_count,
                        "failing_prefix": failing_prefix,
                        "error": error,
                        "prefix_index": prefix_index,
                        "timestamp_us": start_ts,
                    });

                    (*idx, case_obj, result, duration, start_ts)
                })
                .collect()
        };

        // Sort results by original index to restore input order
        par_results.sort_by_key(|(idx, _, _, _, _)| *idx);

        for (_idx, case_obj, result, duration, _start_ts) in par_results {
            let input = case_obj.get("input").and_then(|v| v.as_str()).unwrap_or("");
            case_pb.set_message(format!("\"{}\"", input));

            // Human readable output
            match &result {
                parseable::ParseResult::Pass { .. } => {
                    println!(
                        "✓ ({:?}) {} '{}'",
                        duration,
                        case_obj.get("desc").and_then(|v| v.as_str()).unwrap_or(""),
                        input
                    );
                }
                parseable::ParseResult::Fail {
                    failing_prefix,
                    error,
                    ..
                } => {
                    println!(
                        "✗ {} '{}'",
                        case_obj.get("desc").and_then(|v| v.as_str()).unwrap_or(""),
                        input
                    );
                    println!("    Failed at prefix: '{}'", failing_prefix);
                    println!("    Error: {}", error);
                }
            }

            let passed = matches!(result, parseable::ParseResult::Pass { .. });
            let expect = if case_obj
                .get("xfail")
                .and_then(|v| v.as_bool())
                .unwrap_or(false)
            {
                "FAIL"
            } else {
                "PASS"
            }
            .to_string();
            let detail = match result {
                parseable::ParseResult::Pass { .. } => String::new(),
                parseable::ParseResult::Fail {
                    failing_prefix,
                    error,
                    ..
                } => format!("Failing prefix: '{}'\nError: {}\n", failing_prefix, error),
            };

            results.push(CaseResult {
                suite: name,
                desc: case_obj
                    .get("desc")
                    .and_then(|v| v.as_str())
                    .unwrap_or("")
                    .to_string(),
                input: input.to_string(),
                expect,
                passed,
                duration,
                detail,
                _prefix_count: case_obj
                    .get("prefix_count")
                    .and_then(|v| v.as_u64())
                    .map(|n| n as usize),
                _prefix_index: case_obj
                    .get("prefix_index")
                    .and_then(|v| v.as_u64())
                    .map(|n| n as usize),
            });

            profile_cases.push(case_obj);
            case_pb.inc(1);
        }

        case_pb.finish_and_clear();
        suite_pb.inc(1);
    }

    suite_pb.finish_with_message("done");

    // Summarize
    let passed = results.iter().filter(|r| r.passed).count();
    let failed = results.iter().filter(|r| !r.passed).count();

    // Optional profiling output
    if let Some(profile_path) = &args.profile {
        // Build performance-focused profile
        let perf_cases: Vec<serde_json::Value> = profile_cases
            .iter()
            .map(|c| {
                json!({
                    "desc": c.get("desc").cloned().unwrap_or(json!(null)),
                    "input": c.get("input").cloned().unwrap_or(json!(null)),
                    "passed": c.get("passed").cloned().unwrap_or(json!(false)),
                    "time_ms": c.get("time_ms").cloned().unwrap_or(json!(0)),
                    "time_us": c.get("time_us").cloned().unwrap_or(json!(0)),
                    "prefix_count": c.get("prefix_count").cloned().unwrap_or(json!(null)),
                })
            })
            .collect();

        let failure_cases: Vec<serde_json::Value> = profile_cases
            .iter()
            .filter(|c| !c.get("passed").and_then(|v| v.as_bool()).unwrap_or(false))
            .cloned()
            .collect();

        let perf_obj = json!({
            "generated_by": "aufbau validate parseable",
            "variant": "performance",
            "timestamp": SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs(),
            "summary": {"cases": total_cases, "passed": passed, "failed": failed},
            "cases": perf_cases,
        });
        let fail_obj = json!({
            "generated_by": "aufbau validate parseable",
            "variant": "failures",
            "timestamp": SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs(),
            "summary": {"cases": total_cases, "passed": passed, "failed": failed},
            "cases": failure_cases,
        });

        if let Some(parent) = profile_path.parent() {
            std::fs::create_dir_all(parent).ok();
        }

        let stem = profile_path.file_stem().unwrap().to_string_lossy();
        let perf_path = profile_path
            .parent()
            .unwrap_or(std::path::Path::new("."))
            .join(format!("{}-perf.json", stem));
        let fail_path = profile_path
            .parent()
            .unwrap_or(std::path::Path::new("."))
            .join(format!("{}-failures.json", stem));

        let fperf = fs::File::create(&perf_path).expect("failed to create perf profile file");
        serde_json::to_writer_pretty(fperf, &perf_obj).expect("failed to write perf profile JSON");
        eprintln!("WROTE_PROFILE {}", perf_path.display());

        let ffails = fs::File::create(&fail_path).expect("failed to create failures profile file");
        serde_json::to_writer_pretty(ffails, &fail_obj)
            .expect("failed to write failures profile JSON");
        eprintln!("WROTE_PROFILE {}", fail_path.display());
    }

    // Generate report files
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
    writeln!(
        f,
        "==============================================================================="
    )
    .ok();
    writeln!(f, "  PARSEABLE VALIDATION REPORT").ok();
    writeln!(f, "  timestamp={}", timestamp).ok();
    if let Some(ref flt) = args.filter {
        writeln!(f, "  filter={}", flt).ok();
    }
    writeln!(
        f,
        "  cases={} passed={} failed={}",
        total_cases, passed, failed
    )
    .ok();
    writeln!(
        f,
        "==============================================================================="
    )
    .ok();
    writeln!(f).ok();

    if failed == 0 {
        writeln!(f, "RESULT: ALL {} CASES PASSED", total_cases).ok();
    } else {
        writeln!(f, "RESULT: {} FAILED out of {} cases", failed, total_cases).ok();
    }
    writeln!(f).ok();

    // Passing by suite
    writeln!(
        f,
        "-------------------------------------------------------------------------------"
    )
    .ok();
    writeln!(f, "  PASSING ({}/{})", passed, total_cases).ok();
    writeln!(
        f,
        "-------------------------------------------------------------------------------"
    )
    .ok();

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
                "    ok  {:30} {:>8}us  \"{}\"",
                r.desc,
                r.duration.as_micros(),
                r.input
            )
            .ok();
        }
    }
    writeln!(f).ok();

    // Failures — full detail
    writeln!(
        f,
        "==============================================================================="
    )
    .ok();
    writeln!(f, "  FAILURES ({}/{})", failed, total_cases).ok();
    writeln!(
        f,
        "==============================================================================="
    )
    .ok();

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
                    "    FAIL  {}  (expect={}, time={}us)",
                    r.desc,
                    r.expect,
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
    writeln!(
        f,
        "-------------------------------------------------------------------------------"
    )
    .ok();
    writeln!(f, "  PER-SUITE SUMMARY").ok();
    writeln!(
        f,
        "-------------------------------------------------------------------------------"
    )
    .ok();

    let suites: Vec<&str> = results.iter().map(|r| r.suite).collect();
    let unique_suites: Vec<&str> = {
        let mut v = suites.clone();
        v.sort();
        v.dedup();
        v
    };

    for suite_name in unique_suites {
        let suite_results: Vec<&CaseResult> =
            results.iter().filter(|r| r.suite == suite_name).collect();
        let sp = suite_results.iter().filter(|r| r.passed).count();
        let sf = suite_results.iter().filter(|r| !r.passed).count();
        let st: Duration = suite_results.iter().map(|r| r.duration).sum();
        let status = if sf == 0 { "ok" } else { "FAIL" };
        writeln!(
            f,
            "    {:4}  {:40} passed={} failed={} time={}us",
            status,
            suite_name,
            sp,
            sf,
            st.as_micros()
        )
        .ok();
    }
    writeln!(f).ok();

    writeln!(
        f,
        "==============================================================================="
    )
    .ok();
    writeln!(f, "  END OF REPORT").ok();
    writeln!(
        f,
        "==============================================================================="
    )
    .ok();

    drop(f);

    // Create separate reports: successes and failures for easier consumption
    let successes_path = reports_dir.join(format!("successes-{}.txt", timestamp));
    let mut sf = fs::File::create(&successes_path).expect("failed to create successes file");
    writeln!(
        sf,
        "=============================================================================="
    )
    .ok();
    writeln!(sf, "  PASSING REPORT").ok();
    writeln!(sf, "  timestamp={}", timestamp).ok();
    writeln!(
        sf,
        "  cases={} passed={} failed={}",
        total_cases, passed, failed
    )
    .ok();
    writeln!(sf, "  total_time_us=?").ok();
    writeln!(
        sf,
        "=============================================================================="
    )
    .ok();
    writeln!(sf).ok();

    let mut current_suite = "";
    for r in &results {
        if r.passed {
            if r.suite != current_suite {
                writeln!(sf).ok();
                writeln!(sf, "  [{}]", r.suite).ok();
                current_suite = r.suite;
            }
            writeln!(
                sf,
                "    ok  {:30} {:>8}us  \"{}\"",
                r.desc,
                r.duration.as_micros(),
                r.input
            )
            .ok();
        }
    }

    eprintln!("WROTE_REPORT {}", successes_path.display());

    let failures_path = reports_dir.join(format!("failures-{}.txt", timestamp));
    let mut ff = fs::File::create(&failures_path).expect("failed to create failures file");
    writeln!(
        ff,
        "=============================================================================="
    )
    .ok();
    writeln!(ff, "  FAILURES REPORT").ok();
    writeln!(ff, "  timestamp={}", timestamp).ok();
    writeln!(
        ff,
        "  cases={} passed={} failed={}",
        total_cases, passed, failed
    )
    .ok();
    writeln!(
        ff,
        "=============================================================================="
    )
    .ok();
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
                writeln!(
                    ff,
                    "    FAIL  {}  (expect={}, time={}us)",
                    r.desc,
                    r.expect,
                    r.duration.as_micros()
                )
                .ok();
                writeln!(ff, "    input=\"{}\"", r.input).ok();
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
    if failed == 0 {
        eprintln!("  ALL {} CASES PASSED  beam_fallbacks=?  ", total_cases,);
    } else {
        eprintln!("  {} FAILED / {} total  ", failed, total_cases,);
        eprintln!();
        for r in &results {
            if !r.passed {
                let kind = r.detail.lines().next().unwrap_or("");
                eprintln!("  FAIL  {} / {}  {} ", r.suite, r.desc, kind);
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
