use crate::cli::validate::ValidateCmd;
use indicatif::{MultiProgress, ProgressBar, ProgressStyle};
use serde_json::json;
use std::fs;
use std::io::Write as _;
use std::time::{SystemTime, UNIX_EPOCH};

use p7::validation::complexity::{self, ComplexityData, estimate_complexity_exponent};

pub fn run(args: &ValidateCmd) {
    eprintln!("p7 validation runner - complexity");

    let mut experiments: Vec<(&str, Vec<(String, Vec<ComplexityData>)>)> = Vec::new();

    // Gather experiments from modules. Run complexity experiments single-threaded
    // to avoid biasing timing measurements regardless of --jobs.
    experiments.push(("basic", complexity::basic::experiments(None)));
    experiments.push(("fun", complexity::fun::experiments(None)));
    experiments.push(("stlc", complexity::stlc::experiments(None)));

    // Count total experiments
    let total_exps: usize = experiments.iter().map(|(_, v)| v.len()).sum::<usize>();

    let mp = MultiProgress::new();
    let suite_style = ProgressStyle::with_template("{prefix:.bold} [{bar:30.cyan/dim}] {pos}/{len} suites  {msg}").unwrap().progress_chars("=> ");
    let exp_style = ProgressStyle::with_template("  {prefix:.dim} [{bar:30.green/dim}] {pos}/{len}  {elapsed_precise}  {msg}").unwrap().progress_chars("-> ");

    let suite_pb = mp.add(ProgressBar::new(experiments.len() as u64));
    suite_pb.set_style(suite_style);
    suite_pb.set_prefix("suites".to_string());

    let mut all_perf_records: Vec<serde_json::Value> = Vec::new();
    let mut report_lines: Vec<String> = Vec::new();

    for (suite_name, exps) in experiments {
        suite_pb.set_message(suite_name.to_string());

        let exp_pb = mp.insert_after(&suite_pb, ProgressBar::new(exps.len() as u64));
        exp_pb.set_style(exp_style.clone());
        exp_pb.set_prefix(suite_name.to_string());

        for (exp_name, data_points) in exps {
            exp_pb.set_message(exp_name.clone());

            // Compute exponent
            let k = estimate_complexity_exponent(&data_points);

            // Build performance record
            let points: Vec<serde_json::Value> = data_points
                .iter()
                .map(|d| json!({"n": d.n, "time_ms": d.time.as_millis(), "input_len": d.input.len()}))
                .collect();

            let perf_obj = json!({
                "suite": suite_name,
                "experiment": exp_name,
                "exponent": k,
                "points": points,
            });

            all_perf_records.push(perf_obj.clone());

            // Save human-readable summary for report
            report_lines.push(format!("{}::{}  exponent={:.2}  samples={}", suite_name, exp_name, k, data_points.len()));

            exp_pb.inc(1);
        }

        exp_pb.finish_and_clear();
        suite_pb.inc(1);
    }

    suite_pb.finish_with_message("done");

    // Write profiling files if requested
    if let Some(profile_path) = &args.profile {
        if let Some(parent) = profile_path.parent() {
            std::fs::create_dir_all(parent).ok();
        }
        let stem = profile_path.file_stem().unwrap().to_string_lossy();
        let perf_path = profile_path.parent().unwrap_or(std::path::Path::new(".")).join(format!("{}-complexity-perf.json", stem));
        let fperf = fs::File::create(&perf_path).expect("failed to create perf profile file");
        serde_json::to_writer_pretty(fperf, &json!({"generated_by": "p7 validate complexity", "cases": all_perf_records})).expect("failed to write perf profile JSON");
        eprintln!("WROTE_PROFILE {}", perf_path.display());
    }

    // Write textual report
    let timestamp = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs();
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let reports_dir = std::path::Path::new(manifest_dir).join("validation").join("reports");
    fs::create_dir_all(&reports_dir).expect("failed to create reports dir");
    let report_path = reports_dir.join(format!("complexity-{}.txt", timestamp));
    let mut f = fs::File::create(&report_path).expect("failed to create report file");

    writeln!(f, "COMPLEXITY REPORT").ok();
    writeln!(f, "timestamp={}", timestamp).ok();
    writeln!(f).ok();
    for line in report_lines {
        writeln!(f, "{}", line).ok();
    }

    eprintln!("WROTE_REPORT {}", report_path.display());
}