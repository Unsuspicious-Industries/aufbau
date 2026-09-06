//! Reproducible engine performance report.
//!
//! The corpus and configuration are fixed, while timings are observations. All
//! environment and VCS inspection happens before measurement; measured loops
//! contain only engine work and the clock.

use aufbau::grammar::SPG;
use aufbau::typing::{completeness, Context, Type, TypingSynth};
use serde::Serialize;
use std::hint::black_box;
use std::process::Command;
use std::time::Instant;

const SCHEMA: &str = "aufbau.engine-perf/v2";
const WARMUP: usize = 3;
const SAMPLES: usize = 10;
const CONTEXT_SIZE: usize = 32;
const MASK: &[&str] = &["λ", "(", "x", " ", "->"];
const INPUT: &str = "f x0";
const GRAMMAR: &str = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/examples/stlc.auf"));

#[derive(Serialize)]
struct Report {
    schema: &'static str,
    config: Config,
    machine: Machine,
    build: Build,
    corpus: Corpus,
    benchmarks: Vec<Benchmark>,
}

#[derive(Serialize)]
struct Config {
    warmup_iterations: usize,
    sample_count: usize,
    context_size: usize,
    seed: Option<u64>,
    identity_hash: String,
}

#[derive(Serialize)]
struct Machine {
    hostname: String,
    kernel: String,
    os: &'static str,
    arch: &'static str,
    pointer_width: &'static str,
    cpu_model: String,
    logical_cpu_count: usize,
}

#[derive(Serialize)]
struct Build {
    package: &'static str,
    version: &'static str,
    profile: &'static str,
    features: String,
    debug_assertions: bool,
    rustc: String,
    cargo: String,
    git_head: String,
    git_dirty: bool,
    git_diff_hash: String,
}

#[derive(Serialize)]
struct Corpus {
    grammar: &'static str,
    input: &'static str,
    mask_candidates: &'static [&'static str],
    identity_hash: String,
}

#[derive(Serialize)]
struct Benchmark {
    name: &'static str,
    unit: &'static str,
    samples_ns: Vec<u128>,
    mean_ns: f64,
    median_ns: f64,
    sample_sd_ns: f64,
    standard_error_ns: f64,
    min_ns: u128,
    max_ns: u128,
}

fn fnv1a64(bytes: &[u8]) -> u64 {
    let mut hash = 0xcbf29ce484222325u64;
    for byte in bytes {
        hash ^= u64::from(*byte);
        hash = hash.wrapping_mul(0x100000001b3);
    }
    hash
}

fn identity_hash(value: &str) -> String {
    format!("fnv1a64:{:016x}", fnv1a64(value.as_bytes()))
}

fn command_output(program: &str, args: &[&str]) -> String {
    Command::new(program)
        .args(args)
        .output()
        .ok()
        .filter(|output| output.status.success())
        .map(|output| String::from_utf8_lossy(&output.stdout).trim().to_owned())
        .unwrap_or_else(|| "unknown".to_owned())
}

fn command_bytes(program: &str, args: &[&str]) -> Vec<u8> {
    Command::new(program)
        .args(args)
        .output()
        .ok()
        .filter(|output| output.status.success())
        .map(|output| output.stdout)
        .unwrap_or_default()
}

fn cpu_model() -> String {
    std::fs::read_to_string("/proc/cpuinfo")
        .ok()
        .and_then(|info| {
            info.lines()
                .find_map(|line| line.strip_prefix("model name\t: "))
                .map(str::to_owned)
        })
        .unwrap_or_else(|| command_output("sysctl", &["-n", "machdep.cpu.brand_string"]))
}

fn logical_cpu_count() -> usize {
    std::thread::available_parallelism()
        .map(usize::from)
        .unwrap_or(0)
}

fn feature_names() -> String {
    let mut features = Vec::new();
    if cfg!(feature = "python-ffi") {
        features.push("python-ffi");
    }
    if cfg!(feature = "extension-module") {
        features.push("extension-module");
    }
    if cfg!(feature = "ocaml-ffi") {
        features.push("ocaml-ffi");
    }
    if cfg!(feature = "trace") {
        features.push("trace");
    }
    if features.is_empty() {
        "default".to_owned()
    } else {
        features.join(",")
    }
}

fn measure<F: FnMut()>(name: &'static str, mut operation: F) -> Benchmark {
    for _ in 0..WARMUP {
        operation();
    }
    let mut samples_ns = Vec::with_capacity(SAMPLES);
    for _ in 0..SAMPLES {
        let start = Instant::now();
        operation();
        samples_ns.push(start.elapsed().as_nanos());
    }
    let mut sorted = samples_ns.clone();
    sorted.sort_unstable();
    let count = samples_ns.len() as f64;
    let mean_ns = samples_ns.iter().map(|&sample| sample as f64).sum::<f64>() / count;
    let median_ns = if sorted.len() % 2 == 0 {
        (sorted[sorted.len() / 2 - 1] as f64 + sorted[sorted.len() / 2] as f64) / 2.0
    } else {
        sorted[sorted.len() / 2] as f64
    };
    let variance = samples_ns
        .iter()
        .map(|&sample| {
            let difference = sample as f64 - mean_ns;
            difference * difference
        })
        .sum::<f64>()
        / (count - 1.0);
    let sample_sd_ns = variance.sqrt();
    Benchmark {
        name,
        unit: "ns",
        samples_ns,
        mean_ns,
        median_ns,
        sample_sd_ns,
        standard_error_ns: sample_sd_ns / count.sqrt(),
        min_ns: *sorted.first().unwrap(),
        max_ns: *sorted.last().unwrap(),
    }
}

fn grammar() -> SPG {
    SPG::load(GRAMMAR).expect("fixed benchmark grammar must load")
}

fn input_context(g: &SPG) -> Context {
    let mut ctx = Context::new();
    ctx.add(
        "f".to_string(),
        Type::parse(g, "T0->T1").expect("fixed function type must parse"),
    );
    ctx.add(
        "x0".to_string(),
        Type::parse(g, "T0").expect("fixed argument type must parse"),
    );
    ctx
}

fn main() {
    let loaded = grammar();
    let ctx = input_context(&loaded);
    let mask_candidates: Vec<String> = MASK.iter().map(|s| (*s).to_string()).collect();
    let mut benchmarks = Vec::new();

    benchmarks.push(measure("grammar_load", || {
        black_box(grammar());
    }));
    benchmarks.push(measure("parse_typecheck", || {
        let mut synth = TypingSynth::new(loaded.clone(), INPUT);
        black_box(synth.parse_with(&ctx).expect("fixed input must typecheck"));
    }));
    benchmarks.push(measure("mask_feed", || {
        let mut synth = TypingSynth::new(loaded.clone(), "");
        black_box(synth.mask(&mask_candidates));
        black_box(synth.feed("λ").is_ok());
    }));
    benchmarks.push(measure("context_growth", || {
        let mut synth = TypingSynth::new(loaded.clone(), "");
        let ty = Type::parse(&loaded, "T0").expect("fixed context type must parse");
        let mut growing = Context::new();
        for i in 0..CONTEXT_SIZE {
            growing.add(format!("x{i}"), ty.clone());
            synth.set_context(growing.clone());
        }
        black_box(synth.ctx());
    }));
    benchmarks.push(measure("completeness", || {
        black_box(completeness(&loaded));
    }));

    let config_identity =
        format!("warmup={WARMUP};samples={SAMPLES};context={CONTEXT_SIZE};seed=none");
    let corpus_identity = format!("grammar={GRAMMAR};input={INPUT};mask={MASK:?}");
    let git_head = command_output("git", &["rev-parse", "HEAD"]);
    let git_dirty_output = command_output("git", &["status", "--porcelain"]);
    let git_diff = command_bytes("git", &["diff", "HEAD", "--"]);
    let report = Report {
        schema: SCHEMA,
        config: Config {
            warmup_iterations: WARMUP,
            sample_count: SAMPLES,
            context_size: CONTEXT_SIZE,
            seed: None,
            identity_hash: identity_hash(&config_identity),
        },
        machine: Machine {
            hostname: command_output("hostname", &[]),
            kernel: command_output("uname", &["-srvm"]),
            os: std::env::consts::OS,
            arch: std::env::consts::ARCH,
            pointer_width: if cfg!(target_pointer_width = "64") {
                "64"
            } else if cfg!(target_pointer_width = "32") {
                "32"
            } else {
                "unknown"
            },
            cpu_model: cpu_model(),
            logical_cpu_count: logical_cpu_count(),
        },
        build: Build {
            package: env!("CARGO_PKG_NAME"),
            version: env!("CARGO_PKG_VERSION"),
            profile: if cfg!(debug_assertions) {
                "debug"
            } else {
                "release"
            },
            features: feature_names(),
            debug_assertions: cfg!(debug_assertions),
            rustc: command_output("rustc", &["--version", "--verbose"]),
            cargo: command_output("cargo", &["--version"]),
            git_head,
            git_dirty: !git_dirty_output.is_empty(),
            git_diff_hash: format!("fnv1a64:{:016x}", fnv1a64(&git_diff)),
        },
        corpus: Corpus {
            grammar: "examples/stlc.auf",
            input: INPUT,
            mask_candidates: MASK,
            identity_hash: identity_hash(&corpus_identity),
        },
        benchmarks,
    };
    println!(
        "{}",
        serde_json::to_string_pretty(&report).expect("report must serialize")
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn identity_hash_is_stable() {
        assert_eq!(identity_hash("aufbau"), "fnv1a64:e05c8c6b735ea7fd");
    }

    #[test]
    fn benchmark_statistics_are_sample_statistics() {
        let benchmark = measure("test", || {
            black_box(1u8);
        });
        assert_eq!(benchmark.samples_ns.len(), SAMPLES);
        assert!(benchmark.mean_ns.is_finite());
        assert!(benchmark.sample_sd_ns.is_finite());
        assert!(benchmark.standard_error_ns.is_finite());
    }

    #[test]
    fn schema_and_fixed_identities_are_present() {
        assert_eq!(SCHEMA, "aufbau.engine-perf/v2");
        assert_ne!(identity_hash(GRAMMAR), identity_hash(INPUT));
    }
}
