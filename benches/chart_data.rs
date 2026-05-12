//! Collect chart-growth data and write it as CSV for paper plots.
//!
//! Usage:
//!   cargo run --example chart_data --release
//!   cargo run --example chart_data --release -- data/my_run.csv
//!
//! Output columns:
//!   grammar, experiment, n, tokens, node_count_median,
//!   time_us_min, time_us_p25, time_us_median, time_us_p75, time_us_max
//!
//! With no argument the file is written to `data/chart_growth.csv`
//! (created if necessary). The path is printed to stderr on completion.

use aufbau::logic::grammar::Grammar;
use aufbau::logic::synth::Synthesizer;
use aufbau::logic::typing::{Context, Type};
use std::io::{BufWriter, Write};
use std::path::PathBuf;
use std::time::Instant;

const REPS: usize = 20;

// ── Statistics ────────────────────────────────────────────────────────────────

struct Stats {
    min: u64,
    p25: u64,
    median: u64,
    p75: u64,
    max: u64,
}

fn stats(mut v: Vec<u64>) -> Stats {
    v.sort_unstable();
    let n = v.len();
    Stats {
        min: v[0],
        p25: v[n / 4],
        median: v[n / 2],
        p75: v[3 * n / 4],
        max: v[n - 1],
    }
}

// ── Measurement ───────────────────────────────────────────────────────────────

fn measure(grammar: &Grammar, input: &str, ctx: &Context) -> (usize, Stats) {
    let times: Vec<u64> = (0..REPS)
        .map(|_| {
            let t = Instant::now();
            let mut s = Synthesizer::new(grammar.clone(), input);
            let _ = s.parse_with(ctx).unwrap();
            t.elapsed().as_micros() as u64
        })
        .collect();

    let mut s = Synthesizer::new(grammar.clone(), input);
    let nodes = s.parse_with(ctx).unwrap().node_count();
    (nodes, stats(times))
}

fn token_count(grammar: &Grammar, input: &str) -> usize {
    let mut g = grammar.clone();
    match g.tokenize(input) {
        Ok(segs) => segs.len(),
        Err(_) => input.split_whitespace().count(),
    }
}

// ── Input generators ─────────────────────────────────────────────────────────

fn stlc_chain(n: usize) -> (String, Context) {
    let type_names: Vec<_> = (0..=n).map(|i| format!("T{i}")).collect();
    let mut f_ty = type_names[n].clone();
    for i in (0..n).rev() {
        f_ty = format!("{}->{}", type_names[i], f_ty);
    }
    let mut ctx = Context::new();
    ctx.add("f".to_string(), Type::parse_raw(&f_ty).unwrap());
    for i in 0..n {
        ctx.add(format!("x{i}"), Type::parse_raw(&type_names[i]).unwrap());
    }
    let input = std::iter::once("f".to_string())
        .chain((0..n).map(|i| format!("x{i}")))
        .collect::<Vec<_>>()
        .join(" ");
    (input, ctx)
}

fn imp_block(n: usize) -> String {
    let mut parts = vec!["{".to_string()];
    for i in 0..n {
        parts.extend([
            "let".to_string(),
            format!("v{i}"),
            ":".to_string(),
            "Int".to_string(),
            "=".to_string(),
            (i + 1).to_string(),
            ";".to_string(),
        ]);
    }
    parts.push("}".to_string());
    parts.join(" ")
}

fn arith_flat(n: usize) -> String {
    (1..=n.max(1))
        .map(|i| i.to_string())
        .collect::<Vec<_>>()
        .join(" + ")
}

fn arith_nested(n: usize) -> String {
    format!("{}{}{}", "(".repeat(n), 1, ")".repeat(n))
}

// ── Row writer ────────────────────────────────────────────────────────────────

fn row(
    out: &mut dyn Write,
    grammar_name: &str,
    experiment: &str,
    n: usize,
    tokens: usize,
    nodes: usize,
    s: &Stats,
) {
    writeln!(
        out,
        "{grammar_name},{experiment},{n},{tokens},{nodes},{},{},{},{},{}",
        s.min, s.p25, s.median, s.p75, s.max,
    )
    .unwrap();
}

// ── Main ──────────────────────────────────────────────────────────────────────

fn main() {
    let out_path: PathBuf = std::env::args()
        .nth(1)
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("data/chart_growth.csv"));

    if let Some(parent) = out_path.parent() {
        if !parent.as_os_str().is_empty() {
            std::fs::create_dir_all(parent).unwrap();
        }
    }

    let file = std::fs::File::create(&out_path)
        .unwrap_or_else(|e| panic!("cannot create {}: {e}", out_path.display()));
    let mut out = BufWriter::new(file);

    writeln!(
        out,
        "grammar,experiment,n,tokens,node_count,time_us_min,time_us_p25,time_us_median,time_us_p75,time_us_max"
    )
    .unwrap();

    // STLC: application chains f x0 ... x{n-1}
    eprint!("stlc application chains ");
    let stlc = Grammar::load(include_str!("stlc.auf")).unwrap();
    for n in 1..=12usize {
        let (input, ctx) = stlc_chain(n);
        let toks = token_count(&stlc, &input);
        let (nodes, s) = measure(&stlc, &input, &ctx);
        row(&mut out, "stlc", "application_chain", n, toks, nodes, &s);
        eprint!(".");
    }
    eprintln!(" done");

    // IMP: declaration blocks
    eprint!("imp declaration blocks  ");
    let imp = Grammar::load(include_str!("imp.auf")).unwrap();
    for n in 1..=12usize {
        let input = imp_block(n);
        let toks = token_count(&imp, &input);
        let (nodes, s) = measure(&imp, &input, &Context::new());
        row(&mut out, "imp", "declaration_block", n, toks, nodes, &s);
        eprint!(".");
    }
    eprintln!(" done");

    // Arithmetic: flat sum  1 + 2 + ... + n
    eprint!("arith flat sum          ");
    let arith_src = aufbau::validation::parseable::arithmetic::ARITHMETIC_GRAMMAR;
    let arith = Grammar::load(arith_src).unwrap();
    for n in 1..=14usize {
        let input = arith_flat(n);
        let toks = token_count(&arith, &input);
        let (nodes, s) = measure(&arith, &input, &Context::new());
        row(&mut out, "arith", "flat_sum", n, toks, nodes, &s);
        eprint!(".");
    }
    eprintln!(" done");

    // Arithmetic: nested parentheses (((...1...)))
    eprint!("arith nested parens     ");
    for n in 1..=10usize {
        let input = arith_nested(n);
        let toks = token_count(&arith, &input);
        let (nodes, s) = measure(&arith, &input, &Context::new());
        row(&mut out, "arith", "nested_parens", n, toks, nodes, &s);
        eprint!(".");
    }
    eprintln!(" done");

    out.flush().unwrap();
    eprintln!("written → {}", out_path.display());
}
