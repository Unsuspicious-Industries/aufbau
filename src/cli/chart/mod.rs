use clap::Parser;
use std::io::{BufWriter, Write};
use std::path::PathBuf;
use std::time::Instant;

use aufbau::domains::typing::{Context, TypingSynth};
use aufbau::engine::grammar::SPG;
use aufbau::validation::parseable::arithmetic::ARITHMETIC_GRAMMAR;

pub mod inputs;

#[derive(Parser, Debug)]
pub struct ChartCmd {
    /// Output CSV file path
    #[arg(short, long, default_value = "data/chart_growth.csv")]
    pub output: PathBuf,

    /// Repetitions per data point (higher = more stable percentiles)
    #[arg(short, long, default_value_t = 20)]
    pub reps: usize,

    /// Maximum input size n to sweep up to
    #[arg(long, default_value_t = 12)]
    pub max_n: usize,
}

// ── Statistics ────────────────────────────────────────────────────────────────

struct Stats {
    min: u64,
    p25: u64,
    median: u64,
    p75: u64,
    max: u64,
}

fn percentile_stats(mut v: Vec<u64>) -> Stats {
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

fn measure(grammar: &SPG, input: &str, ctx: &Context, reps: usize) -> (usize, Stats) {
    let times: Vec<u64> = (0..reps)
        .map(|_| {
            let t = Instant::now();
            let mut s = TypingSynth::new(grammar.clone(), input);
            let _ = s.parse_with(ctx).unwrap();
            t.elapsed().as_micros() as u64
        })
        .collect();

    let mut s = TypingSynth::new(grammar.clone(), input);
    let nodes = s.parse_with(ctx).unwrap().node_count();
    (nodes, percentile_stats(times))
}

fn token_count(grammar: &SPG, input: &str) -> usize {
    let mut g = grammar.clone();
    match g.tokenize(input) {
        Ok(segs) => segs.len(),
        Err(_) => input.split_whitespace().count(),
    }
}

fn write_row(
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

// ── Subcommand entry point ────────────────────────────────────────────────────

pub fn run(cmd: &ChartCmd) {
    if let Some(parent) = cmd.output.parent()
        && !parent.as_os_str().is_empty() {
            std::fs::create_dir_all(parent)
                .unwrap_or_else(|e| panic!("cannot create {}: {e}", parent.display()));
        }

    let file = std::fs::File::create(&cmd.output)
        .unwrap_or_else(|e| panic!("cannot create {}: {e}", cmd.output.display()));
    let mut out = BufWriter::new(file);

    writeln!(
        out,
        "grammar,experiment,n,tokens,node_count,time_us_min,time_us_p25,time_us_median,time_us_p75,time_us_max"
    )
    .unwrap();

    let reps = cmd.reps;
    let max_n = cmd.max_n;

    // STLC application chains
    eprint!("stlc application chains ");
    let stlc = SPG::load(include_str!("../../../examples/stlc.auf")).unwrap();
    for n in 1..=max_n {
        let (input, ctx) = inputs::stlc_chain(n);
        let toks = token_count(&stlc, &input);
        let (nodes, s) = measure(&stlc, &input, &ctx, reps);
        write_row(&mut out, "stlc", "application_chain", n, toks, nodes, &s);
        eprint!(".");
    }
    eprintln!(" done");

    // IMP declaration blocks
    eprint!("imp declaration blocks  ");
    let imp = SPG::load(include_str!("../../../examples/imp.auf")).unwrap();
    for n in 1..=max_n {
        let input = inputs::imp_block(n);
        let toks = token_count(&imp, &input);
        let (nodes, s) = measure(&imp, &input, &Context::new(), reps);
        write_row(&mut out, "imp", "declaration_block", n, toks, nodes, &s);
        eprint!(".");
    }
    eprintln!(" done");

    // Arithmetic flat sum
    eprint!("arith flat sum          ");
    let arith = SPG::load(ARITHMETIC_GRAMMAR).unwrap();
    for n in 1..=(max_n + 2) {
        let input = inputs::arith_flat(n);
        let toks = token_count(&arith, &input);
        let (nodes, s) = measure(&arith, &input, &Context::new(), reps);
        write_row(&mut out, "arith", "flat_sum", n, toks, nodes, &s);
        eprint!(".");
    }
    eprintln!(" done");

    // Arithmetic nested parens
    eprint!("arith nested parens     ");
    for n in 1..=max_n {
        let input = inputs::arith_nested(n);
        let toks = token_count(&arith, &input);
        let (nodes, s) = measure(&arith, &input, &Context::new(), reps);
        write_row(&mut out, "arith", "nested_parens", n, toks, nodes, &s);
        eprint!(".");
    }
    eprintln!(" done");

    out.flush().unwrap();
    eprintln!("written → {}", cmd.output.display());
}
