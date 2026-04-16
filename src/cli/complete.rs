//! `complete` — synthesize a complete program from a partial prefix read from stdin.
//!
//! Reads a grammar specification from `--spec` and a partial program from
//! stdin, then runs the priority-guided search to find the shortest well-typed
//! completion.  The completed program is written to stdout.
//!
//! Exit codes
//! ----------
//! 0  A completion was found; the full program is printed to stdout.
//! 1  No completion found within the configured budget (exhausted or invalid).
//! 2  Usage / I/O error (bad spec file, missing flag, …).

use clap::Args;
use std::io::{self, Read};
use std::path::PathBuf;

use aufbau::logic::grammar::Grammar;
use aufbau::validation::completability;

/// Complete a partial program read from stdin using the type-aware synthesizer.
///
/// The search is a priority-guided DFS that tries to extend the partial input
/// one token at a time until a complete, well-typed program is found.  The
/// budget can be tuned with `--depth`.
///
/// Examples
/// --------
///
///   echo "let x : Int =" | aufbau complete -s examples/fun.auf
///
///   echo "let" | aufbau complete -s examples/fun.auf --depth 12
///
///   echo "(x : Int) =>" | aufbau complete -s examples/fun.auf --info
#[derive(Args, Debug, Clone)]
pub struct CompleteCmd {
    /// Path to the grammar / typing-rules specification file (.auf)
    #[arg(short = 's', long = "spec", value_name = "FILE")]
    pub spec: PathBuf,

    /// Maximum number of completion steps (token extensions) from the prefix
    #[arg(long = "depth", default_value_t = 10)]
    pub depth: usize,

    /// Print extra information: depth reached, states explored, completion path
    #[arg(short = 'i', long = "info", action = clap::ArgAction::SetTrue)]
    pub info: bool,

    /// On failure, print the visited states sample to help diagnose the search
    #[arg(long = "dump-visited", action = clap::ArgAction::SetTrue)]
    pub dump_visited: bool,
}

pub fn run(args: &CompleteCmd) {
    // ── 1. Load grammar ──────────────────────────────────────────────────
    let spec_src = match std::fs::read_to_string(&args.spec) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("error: cannot read spec '{}': {}", args.spec.display(), e);
            std::process::exit(2);
        }
    };
    let grammar = match Grammar::load(&spec_src) {
        Ok(g) => g,
        Err(e) => {
            eprintln!("error: failed to load grammar: {}", e);
            std::process::exit(2);
        }
    };

    // ── 2. Read partial program from stdin ───────────────────────────────
    let mut input = String::new();
    if let Err(e) = io::stdin().read_to_string(&mut input) {
        eprintln!("error: failed to read stdin: {}", e);
        std::process::exit(2);
    }
    let input = input.trim_end_matches('\n');

    if args.info {
        eprintln!("input    : {:?}", input);
        eprintln!("depth    : {}", args.depth);
    }

    // ── 3. Run completion search ─────────────────────────────────────────
    let results = completability::complete_k(&grammar, input, args.depth, 1, None);
    if let Some(complete_input) = results.first() {
        if args.info {
            eprintln!("status   : success");
        }
        println!("{}", complete_input);
        std::process::exit(0);
    }

    eprintln!("error: no completion found");
    std::process::exit(1);
}
