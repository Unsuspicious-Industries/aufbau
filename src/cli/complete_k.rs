//! `complete-k` — return up to k complete programs for a partial prefix.

use clap::Args;
use std::io::{self, Read};
use std::path::PathBuf;

use aufbau::logic::grammar::Grammar;
use aufbau::validation::completability;

#[derive(Args, Debug, Clone)]
pub struct CompleteKCmd {
    /// Path to the grammar / typing-rules specification file (.auf)
    #[arg(short = 's', long = "spec", value_name = "FILE")]
    pub spec: PathBuf,

    /// Maximum number of completions to return
    #[arg(short = 'k', long = "count", default_value_t = 3)]
    pub count: usize,

    /// Maximum number of completion steps (token extensions) from the prefix
    #[arg(long = "depth", default_value_t = 10)]
    pub depth: usize,
}

pub fn run(args: &CompleteKCmd) {
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

    let mut input = String::new();
    if let Err(e) = io::stdin().read_to_string(&mut input) {
        eprintln!("error: failed to read stdin: {}", e);
        std::process::exit(2);
    }
    let input = input.trim_end_matches('\n');

    let results = completability::complete_k(&grammar, input, args.depth, args.count, None);

    if results.is_empty() {
        eprintln!("error: no completions found");
        std::process::exit(1);
    }

    for result in results {
        println!("{}", result);
    }
}
