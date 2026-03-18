//! `complete-k` — return up to k complete programs for a partial prefix.

use clap::Args;
use std::io::{self, Read};
use std::path::PathBuf;

use aufbau::logic::grammar::Grammar;
use aufbau::logic::typing::Context;
use aufbau::logic::{search_k, SearchConfig};

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

    /// Maximum total search states explored before giving up
    #[arg(long = "states", default_value_t = 96)]
    pub states: usize,

    /// Maximum children kept per expanded state (beam width)
    #[arg(long = "children", default_value_t = 12)]
    pub children: usize,

    /// Maximum concrete string examples tried per regex token
    #[arg(long = "examples", default_value_t = 1)]
    pub examples: usize,
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

    let config = SearchConfig {
        max_depth: args.depth,
        max_token_examples: args.examples,
        max_states: args.states,
        max_children_per_state: args.children,
    };

    let ctx = Context::new();
    let results = search_k(&grammar, input, args.count, &config, &ctx);

    if results.is_empty() {
        eprintln!("error: no completions found");
        std::process::exit(1);
    }

    for result in results {
        println!("{}", result.text());
    }
}
