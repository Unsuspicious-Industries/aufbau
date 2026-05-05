//! `check`: parses and type-check a program (or partial program) from stdin.

use clap::Args;
use std::io::{self, Read};
use std::path::PathBuf;

use aufbau::logic::grammar::Grammar;
use aufbau::logic::structure::FusionNode;
use aufbau::logic::synth::Synthesizer;
use aufbau::logic::typing::{Context, SharedType, Type};

#[derive(Args, Debug, Clone)]
pub struct CheckCmd {
    /// Path to the grammar / typing-rules specification file (.auf)
    #[arg(short = 's', long = "spec", value_name = "FILE")]
    pub spec: PathBuf,

    /// Print the full AST tree for every root, not just the top-level type
    #[arg(long = "ast", action = clap::ArgAction::SetTrue)]
    pub ast: bool,

    /// Show all ambiguous parse candidates, even when a unique complete
    /// root exists (by default only the best / complete root is shown)
    #[arg(long = "all", action = clap::ArgAction::SetTrue)]
    pub all: bool,
}

pub fn run(args: &CheckCmd) {
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

    let mut synth = Synthesizer::new(grammar, input);
    let typed = match synth.parse_with(&Context::new()) {
        Ok(t) => t,
        Err(e) => {
            eprintln!("error: {}", e);
            std::process::exit(1);
        }
    };

    if typed.is_empty() {
        eprintln!("error: no parse found for input");
        std::process::exit(1);
    }

    let runtime = synth.runtime().clone();
    let complete_roots: Vec<_> = typed.roots().filter(|r| r.is_complete()).collect();
    let partial_roots: Vec<_> = typed.roots().filter(|r| !r.is_complete()).collect();

    let is_partial = complete_roots.is_empty();

    let display_roots: Vec<FusionNode> = if args.all {
        typed.roots().collect()
    } else if !complete_roots.is_empty() {
        let well_typed: Vec<FusionNode> = complete_roots
            .iter()
            .filter(|r| {
                let ty: SharedType = r.ty(&runtime);
                let ty_inner: &Type = &ty;
                !matches!(ty_inner, Type::Any | Type::Meta(_))
            })
            .cloned()
            .collect();
        if !well_typed.is_empty() {
            well_typed
        } else {
            complete_roots.clone()
        }
    } else {
        partial_roots
    };

    let mut seen_types: Vec<String> = Vec::new();
    let mut unique_roots = Vec::new();
    for root in &display_roots {
        let ty: SharedType = root.ty(&runtime);
        let ty_s = format!("{}", ty);
        if args.all || !seen_types.contains(&ty_s) {
            seen_types.push(ty_s);
            unique_roots.push(*root);
        }
    }

    if is_partial {
        println!("partial  \"{}\"", input);
        println!();
        if unique_roots.len() == 1 {
            let root = unique_roots[0];
            let ty = root.ty(&runtime);
            println!("type : {}", ty);
        } else {
            println!("{} candidate type(s):", unique_roots.len());
            for (i, root) in unique_roots.iter().enumerate() {
                println!("  [{}] : {}", i + 1, root.ty(&runtime));
            }
        }
    } else if unique_roots.len() == 1 {
        let root = unique_roots[0];
        println!("{} : {}", input, root.ty(&runtime));
        if args.ast {
            println!();
            print!("{}", root);
        }
    } else {
        println!("\"{}\"", input);
        println!();
        println!("{} type(s) (ambiguous parse):", unique_roots.len());
        for (i, root) in unique_roots.iter().enumerate() {
            println!("  [{}] : {}", i + 1, root.ty(&runtime));
            if args.ast {
                println!();
                print!("{}", root);
                println!();
            }
        }
    }

    std::process::exit(0);
}
