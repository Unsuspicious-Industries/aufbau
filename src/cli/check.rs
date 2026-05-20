//! `check`: parses and type-check a program (or partial program) from stdin.

use clap::Args;
use std::io::{self, Read};
use std::path::PathBuf;

use aufbau::domains::typing::{Context, Type};
use aufbau::domains::typing::{TypingDomain, TypingSynth};
use aufbau::engine::grammar::SPG;
use aufbau::engine::structure::FusionNode;

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
    let grammar = match SPG::<TypingDomain>::load(&spec_src) {
        Ok(g) => g,
        Err(e) => {
            eprintln!("error: failed to load grammar: {e}");
            std::process::exit(2);
        }
    };

    let mut input = String::new();
    if let Err(e) = io::stdin().read_to_string(&mut input) {
        eprintln!("error: failed to read stdin: {e}");
        std::process::exit(2);
    }
    let input = input.trim_end_matches('\n');

    let mut synth = TypingSynth::new(grammar, input);
    let typed = match synth.parse_with(&Context::new()) {
        Ok(t) => t,
        Err(e) => {
            eprintln!("error: {e}");
            std::process::exit(1);
        }
    };

    if typed.is_empty() {
        eprintln!("error: no parse found for input");
        std::process::exit(1);
    }

    let runtime = synth.runtime().clone();
    let complete_roots: Vec<_> = typed.roots().filter(aufbau::engine::structure::FusionNode::is_complete).collect();
    let partial_roots: Vec<_> = typed.roots().filter(|r| !r.is_complete()).collect();

    let is_partial = complete_roots.is_empty();

    let display_roots: Vec<FusionNode<TypingDomain>> = if args.all {
        typed.roots().collect()
    } else if !complete_roots.is_empty() {
        let well_typed: Vec<FusionNode<TypingDomain>> = complete_roots
            .iter()
            .filter(|r| {
                let ty = runtime.evidence_of(r.evidence());
                ty.is_some_and(|t| !matches!(t, Type::Any))
            })
            .cloned()
            .collect();
        if well_typed.is_empty() {
            complete_roots.clone()
        } else {
            well_typed
        }
    } else {
        partial_roots
    };

    let mut seen_types: Vec<String> = Vec::new();
    let mut unique_roots = Vec::new();
    for root in &display_roots {
        let ty = runtime.evidence_of(root.evidence());
        let ty_s = match ty {
            Some(t) => format!("{t}"),
            None => String::new(),
        };
        if args.all || !seen_types.contains(&ty_s) {
            seen_types.push(ty_s);
            unique_roots.push(root.clone());
        }
    }

    if is_partial {
        println!("partial  \"{input}\"");
        println!();
        if unique_roots.len() == 1 {
            let root = unique_roots[0].clone();
            let ty = runtime.evidence_of(root.evidence());
            println!(
                "type : {}",
                ty.map(|t| format!("{t}")).unwrap_or_default()
            );
        } else {
            println!("{} candidate type(s):", unique_roots.len());
            for (i, root) in unique_roots.iter().enumerate() {
                let ty = runtime.evidence_of(root.evidence());
                println!(
                    "  [{}] : {}",
                    i + 1,
                    ty.map(|t| format!("{t}")).unwrap_or_default()
                );
            }
        }
    } else if unique_roots.len() == 1 {
        let root = unique_roots[0].clone();
        println!(
            "{} : {}",
            input,
            runtime
                .evidence_of(root.evidence())
                .map(|t| format!("{t}"))
                .unwrap_or_default()
        );
        if args.ast {
            println!();
            print!("{root}");
        }
    } else {
        println!("\"{input}\"");
        println!();
        println!("{} type(s) (ambiguous parse):", unique_roots.len());
        for (i, root) in unique_roots.iter().enumerate() {
            let ty = runtime.evidence_of(root.evidence());
            println!(
                "  [{}] : {}",
                i + 1,
                ty.map(|t| format!("{t}")).unwrap_or_default()
            );
            if args.ast {
                println!();
                print!("{root}");
                println!();
            }
        }
    }

    std::process::exit(0);
}
