use clap::{Args, Subcommand};
use std::fs;
use std::path::PathBuf;

use aufbau::logic::debug::{add_module_filter, set_debug_input, set_debug_level, DebugLevel};
use aufbau::logic::{grammar::Grammar, Parser};

#[derive(Args, Debug, Clone)]
pub struct LogicCmd {
    #[command(subcommand)]
    pub command: LogicSubcommand,
}

#[derive(Subcommand, Debug, Clone)]
pub enum LogicSubcommand {
    /// Launch the visualization server
    Viz(VizArgs),
    /// Get valid completions for partial input
    Complete(CompleteArgs),
}

#[derive(Args, Debug, Clone)]
pub struct CheckArgs {
    /// Path to grammar specification file
    #[arg(short = 's', long = "spec", value_name = "FILE")]
    pub spec_path: PathBuf,

    /// Path to source code file to typecheck
    #[arg(value_name = "CODE_FILE")]
    pub code_path: PathBuf,

    /// Explicit start symbol override
    #[arg(long = "start")]
    pub start: Option<String>,
}

#[derive(Args, Debug, Clone)]
pub struct VizArgs {
    /// Optional port to bind the server
    #[arg(short = 'p', long = "port", default_value_t = 5173)]
    pub port: u16,

    /// Optional grammar spec to use when launching the viz server
    #[arg(short = 's', long = "spec", value_name = "FILE")]
    pub spec: Option<PathBuf>,
}

#[derive(Args, Debug, Clone)]
pub struct SynthArgs {
    /// Path to grammar specification file
    #[arg(short = 's', long = "spec", value_name = "FILE")]
    pub spec_path: PathBuf,

    /// Beam width (number of candidates kept)
    #[arg(short = 'k', long = "beam", default_value_t = 5)]
    pub beam_width: i32,

    /// Maximum expansion steps
    #[arg(long = "steps", default_value_t = 128)]
    pub steps: usize,

    /// Ranker backend to use (random)
    #[arg(long = "backend", default_value = "random")]
    pub backend: String,

    /// Optional initial prompt/seed code
    #[arg(long = "seed", default_value = "")]
    pub seed: String,
}

#[derive(Args, Debug, Clone)]
pub struct CompleteArgs {
    /// Path to grammar specification file
    #[arg(short = 's', long = "spec", value_name = "FILE")]
    pub spec_path: PathBuf,

    /// Partial input to complete (as string argument)
    #[arg(short = 'i', long = "input", value_name = "TEXT")]
    pub input: Option<String>,

    /// Path to file containing partial input (alternative to --input)
    #[arg(short = 'f', long = "file", value_name = "FILE")]
    pub input_file: Option<PathBuf>,

    /// Explicit start symbol override
    #[arg(long = "start")]
    pub start: Option<String>,

    /// Maximum number of completions to show (default: unlimited)
    #[arg(short = 'k', long = "max", value_name = "NUM")]
    pub max_completions: Option<usize>,

    /// Show detailed metadata for each completion
    #[arg(long = "show-details")]
    pub show_details: bool,
}

pub fn dispatch(cli: &crate::cli::Cli) {
    // Wire verbosity to debug level, with --trace overriding verbose count
    let level = if cli.trace {
        DebugLevel::Trace
    } else {
        match cli.verbose {
            0 => DebugLevel::Error,
            1 => DebugLevel::Warn,
            2 => DebugLevel::Info,
            3 => DebugLevel::Debug,
            _ => DebugLevel::Trace,
        }
    };
    set_debug_level(level);

    if let Some(mods) = &cli.modules {
        for m in mods.split(',').map(|s| s.trim()).filter(|s| !s.is_empty()) {
            add_module_filter(m);
        }
    }

    if let crate::cli::Commands::Logic(cmd) = &cli.command {
        match &cmd.command {
            LogicSubcommand::Viz(args) => run_viz(args, level),
            LogicSubcommand::Complete(args) => run_complete(args, cli.with_input, level),
        }
    }
}

fn run_viz(args: &VizArgs, debug_level: DebugLevel) {
    let bind = format!("127.0.0.1:{}", args.port);
    eprintln!("Starting viz server on http://{}", bind);
    let _ = debug_level; // silence for now; wired globally above

    aufbau::viz::serve(&bind);
}

fn run_complete(args: &CompleteArgs, with_input: bool, debug_level: DebugLevel) {
    let _ = debug_level; // wired globally above

    // Load grammar spec
    let spec = match fs::read_to_string(&args.spec_path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!(
                "error: failed to read spec '{}': {}",
                args.spec_path.display(),
                e
            );
            std::process::exit(2);
        }
    };
    let mut grammar = match Grammar::load(&spec) {
        Ok(g) => g,
        Err(e) => {
            eprintln!("error: failed to parse grammar spec: {}", e);
            std::process::exit(2);
        }
    };
    if let Some(start) = &args.start {
        grammar.set_start(start.clone());
    }

    // Get input from either --input or --file
    let input = match (&args.input, &args.input_file) {
        (Some(text), None) => text.clone(),
        (None, Some(path)) => match fs::read_to_string(path) {
            Ok(s) => s,
            Err(e) => {
                eprintln!(
                    "error: failed to read input file '{}': {}",
                    path.display(),
                    e
                );
                std::process::exit(2);
            }
        },
        (Some(_), Some(_)) => {
            eprintln!("error: cannot specify both --input and --file");
            std::process::exit(2);
        }
        (None, None) => {
            eprintln!("error: must specify either --input or --file");
            std::process::exit(2);
        }
    };

    if with_input {
        set_debug_input(Some(input.clone()));
    }

    // Parse partial input
    let mut parser = Parser::new(grammar.clone());
    let past = match parser.partial(&input).into_result() {
        Ok(p) => p,
        Err(e) => {
            eprintln!("parse error: {}", e);
            std::process::exit(1);
        }
    };

    // Get completions
    let completions = past.completions(&grammar);
    let mut candidates = completions.tokens.clone();

    // Apply max limit if specified
    if let Some(max) = args.max_completions {
        candidates.truncate(max);
    }

    // Display results
    if candidates.is_empty() {
        println!("No completions available");
        std::process::exit(0);
    }

    println!("Found {} completion(s):", candidates.len());
    println!();

    for (idx, token) in candidates.iter().enumerate() {
        println!("  {}. '{}'", idx + 1, token.to_pattern());
    }

    std::process::exit(0);
}
