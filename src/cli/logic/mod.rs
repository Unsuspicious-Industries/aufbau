use clap::{Args, Subcommand};
use std::fs;
use std::path::PathBuf;

use anstyle::{AnsiColor, Style};
use beam::engine::Synthesizer;
use beam::engine::rank::{DefaultRanker, StlcRanker};
use beam::logic::debug::{DebugLevel, add_module_filter, set_debug_input, set_debug_level};
use beam::logic::{check::TypeChecker, grammar::Grammar, parser::Parser};

#[derive(Args, Debug, Clone)]
pub struct LogicCmd {
    #[command(subcommand)]
    pub command: LogicSubcommand,
}

#[derive(Subcommand, Debug, Clone)]
pub enum LogicSubcommand {
    /// Typecheck a source file given a grammar spec
    Check(CheckArgs),
    /// Launch the visualization server
    Viz(VizArgs),
    /// Synthesize a well-typed program from a grammar
    Synthesize(SynthArgs),
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
    /// Path to grammar specification file
    #[arg(short = 's', long = "spec", value_name = "FILE")]
    pub spec_path: PathBuf,

    /// Optional port to bind the server
    #[arg(short = 'p', long = "port", default_value_t = 5173)]
    pub port: u16,
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

    match &cli.command {
        crate::cli::Commands::Logic(cmd) => match &cmd.command {
            LogicSubcommand::Check(args) => run_check(args, cli.with_input, level),
            LogicSubcommand::Viz(args) => run_viz(args, level),
            LogicSubcommand::Synthesize(args) => run_synthesize(args),
        },
    }
}

fn run_check(args: &CheckArgs, with_input: bool, debug_level: DebugLevel) {
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

    // Load code
    let code = match fs::read_to_string(&args.code_path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!(
                "error: failed to read code '{}': {}",
                args.code_path.display(),
                e
            );
            std::process::exit(2);
        }
    };
    if with_input {
        set_debug_input(Some(code.clone()));
    }

    // Parse (partial-first)
    let mut parser = Parser::new(grammar);
    let past = match parser.partial(&code) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("parse error: {}", e);
            std::process::exit(1);
        }
    };

    // Typecheck using partial checker
    let mut checker = TypeChecker::new();
    match checker.check_partial(past.root()) {
        Ok(Some(ty)) => {
            let ok = Style::new().fg_color(Some(AnsiColor::Green.into()));
            println!("{ok}Type:{ok:#} {:?}", ty);
            std::process::exit(0);
        }
        Ok(None) => {
            let warn = Style::new().fg_color(Some(AnsiColor::Yellow.into()));
            println!("{warn}No type inferred{warn:#} (no applicable rule)");
            std::process::exit(0);
        }
        Err(e) => {
            let err = Style::new().fg_color(Some(AnsiColor::Red.into()));
            eprintln!("{err}Type error:{err:#} {}", e);
            std::process::exit(1);
        }
    }
}

fn run_viz(args: &VizArgs, debug_level: DebugLevel) {
    let bind = format!("127.0.0.1:{}", args.port);
    eprintln!("Starting viz server on http://{}", bind);
    let _ = debug_level; // silence for now; wired globally above
    beam::viz::serve(&bind);
}

fn run_synthesize(args: &SynthArgs) {
    // Load grammar spec text
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

    // Select ranker backend
    let ranker: Box<dyn beam::engine::rank::Ranker> = match args.backend.as_str() {
        "random" => Box::new(DefaultRanker),
        "stlc" => Box::new(StlcRanker),
        other => {
            eprintln!(
                "error: unknown backend '{}'. Available: random, stlc",
                other
            );
            std::process::exit(2);
        }
    };

    let mut synth = match Synthesizer::new(&spec, ranker) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("error: failed to initialize synthesizer: {}", e);
            std::process::exit(2);
        }
    };

    // Provide seed as debug input for better tracing context
    set_debug_input(Some(args.seed.clone()));

    match synth.run_with(&args.seed, args.beam_width, args.steps) {
        Ok(program) => {
            println!("{}", program);
            std::process::exit(0);
        }
        Err(e) => {
            eprintln!("synthesis failed: {}", e);
            std::process::exit(1);
        }
    }
}
