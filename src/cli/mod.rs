pub mod check;
pub mod complete;
pub mod complete_k;
pub mod examine;
pub mod exp;
pub mod logic;
pub mod validate;
pub mod verify;

use aufbau::logic::debug::{add_module_filter, set_debug_input, set_debug_level, DebugLevel};
use clap::{ArgAction, Parser, Subcommand};

#[derive(Parser)]
#[command(name = "aufbau", version, about = "aufbau toolkit", long_about = None)]
#[command(propagate_version = true)]
pub struct Cli {
    /// Increase verbosity (-v, -vv, -vvv)
    #[arg(short = 'v', long = "verbose", action = ArgAction::Count, global = true)]
    pub verbose: u8,

    /// Set debug level to trace (overrides verbose)
    #[arg(long = "trace", action = ArgAction::SetTrue, global = true)]
    pub trace: bool,

    /// Filter debug output to modules (comma-separated: parser,grammar,bind,check)
    #[arg(long = "modules", value_name = "LIST", global = true)]
    pub modules: Option<String>,

    /// Include input text in span messages
    #[arg(long = "with-input", action = ArgAction::SetTrue, global = true)]
    pub with_input: bool,

    #[command(subcommand)]
    pub command: Commands,
}

#[derive(Subcommand)]
pub enum Commands {
    /// Type-check a program (or partial program) read from stdin.
    ///
    /// Reads a grammar spec from --spec and the program from stdin, then
    /// runs the partial type-checker.  For complete programs the inferred
    /// type is printed; for partial programs every surviving candidate type
    /// is shown so callers can see what completions remain possible.
    ///
    /// Exit codes: 0 = typed OK (complete or partial), 1 = parse/type error,
    /// 2 = usage/I-O error.
    Check(self::check::CheckCmd),

    /// Complete a partial program read from stdin using the type-aware synthesizer.
    ///
    /// Reads a grammar spec from --spec and a partial program from stdin,
    /// then runs a priority-guided search to find the shortest well-typed
    /// completion.  The completed program is written to stdout.
    ///
    /// Exit codes: 0 = completion found (output on stdout), 1 = no completion
    /// found within budget, 2 = usage/I-O error.
    Complete(self::complete::CompleteCmd),

    /// Return up to k complete programs extending a partial prefix
    CompleteK(self::complete_k::CompleteKCmd),

    /// Logic-related commands (viz, completions)
    Logic(self::logic::LogicCmd),

    /// Run validation test suites with progress and report
    Validate(self::validate::ValidateCmd),

    /// Complete a prefix and validate it against the Coq verifier
    Verify(self::verify::VerifyCmd),

    /// Quick helper to examine completability for an input or test-case
    Examine(self::examine::ExamineCmd),

    /// Run experimental parser/cache/SPPF benchmarks
    Exp(self::exp::ExpCmd),
}

pub fn run() {
    let cli = Cli::parse();

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

    if cli.with_input {
        set_debug_input(None);
    }

    match &cli.command {
        Commands::Check(args) => self::check::run(args),
        Commands::Complete(args) => self::complete::run(args),
        Commands::CompleteK(args) => self::complete_k::run(args),
        Commands::Logic(_) => self::logic::dispatch(&cli),
        Commands::Validate(args) => self::validate::run(args),
        Commands::Verify(args) => self::verify::run(args),
        Commands::Examine(args) => self::examine::run(args),
        Commands::Exp(args) => self::exp::run(args),
    }
}
