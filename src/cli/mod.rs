pub mod check;
pub mod examine;
pub mod typecheck;
pub mod validate;

use aufbau::logic::debug::{DebugLevel, add_module_filter, set_debug_input, set_debug_level};
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
    /// Run parseable/feed validation.
    Check(self::check::CheckCmd),

    /// Type-check a program (or partial program) read from stdin.
    Typecheck(self::typecheck::TypecheckCmd),


    /// Run validation test suites with progress and report
    Validate(self::validate::ValidateCmd),

    /// Quick helper to examine feed acceptance for an input or test-case
    Examine(self::examine::ExamineCmd),
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
        Commands::Typecheck(args) => self::typecheck::run(args),
        Commands::Validate(args) => self::validate::run(args),
        Commands::Examine(args) => self::examine::run(args),
    }
}
