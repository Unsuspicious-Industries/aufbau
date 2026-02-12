pub mod logic;
pub mod validate;

use clap::{ArgAction, Parser, Subcommand};
use p7::logic::debug::{add_module_filter, set_debug_input, set_debug_level, DebugLevel};

#[derive(Parser)]
#[command(name = "p7", version, about = "p7 toolkit", long_about = None)]
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
    /// Logic-related commands
    Logic(self::logic::LogicCmd),
    /// Run validation test suites with progress and report
    Validate(self::validate::ValidateCmd),
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
        // leave actual input empty for now; specific commands may set the input string
        set_debug_input(None);
    }

    match &cli.command {
        Commands::Logic(_) => self::logic::dispatch(&cli),
        Commands::Validate(args) => self::validate::run(args),
    }
}
