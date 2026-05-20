pub mod chart;
pub mod check;
pub mod validate;

use aufbau::engine::debug::{DebugLevel, add_module_filter, set_debug_level};
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

    #[command(subcommand)]
    pub command: Commands,
}

#[derive(Subcommand)]
pub enum Commands {
    /// Type-check a program (or partial program) read from stdin.
    Check(self::check::CheckCmd),

    /// Run validation test suites with progress and report
    Validate(self::validate::ValidateCmd),

    /// Collect chart-growth data and write a CSV for paper plots
    Chart(self::chart::ChartCmd),
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
        for m in mods.split(',').map(str::trim).filter(|s| !s.is_empty()) {
            add_module_filter(m);
        }
    }

    match &cli.command {
        Commands::Check(args) => self::check::run(args),
        Commands::Validate(args) => self::validate::run(args),
        Commands::Chart(args) => self::chart::run(args),
    }
}
