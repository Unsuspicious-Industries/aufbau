pub mod logic;

use clap::{ArgAction, Parser, Subcommand};

#[derive(Parser)]
#[command(name = "beam", version, about = "Beam toolkit", long_about = None)]
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
}

pub fn run() {
    let cli = Cli::parse();
    self::logic::dispatch(&cli);
}
