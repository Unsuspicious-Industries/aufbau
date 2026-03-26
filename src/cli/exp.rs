use clap::Args;
use std::path::PathBuf;

#[derive(Args, Debug, Clone)]
pub struct ExpCmd {
    /// Maximum generator size per suite
    #[arg(long = "max-n", default_value_t = 2)]
    pub max_n: usize,

    /// Include standard parser benchmark suites
    #[arg(long = "standard", default_value_t = false)]
    pub standard: bool,

    /// Include incremental `feed` experiments
    #[arg(long = "incremental", default_value_t = false)]
    pub incremental: bool,

    /// Maximum sampled prefixes for incremental runs
    #[arg(long = "max-prefixes", default_value_t = 2)]
    pub max_prefixes: usize,

    /// Include driver-isolation measurements for parser/feed/typing
    #[arg(long = "drivers", default_value_t = true)]
    pub drivers: bool,

    /// Optional path for JSON output
    #[arg(long = "output", value_name = "FILE")]
    pub output: Option<PathBuf>,
}

pub fn run(args: &ExpCmd) {
    let _ = aufbau::exp::run(aufbau::exp::ExpConfig {
        max_n: args.max_n,
        include_standard: args.standard,
        include_incremental: args.incremental,
        max_prefixes: args.max_prefixes,
        include_drivers: args.drivers,
        output: args.output.clone(),
    });
}
