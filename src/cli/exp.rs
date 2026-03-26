use clap::Args;
use std::path::PathBuf;

#[derive(Args, Debug, Clone)]
pub struct ExpCmd {
    /// Maximum generator size per suite
    #[arg(long = "max-n", default_value_t = 4)]
    pub max_n: usize,

    /// Optional path for JSON output
    #[arg(long = "output", value_name = "FILE")]
    pub output: Option<PathBuf>,
}

pub fn run(args: &ExpCmd) {
    let _ = aufbau::exp::run(aufbau::exp::ExpConfig {
        max_n: args.max_n,
        output: args.output.clone(),
    });
}
