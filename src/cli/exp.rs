use clap::Args;
use std::path::PathBuf;

#[derive(Args, Debug, Clone)]
pub struct ExpCmd {
    /// Include the bounded, memory-safe diagnostic suite
    #[arg(long = "safe", default_value_t = true)]
    pub safe: bool,

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
    #[arg(long = "drivers", default_value_t = false)]
    pub drivers: bool,

    /// Maximum number of steps per safe diagnostic scenario
    #[arg(long = "safe-max-steps", default_value_t = 6)]
    pub safe_max_steps: usize,

    /// Run only one safe scenario: parser, repeat, feed, or stage
    #[arg(long = "safe-only")]
    pub safe_only: Option<String>,

    /// Abort safe diagnostic scenarios when a step exceeds this time budget
    #[arg(long = "safe-max-step-ms", default_value_t = 250)]
    pub safe_max_step_ms: u64,

    /// Abort safe diagnostic scenarios when RSS exceeds this budget in MB
    #[arg(long = "safe-max-rss-mb", default_value_t = 256)]
    pub safe_max_rss_mb: u64,

    /// Optional path for JSON output
    #[arg(long = "output", value_name = "FILE")]
    pub output: Option<PathBuf>,
}

pub fn run(args: &ExpCmd) {
    let _ = aufbau::exp::run(aufbau::exp::ExpConfig {
        include_safe: args.safe,
        max_n: args.max_n,
        include_standard: args.standard,
        include_incremental: args.incremental,
        max_prefixes: args.max_prefixes,
        include_drivers: args.drivers,
        safe_max_steps: args.safe_max_steps,
        safe_only: args.safe_only.clone(),
        safe_max_step_ms: args.safe_max_step_ms,
        safe_max_rss_kb: args.safe_max_rss_mb.saturating_mul(1024),
        output: args.output.clone(),
    });
}
