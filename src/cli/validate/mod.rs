use clap::Args;

pub mod completable;
pub mod parseable;
pub mod complexity;

#[derive(clap::ValueEnum, Clone, Debug)]
pub enum ValidationModule {
    /// Run completable validation tests (BFS completion search)
    Completable,
    /// Run parseable validation tests (fast prefix parsing)
    Parseable,
    /// Run complexity validation tests
    Complexity,
}

#[derive(Args, Debug, Clone)]
pub struct ValidateCmd {
    /// Validation module to run. If omitted, run all modules.
    #[arg(short = 'm', long = "module")]
    pub module: Option<ValidationModule>,

    /// Filter suites by name substring (e.g. "stlc", "fun::lambda")
    #[arg(short = 'f', long = "filter")]
    pub filter: Option<String>,

    /// Optional profile output file (Chrome trace format). If provided, the
    /// validation run will emit a trace JSON that can be opened with
    /// Chrome tracing or Firefox profiler ("Load profile" -> "Import").
    #[arg(long = "profile", value_name = "FILE")]
    pub profile: Option<std::path::PathBuf>,

    /// Optional number of worker threads to use for parallel test execution.
    /// If omitted, Rayon will use its default thread pool size.
    #[arg(long = "jobs", short = 'j', value_name = "N")]
    pub jobs: Option<usize>,
}


pub fn run(args: &ValidateCmd) {
    match &args.module {
        Some(ValidationModule::Completable) => completable::run(args),
        Some(ValidationModule::Parseable) => parseable::run(args),
        Some(ValidationModule::Complexity) => complexity::run(args),
        None => {
            // Run all modules sequentially
            completable::run(args);
            parseable::run(args);
            complexity::run(args);
        }
    }
}
