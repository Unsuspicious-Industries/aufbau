use clap::Args;
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::thread;
use std::time::{Duration, Instant};

#[derive(Args, Debug, Clone)]
pub struct CheckCmd {
    /// Number of worker threads for Rust validation.
    #[arg(long = "jobs", short = 'j', value_name = "N")]
    pub jobs: Option<usize>,

    /// Override per-case completable timeout in seconds.
    #[arg(long = "completable-timeout-secs", value_name = "N")]
    pub completable_timeout_secs: Option<u64>,

    /// Timeout for the parseable validation command.
    #[arg(
        long = "parseable-timeout-secs",
        value_name = "N",
        default_value_t = 3600
    )]
    pub parseable_timeout_secs: u64,

    /// Timeout for each completable validation command.
    #[arg(
        long = "completable-command-timeout-secs",
        value_name = "N",
        default_value_t = 5400
    )]
    pub completable_command_timeout_secs: u64,

    /// Skip parseable validation.
    #[arg(long = "skip-parseable", action = clap::ArgAction::SetTrue)]
    pub skip_parseable: bool,

    /// Skip completable validation.
    #[arg(long = "skip-completable", action = clap::ArgAction::SetTrue)]
    pub skip_completable: bool,

    /// Optional parseable suite filter.
    #[arg(long = "parseable-filter", value_name = "SUBSTR")]
    pub parseable_filter: Option<String>,

    /// Completable suite filters. Defaults to arithmetic, stlc, imp, fun, weird.
    #[arg(long = "completable-filter", value_name = "SUBSTR")]
    pub completable_filters: Vec<String>,
}

fn current_exe() -> PathBuf {
    std::env::current_exe().unwrap_or_else(|e| {
        eprintln!("error: failed to locate current executable: {}", e);
        std::process::exit(2);
    })
}

fn run_step(label: &str, timeout_secs: u64, args: &[String]) {
    eprintln!("==> {}", label);
    let mut child = Command::new(current_exe())
        .args(args)
        .stdin(Stdio::inherit())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .spawn()
        .unwrap_or_else(|e| {
            eprintln!("error: failed to spawn '{}': {}", label, e);
            std::process::exit(2);
        });

    let deadline = Instant::now() + Duration::from_secs(timeout_secs.max(1));
    loop {
        match child.try_wait() {
            Ok(Some(status)) => {
                if !status.success() {
                    std::process::exit(status.code().unwrap_or(1));
                }
                return;
            }
            Ok(None) => {
                if Instant::now() >= deadline {
                    let _ = child.kill();
                    let _ = child.wait();
                    eprintln!(
                        "error: '{}' timed out after {}s",
                        label,
                        timeout_secs.max(1)
                    );
                    std::process::exit(1);
                }
                thread::sleep(Duration::from_millis(200));
            }
            Err(e) => {
                eprintln!("error: failed while waiting for '{}': {}", label, e);
                std::process::exit(2);
            }
        }
    }
}

fn push_jobs(args: &mut Vec<String>, jobs: Option<usize>) {
    if let Some(jobs) = jobs {
        args.push("--jobs".to_string());
        args.push(jobs.to_string());
    }
}

pub fn run(args: &CheckCmd) {
    if !args.skip_parseable {
        let mut step_args = vec![
            "validate".to_string(),
            "-m".to_string(),
            "parseable".to_string(),
        ];
        if let Some(filter) = &args.parseable_filter {
            step_args.push("-f".to_string());
            step_args.push(filter.clone());
        }
        push_jobs(&mut step_args, args.jobs);
        run_step(
            "parseable validation",
            args.parseable_timeout_secs,
            &step_args,
        );
    }

    if !args.skip_completable {
        let filters = if args.completable_filters.is_empty() {
            vec![
                "arithmetic::".to_string(),
                "stlc::".to_string(),
                "imp::".to_string(),
                "fun::".to_string(),
                "weird::".to_string(),
            ]
        } else {
            args.completable_filters.clone()
        };

        for filter in filters {
            let mut step_args = vec![
                "validate".to_string(),
                "-m".to_string(),
                "completable".to_string(),
                "-f".to_string(),
                filter.clone(),
            ];
            push_jobs(&mut step_args, args.jobs);
            if let Some(timeout_secs) = args.completable_timeout_secs {
                step_args.push("--completable-timeout-secs".to_string());
                step_args.push(timeout_secs.to_string());
            }
            run_step(
                &format!("completable validation ({})", filter),
                args.completable_command_timeout_secs,
                &step_args,
            );
        }
    }
}
