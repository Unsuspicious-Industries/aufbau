//! `verify` — complete a prefix, then validate it with the Coq verifier.
//!
//! Reads a partial program from stdin and dispatches to `verification/check.sh`,
//! which in turn re-checks the Coq artifacts, runs `aufbau complete`, and
//! compares the result against the verified checker for the chosen language.

use clap::{Args, ValueEnum};
use std::io::{self, Read};
use std::path::PathBuf;
use std::process::{Command, Stdio};

#[derive(ValueEnum, Debug, Clone, Copy)]
pub enum VerifyLanguage {
    Stlc,
    Fun,
    Imp,
}

impl VerifyLanguage {
    fn as_script_arg(self) -> &'static str {
        match self {
            Self::Stlc => "stlc",
            Self::Fun => "fun",
            Self::Imp => "imp",
        }
    }
}

#[derive(Args, Debug, Clone)]
pub struct VerifyCmd {
    /// Language verifier to use
    #[arg(value_enum)]
    pub language: VerifyLanguage,

    /// Override the verification script path
    #[arg(long = "script", value_name = "FILE")]
    pub script: Option<PathBuf>,
}

pub fn run(args: &VerifyCmd) {
    let mut prefix = String::new();
    if let Err(e) = io::stdin().read_to_string(&mut prefix) {
        eprintln!("error: failed to read stdin: {}", e);
        std::process::exit(2);
    }
    let prefix = prefix.trim_end_matches('\n').to_string();

    let default_script = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("verification")
        .join("check.sh");
    let script = args.script.clone().unwrap_or(default_script);

    if !script.exists() {
        eprintln!(
            "error: verification script not found at '{}'",
            script.display()
        );
        std::process::exit(2);
    }

    let status = match Command::new("bash")
        .arg(&script)
        .arg(args.language.as_script_arg())
        .arg(&prefix)
        .stdin(Stdio::null())
        .status()
    {
        Ok(status) => status,
        Err(e) => {
            eprintln!(
                "error: failed to run verification script '{}': {}",
                script.display(),
                e
            );
            std::process::exit(2);
        }
    };

    match status.code() {
        Some(code) => std::process::exit(code),
        None => {
            eprintln!("error: verification process terminated by signal");
            std::process::exit(1);
        }
    }
}
