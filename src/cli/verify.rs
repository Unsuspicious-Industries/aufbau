use clap::{Args, ValueEnum};
use std::ffi::OsString;
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::thread;
use std::time::{Duration, Instant};

#[derive(ValueEnum, Debug, Clone, Copy)]
pub enum VerifyLanguage {
    Stlc,
    Fun,
    Imp,
    Typescript,
}

impl VerifyLanguage {
    fn as_arg(self) -> &'static str {
        match self {
            Self::Stlc => "stlc",
            Self::Fun => "fun",
            Self::Imp => "imp",
            Self::Typescript => "typescript",
        }
    }
}

#[derive(Args, Debug, Clone)]
pub struct VerifyCmd {
    /// Language verifier to use for single-prefix or single-program mode.
    #[arg(value_enum)]
    pub language: Option<VerifyLanguage>,

    /// Treat the input as a complete program instead of a prefix.
    #[arg(long = "program", action = clap::ArgAction::SetTrue)]
    pub program: bool,

    /// Verify a prefix file instead of a single stdin/argument input.
    #[arg(short = 'f', long = "file", value_name = "FILE")]
    pub prefix_file: Option<PathBuf>,

    /// Number of completions per prefix-file entry.
    #[arg(long = "count", short = 'k', default_value_t = 3)]
    pub count: usize,

    /// Optional completion depth.
    #[arg(long = "depth", value_name = "N")]
    pub depth: Option<usize>,

    /// Worker count for prefix-file verification.
    #[arg(long = "jobs", short = 'j', value_name = "N")]
    pub jobs: Option<usize>,

    /// Show verbose orchestrator logs.
    #[arg(long = "orchestrator-verbose", action = clap::ArgAction::SetTrue)]
    pub orchestrator_verbose: bool,

    /// Enable traced Rust search logging during verification.
    #[arg(long = "trace-search", action = clap::ArgAction::SetTrue)]
    pub trace_search: bool,

    /// Disable orchestrator timeout (for very long runs).
    #[arg(long = "no-timeout", action = clap::ArgAction::SetTrue)]
    pub no_timeout: bool,

    /// Optional input text instead of stdin.
    #[arg(value_name = "INPUT")]
    pub input: Option<String>,
}

fn root_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

fn verification_dir() -> PathBuf {
    root_dir().join("verification")
}

fn ffi_lib_dir() -> PathBuf {
    root_dir().join("target").join("release")
}

fn prepend_library_path(cmd: &mut Command, dir: &Path) {
    let mut paths = vec![dir.to_path_buf()];
    if let Some(existing) = std::env::var_os("LD_LIBRARY_PATH") {
        paths.extend(std::env::split_paths(&existing));
    }
    if let Ok(joined) = std::env::join_paths(paths) {
        cmd.env("LD_LIBRARY_PATH", joined);
    } else {
        cmd.env("LD_LIBRARY_PATH", dir.as_os_str());
    }
}

fn orchestrator_path() -> PathBuf {
    verification_dir()
        .join("_build")
        .join("default")
        .join("orchestrator.exe")
}

fn stale_due_to(source: &Path, target: &Path) -> bool {
    let Ok(src_meta) = std::fs::metadata(source) else {
        return false;
    };
    let Ok(tgt_meta) = std::fs::metadata(target) else {
        return false;
    };
    let Ok(src_time) = src_meta.modified() else {
        return false;
    };
    let Ok(tgt_time) = tgt_meta.modified() else {
        return false;
    };
    src_time > tgt_time
}

fn ensure_orchestrator_ready() {
    let exe = orchestrator_path();
    if !exe.exists() {
        eprintln!(
            "error: verification orchestrator is not built: {}",
            exe.display()
        );
        eprintln!(
            "hint: build it in your build system (e.g. `dune build orchestrator.exe` in `verification/`)"
        );
        std::process::exit(2);
    }

    let lib = ffi_lib_dir().join("libaufbau.so");
    if !lib.exists() {
        eprintln!("error: OCaml FFI library is not built: {}", lib.display());
        eprintln!(
            "hint: build it in your build system (e.g. `cargo build --release --features ocaml-ffi`)"
        );
        std::process::exit(2);
    }

    let orchestrator_sources = [
        verification_dir().join("orchestrator.ml"),
        verification_dir().join("aufbau.ml"),
        verification_dir().join("aufbau.mli"),
        verification_dir().join("dune"),
    ];
    if orchestrator_sources
        .iter()
        .any(|src| stale_due_to(src, &exe))
    {
        eprintln!(
            "error: verification orchestrator artifact is stale: {}",
            exe.display()
        );
        eprintln!(
            "hint: rebuild in your build system (e.g. `AUFBAU_ROOT={} AUFBAU_VERIFICATION_DIR={} dune build orchestrator.exe` in `verification/`)",
            root_dir().display(),
            verification_dir().display()
        );
        std::process::exit(2);
    }
}

fn wait_with_timeout(child: &mut std::process::Child, no_timeout: bool) -> Result<i32, String> {
    if no_timeout {
        return child
            .wait()
            .map(|s| s.code().unwrap_or(1))
            .map_err(|e| format!("process failed: {}", e));
    }

    let timeout = Duration::from_secs(300);
    let start = Instant::now();
    loop {
        match child.try_wait() {
            Ok(Some(status)) => return Ok(status.code().unwrap_or(1)),
            Ok(None) => {
                if start.elapsed() >= timeout {
                    let _ = child.kill();
                    let _ = child.wait();
                    return Err("process timed out after 300s (use --no-timeout to disable)".into());
                }
                thread::sleep(Duration::from_millis(50));
            }
            Err(e) => return Err(format!("process wait failed: {}", e)),
        }
    }
}

fn read_input(cmd: &VerifyCmd) -> String {
    if let Some(input) = &cmd.input {
        return input.clone();
    }

    if atty::is(atty::Stream::Stdin) {
        return String::new();
    }

    let mut input = String::new();
    if let Err(e) = io::stdin().read_to_string(&mut input) {
        eprintln!("error: failed to read stdin: {}", e);
        std::process::exit(2);
    }
    input.trim_end_matches('\n').to_string()
}

fn run_orchestrator(args: &[OsString], no_timeout: bool) -> i32 {
    ensure_orchestrator_ready();

    let mut cmd = Command::new(orchestrator_path());
    cmd.args(args)
        .current_dir(verification_dir())
        .env("AUFBAU_ROOT", root_dir())
        .env("AUFBAU_VERIFICATION_DIR", verification_dir())
        .stdin(Stdio::inherit())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit());

    prepend_library_path(&mut cmd, &ffi_lib_dir());

    let mut child = match cmd.spawn() {
        Ok(c) => c,
        Err(e) => {
            eprintln!("error: {}", e);
            std::process::exit(2);
        }
    };

    match wait_with_timeout(&mut child, no_timeout) {
        Ok(code) => code,
        Err(e) => {
            eprintln!("error: {}", e);
            std::process::exit(2);
        }
    }
}

fn push_common_args(out: &mut Vec<OsString>, cmd: &VerifyCmd) {
    if let Some(depth) = cmd.depth {
        out.push("--depth".into());
        out.push(depth.to_string().into());
    }
    if let Some(jobs) = cmd.jobs {
        out.push("--jobs".into());
        out.push(jobs.to_string().into());
    }
    if cmd.orchestrator_verbose {
        out.push("--verbose".into());
    }
    if cmd.trace_search {
        out.push("--trace-search".into());
    }
}

fn ensure_language(language: Option<VerifyLanguage>) -> VerifyLanguage {
    match language {
        Some(language) => language,
        None => {
            eprintln!("error: language is required unless -f/--file is used");
            std::process::exit(2);
        }
    }
}

pub fn default_prefix_file(language: VerifyLanguage) -> PathBuf {
    verification_dir()
        .join("prefixes")
        .join(format!("{}.txt", language.as_arg()))
}

pub fn run_prefix_file(
    path: &Path,
    count: usize,
    depth: Option<usize>,
    jobs: Option<usize>,
    verbose: bool,
    trace_search: bool,
    no_timeout: bool,
) -> i32 {
    let mut args = vec![OsString::from("-f"), path.as_os_str().to_os_string()];
    args.push("--count".into());
    args.push(count.to_string().into());
    let cmd = VerifyCmd {
        language: None,
        program: false,
        prefix_file: Some(path.to_path_buf()),
        count,
        depth,
        jobs,
        orchestrator_verbose: verbose,
        trace_search,
        no_timeout,
        input: None,
    };
    push_common_args(&mut args, &cmd);
    run_orchestrator(&args, no_timeout)
}

pub fn execute(cmd: &VerifyCmd) -> i32 {
    if let Some(path) = &cmd.prefix_file {
        return run_prefix_file(
            path,
            cmd.count,
            cmd.depth,
            cmd.jobs,
            cmd.orchestrator_verbose,
            cmd.trace_search,
            cmd.no_timeout,
        );
    }

    eprintln!("Running with args {:?}", cmd);

    let language = ensure_language(cmd.language);

    let input = read_input(cmd);
    if input.is_empty() {
        if !cmd.program {
            let default_file = default_prefix_file(language);
            if default_file.exists() {
                if cmd.orchestrator_verbose {
                    eprintln!(
                        "No stdin/input provided; defaulting to prefix file: {}",
                        default_file.display()
                    );
                }
                return run_prefix_file(
                    &default_file,
                    cmd.count,
                    cmd.depth,
                    cmd.jobs,
                    cmd.orchestrator_verbose,
                    cmd.trace_search,
                    cmd.no_timeout,
                );
            }

            eprintln!("error: input is required (provide via argument or pipe to stdin)");
            eprintln!("usage: aufbau verify [LANGUAGE] [INPUT]");
            eprintln!("   or: echo 'INPUT' | aufbau verify [LANGUAGE]");
            eprintln!("   or: aufbau verify --file {}", default_file.display());
            std::process::exit(2);
        }

        eprintln!("error: input is required for --program mode");
        std::process::exit(2);
    }

    let mut args = Vec::new();
    if cmd.program {
        args.push(OsString::from("--program"));
    }
    args.push(OsString::from(language.as_arg()));
    push_common_args(&mut args, cmd);

    eprintln!("Verification input:\n{}\n", input);
    args.push(OsString::from(input));
    run_orchestrator(&args, cmd.no_timeout)
}

pub fn run(cmd: &VerifyCmd) {
    std::process::exit(execute(cmd));
}
