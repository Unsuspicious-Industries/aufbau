use clap::{Args, Subcommand};
use std::fs;
use std::path::PathBuf;

use beam::logic::{check::TypeChecker, grammar::Grammar, parser::Parser};
use beam::logic::debug::{DebugLevel, set_debug_level, add_module_filter, set_debug_input};
use anstyle::{AnsiColor, Style};

#[derive(Args, Debug, Clone)]
pub struct LogicCmd {
    #[command(subcommand)]
    pub command: LogicSubcommand,
}

#[derive(Subcommand, Debug, Clone)]
pub enum LogicSubcommand {
    /// Typecheck a source file given a grammar spec
    Check(CheckArgs),
}

#[derive(Args, Debug, Clone)]
pub struct CheckArgs {
    /// Path to grammar specification file
    #[arg(short = 's', long = "spec", value_name = "FILE")] 
    pub spec_path: PathBuf,

    /// Path to source code file to typecheck
    #[arg(value_name = "CODE_FILE")] 
    pub code_path: PathBuf,

    /// Explicit start symbol override
    #[arg(long = "start")] 
    pub start: Option<String>,
}

pub fn dispatch(cli: &crate::cli::Cli) {
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

    match &cli.command {
        crate::cli::Commands::Logic(cmd) => match &cmd.command {
            LogicSubcommand::Check(args) => run_check(args, cli.with_input, level),
        },
    }
}

fn run_check(args: &CheckArgs, with_input: bool, debug_level: DebugLevel) {
    // Load grammar spec
    let spec = match fs::read_to_string(&args.spec_path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("error: failed to read spec '{}': {}", args.spec_path.display(), e);
            std::process::exit(2);
        }
    };
    let mut grammar = match Grammar::load(&spec) {
        Ok(g) => g,
        Err(e) => {
            eprintln!("error: failed to parse grammar spec: {}", e);
            std::process::exit(2);
        }
    };
    if let Some(start) = &args.start {
        grammar.set_start(start.clone());
    }

    // Load code
    let code = match fs::read_to_string(&args.code_path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("error: failed to read code '{}': {}", args.code_path.display(), e);
            std::process::exit(2);
        }
    };
    if with_input { set_debug_input(Some(code.clone())); }

    // Parse
    let mut parser = Parser::new(grammar);
    let ast = match parser.parse(&code) {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!("parse error: {}", e);
            std::process::exit(1);
        }
    };

    // if debug level is trace, print the AST
    if matches!(debug_level, DebugLevel::Trace) {
        println!("AST: {}", ast.show_simple());
    }
    

    // Typecheck
    let mut checker = TypeChecker::new();

    checker.debug_at_span(&ast, "typechecking...");

    match checker.check(&ast) {
        Ok(Some(ty)) => {
            let ok = Style::new().fg_color(Some(AnsiColor::Green.into()));
            println!("{ok}Type:{ok:#} {:?}", ty);
            std::process::exit(0);
        }
        Ok(None) => {
            let warn = Style::new().fg_color(Some(AnsiColor::Yellow.into()));
            println!("{warn}No type inferred{warn:#} (terminal-only or missing typing rule)");
            std::process::exit(0);
        }
        Err(e) => {
            let err = Style::new().fg_color(Some(AnsiColor::Red.into()));
            eprintln!("{err}Type error:{err:#} {}", e);
            std::process::exit(1);
        }
    }
}


