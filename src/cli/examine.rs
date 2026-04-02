use clap::Args;
use std::fs;
use std::path::PathBuf;

use aufbau::logic::debug::set_debug_input;
use aufbau::logic::fusion::Synthesizer;
use aufbau::logic::grammar::Grammar;
use aufbau::logic::typing::Context;
use aufbau::validation::completable::{self, TypedCompletionTestCase};

/// Quick helper to examine completability for an input or a named test case
#[derive(clap::ValueEnum, Clone, Debug)]
pub enum ExpectedOutcome {
    Ok,
    Fail,
    TypeError,
}

#[derive(Args, Debug, Clone)]
pub struct ExamineCmd {
    /// Grammar spec file (required when using --input)
    #[arg(short = 's', long = "spec", value_name = "FILE")]
    pub spec: Option<PathBuf>,

    /// Raw partial input to test (use with --spec)
    #[arg(short = 'i', long = "input", value_name = "TEXT")]
    pub input: Option<String>,

    /// Substring to match a validation `TypedCompletionTestCase` description
    #[arg(short = 'c', long = "case", value_name = "DESC")]
    pub case: Option<String>,

    /// Filter suites by name substring (e.g. "stlc", "fun", "imp")
    #[arg(short = 'f', long = "filter")]
    pub filter: Option<String>,

    /// Expected outcome for the checked input/case.
    /// In completable mode, only `ok` is supported.
    #[arg(long = "expected", value_enum)]
    pub expected: Option<ExpectedOutcome>,

    /// Require prefix-soundness check (default: use full completion only for --input)
    #[arg(long = "sound", action = clap::ArgAction::SetTrue)]
    pub sound: bool,

    /// Maximum search depth (default: 10). When used with --case this overrides
    /// the case's configured depth.
    #[arg(long = "depth", default_value_t = 10)]
    pub depth: usize,

    /// Print full ASTs / debug structures (off by default)
    #[arg(long = "dump-ast", action = clap::ArgAction::SetTrue)]
    pub dump_ast: bool,

    /// Print completion sets and example tokens
    #[arg(long = "dump-completions", action = clap::ArgAction::SetTrue)]
    pub dump_completions: bool,
}

fn dump_completions(grammar: &Grammar, input: &str, ctx: &Context) {
    let mut synth = Synthesizer::new(grammar.clone(), input);
    let typed = synth.tokens_with(ctx);
    println!("\n-- completions --");
    for (i, token) in typed.iter().enumerate() {
        println!(
            "  [{}] token='{}' example={:?}",
            i,
            token.to_pattern(),
            token.example()
        );
    }
    if typed.is_empty() {
        println!("  (no completions)");
    }
}

fn collect_suites() -> Vec<(&'static str, Grammar, Vec<TypedCompletionTestCase>)> {
    let mut out = Vec::new();
    out.extend(completable::arithmetic::suites());
    out.extend(completable::stlc::suites());
    out.extend(completable::toy::suites());
    out.extend(completable::fun::suites());
    out.extend(completable::imp::suites());
    out.extend(completable::weird::suites());
    out
}

pub fn run(args: &ExamineCmd) {
    // Mode 1: run a named test case from the built-in suites
    if let Some(desc) = &args.case {
        let suites = match &args.filter {
            Some(f) => {
                let filtered: Vec<_> = collect_suites()
                    .into_iter()
                    .filter(|(name, _, _)| name.contains(f.as_str()))
                    .collect();
                eprintln!("  filter: {}", f);
                filtered
            }
            None => collect_suites(),
        };

        let mut matches: Vec<(String, Grammar, TypedCompletionTestCase)> = Vec::new();
        for (suite_name, grammar, cases) in suites.into_iter() {
            for case in cases.into_iter() {
                if case.description.contains(desc) {
                    matches.push((suite_name.to_string(), grammar.clone(), case));
                }
            }
        }

        if matches.is_empty() {
            eprintln!(
                "no matching test cases found for '{}'. Try a shorter/alternate substring",
                desc
            );
            std::process::exit(2);
        }

        // Pick the first match (convenience) and run it with full test harness
        let (suite_name, grammar, mut case) = matches.remove(0);

        set_debug_input(Some(case.input.to_string()));

        // If user provided --expected or --depth, overwrite the case configuration
        if let Some(exp) = &args.expected {
            match exp {
                ExpectedOutcome::Ok => {}
                ExpectedOutcome::Fail | ExpectedOutcome::TypeError => {
                    eprintln!(
                        "warning: completable no longer supports expected fail/type_error; use parseable validation for negative cases"
                    );
                }
            }
            // Always override depth when explicitly provided on the CLI
            case.completion_budget = args.depth;
            eprintln!("Overrode case expected={:?} depth={}", exp, args.depth);
        } else {
            // If no explicit expected was given still allow depth override
            case.completion_budget = args.depth;
        }

        eprintln!(
            "Running case from suite '{}' - {}\n",
            suite_name, case.description
        );

        let case_input = case.input;

        // === Parser / Partial AST ===
        let mut synth = Synthesizer::new_with_max_depth(grammar.clone(), case_input, 64);
        match synth.parse_with(&Context::new()) {
            Ok(partial_ast) => {
                let root_count = partial_ast.len();
                eprintln!("-- parsed FusionAST ({} root(s)) --", root_count);
                if args.dump_ast {
                    eprintln!("{:#?}", partial_ast);
                } else {
                    eprintln!("  (FusionAST suppressed; use --dump-ast to print full FusionAST)");
                }

                // Typed filter / typed attempt
                let mut ctx = Context::new();
                for (var, ty_str) in &case.context {
                    if let Ok(ty) = aufbau::logic::typing::Type::parse(ty_str) {
                        ctx.add(var.to_string(), ty);
                    }
                }
                match synth.parse_with(&ctx) {
                    Ok(typed_ast) => {
                        eprintln!(
                            "FusionAST typed successfully - typed AST has {} root(s)",
                            typed_ast.len()
                        );
                        if args.dump_ast {
                            eprintln!("{:#?}", typed_ast);
                        } else {
                            eprintln!(
                                "  (typed AST suppressed; use --dump-ast to print full typed AST)"
                            );
                        }
                    }
                    Err(e) => {
                        eprintln!("FusionAST typed failed: {}", e);
                    }
                }

                if args.dump_completions {
                    dump_completions(&grammar, case_input, &ctx);
                }
            }
            Err(e) => {
                eprintln!("parser.partial() error: {}", e);
            }
        }

        // Run the full test harness (prefix soundness / completion) and print rich metadata
        let (result, duration, meta) =
            aufbau::validation::completable::run_test_timed_meta(&grammar, &case);

        eprintln!("-- test result (duration={} ms) --", duration.as_millis());
        match &result {
            aufbau::validation::completable::TestResult::Pass(_) => {
                println!("PASS  ({} ms)", duration.as_millis());
                // NOTE: completed string will be printed later (after serialization)
            }
            aufbau::validation::completable::TestResult::Fail(msg) => {
                println!("FAIL  ({} ms)", duration.as_millis());
                for line in msg.lines() {
                    println!("  {}", line);
                }
            }
        }

        // Extended metadata
        println!("\n=== Detailed metadata ===");
        println!("case.input = '{}'", case.input);
        println!("case.description = '{}'", case.description);
        println!("case.completion_budget = {}", case.completion_budget);

        if let Some(se) = meta.states_explored {
            println!("states_explored = {}", se);
        }
        if let Some(pc) = meta.prefixes_checked {
            println!("prefixes_checked = {}", pc);
        }
        if let Some(tus) = meta.total_prefix_time_us {
            println!("prefix_total_time_us = {}", tus);
        }

        if let Some(prefix_meta) = meta.prefix_meta {
            println!("\nPer-prefix metadata (full):");
            for (i, pd) in prefix_meta.iter().enumerate() {
                println!("--- prefix[{}] = '{}' ---", i, pd.prefix);
                println!("  ok = {}", pd.ok);
                println!("  time_us = {}", pd.time_us);
                println!("  states_explored = {:?}", pd.states_explored);
                println!("  visited_count = {:?}", pd.visited_count);
                if !pd.visited_sample.is_empty() {
                    println!("  visited_sample ({}):", pd.visited_sample.len());
                    for (j, s) in pd.visited_sample.iter().enumerate() {
                        println!("    [{}] {}", j, s);
                    }
                }
            }
        }

        // For convenience, also print failing prefix visited states if present in the TestResult message
        if let aufbau::validation::completable::TestResult::Fail(msg) = &result {
            for line in msg.lines() {
                if line.starts_with("failing_visited_")
                    || line.starts_with("failing_prefix_visited_states=")
                {
                    println!("DETAIL: {}", line);
                }
            }
        }

        // If we have a completed string, print it
        if let aufbau::validation::completable::TestResult::Pass(opt_comp) = &result
            && let Some(comp_str) = opt_comp.clone()
        {
            println!("\nFULL COMPLETED OUTPUT:\n{}", comp_str);
        }

        std::process::exit(if result.is_pass() { 0 } else { 1 });
    }

    // Mode 2: run ad-hoc input against a provided grammar spec
    if let Some(input) = &args.input {
        let input_str = input.as_str();
        if args.dump_completions {
            set_debug_input(Some(input_str.to_string()));
        }
        let spec_path = match &args.spec {
            Some(p) => p.clone(),
            None => {
                eprintln!("error: --spec is required when using --input");
                std::process::exit(2);
            }
        };

        let spec = match fs::read_to_string(&spec_path) {
            Ok(s) => s,
            Err(e) => {
                eprintln!(
                    "error: failed to read spec '{}': {}",
                    spec_path.display(),
                    e
                );
                std::process::exit(2);
            }
        };
        let grammar = match Grammar::load(&spec) {
            Ok(g) => g,
            Err(e) => {
                eprintln!("error: failed to parse grammar spec: {}", e);
                std::process::exit(2);
            }
        };

        if args.dump_completions {
            let ctx = Context::new();
            dump_completions(&grammar, input_str, &ctx);
        }

        if args.sound {
            let (res, dur) =
                completable::timed_sound_complete(&grammar, input_str, args.depth, None);
            println!("sound_complete: time={} ms", dur.as_millis());
            println!("  is_sound = {}", res.is_sound);
            if let Some(fp) = res.failing_prefix {
                println!("  failing_prefix = '{}'", fp);
            }
            if let Some(sv) = res.failing_prefix_visited_states {
                println!("  failing_prefix_visited_states = {:?}", sv);
            }
            println!("  prefixes_checked = {}", res.prefixes_checked);
            if let Some(comp) = res.complete_string {
                println!("  completed_to = '{}'", comp);
            }
            if !res.prefix_meta.is_empty() {
                println!("\nPer-prefix metadata:");
                for (i, pd) in res.prefix_meta.iter().enumerate() {
                    println!(
                        "  [{}] prefix='{}' ok={} time_us={} states_explored={:?} visited_count={:?} ",
                        i, pd.prefix, pd.ok, pd.time_us, pd.states_explored, pd.visited_count
                    );
                }
            }

            std::process::exit(if res.is_sound { 0 } else { 1 });
        } else {
            let res = completable::timed_complete(&grammar, input_str, args.depth, None);
            println!("complete: time={} ms", res.1.as_millis());
            match &res.0 {
                aufbau::validation::completability::CompletionResult::Success {
                    complete_input,
                    completion_depth: depth,
                    ..
                } => {
                    println!(
                        "  Success: completed_to='{}' depth={}",
                        complete_input, depth
                    );
                    std::process::exit(0);
                }
                aufbau::validation::completability::CompletionResult::Failure {
                    max_depth_reached,
                    states_explored,
                    visited_states,
                } => {
                    println!(
                        "  Failure: max_depth_reached={} states_explored={}",
                        max_depth_reached, states_explored
                    );
                    if !visited_states.is_empty() {
                        println!("  visited_states sample: {:?}", visited_states);
                    }
                    std::process::exit(1);
                }
                aufbau::validation::completability::CompletionResult::Invalid(msg)
                | aufbau::validation::completability::CompletionResult::Inconsistency(msg)
                | aufbau::validation::completability::CompletionResult::Error(msg) => {
                    println!("  Error: {}", msg);
                    std::process::exit(2);
                }
            }
        }
    }

    eprintln!("error: either --case or --input must be provided");
    std::process::exit(2);
}
