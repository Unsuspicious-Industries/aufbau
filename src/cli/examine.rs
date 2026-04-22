use clap::Args;
use std::fs;
use std::path::PathBuf;

use aufbau::logic::debug::set_debug_input;
use aufbau::logic::synth::Synthesizer;
use aufbau::logic::grammar::Grammar;
use aufbau::logic::typing::Context;
use aufbau::validation::completable::{self, TypedCompletionTestCase};

/// Quick helper to examine feed acceptance for an input or a named test case
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
    /// In feed-check mode, only `ok` is supported.
    #[arg(long = "expected", value_enum)]
    pub expected: Option<ExpectedOutcome>,

    /// Require prefix-soundness check for raw input
    #[arg(long = "sound", action = clap::ArgAction::SetTrue)]
    pub sound: bool,

    /// Maximum search depth (default: 10). When used with --case this overrides
    /// the case's configured depth.
    #[arg(long = "depth", default_value_t = 10)]
    pub depth: usize,

    /// Print full ASTs / debug structures (off by default)
    #[arg(long = "dump-ast", action = clap::ArgAction::SetTrue)]
    pub dump_ast: bool,

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
        let (suite_name, grammar, case) = matches.remove(0);

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
            eprintln!("Overrode case expected={:?}", exp);
        }

        eprintln!(
            "Running case from suite '{}' - {}\n",
            suite_name, case.description
        );

        let case_input = case.input;

        // === Parser / Partial AST ===
        let mut synth = Synthesizer::new(grammar.clone(), case_input);
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
        if let Some(pc) = meta.prefixes_checked {
            println!("prefixes_checked = {}", pc);
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
        let mut grammar = match Grammar::load(&spec) {
            Ok(g) => g,
            Err(e) => {
                eprintln!("error: failed to parse grammar spec: {}", e);
                std::process::exit(2);
            }
        };

        if args.sound {
            let (res, dur) = completable::timed_sound_complete(&mut grammar, input_str, None);
            println!("sound_complete: time={} ms", dur.as_millis());
            println!("  is_sound = {}", res.is_sound);
            if let Some(fp) = res.failing_prefix {
                println!("  failing_prefix = '{}'", fp);
            }
            println!("  prefixes_checked = {}", res.prefixes_checked);
            println!("  accepted_input = '{}'", res.accepted_input);
            if let Some(failure) = res.failure {
                println!("  failure = '{}'", failure);
            }

            std::process::exit(if res.is_sound { 0 } else { 1 });
        } else {
            let (res, dur) = completable::timed_sound_complete(&mut grammar, input_str, None);
            println!("feed_replay: time={} ms", dur.as_millis());
            println!("  accepted = {}", res.is_sound);
            if let Some(prefix) = res.failing_prefix {
                println!("  failing_prefix = '{}'", prefix);
            }
            println!("  accepted_input = '{}'", res.accepted_input);
            if let Some(failure) = res.failure {
                println!("  failure = '{}'", failure);
            }
            std::process::exit(if res.is_sound { 0 } else { 1 });
        }
    }

    eprintln!("error: either --case or --input must be provided");
    std::process::exit(2);
}
