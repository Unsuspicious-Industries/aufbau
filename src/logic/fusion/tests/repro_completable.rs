use crate::logic::fusion::Synthesizer;
use crate::logic::fusion::{RuleRuntime, TypedParser};
use crate::logic::grammar::Grammar;
use crate::logic::typing::Context;
use crate::validation::completability::complete;
use crate::validation::completability::sound_complete;
use crate::validation::completable;

#[test]
fn debug_lambda_with_var_completions() {
    let spec = std::fs::read_to_string("examples/stlc.auf").unwrap();
    let grammar = Grammar::load(&spec).unwrap();

    let ctx = Context::new();
    let input = "λx";
    let max_depth = 10;

    // Test lambda with var prefix
    let mut synth = Synthesizer::new_with_max_depth(grammar.clone(), input, max_depth);
    let typed = synth.parse_with(&ctx);
    eprintln!("λx prefix - partial_typed_ctx: {:?}", typed.is_ok());
    if let Ok(ast) = &typed {
        eprintln!("λx prefix - typed.is_empty(): {:?}", ast.is_empty());
    }

    let tokens = synth.tokens_with(&ctx);
    eprintln!("λx prefix - completions: {:?}", tokens.tokens.len());

    // Check completion_tokens_are_sound
    for token in tokens.iter() {
        if let Some(example) = token.example() {
            eprintln!(
                "  trying token '{}' example '{}'",
                token.to_pattern(),
                example
            );
            let mut synth2 = Synthesizer::new_with_max_depth(grammar.clone(), input, max_depth);
            let result = synth2.feed(&example, &ctx);
            eprintln!("    result: {:?}", result.is_ok());
            if let Err(e) = &result {
                eprintln!("    error: {}", e);
            }
        }
    }
}

#[test]
fn debug_lambda_only_completions() {
    let spec = std::fs::read_to_string("examples/stlc.auf").unwrap();
    let grammar = Grammar::load(&spec).unwrap();

    let ctx = Context::new();
    let input = "λ";
    let max_depth = 10;

    let mut synth = Synthesizer::new_with_max_depth(grammar.clone(), input, max_depth);
    let typed = synth.parse_with(&ctx);
    eprintln!("λ prefix - partial_typed_ctx: {:?}", typed.is_ok());
    if let Ok(ast) = &typed {
        eprintln!("λ prefix - typed.is_empty(): {:?}", ast.is_empty());
    }

    let tokens = synth.tokens_with(&ctx);
    eprintln!("λ prefix - completions: {:?}", tokens.tokens.len());
    for token in tokens.iter() {
        eprintln!(
            "  token '{}' example {:?} candidates {:?}",
            token.to_pattern(),
            token.example(),
            synth.regex_gather_candidates(token, &ctx)
        );
        for candidate in synth.regex_gather_candidates(token, &ctx) {
            let mut synth2 = Synthesizer::new_with_max_depth(grammar.clone(), input, max_depth);
            let result = synth2.feed(&candidate, &ctx);
            eprintln!("    candidate {:?} ok={}", candidate, result.is_ok());
        }
    }
}

#[test]
fn debug_triple_nested_lambda_prefixes() {
    let spec = std::fs::read_to_string("examples/stlc.auf").unwrap();
    let grammar = Grammar::load(&spec).unwrap();
    let ctx = Context::new();

    for input in [
        "λx:A.λy:B.λ",
        "λx:A.λy:B.λa",
        "λx:A.λy:B.λa:",
        "λx:A.λy:B.λz",
        "λx:A.λy:B.λz:",
    ] {
        let mut synth = Synthesizer::new_with_max_depth(grammar.clone(), input, 10);
        let typed = synth.parse_with(&ctx);
        eprintln!("input={input:?} typed_ok={}", typed.is_ok());
        if let Ok(ast) = &typed {
            eprintln!("  complete={} empty={}", ast.is_complete(), ast.is_empty());
        }
        let tokens = synth.tokens_with(&ctx);
        let examples: Vec<_> = tokens
            .iter()
            .map(|t| (t.to_pattern(), t.example()))
            .collect();
        eprintln!("  tokens={examples:?}");
        if input == "λx:A.λy:B.λa" {
            for token in tokens.iter() {
                let mut synth2 = Synthesizer::new_with_max_depth(grammar.clone(), input, 10);
                let _ = synth2.tokens_with(&ctx);
                let result = synth2.extend_with_regex(token, &ctx);
                eprintln!(
                    "  extend token={} -> {:?}",
                    token.to_pattern(),
                    result.as_ref().map(|(_, text)| text.as_str())
                );
            }
        }
    }
}

#[test]
fn debug_incremental_triple_nested_lambda_chain() {
    let spec = std::fs::read_to_string("examples/stlc.auf").unwrap();
    let grammar = Grammar::load(&spec).unwrap();
    let ctx = Context::new();

    let mut synth = Synthesizer::new_with_max_depth(grammar, "λx:A.λy:B.λ", 10);
    let _ = synth.tokens_with(&ctx);
    eprintln!("extend a -> {:?}", synth.feed("a", &ctx).is_ok());
    let _ = synth.tokens_with(&ctx);
    eprintln!("extend : -> {:?}", synth.feed(":", &ctx).is_ok());
}

#[test]
fn debug_min_depth_nested_lambda_prefix() {
    crate::set_debug_level(crate::logic::debug::DebugLevel::Trace);
    crate::add_module_filter("fusion_parser");
    let spec = std::fs::read_to_string("examples/stlc.auf").unwrap();
    let grammar = Grammar::load(&spec).unwrap();
    let ctx = Context::new();

    for depth in [10, 11] {
        let runtime = RuleRuntime::new(grammar.clone());
        let ctx_id = runtime.intern_context(ctx.clone());
        let mut parser = TypedParser::new(grammar.clone(), runtime).with_max_depth(depth);
        let result = parser.parse("λx:A.λy:B.λa:", ctx_id);
        eprintln!("depth={depth} ok={}", result.is_ok());
        if let Err(err) = result {
            eprintln!("  err={err:?}");
        }
    }
}

#[test]
fn debug_fun_let_completion_frontier() {
    let spec = std::fs::read_to_string("examples/fun.auf").unwrap();
    let grammar = Grammar::load(&spec).unwrap();
    let ctx = Context::new();
    let mut synth = Synthesizer::new_with_max_depth(grammar.clone(), "let", 7);
    let typed = synth.parse_with(&ctx).unwrap();
    eprintln!(
        "complete={} open_slots={} fullness={} completeness={} roots={}",
        typed.is_complete(),
        typed.min_open_slots(&grammar),
        typed.production_fullness_score(&grammar),
        typed.completeness_score(),
        typed.root_ids().len()
    );
    for root in typed.roots() {
        let node = typed.arena().node(root.node_id()).unwrap();
        let nt_name = grammar.nt_name(node.nt.0).unwrap_or("<?>");
        eprintln!(
            "root nt={} text={:?} complete={} children={} rhs_len={}",
            nt_name,
            root.text(),
            root.is_complete(),
            root.child_count(),
            root.rhs_len(&grammar)
        );
    }
    let tokens = synth.tokens_with(&ctx);
    eprintln!("token_count={}", tokens.len());
    for token in tokens.iter() {
        let candidates = synth.regex_gather_candidates(token, &ctx);
        eprintln!(
            "token={} example={:?} candidates={:?}",
            token.to_pattern(),
            token.example(),
            candidates
        );
        for candidate in candidates {
            if let Ok(next_typed) = synth.feed(&candidate, &ctx) {
                let next_input = synth.input().to_string();
                eprintln!(
                    "  -> {:?} complete={} open_slots={} fullness={} completeness={}",
                    next_input,
                    next_typed.is_complete(),
                    next_typed.min_open_slots(&grammar),
                    next_typed.production_fullness_score(&grammar),
                    next_typed.completeness_score()
                );
            }
        }
    }
}

#[test]
fn debug_fun_let_path_frontiers() {
    let spec = std::fs::read_to_string("examples/fun.auf").unwrap();
    let grammar = Grammar::load(&spec).unwrap();
    let ctx = Context::new();

    for input in [
        "let x",
        "let x:",
        "let x:Bool",
        "let x:Bool=true",
        "let x:Bool=true;",
    ] {
        let mut synth = Synthesizer::new_with_max_depth(grammar.clone(), input, 7);
        let typed = synth.parse_with(&ctx).unwrap();
        eprintln!(
            "INPUT {input:?} complete={} completeness={}",
            typed.is_complete(),
            typed.completeness_score()
        );
        let tokens = synth.tokens_with(&ctx);
        for token in tokens.iter() {
            let candidates = synth.regex_gather_candidates(token, &ctx);
            eprintln!("  token={} candidates={:?}", token.to_pattern(), candidates);
            for candidate in candidates {
                if let Ok(next_typed) = synth.feed(&candidate, &ctx) {
                    let next_input = synth.input().to_string();
                    eprintln!(
                        "    -> {:?} complete={} completeness={} fullness={}",
                        next_input,
                        next_typed.is_complete(),
                        next_typed.completeness_score(),
                        next_typed.production_fullness_score(&grammar)
                    );
                }
            }
        }
    }
}

#[test]
fn debug_fun_let_complete() {
    let spec = std::fs::read_to_string("examples/fun.auf").unwrap();
    let grammar = Grammar::load(&spec).unwrap();
    let ctx = Context::new();

    let start = std::time::Instant::now();
    let result = crate::logic::search::complete(&grammar, "let", 7, Some(ctx));
    eprintln!("completed in {}ms", start.elapsed().as_millis());
    eprintln!("result={result:?}");
}

#[test]
fn debug_fun_let_greedy_path() {
    let spec = std::fs::read_to_string("examples/fun.auf").unwrap();
    let grammar = Grammar::load(&spec).unwrap();
    let ctx = Context::new();
    let mut current = "let".to_string();

    for step in 0..7 {
        let mut synth = Synthesizer::new_with_max_depth(grammar.clone(), &current, 7);
        let typed = synth.parse_with(&ctx).unwrap();
        eprintln!(
            "step={step} current={current:?} complete={}",
            typed.is_complete()
        );
        if typed.is_complete() {
            break;
        }

        let tokens = synth.tokens_with(&ctx);
        let mut best: Option<(String, f64, bool)> = None;
        for token in tokens.iter() {
            for candidate in synth
                .regex_gather_candidates(token, &ctx)
                .into_iter()
                .take(4)
            {
                if let Ok(next_typed) = synth.feed(&candidate, &ctx) {
                    let next_input = synth.input().to_string();
                    let score = next_typed.completeness_score();
                    let complete = next_typed.is_complete();
                    eprintln!(
                        "  cand token={} candidate={:?} next={:?} complete={} score={}",
                        token.to_pattern(),
                        candidate,
                        next_input,
                        complete,
                        score
                    );
                    let replace = match &best {
                        None => true,
                        Some((_, best_score, best_complete)) => {
                            (complete && !best_complete)
                                || (complete == *best_complete && score > *best_score)
                        }
                    };
                    if replace {
                        best = Some((next_input, score, complete));
                    }
                }
            }
        }

        let Some((next, _, _)) = best else {
            break;
        };
        current = next;
    }
}

#[test]
fn debug_stlc_nested_lambda_complete() {
    let spec = std::fs::read_to_string("examples/stlc.auf").unwrap();
    let grammar = Grammar::load(&spec).unwrap();
    let result = complete(&grammar, "λx:A.λy:B.λ", 10, Some(Context::new()));
    eprintln!("result={result:?}");
}

#[test]
fn debug_stlc_lambda_path_frontiers() {
    let spec = std::fs::read_to_string("examples/stlc.auf").unwrap();
    let grammar = Grammar::load(&spec).unwrap();
    let ctx = Context::new();

    for input in ["λx:A.λy:B.λa:", "λx:A.λy:B.λa:A"] {
        let mut synth = Synthesizer::new_with_max_depth(grammar.clone(), input, 10);
        let typed = match synth.parse_with(&ctx) {
            Ok(typed) => typed,
            Err(err) => {
                eprintln!("INPUT {input:?} err={err}");
                continue;
            }
        };
        eprintln!(
            "INPUT {input:?} complete={} completeness={}",
            typed.is_complete(),
            typed.completeness_score()
        );
        let tokens = synth.tokens_with(&ctx);
        for token in tokens.iter() {
            let candidates = synth.regex_gather_candidates(token, &ctx);
            eprintln!("  token={} candidates={:?}", token.to_pattern(), candidates);
            for candidate in candidates {
                if let Ok(next_typed) = synth.feed(&candidate, &ctx) {
                    let next_input = synth.input().to_string();
                    eprintln!(
                        "    -> {:?} complete={} completeness={}",
                        next_input,
                        next_typed.is_complete(),
                        next_typed.completeness_score()
                    );
                }
            }
        }
    }
}

#[test]
fn debug_stlc_greedy_path() {
    let spec = std::fs::read_to_string("examples/stlc.auf").unwrap();
    let grammar = Grammar::load(&spec).unwrap();
    let ctx = Context::new();
    let mut current = "λx:A.λy:B.λ".to_string();

    for step in 0..8 {
        let mut synth = Synthesizer::new_with_max_depth(grammar.clone(), &current, 10);
        let typed = synth.parse_with(&ctx).unwrap();
        eprintln!(
            "step={step} current={current:?} complete={}",
            typed.is_complete()
        );
        if typed.is_complete() {
            break;
        }

        let tokens = synth.tokens_with(&ctx);
        let mut best: Option<(String, usize, bool)> = None;
        let mut ordered_tokens: Vec<_> = tokens.iter().collect();
        ordered_tokens.sort_by_key(|token| {
            token
                .example()
                .as_deref()
                .map(|candidate| match candidate {
                    ":" | "." | ")" => 0,
                    "(" | "λ" | "->" => 2,
                    _ => 1,
                })
                .unwrap_or(1)
        });

        for token in ordered_tokens {
            for candidate in synth
                .regex_gather_candidates(token, &ctx)
                .into_iter()
                .take(4)
            {
                if let Ok(next_typed) = synth.feed(&candidate, &ctx) {
                    let next_input = synth.input().to_string();
                    let complete = next_typed.is_complete();
                    let prio = match candidate.as_str() {
                        ":" | "." | ")" => 0,
                        "(" | "λ" | "->" => 2,
                        _ => 1,
                    };
                    eprintln!(
                        "  cand token={} candidate={:?} next={:?} complete={} prio={}",
                        token.to_pattern(),
                        candidate,
                        next_input,
                        complete,
                        prio
                    );
                    let replace = match &best {
                        None => true,
                        Some((_, best_prio, best_complete)) => {
                            (complete && !best_complete)
                                || (complete == *best_complete && prio < *best_prio)
                        }
                    };
                    if replace {
                        best = Some((next_input, prio, complete));
                    }
                }
            }
        }

        let Some((next, _, _)) = best else {
            break;
        };
        current = next;
    }
}

#[test]
fn debug_stlc_direct_witness_path() {
    let spec = std::fs::read_to_string("examples/stlc.auf").unwrap();
    let grammar = Grammar::load(&spec).unwrap();
    let ctx = Context::new();
    let mut synth = Synthesizer::new_with_max_depth(grammar, "λx:A.λy:B.λ", 10);

    for token in ["a", ":", "A", ".", "x"] {
        let result = synth.feed(token, &ctx);
        eprintln!("extend {token:?} ok={}", result.is_ok());
        if let Err(err) = result {
            eprintln!("  err={err}");
            break;
        }
    }

    if let Some(tree) = synth.ast() {
        eprintln!("complete={} input={:?}", tree.is_complete(), synth.input());
    }
}

#[test]
fn debug_stlc_suite_case_timings() {
    let suites = completable::stlc::suites();
    let (_, grammar, cases) = suites.into_iter().next().unwrap();
    for case in cases {
        let start = std::time::Instant::now();
        let (result, _, _) = completable::run_test_timed_meta(&grammar, &case);
        eprintln!(
            "case={:?} pass={} ms={}",
            case.description,
            result.is_pass(),
            start.elapsed().as_millis()
        );
        if !result.is_pass() {
            break;
        }
    }
}

#[test]
fn debug_stlc_case_timings_direct() {
    let grammar = Grammar::load(&std::fs::read_to_string("examples/stlc.auf").unwrap()).unwrap();
    let cases = vec![
        ("identity", "λx:A.x", 10usize, None),
        ("nested lambdas", "λx:A.λy:B.x", 10, None),
        ("triple nested", "λx:A.λy:B.λz:C.x", 10, None),
        ("use inner var", "λx:A.λy:B.y", 10, None),
        ("lambda prefix", "λ", 10, None),
        ("lambda with var", "λx", 10, None),
        ("lambda with colon", "λx:", 10, None),
        ("lambda with type", "λx:A", 10, None),
        ("lambda with dot", "λx:A.", 10, None),
        ("lambda long var", "λfoo:A0->B0.foo", 10, None),
    ];

    for (desc, input, depth, ctx) in cases {
        eprintln!("begin_case={desc:?}");
        let start = std::time::Instant::now();
        let sound = sound_complete(&grammar, input, depth, ctx.clone());
        let complete_res = complete(&grammar, input, depth, ctx);
        eprintln!(
            "case={desc:?} sound={} complete={} ms={}",
            sound.is_sound,
            matches!(
                complete_res,
                crate::validation::completability::CompletionResult::Success { .. }
            ),
            start.elapsed().as_millis()
        );
    }
}

#[test]
fn debug_stlc_identity_prefixes_separate() {
    let grammar = Grammar::load(&std::fs::read_to_string("examples/stlc.auf").unwrap()).unwrap();
    for input in ["λ", "λx", "λx:", "λx:A", "λx:A.", "λx:A.x"] {
        let start = std::time::Instant::now();
        let result = sound_complete(&grammar, input, 10, Some(Context::new()));
        eprintln!(
            "input={input:?} sound={} prefixes={} ms={}",
            result.is_sound,
            result.prefixes_checked,
            start.elapsed().as_millis()
        );
    }
}

#[test]
fn debug_stlc_sound_identity() {
    let spec = std::fs::read_to_string("examples/stlc.auf").unwrap();
    let grammar = Grammar::load(&spec).unwrap();
    let input = "λx:A.x";
    for prefix in ["", "λ", "λx", "λx:", "λx:A", "λx:A.", "λx:A.x"] {
        let start = std::time::Instant::now();
        let result = complete(&grammar, prefix, 10, Some(Context::new()));
        eprintln!(
            "manual prefix={prefix:?} ok={} ms={}",
            matches!(
                result,
                crate::validation::completability::CompletionResult::Success { .. }
            ),
            start.elapsed().as_millis()
        );
    }
    let start = std::time::Instant::now();
    let result = sound_complete(&grammar, input, 10, Some(Context::new()));
    eprintln!("sound_complete ms={}", start.elapsed().as_millis());
    eprintln!(
        "sound={} prefixes={}",
        result.is_sound, result.prefixes_checked
    );
}

#[test]
fn debug_stlc_sound_nested() {
    let spec = std::fs::read_to_string("examples/stlc.auf").unwrap();
    let grammar = Grammar::load(&spec).unwrap();
    let result = sound_complete(&grammar, "λx:A.λy:B.x", 10, Some(Context::new()));
    eprintln!(
        "sound={} prefixes={}",
        result.is_sound, result.prefixes_checked
    );
    if let Some(prefix) = result.failing_prefix {
        eprintln!("failing_prefix={prefix}");
    }
}

#[test]
fn debug_stlc_identity_prefix_completions() {
    let spec = std::fs::read_to_string("examples/stlc.auf").unwrap();
    let grammar = Grammar::load(&spec).unwrap();
    for input in ["λ", "λx", "λx:", "λx:A", "λx:A.", "λx:A.x"] {
        let start = std::time::Instant::now();
        let result = complete(&grammar, input, 10, Some(Context::new()));
        eprintln!(
            "input={input:?} ok={} ms={}",
            matches!(
                result,
                crate::validation::completability::CompletionResult::Success { .. }
            ),
            start.elapsed().as_millis()
        );
    }
}

#[test]
fn debug_fun_sound_let_name() {
    let spec = std::fs::read_to_string("examples/fun.auf").unwrap();
    let grammar = Grammar::load(&spec).unwrap();
    let start = std::time::Instant::now();
    let result = sound_complete(&grammar, "let x", 6, Some(Context::new()));
    eprintln!(
        "sound={} prefixes={} ms={}",
        result.is_sound,
        result.prefixes_checked,
        start.elapsed().as_millis()
    );
    if let Some(prefix) = result.failing_prefix {
        eprintln!("failing_prefix={prefix}");
    }
}

#[test]
fn debug_stlc_complete_empty_prefix() {
    let spec = std::fs::read_to_string("examples/stlc.auf").unwrap();
    let grammar = Grammar::load(&spec).unwrap();
    let start = std::time::Instant::now();
    let result = complete(&grammar, "", 10, Some(Context::new()));
    eprintln!("ms={}", start.elapsed().as_millis());
    eprintln!("result={result:?}");
}

#[test]
fn debug_stlc_complete_lambda_prefix_only() {
    let spec = std::fs::read_to_string("examples/stlc.auf").unwrap();
    let grammar = Grammar::load(&spec).unwrap();
    let start = std::time::Instant::now();
    let result = complete(&grammar, "λ", 10, Some(Context::new()));
    eprintln!("ms={}", start.elapsed().as_millis());
    eprintln!("result={result:?}");
}

#[test]
fn repro_imp_simple_add_prefix_sound() {
    let (name, grammar, cases) = crate::validation::completable::imp::suites()
        .into_iter()
        .find(|(name, _, _)| *name == "imp::completable")
        .expect("missing imp::completable suite");
    let case = cases
        .into_iter()
        .find(|c| c.description == "simple add")
        .expect("missing simple add case");
    let (result, _, _) = crate::validation::completable::run_test_timed_meta(&grammar, &case);
    assert!(
        result.is_pass(),
        "suite={name} case={} should pass",
        case.description
    );
}

#[test]
fn repro_weird_chain_typed_prefix_sound() {
    let (name, grammar, cases) = crate::validation::completable::weird::suites()
        .into_iter()
        .find(|(name, _, _)| *name == "weird::chain_typed_ok")
        .expect("missing weird::chain_typed_ok suite");
    let case = cases
        .into_iter()
        .find(|c| c.description == "annotated var")
        .expect("missing annotated var case");
    let (result, _, _) = crate::validation::completable::run_test_timed_meta(&grammar, &case);
    assert!(
        result.is_pass(),
        "suite={name} case={} should pass",
        case.description
    );
}

#[test]
fn repro_weird_stmt_typed_prefix_sound() {
    let (name, grammar, cases) = crate::validation::completable::weird::suites()
        .into_iter()
        .find(|(name, _, _)| *name == "weird::stmt_typed_ok")
        .expect("missing weird::stmt_typed_ok suite");
    let case = cases
        .into_iter()
        .find(|c| c.description == "single decl")
        .expect("missing single decl case");
    let (result, _, _) = crate::validation::completable::run_test_timed_meta(&grammar, &case);
    assert!(
        result.is_pass(),
        "suite={name} case={} should pass",
        case.description
    );
}

#[test]
fn repro_weird_mutual_typed_prefix_sound() {
    let (name, grammar, cases) = crate::validation::completable::weird::suites()
        .into_iter()
        .find(|(name, _, _)| *name == "weird::mutual_typed_ok")
        .expect("missing weird::mutual_typed_ok suite");
    let case = cases
        .into_iter()
        .find(|c| c.description == "simple bind")
        .expect("missing simple bind case");
    let (result, _, _) = crate::validation::completable::run_test_timed_meta(&grammar, &case);
    assert!(
        result.is_pass(),
        "suite={name} case={} should pass",
        case.description
    );
}

#[test]
fn debug_imp_failing_prefix_candidates() {
    let grammar = crate::validation::completable::imp::imp_grammar();
    let ctx = Context::new();
    let input = "{ let x:Int=1+2;";
    let mut synth = Synthesizer::new_with_max_depth(grammar, input, 8);
    let typed = synth.parse_with(&ctx).expect("prefix should parse");
    eprintln!("complete={} input={:?}", typed.is_complete(), input);
    let tokens = synth.tokens_with(&ctx);
    eprintln!("tokens={}", tokens.len());
    for token in tokens.iter() {
        let candidates = synth.regex_gather_candidates(token, &ctx);
        eprintln!(
            "token={} example={:?} candidates={:?}",
            token.to_pattern(),
            token.example(),
            candidates
        );
        for cand in candidates {
            let mut synth2 = Synthesizer::new_with_max_depth(
                crate::validation::completable::imp::imp_grammar(),
                input,
                8,
            );
            let ok = synth2.feed(&cand, &ctx).is_ok();
            eprintln!("  feed {:?} -> {}", cand, ok);
        }
    }
}

#[test]
fn debug_weird_failing_prefix_candidates() {
    for (suite_name, input, depth) in [
        ("weird::chain_typed_ok", "", 8usize),
        ("weird::stmt_typed_ok", "{ var", 8usize),
        ("weird::mutual_typed_ok", "set", 8usize),
    ] {
        let suites = crate::validation::completable::weird::suites();
        let (_, grammar, _) = suites
            .into_iter()
            .find(|(name, _, _)| *name == suite_name)
            .expect("missing suite");
        let ctx = Context::new();
        let mut synth = Synthesizer::new_with_max_depth(grammar, input, depth);
        let parsed = synth.parse_with(&ctx);
        eprintln!(
            "suite={suite_name} input={input:?} parse_ok={}",
            parsed.is_ok()
        );
        if let Ok(ast) = parsed {
            eprintln!("  complete={}", ast.is_complete());
        }
        let tokens = synth.tokens_with(&ctx);
        eprintln!("  token_count={}", tokens.len());
        for token in tokens.iter() {
            let candidates = synth.regex_gather_candidates(token, &ctx);
            eprintln!(
                "  token={} example={:?} candidates={:?}",
                token.to_pattern(),
                token.example(),
                candidates
            );
            for cand in candidates {
                let suites = crate::validation::completable::weird::suites();
                let (_, grammar2, _) = suites
                    .into_iter()
                    .find(|(name, _, _)| *name == suite_name)
                    .expect("missing suite");
                let mut synth2 = Synthesizer::new_with_max_depth(grammar2, input, depth);
                let ok = synth2.feed(&cand, &ctx).is_ok();
                eprintln!("    feed {:?} -> {}", cand, ok);
            }
        }
    }
}

#[test]
fn repro_weird_context_extending_nested_let_prefix_sound() {
    let (name, grammar, cases) = crate::validation::completable::weird::suites()
        .into_iter()
        .find(|(name, _, _)| *name == "weird::context_extending_ok")
        .expect("missing weird::context_extending_ok suite");
    let case = cases
        .into_iter()
        .find(|c| c.description == "nested let inner")
        .expect("missing nested let inner case");
    let (result, _, _) = crate::validation::completable::run_test_timed_meta(&grammar, &case);
    assert!(
        result.is_pass(),
        "suite={name} case={} should pass",
        case.description
    );
}

#[test]
fn repro_imp_if_statement_case() {
    let (name, grammar, cases) = crate::validation::completable::imp::suites()
        .into_iter()
        .find(|(name, _, _)| *name == "imp::completable")
        .expect("missing imp::completable suite");
    let case = cases
        .into_iter()
        .find(|c| c.description == "if statement")
        .expect("missing if statement case");
    let (result, _, _) = crate::validation::completable::run_test_timed_meta(&grammar, &case);
    assert!(
        result.is_pass(),
        "suite={name} case={} should pass",
        case.description
    );
}

#[test]
fn repro_imp_while_statement_case() {
    let (name, grammar, cases) = crate::validation::completable::imp::suites()
        .into_iter()
        .find(|(name, _, _)| *name == "imp::completable")
        .expect("missing imp::completable suite");
    let case = cases
        .into_iter()
        .find(|c| c.description == "while statement")
        .expect("missing while statement case");
    let (result, _, _) = crate::validation::completable::run_test_timed_meta(&grammar, &case);
    assert!(
        result.is_pass(),
        "suite={name} case={} should pass",
        case.description
    );
}

#[test]
fn debug_imp_if_statement_case_result() {
    let (_, grammar, cases) = crate::validation::completable::imp::suites()
        .into_iter()
        .find(|(name, _, _)| *name == "imp::completable")
        .expect("missing imp::completable suite");
    let case = cases
        .into_iter()
        .find(|c| c.description == "if statement")
        .expect("missing if statement case");
    let (result, elapsed, meta) =
        crate::validation::completable::run_test_timed_meta(&grammar, &case);
    eprintln!("elapsed={elapsed:?}");
    eprintln!("result={result:?}");
    eprintln!("meta={meta:?}");
}

#[test]
fn debug_imp_if_statement_timing_split() {
    let grammar = crate::validation::completable::imp::imp_grammar();
    let input = "{ if (1==1) { let x:Int=1; } else { let x:Int=2; } }";
    let ctx = Context::new();
    let (sound, sound_elapsed) =
        crate::validation::completable::timed_sound_complete(&grammar, input, 2, Some(ctx.clone()));
    eprintln!("sound_elapsed={sound_elapsed:?} sound={}", sound.is_sound);
    eprintln!("failing_prefix={:?}", sound.failing_prefix);
    let (comp, comp_elapsed) =
        crate::validation::completable::timed_complete(&grammar, input, 2, Some(ctx));
    eprintln!("comp_elapsed={comp_elapsed:?} comp={comp:?}");
    for (i, meta) in sound.prefix_meta.iter().enumerate() {
        eprintln!(
            "prefix[{i}] ok={} time_us={} visited={:?} prefix={:?}",
            meta.ok, meta.time_us, meta.visited_count, meta.prefix
        );
    }
}

#[test]
fn debug_weird_context_extending_let_prefix_candidates() {
    let suites = crate::validation::completable::weird::suites();
    let (_, grammar, _) = suites
        .into_iter()
        .find(|(name, _, _)| *name == "weird::context_extending_ok")
        .expect("missing weird::context_extending_ok suite");
    let ctx = Context::new();
    let input = "let x : int in let";
    let mut synth = Synthesizer::new_with_max_depth(grammar, input, 12);
    let parsed = synth.parse_with(&ctx).expect("prefix should parse");
    eprintln!("input={input:?} complete={}", parsed.is_complete());
    let tokens = synth.tokens_with(&ctx);
    eprintln!("token_count={}", tokens.len());
    for token in tokens.iter() {
        let candidates = synth.regex_gather_candidates(token, &ctx);
        eprintln!(
            "token={} example={:?} candidates={:?}",
            token.to_pattern(),
            token.example(),
            candidates
        );
        for cand in candidates {
            let suites2 = crate::validation::completable::weird::suites();
            let (_, grammar2, _) = suites2
                .into_iter()
                .find(|(name, _, _)| *name == "weird::context_extending_ok")
                .expect("missing weird::context_extending_ok suite");
            let mut synth2 = Synthesizer::new_with_max_depth(grammar2, input, 12);
            let ok = synth2.feed(&cand, &ctx).is_ok();
            eprintln!("  feed {:?} -> {}", cand, ok);
        }
    }
}

#[test]
fn debug_imp_assignment_completion_tokens() {
    use crate::logic::typing::Type;

    for input in ["{ x", "{a"] {
        let grammar = crate::validation::completable::imp::imp_grammar();
        let mut synth = Synthesizer::new(grammar.clone(), input);
        let mut ctx = Context::new();
        ctx.add("x".to_string(), Type::Raw("Int".to_string()));
        ctx.add("a".to_string(), Type::Raw("Int".to_string()));

        match synth.parse_with(&ctx) {
            Ok(ast) => {
                eprintln!("input={input:?} parse_ok complete={}", ast.is_complete());
                eprintln!("ast:\n{}", ast);
            }
            Err(err) => eprintln!("input={input:?} parse_err={err}"),
        }

        let tokens = synth.tokens_with(&ctx);
        eprintln!("input={input:?} token_count={}", tokens.len());
        for token in tokens.iter() {
            let example = token.example();
            let candidates = synth.regex_gather_candidates(token, &ctx);
            eprintln!(
                "  token={} example={example:?} candidates={:?}",
                token.to_pattern(),
                candidates
            );
            for cand in candidates {
                let mut synth2 = Synthesizer::new(grammar.clone(), input);
                let ok = synth2.feed(&cand, &ctx).is_ok();
                eprintln!("    feed {:?} -> {}", cand, ok);
            }
        }
    }
}

#[test]
fn debug_imp_block_prefix_tokens() {
    let grammar = crate::validation::completable::imp::imp_grammar();
    for input in ["", "{", "{ let"] {
        let mut synth = Synthesizer::new_with_max_depth(grammar.clone(), input, 12);
        let ctx = Context::new();
        match synth.parse_with(&ctx) {
            Ok(ast) => {
                eprintln!("input={input:?} complete={}", ast.is_complete());
                eprintln!("ast:\n{}", ast);
            }
            Err(err) => eprintln!("input={input:?} parse_err={err}"),
        }
        let tokens = synth.tokens_with(&ctx);
        eprintln!("input={input:?} token_count={}", tokens.len());
        for token in tokens.iter() {
            let candidates = synth.regex_gather_candidates(token, &ctx);
            eprintln!(
                "  token={} example={:?} candidates={:?}",
                token.to_pattern(),
                token.example(),
                candidates
            );
            for cand in candidates {
                let mut synth2 = Synthesizer::new_with_max_depth(grammar.clone(), input, 12);
                let ok = synth2.feed(&cand, &ctx).is_ok();
                eprintln!("    feed {:?} -> {}", cand, ok);
            }
        }
    }
}

#[test]
fn debug_imp_advance_from_open_brace_to_let() {
    use crate::logic::fusion::runtime::RuleRuntime;
    use crate::logic::fusion::TypedParser;

    let grammar = crate::validation::completable::imp::imp_grammar();
    let runtime = RuleRuntime::new(grammar.clone());
    let ctx_id = runtime.intern_context(Context::new());
    let mut parser = TypedParser::new(grammar.clone(), runtime.clone());

    let prefix = parser.parse("{", ctx_id).expect("parse '{'");
    eprintln!("parsed '{{' roots={:?}", prefix.roots);
    let ast = parser.materialize(
        &prefix.roots,
        grammar.tokenize("{").unwrap(),
        "{".to_string(),
    );
    eprintln!("ast for '{{':\n{}", ast);

    match parser.advance(&prefix, "{ let", ctx_id) {
        Ok(next) => {
            eprintln!("advance ok roots={:?}", next.roots);
            let ast = parser.materialize(
                &next.roots,
                grammar.tokenize("{ let").unwrap(),
                "{ let".to_string(),
            );
            eprintln!("advanced ast:\n{}", ast);
        }
        Err(err) => {
            eprintln!("advance err: {err}");
            panic!("advance from '{{' to '{{ let' failed");
        }
    }
}

#[test]
fn debug_imp_advance_from_open_brace_to_if() {
    use crate::logic::fusion::runtime::RuleRuntime;
    use crate::logic::fusion::TypedParser;

    let grammar = crate::validation::completable::imp::imp_grammar();
    let runtime = RuleRuntime::new(grammar.clone());
    let ctx_id = runtime.intern_context(Context::new());
    let mut parser = TypedParser::new(grammar.clone(), runtime);

    let prefix = parser.parse("{", ctx_id).expect("parse '{'");
    match parser.advance(&prefix, "{ if", ctx_id) {
        Ok(next) => {
            let ast = parser.materialize(
                &next.roots,
                grammar.tokenize("{ if").unwrap(),
                "{ if".to_string(),
            );
            eprintln!("advance to if ok complete={}\n{}", ast.is_complete(), ast);
        }
        Err(err) => panic!("advance from '{{' to '{{ if' failed: {err}"),
    }
}

#[test]
fn debug_weird_stmt_advance_from_open_brace_to_var() {
    use crate::logic::fusion::runtime::RuleRuntime;
    use crate::logic::fusion::TypedParser;

    let (_, grammar, _) = crate::validation::completable::weird::suites()
        .into_iter()
        .find(|(name, _, _)| *name == "weird::stmt_typed_ok")
        .expect("missing stmt typed suite");
    let runtime = RuleRuntime::new(grammar.clone());
    let ctx_id = runtime.intern_context(Context::new());
    let mut parser = TypedParser::new(grammar.clone(), runtime);

    let prefix = parser.parse("{", ctx_id).expect("parse '{'");
    match parser.advance(&prefix, "{ var", ctx_id) {
        Ok(next) => {
            let ast = parser.materialize(
                &next.roots,
                grammar.tokenize("{ var").unwrap(),
                "{ var".to_string(),
            );
            eprintln!("advance to var ok complete={}\n{}", ast.is_complete(), ast);
        }
        Err(err) => panic!("advance from '{{' to '{{ var' failed: {err}"),
    }
}

#[test]
fn debug_imp_if_witness_feed_chain() {
    let grammar = crate::validation::completable::imp::imp_grammar();
    let input = "{ if (1==1) { let x:Int=1; } else { let x:Int=2; } }";
    let prefix = "{ if";
    let ctx = Context::new();
    let segments = grammar.tokenize(input).expect("tokenize");
    let mut synth = Synthesizer::new_with_max_depth(grammar.clone(), prefix, 16);
    synth.parse_with(&ctx).expect("parse prefix");
    for seg in segments.iter().filter(|seg| seg.start >= prefix.len()) {
        let ok = synth.feed(seg.as_str(), &ctx).is_ok();
        eprintln!("feed {:?} -> {}", seg.as_str(), ok);
        if !ok {
            panic!(
                "failed feeding {:?} after prefix {:?}",
                seg.as_str(),
                prefix
            );
        }
    }
}

#[test]
fn debug_weird_stmt_witness_feed_chain() {
    let (_, grammar, _) = crate::validation::completable::weird::suites()
        .into_iter()
        .find(|(name, _, _)| *name == "weird::stmt_typed_ok")
        .expect("missing stmt typed suite");
    let input = "{ var x : I = 1 ; }";
    let prefix = "{ var";
    let ctx = Context::new();
    let segments = grammar.tokenize(input).expect("tokenize");
    let mut synth = Synthesizer::new_with_max_depth(grammar.clone(), prefix, 16);
    synth.parse_with(&ctx).expect("parse prefix");
    for seg in segments.iter().filter(|seg| seg.start >= prefix.len()) {
        let ok = synth.feed(seg.as_str(), &ctx).is_ok();
        eprintln!("feed {:?} -> {}", seg.as_str(), ok);
        if !ok {
            panic!(
                "failed feeding {:?} after prefix {:?}",
                seg.as_str(),
                prefix
            );
        }
    }
}

#[test]
fn debug_arithmetic_spacing_parse() {
    let grammar = crate::validation::completable::arithmetic::suites()
        .into_iter()
        .find(|(name, _, _)| *name == "arithmetic::completable")
        .expect("missing arithmetic suite")
        .1;

    for input in ["1 + 2 *", "1 + 2 *0", "1 + 2 * 0", "1+2*0"] {
        let segments = grammar.tokenize(input).expect("tokenize");
        eprintln!("input={input:?}");
        eprintln!(
            "segments={:?}",
            segments
                .iter()
                .map(|s| s.text().to_string())
                .collect::<Vec<_>>()
        );
        let mut synth = Synthesizer::new_with_max_depth(grammar.clone(), input, 8);
        match synth.parse_with(&Context::new()) {
            Ok(ast) => {
                eprintln!("complete={}", ast.is_complete());
                eprintln!("ast:\n{}", ast);
                let toks = synth.tokens_with(&Context::new());
                eprintln!(
                    "tokens={:?}",
                    toks.iter()
                        .map(|t| (t.to_pattern(), t.example()))
                        .collect::<Vec<_>>()
                );
            }
            Err(err) => eprintln!("parse_err={err}"),
        }
    }
}

#[test]
fn debug_arithmetic_advance_after_operator() {
    use crate::logic::fusion::runtime::RuleRuntime;
    use crate::logic::fusion::TypedParser;

    let grammar = crate::validation::completable::arithmetic::suites()
        .into_iter()
        .find(|(name, _, _)| *name == "arithmetic::completable")
        .expect("missing arithmetic suite")
        .1;
    let runtime = RuleRuntime::new(grammar.clone());
    let ctx_id = runtime.intern_context(Context::new());
    let mut parser = TypedParser::new(grammar.clone(), runtime);

    let prefix = parser.parse("1 + 2 *", ctx_id).expect("parse prefix");
    let prefix_ast = parser.materialize(
        &prefix.roots,
        grammar.tokenize("1 + 2 *").unwrap(),
        "1 + 2 *".to_string(),
    );
    eprintln!(
        "prefix complete={}\n{}",
        prefix_ast.is_complete(),
        prefix_ast
    );

    let advanced = parser
        .advance(&prefix, "1 + 2 *0", ctx_id)
        .expect("advance");
    let advanced_ast = parser.materialize(
        &advanced.roots,
        grammar.tokenize("1 + 2 *0").unwrap(),
        "1 + 2 *0".to_string(),
    );
    eprintln!(
        "advanced complete={}\n{}",
        advanced_ast.is_complete(),
        advanced_ast
    );

    let reparsed = parser.parse("1 + 2 *0", ctx_id).expect("reparse");
    let reparsed_ast = parser.materialize(
        &reparsed.roots,
        grammar.tokenize("1 + 2 *0").unwrap(),
        "1 + 2 *0".to_string(),
    );
    eprintln!(
        "reparsed complete={}\n{}",
        reparsed_ast.is_complete(),
        reparsed_ast
    );
}

#[test]
fn debug_imp_advance_after_add_operator() {
    use crate::logic::fusion::runtime::RuleRuntime;
    use crate::logic::fusion::TypedParser;

    let grammar = crate::validation::completable::imp::imp_grammar();
    let runtime = RuleRuntime::new(grammar.clone());
    let ctx_id = runtime.intern_context(Context::new());
    let mut parser = TypedParser::new(grammar.clone(), runtime);

    let from = "{ let x:Int=1+";
    let to = "{ let x:Int=1+2";

    let prefix = parser.parse(from, ctx_id).expect("parse prefix");
    let prefix_ast = parser.materialize(
        &prefix.roots,
        grammar.tokenize(from).unwrap(),
        from.to_string(),
    );
    eprintln!(
        "prefix complete={}\n{}",
        prefix_ast.is_complete(),
        prefix_ast
    );

    let advanced = parser.advance(&prefix, to, ctx_id).expect("advance");
    let advanced_ast = parser.materialize(
        &advanced.roots,
        grammar.tokenize(to).unwrap(),
        to.to_string(),
    );
    eprintln!(
        "advanced complete={}\n{}",
        advanced_ast.is_complete(),
        advanced_ast
    );

    let reparsed = parser.parse(to, ctx_id).expect("reparse");
    let reparsed_ast = parser.materialize(
        &reparsed.roots,
        grammar.tokenize(to).unwrap(),
        to.to_string(),
    );
    eprintln!(
        "reparsed complete={}\n{}",
        reparsed_ast.is_complete(),
        reparsed_ast
    );
}

#[test]
fn debug_imp_add_prefix_tokens() {
    let grammar = crate::validation::completable::imp::imp_grammar();
    for input in [
        "{ let x:Int=1+",
        "{ let x:Int=1+1",
        "{ let x:Int=1+2",
        "{ let x:Int=1+2;",
    ] {
        let mut synth = Synthesizer::new_with_max_depth(grammar.clone(), input, 8);
        let ctx = Context::new();
        match synth.parse_with(&ctx) {
            Ok(ast) => {
                eprintln!("input={input:?} complete={}", ast.is_complete());
                eprintln!("ast:\n{}", ast);
            }
            Err(err) => eprintln!("input={input:?} parse_err={err}"),
        }
        let toks = synth.tokens_with(&ctx);
        eprintln!(
            "tokens={:?}",
            toks.iter()
                .map(|t| (t.to_pattern(), t.example()))
                .collect::<Vec<_>>()
        );
    }
}

#[test]
fn debug_imp_advance_after_semicolon() {
    use crate::logic::fusion::runtime::RuleRuntime;
    use crate::logic::fusion::TypedParser;

    let grammar = crate::validation::completable::imp::imp_grammar();
    let runtime = RuleRuntime::new(grammar.clone());
    let ctx_id = runtime.intern_context(Context::new());
    let mut parser = TypedParser::new(grammar.clone(), runtime);

    for (from, to) in [
        ("{ let x:Int=1+1", "{ let x:Int=1+1;"),
        ("{ let x:Int=1+2", "{ let x:Int=1+2;"),
        ("{ let x:Int=1+2;", "{ let x:Int=1+2; }"),
    ] {
        let prefix = parser.parse(from, ctx_id).expect("parse prefix");
        let advanced = parser.advance(&prefix, to, ctx_id);
        eprintln!("advance {from:?} -> {to:?}: ok={}", advanced.is_ok());
        match advanced {
            Ok(next) => {
                let ast =
                    parser.materialize(&next.roots, grammar.tokenize(to).unwrap(), to.to_string());
                eprintln!("complete={}\n{}", ast.is_complete(), ast);
            }
            Err(err) => eprintln!("err={err}"),
        }
    }
}

#[test]
fn debug_imp_nested_paren_tokens() {
    let grammar = crate::validation::completable::imp::imp_grammar();
    for input in [
        "{ let x:Int=((1",
        "{ let x:Int=((1+",
        "{ let x:Int=((1+2",
        "{ let x:Int=((1+2)",
        "{ let x:Int=((1+2))",
    ] {
        let mut synth = Synthesizer::new_with_max_depth(grammar.clone(), input, 10);
        let ctx = Context::new();
        match synth.parse_with(&ctx) {
            Ok(ast) => {
                eprintln!("input={input:?} complete={}", ast.is_complete());
                eprintln!("ast:\n{}", ast);
            }
            Err(err) => eprintln!("input={input:?} parse_err={err}"),
        }
        let toks = synth.tokens_with(&ctx);
        eprintln!(
            "tokens={:?}",
            toks.iter()
                .map(|t| (t.to_pattern(), t.example()))
                .collect::<Vec<_>>()
        );
    }
}

#[test]
fn debug_imp_nested_paren_advance() {
    use crate::logic::fusion::runtime::RuleRuntime;
    use crate::logic::fusion::TypedParser;

    let grammar = crate::validation::completable::imp::imp_grammar();
    let runtime = RuleRuntime::new(grammar.clone());
    let ctx_id = runtime.intern_context(Context::new());
    let mut parser = TypedParser::new(grammar.clone(), runtime);

    for (from, to) in [
        ("{ let x:Int=((1+2", "{ let x:Int=((1+2)"),
        ("{ let x:Int=((1+2)", "{ let x:Int=((1+2))"),
        ("{ let x:Int=((1+2))", "{ let x:Int=((1+2));"),
        ("{ let x:Int=((1+2));", "{ let x:Int=((1+2)); }"),
    ] {
        let prefix = parser.parse(from, ctx_id).expect("parse prefix");
        let advanced = parser.advance(&prefix, to, ctx_id);
        eprintln!("advance {from:?} -> {to:?}: ok={}", advanced.is_ok());
        match advanced {
            Ok(next) => {
                let ast =
                    parser.materialize(&next.roots, grammar.tokenize(to).unwrap(), to.to_string());
                eprintln!("complete={}\n{}", ast.is_complete(), ast);
            }
            Err(err) => eprintln!("err={err}"),
        }
    }
}

#[test]
fn debug_imp_nested_paren_feed_candidates() {
    let grammar = crate::validation::completable::imp::imp_grammar();
    let input = "{ let x:Int=((1+2";
    let ctx = Context::new();
    let mut synth = Synthesizer::new_with_max_depth(grammar.clone(), input, 30);
    let tokens = synth.tokens_with(&ctx);
    for token in tokens.iter() {
        let candidates = synth.regex_gather_candidates(token, &ctx);
        eprintln!(
            "token={} example={:?} candidates={:?}",
            token.to_pattern(),
            token.example(),
            candidates
        );
        for cand in candidates {
            let mut synth2 = Synthesizer::new_with_max_depth(grammar.clone(), input, 10);
            let ok = synth2.feed(&cand, &ctx).is_ok();
            eprintln!("  feed {:?} -> {}", cand, ok);
        }
    }
}

#[test]
fn debug_imp_if_condition_prefix_tokens() {
    let grammar = crate::validation::completable::imp::imp_grammar();
    let input = "{ if (1";
    let mut synth = Synthesizer::new_with_max_depth(grammar.clone(), input, 10);
    let ctx = Context::new();
    match synth.parse_with(&ctx) {
        Ok(ast) => {
            eprintln!("input={input:?} complete={}", ast.is_complete());
            eprintln!("ast:\n{}", ast);
        }
        Err(err) => eprintln!("parse_err={err}"),
    }
    let toks = synth.tokens_with(&ctx);
    eprintln!(
        "tokens={:?}",
        toks.iter()
            .map(|t| (t.to_pattern(), t.example()))
            .collect::<Vec<_>>()
    );
    for token in toks.iter() {
        let candidates = synth.regex_gather_candidates(token, &ctx);
        eprintln!(
            "token={} example={:?} candidates={:?}",
            token.to_pattern(),
            token.example(),
            candidates
        );
        for cand in candidates {
            let mut synth2 = Synthesizer::new_with_max_depth(grammar.clone(), input, 10);
            let ok = synth2.feed(&cand, &ctx).is_ok();
            eprintln!("  feed {:?} -> {}", cand, ok);
        }
    }
}

#[test]
fn debug_imp_if_condition_alternatives() {
    let grammar = crate::validation::completable::imp::imp_grammar();
    let input = "{ if (1";
    let ctx = Context::new();
    let mut synth = Synthesizer::new_with_max_depth(grammar.clone(), input, 10);
    let typed = synth.parse_with(&ctx).expect("parse");
    for root in typed.roots() {
        let root_node = typed.arena().node(root.node_id()).unwrap();
        eprintln!(
            "root nt={} status={:?} span={:?}",
            grammar.nt_name(root_node.nt.0).unwrap_or("<?>"),
            root_node.status,
            root_node.span
        );
        dump_alts(&grammar, typed.arena(), root.node_id(), 0);
    }
    for idx in 0..typed.arena().node_count() {
        let node_id = crate::logic::parse::arena::NodeId(idx);
        if let Some(node) = typed.arena().node(node_id) {
            let nt = grammar.nt_name(node.nt.0).unwrap_or("<?>");
            if nt == "CompOp" || nt == "CompOperator" || nt == "Expression" {
                eprintln!(
                    "scan node#{idx} nt={nt} status={:?} span={:?}",
                    node.status, node.span
                );
                if nt == "Expression" {
                    dump_alts(&grammar, typed.arena(), node_id, 1);
                }
            }
        }
    }
}

#[test]
fn debug_imp_expression_component() {
    use crate::logic::fusion::runtime::RuleRuntime;
    use crate::logic::fusion::TypedParser;

    let grammar = crate::validation::completable::imp::imp_grammar();
    let runtime = RuleRuntime::new(grammar.clone());
    let parser = TypedParser::new(grammar.clone(), runtime);
    let expr = grammar.nt_index("Expression").expect("Expression nt");
    let component = parser.left_component(crate::logic::parse::arena::NtId(expr));
    eprintln!(
        "component={:?}",
        component
            .iter()
            .map(|nt| grammar.nt_name(nt.0).unwrap_or("<?>"))
            .collect::<Vec<_>>()
    );
}

#[test]
fn debug_imp_if_condition_trace() {
    crate::set_debug_level(crate::logic::debug::DebugLevel::Trace);
    crate::add_module_filter("fusion_parser");
    crate::add_module_filter("fusion_typing");

    let grammar = crate::validation::completable::imp::imp_grammar();
    let ctx = Context::new();
    let mut synth = Synthesizer::new_with_max_depth(grammar, "{ if (1", 12);
    let _ = synth.parse_with(&ctx);
}

fn dump_alts(
    grammar: &Grammar,
    arena: &crate::logic::parse::arena::ParseArena,
    node_id: crate::logic::parse::arena::NodeId,
    depth: usize,
) {
    let indent = "  ".repeat(depth);
    let node = arena.node(node_id).unwrap();
    eprintln!(
        "{}node nt={} status={:?} span={:?}",
        indent,
        grammar.nt_name(node.nt.0).unwrap_or("<?>"),
        node.status,
        node.span
    );
    if let Some(alts) = arena.alts_for(node_id) {
        for (i, alt) in alts.iter().enumerate() {
            eprintln!(
                "{}  alt[{i}] prod_id={} children={}",
                indent,
                alt.prod.0,
                alt.children.len()
            );
            for child in &alt.children {
                match child {
                    crate::logic::parse::arena::ChildRef::Terminal(tok) => {
                        eprintln!(
                            "{}    term start={} end={} complete={}",
                            indent, tok.start, tok.end, tok.complete
                        );
                    }
                    crate::logic::parse::arena::ChildRef::Node(child_id) => {
                        dump_alts(grammar, arena, *child_id, depth + 2);
                    }
                }
            }
        }
    }
}
