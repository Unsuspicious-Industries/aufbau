//! Reproduction tests for parseable failures.
//!
//! Each test reproduces a specific failure from the parseable test suite.

use super::harness::{FusionTestCase, run_cases};

// ============================================================================
// STLC failures
// ============================================================================

#[test]
fn repro_stlc_nested_lambda_prefix() {
    let g = crate::testing::load_example_grammar("stlc");
    run_cases(&[FusionTestCase::pass(
        "stlc nested lambda prefix",
        g,
        "λx:A.λ",
    )]);
}

#[test]
fn repro_stlc_lambda_with_app() {
    let g = crate::testing::load_example_grammar("stlc");
    run_cases(&[FusionTestCase::pass(
        "stlc lambda with app prefix",
        g,
        "λf:A->B.λ",
    )]);
}

#[test]
fn repro_stlc_lambda_with_double_app() {
    let g = crate::testing::load_example_grammar("stlc");
    run_cases(&[FusionTestCase::pass(
        "stlc lambda with double app prefix",
        g,
        "λf:A->B->C.λ",
    )]);
}

#[test]
fn repro_stlc_left_recursive_application() {
    let g = crate::testing::load_example_grammar("stlc");
    // Known failing: keep coverage as XFail until Fusion supports this reliably.
    run_cases(&[
        FusionTestCase::xfail("stlc app left-rec f x", g.clone(), "f x"),
        FusionTestCase::xfail("stlc app left-rec f x y", g.clone(), "f x y"),
        FusionTestCase::xfail("stlc app left-rec f x y z w", g, "f x y z w"),
    ]);
}

// ============================================================================
// Fun failures
// ============================================================================

#[test]
fn repro_fun_invalid_expression_float_expected_int() {
    let g = crate::testing::load_example_grammar("fun");
    run_cases(&[FusionTestCase::pass(
        "fun invalid mixed (float + int) should still parse structurally",
        g,
        "1.0 + 2",
    )]);
}

#[test]
fn repro_fun_invalid_expression_mixed_operators() {
    let g = crate::testing::load_example_grammar("fun");
    run_cases(&[FusionTestCase::pass(
        "fun invalid mixed (int + float) should still parse structurally",
        g,
        "1 + 2.0",
    )]);
}

// ============================================================================
// Imp failures
// ============================================================================

#[test]
fn repro_imp_assign_arithmetic() {
    let g = crate::testing::load_example_grammar("imp");
    run_cases(&[
        FusionTestCase::pass("imp arithmetic decl", g, "{ let x: Int = 1 + 2; }")
            .with_max_depth(48),
    ]);
}

#[test]
fn repro_imp_long_decl_chain() {
    let g = crate::testing::load_example_grammar("imp");
    run_cases(&[FusionTestCase::pass(
        "imp long decl chain",
        g,
        "{ let x: Int = 1; let y: Int = 2; let z: Int = 3; }",
    )
    .with_max_depth(48)]);
}

#[test]
fn repro_imp_sequential_var_reuse() {
    let g = crate::testing::load_example_grammar("imp");
    run_cases(&[FusionTestCase::pass(
        "imp sequential var reuse",
        g,
        "{ let x: Int = 1; let x: Int = 2; }",
    )
    .with_max_depth(48)]);
}

#[test]
fn repro_imp_if_expression() {
    let g = crate::testing::load_example_grammar("imp");
    run_cases(&[FusionTestCase::pass(
        "imp if/else program",
        g,
        "{ if (1==1) { let x:Int=1; } else { let y:Int=2; } }",
    )
    .with_max_depth(48)]);
}

#[test]
fn repro_imp_union_decl_prefix_should_parse() {
    crate::set_debug_level(crate::logic::debug::DebugLevel::Trace);
    crate::add_module_filter("fusion_typing");
    let g = crate::testing::load_example_grammar("imp");
    run_cases(&[
        FusionTestCase::pass("imp union decl prefix", g, "{ let u:Int|Bool=true;")
            .with_max_depth(48),
    ]);
}

#[test]
fn repro_imp_sequential_decl_prefix_should_parse() {
    crate::set_debug_level(crate::logic::debug::DebugLevel::Trace);
    crate::add_module_filter("fusion_typing");
    let g = crate::testing::load_example_grammar("imp");
    run_cases(&[FusionTestCase::pass(
        "imp sequential decl prefix",
        g,
        "{ let x:Int=5; let y:Int=x",
    )
    .with_max_depth(48)]);
}

#[test]
fn repro_inline_arithmetic_empty_prefix_has_completions() {
    let g = crate::logic::grammar::Grammar::load(
        r#"
        Number ::= /[0-9]+/
        Identifier ::= /[a-z][a-zA-Z0-9]*/
        Literal ::= Number
        Variable ::= Identifier
        Operator ::= '+' | '-' | '*' | '/'
        Primary ::= Literal | Variable | '(' Expression ')'
        Expression ::= Primary | Primary Operator Expression
    "#,
    )
    .unwrap();

    let mut synth = crate::logic::fusion::Synthesizer::new_with_max_depth(g.clone(), "", 6);
    let tokens = synth.tokens();
    assert!(
        !tokens.is_empty(),
        "expected non-empty completions for empty prefix"
    );
}

#[test]
fn repro_inline_arithmetic_can_extend_add_prefix_with_number() {
    let g = crate::logic::grammar::Grammar::load(
        r#"
        Number ::= /[0-9]+/
        Identifier ::= /[a-z][a-zA-Z0-9]*/
        Literal ::= Number
        Variable ::= Identifier
        Operator ::= '+' | '-' | '*' | '/'
        Primary ::= Literal | Variable | '(' Expression ')'
        Expression ::= Primary | Primary Operator Expression
    "#,
    )
    .unwrap();

    let ctx = crate::logic::typing::Context::new();
    let mut synth = crate::logic::fusion::Synthesizer::new_with_max_depth(g, "1 +", 6);
    let _ = synth.feed("1 +", &ctx);
    synth
        .feed("0", &ctx)
        .unwrap_or_else(|e| panic!("expected '1 +' to extend with number token, got: {e}"));
}

#[test]
fn repro_inline_arithmetic_add_prefix_has_digit_completion_token() {
    let g = crate::logic::grammar::Grammar::load(
        r#"
        Number ::= /[0-9]+/
        Identifier ::= /[a-z][a-zA-Z0-9]*/
        Literal ::= Number
        Variable ::= Identifier
        Operator ::= '+' | '-' | '*' | '/'
        Primary ::= Literal | Variable | '(' Expression ')'
        Expression ::= Primary | Primary Operator Expression
    "#,
    )
    .unwrap();

    let ctx = crate::logic::typing::Context::new();
    let mut synth = crate::logic::fusion::Synthesizer::new_with_max_depth(g, "1 +", 6);
    let _ = synth.feed("1 +", &ctx);
    let tokens = synth.tokens();
    assert!(
        tokens.iter().any(|t| t.example().as_deref() == Some("0")),
        "expected a digit completion (example '0'), got: {:?}",
        tokens
            .iter()
            .map(|t| (t.to_pattern(), t.example()))
            .collect::<Vec<_>>()
    );
}

#[test]
fn repro_inline_arithmetic_ast_completions_include_digit_after_plus() {
    let g = crate::logic::grammar::Grammar::load(
        r#"
        Number ::= /[0-9]+/
        Identifier ::= /[a-z][a-zA-Z0-9]*/
        Literal ::= Number
        Variable ::= Identifier
        Operator ::= '+' | '-' | '*' | '/'
        Primary ::= Literal | Variable | '(' Expression ')'
        Expression ::= Primary | Primary Operator Expression
    "#,
    )
    .unwrap();
    eprintln!(
        "Primary prods={:?} Literal prods={:?} Number prods={:?}",
        g.productions.get("Primary").map(|p| p.len()),
        g.productions.get("Literal").map(|p| p.len()),
        g.productions.get("Number").map(|p| p.len()),
    );

    let rt = crate::logic::fusion::RuleRuntime::new(g.clone());
    let ctx_id = rt.intern_context(crate::logic::typing::Context::new());
    let parser = crate::logic::fusion::TypedParser::new(g.clone(), rt).with_max_depth(32);
    let meta = crate::logic::fusion::MetaTypedParser::new(parser)
        .with_start_depth(4)
        .with_max_depth(32);
    let (state, _depth, arena) = meta.parse_with_arena("1 +", ctx_id).unwrap();
    eprintln!("roots={}", state.roots.len());
    let ast = crate::logic::fusion::FusionAST::new(
        arena,
        g.tokenize("1 +").unwrap_or_default(),
        state.roots.clone(),
        "1 +".to_string(),
    );

    let toks = ast.completions(&g);
    assert!(
        toks.iter().any(|t| t.example().as_deref() == Some("0")),
        "expected AST completions to include digit (example '0'), got: {:?}",
        toks.iter()
            .map(|t| (t.to_pattern(), t.example()))
            .collect::<Vec<_>>()
    );
}

#[test]
fn repro_fun_application_prefix_f_open_paren_has_arg_completions() {
    let g = crate::testing::load_example_grammar("fun");
    let mut ctx = crate::logic::typing::Context::new();
    ctx.add(
        "f".to_string(),
        crate::logic::typing::Type::parse_raw("Int -> Int").unwrap(),
    );
    let mut synth = crate::logic::fusion::Synthesizer::new_with_max_depth(g, "f(", 32);

    // Intern the context in the synthesizer's shared runtime
    let _ctx_id = synth.runtime().intern_context(ctx.clone());

    let typed = synth.parse_with(&ctx);
    assert!(
        typed.is_ok(),
        "expected 'f(' to be partially parseable/typable, got: {:?}",
        typed.err()
    );

    // Check completions at "f(" using the same context
    let tokens = synth.tokens_with(&ctx);
    assert!(!tokens.is_empty(), "expected completions at 'f('");
}

#[test]
fn repro_fun_f_open_paren_is_partially_typed_under_context() {
    let g = crate::testing::load_example_grammar("fun");
    let mut ctx = crate::logic::typing::Context::new();
    ctx.add(
        "f".to_string(),
        crate::logic::typing::Type::parse_raw("Int -> Int").unwrap(),
    );

    let mut synth = crate::logic::fusion::Synthesizer::new_with_max_depth(g, "f(", 32);

    // Intern the context in the synthesizer's runtime so the parser can find it
    let ctx_id = synth.runtime().intern_context(ctx.clone());
    eprintln!("ctx_id = {:?}", ctx_id);
    eprintln!("stored: {:?}", synth.runtime().context(ctx_id));

    // Now partial_typed_ctx should work because the runtime is shared via Rc
    let result = synth.parse_with(&ctx);
    eprintln!("synth.partial_typed_ctx: ok={}", result.is_ok());

    result.unwrap_or_else(|e| panic!("expected partial typed parse for 'f(', got: {e}"));
}

#[test]
fn repro_fun_feed_tokens_are_sound_for_int_add_complete() {
    let g = crate::testing::load_example_grammar("fun");
    let ctx = crate::logic::typing::Context::new();
    let mut synth = crate::logic::fusion::Synthesizer::new_with_max_depth(g, "1 + 2", 16);
    let tokens = synth.tokens_with(&ctx);
    let mut bad = Vec::new();
    for t in tokens.iter() {
        let Some(ex) = t.example() else { continue };
        if synth.feed(&ex, &ctx).is_err() {
            bad.push(ex);
        }
    }
    assert!(bad.is_empty(), "unsound tokens: {:?}", bad);
}

#[test]
fn repro_fun_validation_full_prefix_shortcut_for_int_add() {
    let g = crate::testing::load_example_grammar("fun");
    let ctx = crate::logic::typing::Context::new();
    let mut synth = crate::logic::fusion::Synthesizer::new(g, "");
    let _ = synth.feed("1 + 2", &ctx);
    let tokens = synth.tokens_with(&ctx);
    let typed = synth.parse_with(&ctx).expect("typed parse should succeed");
    assert!(typed.is_complete(), "typed parse should be complete");
    for t in tokens.iter() {
        let ex = t.example().unwrap();
        assert!(synth.feed(&ex, &ctx).is_ok(), "token {ex:?} not extendable");
    }
}

#[test]
fn repro_fun_int_plus_prefix_completions_include_digit() {
    let g = crate::testing::load_example_grammar("fun");
    let ctx = crate::logic::typing::Context::new();
    let mut synth = crate::logic::fusion::Synthesizer::new_with_max_depth(g, "1 +", 8);
    let guide = synth.tokens_with(&ctx);
    let mut examples: Vec<String> = guide.iter().filter_map(|t| t.example()).collect();
    examples.sort();
    examples.dedup();
    assert!(
        examples
            .iter()
            .any(|e| e.chars().all(|c| c.is_ascii_digit())),
        "expected a digit completion, got examples={:?}",
        examples
    );
}

#[test]
fn repro_fun_int_add_is_complete_typed() {
    let g = crate::testing::load_example_grammar("fun");
    let ctx = crate::logic::typing::Context::new();
    let mut synth = crate::logic::fusion::Synthesizer::new_with_max_depth(g, "1 + 2", 16);
    let typed = synth.parse_with(&ctx).expect("typed parse should succeed");
    eprintln!("is_complete={}", typed.is_complete());
    eprintln!(
        "grounded_roots={}",
        typed.grounded_root_count(synth.runtime())
    );
    eprintln!("ast:\n{}", typed);
    assert!(typed.is_complete(), "expected '1 + 2' to be complete");
}

#[test]
fn repro_fun_incremental_operator_prefix_stays_typed() {
    let grammar = crate::testing::load_example_grammar("fun");
    let runtime = crate::logic::fusion::RuleRuntime::new(grammar.clone());
    let ctx_id = runtime.intern_context(crate::logic::typing::Context::new());
    let mut parser = crate::logic::fusion::TypedParser::new(grammar, runtime).with_max_depth(64);

    let prefix = parser
        .parse("1", ctx_id)
        .expect("prefix parse should succeed");
    let advanced = parser
        .advance(&prefix, "1 *", ctx_id)
        .expect("incremental operator prefix should stay typed");

    assert!(
        !advanced.roots.is_empty(),
        "incremental advance should keep roots"
    );
    assert!(advanced.roots.iter().any(|root_id| {
        parser.arena().node(*root_id).is_some_and(|node| {
            node.span.end == 2 && matches!(node.status, crate::logic::fusion::NodeStatus::Partial)
        })
    }));
}

#[test]
fn repro_imp_parenthesized_expr_prefix_should_parse() {
    crate::set_debug_level(crate::logic::debug::DebugLevel::Trace);
    crate::add_module_filter("fusion_typing");
    let g = crate::testing::load_example_grammar("imp");
    let rt = crate::logic::fusion::RuleRuntime::new(g.clone());
    eprintln!(
        "prod15={:?} prod16={:?} prod28={:?} prod30={:?}\nprod15p={:?}\nprod28p={:?}\nprod30p={:?}",
        rt.production_rule_name(crate::logic::fusion::ProdId(15)),
        rt.production_rule_name(crate::logic::fusion::ProdId(16)),
        rt.production_rule_name(crate::logic::fusion::ProdId(28)),
        rt.production_rule_name(crate::logic::fusion::ProdId(30)),
        rt.production(crate::logic::fusion::ProdId(15)),
        rt.production(crate::logic::fusion::ProdId(28)),
        rt.production(crate::logic::fusion::ProdId(30)),
    );
    run_cases(&[FusionTestCase::pass(
        "imp paren expr prefix",
        g,
        "{ let x:Int=5; let y:Int=(x+1);",
    )
    .with_max_depth(48)]);
}

// ============================================================================
// TypeScript failures
// ============================================================================

#[test]
fn repro_ts_function_prefix() {
    let g = crate::testing::load_example_grammar("typescript");
    run_cases(&[FusionTestCase::pass(
        "ts function def",
        g,
        "function foo(x: number): void { return; }",
    )
    .with_max_depth(48)]);
}

#[test]
fn repro_ts_call_prefix() {
    let g = crate::testing::load_example_grammar("typescript");
    run_cases(&[FusionTestCase::pass(
        "ts useIt( call prefix",
        g,
        "function useIt(v: number[]): void { return; } const xs: number[] = [1, 2]; useIt(",
    )
    .with_max_depth(48)]);
}
