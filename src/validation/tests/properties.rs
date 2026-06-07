use proptest::prelude::*;

use crate::typing::{Context, Type};
use crate::typing::{TypingSynth};
use crate::engine::grammar::SPG;
use crate::validation::parseable::{
    arithmetic::ARITHMETIC_GRAMMAR, check_all_prefixes_parseable, check_parse_fails,
    load_example_grammar,
};

// ── helpers ───────────────────────────────────────────────────────────────

fn parse_accepted(grammar: &SPG, input: &str, ctx: &Context) -> bool {
    let mut s = TypingSynth::new(grammar.clone(), input);
    s.parse_with(ctx).is_ok()
}

// ── 1. Empty-prefix acceptance ────────────────────────────────────────────

#[test]
fn empty_prefix_always_accepted_stlc() {
    let g = load_example_grammar("stlc");
    assert!(parse_accepted(&g, "", &Context::new()));
}

#[test]
fn empty_prefix_always_accepted_imp() {
    let g = load_example_grammar("imp");
    assert!(parse_accepted(&g, "", &Context::new()));
}

#[test]
fn empty_prefix_always_accepted_arithmetic() {
    let g = SPG::load(ARITHMETIC_GRAMMAR).unwrap();
    assert!(parse_accepted(&g, "", &Context::new()));
}

#[test]
fn empty_prefix_always_accepted_fun() {
    let g = load_example_grammar("fun");
    assert!(parse_accepted(&g, "", &Context::new()));
}

// ── 2. Prefix monotonicity ────────────────────────────────────────────────
//
// Strategy: generate arithmetic expressions from a small token alphabet,
// check that every token-boundary prefix is also accepted.

prop_compose! {
    fn arith_input()(
        nums in prop::collection::vec(1u32..=99, 1..=6),
        ops in prop::collection::vec(prop::sample::select(vec!["+", "-", "*", "/"]), 0..=5),
    ) -> String {
        let terms: usize = nums.len();
        let op_count = ops.len().min(terms.saturating_sub(1));
        let mut parts = Vec::new();
        for i in 0..terms {
            parts.push(nums[i].to_string());
            if i < op_count {
                parts.push(ops[i].to_string());
            }
        }
        parts.join(" ")
    }
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(512))]

    /// Prefix monotonicity on arithmetic expressions.
    #[test]
    fn arith_prefix_monotone(input in arith_input()) {
        let mut g = SPG::load(ARITHMETIC_GRAMMAR).unwrap();
        let ctx = Context::new();
        // Only assert monotonicity when the full input is parseable.
        if parse_accepted(&g, &input, &ctx) {
            let result = check_all_prefixes_parseable(&mut g, &input, &ctx);
            prop_assert!(
                result.is_pass(),
                "prefix monotonicity violated for input {:?}",
                input
            );
        }
    }
}

prop_compose! {
    /// n-argument STLC application chain with a matching well-typed context.
    fn stlc_chain_input()(n in 1usize..=6) -> (String, Context) {
        let type_names: Vec<_> = (0..=n).map(|i| format!("T{i}")).collect();
        let mut f_ty = type_names[n].clone();
        for i in (0..n).rev() {
            f_ty = format!("{}->{}", type_names[i], f_ty);
        }
        let mut ctx = Context::new();
        ctx.add("f".to_string(), Type::parse_raw(&f_ty).unwrap());
        for i in 0..n {
            ctx.add(format!("x{i}"), Type::parse_raw(&type_names[i]).unwrap());
        }
        let input = std::iter::once("f".to_string())
            .chain((0..n).map(|i| format!("x{i}")))
            .collect::<Vec<_>>()
            .join(" ");
        (input, ctx)
    }
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(60))]

    /// Prefix monotonicity on STLC application chains.
    #[test]
    fn stlc_chain_prefix_monotone((input, ctx) in stlc_chain_input()) {
        let mut g = load_example_grammar("stlc");
        let result = check_all_prefixes_parseable(&mut g, &input, &ctx);
        prop_assert!(
            result.is_pass(),
            "prefix monotonicity violated for stlc chain {:?}",
            input
        );
    }
}

// ── 3. Feed–parse agreement ───────────────────────────────────────────────
//
// Replaying the token stream through incremental feed must remain sound.

/// Simple feed-replay: tokenize, feed each token, verify success at each step.
fn feed_replay_ok(g: &SPG, input: &str, ctx: &Context) -> bool {
    let mut grammar = g.clone();
    let tokens: Vec<String> = match grammar.tokenize(input) {
        Ok(segs) => segs.into_iter().map(|s| s.text().to_string()).collect(),
        Err(_) => return false,
    };
    let mut built = String::new();
    for token in &tokens {
        if built.is_empty() {
            built = token.clone();
        } else {
            built.push(' ');
            built.push_str(token);
        }
        let mut s = TypingSynth::new(g.clone(), built.clone());
        if s.parse_with(ctx).is_err() {
            return false;
        }
    }
    true
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(60))]

    /// Feed–parse agreement on STLC chains.
    #[test]
    fn stlc_chain_feed_agrees_with_parse((input, ctx) in stlc_chain_input()) {
        let g = load_example_grammar("stlc");
        let ok = feed_replay_ok(&g, &input, &ctx);
        prop_assert!(
            ok,
            "feed-parse disagreement on input {:?}",
            input,
        );
    }
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(80))]

    /// Feed–parse agreement on arithmetic expressions.
    #[test]
    fn arith_feed_agrees_with_parse(input in arith_input()) {
        let g = SPG::load(ARITHMETIC_GRAMMAR).unwrap();
        let ctx = Context::new();
        if parse_accepted(&g, &input, &ctx) {
            let ok = feed_replay_ok(&g, &input, &ctx);
            prop_assert!(
                ok,
                "feed-parse disagreement on input {:?}",
                input,
            );
        }
    }
}

// ── 4. Rejection stability ────────────────────────────────────────────────
//
// Well-known syntactically invalid inputs must stay rejected even when the
// typing context is extended with arbitrary bindings.
// This is a bit dubious when you think about it, because start context extesion doesnt obey mnotonicity
// passing is only due to the staticity of values

prop_compose! {
    fn random_context()(
        names in prop::collection::vec("[a-z]{1,3}", 0..=4),
    ) -> Context {
        let mut ctx = Context::new();
        for (i, name) in names.into_iter().enumerate() {
            let ty_str = if i % 2 == 0 { "Int" } else { "Bool" };
            if let Ok(ty) = Type::parse_raw(ty_str) {
                ctx.add(name, ty);
            }
        }
        ctx
    }
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(80))]

    #[test]
    fn arith_invalid_stays_rejected_under_extended_ctx(ctx in random_context()) {
        let g = SPG::load(ARITHMETIC_GRAMMAR).unwrap();
        // These are hardcoded ill-formed inputs — not parseable by design.
        let invalid_inputs = [
            "+ 1",
            "1 + + 2",
            ")",
            "( )",
            "1 2",
        ];
        for input in invalid_inputs {
            let result = check_parse_fails(&g, input, &ctx);
            prop_assert!(
                result.is_pass(),
                "invalid input {:?} unexpectedly accepted under context {:?}",
                input,
                ctx
            );
        }
    }
}
