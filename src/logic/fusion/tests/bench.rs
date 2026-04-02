use crate::logic::fusion::{MetaTypedParser, RuleRuntime, TypedParser};
use crate::logic::typing::{Context, Type};

use serde_json::json;
use std::time::Instant;

fn bench_one(name: &str, grammar_name: &str, input: &str, max_depth: u16, ctx: Context) {
    let grammar = crate::testing::load_example_grammar(grammar_name);
    let runtime = RuleRuntime::new(grammar.clone());
    let ctx_id = runtime.intern_context(ctx.clone());

    let parser = TypedParser::new(grammar.clone(), runtime.clone()).with_max_depth(max_depth);
    let meta = MetaTypedParser::new(parser)
        .with_start_depth(4)
        .with_max_depth(max_depth);

    let t0 = Instant::now();
    let parsed = meta.parse_with_arena(input, ctx_id);
    let parse_ms = t0.elapsed().as_millis();

    // Completion pass (same grammar/input) via Synthesizer, to track completion token count.
    let mut synth = crate::logic::fusion::Synthesizer::new_with_max_depth(
        grammar.clone(),
        input,
        max_depth as usize,
    );
    let t1 = Instant::now();
    let _ = synth.parse_with(&ctx).unwrap();
    let tokens = synth.tokens_with(&ctx);
    let completion_ms = t1.elapsed().as_millis();

    let (ok, depth, roots, arena_nodes, arena_alts, err) = match parsed {
        Ok((state, depth, arena)) => (
            true,
            Some(depth),
            Some(state.roots.len()),
            Some(arena.node_count()),
            Some(arena.alt_count()),
            None,
        ),
        Err(e) => (false, None, None, None, None, Some(e.to_string())),
    };

    eprintln!(
        "{}",
        json!({
            "event": "fusion_bench",
            "name": name,
            "grammar": grammar_name,
            "input_len": input.len(),
            "max_depth": max_depth,
            "ok": ok,
            "iter_depth_used": depth,
            "roots": roots,
            "arena_nodes": arena_nodes,
            "arena_alts": arena_alts,
            "interned_types": runtime.interned_type_count(),
            "interned_contexts": runtime.interned_context_count(),
            "parse_ms": parse_ms,
            "completion_tokens": tokens.len(),
            "completion_ms": completion_ms,
            "error": err,
        })
    );
}

/// Micro-benchmarks for fusion parser/runtime.
///
/// Run with:
/// `cargo test -q logic::fusion::tests::bench -- --ignored --nocapture`
#[test]
#[ignore = "benchmark"]
fn bench_fusion_examples() {
    bench_one("stlc_small", "stlc", "λx:A.x", 64, Context::new());

    let mut stlc_ctx = Context::new();
    stlc_ctx.add("f".to_string(), Type::parse_raw("A->B->C").unwrap());
    stlc_ctx.add("x".to_string(), Type::parse_raw("A").unwrap());
    stlc_ctx.add("y".to_string(), Type::parse_raw("B").unwrap());
    bench_one("stlc_app_chain", "stlc", "f x y", 64, stlc_ctx);

    bench_one(
        "fun_let",
        "fun",
        "let x: Int = 1; x + 2",
        64,
        Context::new(),
    );
    bench_one(
        "imp_block_prefix",
        "imp",
        "{ let x:Int=5; let y:Int=x",
        64,
        Context::new(),
    );
}
