//! Empirical chart-growth benchmarks.
//!
//! Sweeps input size N and measures parse time and arena node count.
//! Run with:
//!   cargo bench --bench chart_growth
//!   cargo bench --bench chart_growth -- --save-baseline <tag>
//!
//! HTML reports land in target/criterion/.

use aufbau::domains::typing::{Context, Type, TypingDomain, TypingSynth};
use aufbau::engine::grammar::SPG;
use aufbau::validation::parseable::arithmetic::ARITHMETIC_GRAMMAR;
use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main};
use std::time::Duration;

// ── Input generators ──────────────────────────────────────────────────────────

fn stlc_chain(n: usize) -> (String, Context) {
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

fn imp_block(n: usize) -> String {
    let mut parts = vec!["{".to_string()];
    for i in 0..n {
        parts.extend([
            "let".to_string(),
            format!("v{i}"),
            ":".to_string(),
            "Int".to_string(),
            "=".to_string(),
            (i + 1).to_string(),
            ";".to_string(),
        ]);
    }
    parts.push("}".to_string());
    parts.join(" ")
}

fn arith_flat(n: usize) -> String {
    (1..=n.max(1))
        .map(|i| i.to_string())
        .collect::<Vec<_>>()
        .join(" + ")
}

fn arith_nested(n: usize) -> String {
    format!("{}{}{}", "(".repeat(n), 1, ")".repeat(n))
}

// ── Benchmarks ────────────────────────────────────────────────────────────────

fn parse_with_ctx(grammar: &SPG<TypingDomain>, input: &str, ctx: &Context) -> usize {
    let mut synth = TypingSynth::new(grammar.clone(), input);
    synth.parse_with(ctx).unwrap().node_count()
}

fn bench_stlc_chain(c: &mut Criterion) {
    let grammar = SPG::<TypingDomain>::load(include_str!("../examples/stlc.auf")).unwrap();
    let mut group = c.benchmark_group("stlc/application_chain");
    group.measurement_time(Duration::from_secs(8));
    for n in 1..=8usize {
        let (input, ctx) = stlc_chain(n);
        group.bench_with_input(BenchmarkId::from_parameter(n), &n, |b, _| {
            b.iter(|| parse_with_ctx(&grammar, &input, &ctx))
        });
    }
    group.finish();
}

fn bench_imp_block(c: &mut Criterion) {
    let grammar = SPG::<TypingDomain>::load(include_str!("../examples/imp.auf")).unwrap();
    let mut group = c.benchmark_group("imp/declaration_block");
    group.measurement_time(Duration::from_secs(8));
    for n in 1..=8usize {
        let input = imp_block(n);
        group.bench_with_input(BenchmarkId::from_parameter(n), &n, |b, _| {
            b.iter(|| parse_with_ctx(&grammar, &input, &Context::new()))
        });
    }
    group.finish();
}

fn bench_arith_flat(c: &mut Criterion) {
    let grammar = SPG::<TypingDomain>::load(ARITHMETIC_GRAMMAR).unwrap();
    let mut group = c.benchmark_group("arith/flat_sum");
    group.measurement_time(Duration::from_secs(8));
    for n in 1..=10usize {
        let input = arith_flat(n);
        group.bench_with_input(BenchmarkId::from_parameter(n), &n, |b, _| {
            b.iter(|| parse_with_ctx(&grammar, &input, &Context::new()))
        });
    }
    group.finish();
}

fn bench_arith_nested(c: &mut Criterion) {
    let grammar = SPG::<TypingDomain>::load(ARITHMETIC_GRAMMAR).unwrap();
    let mut group = c.benchmark_group("arith/nested_parens");
    group.measurement_time(Duration::from_secs(8));
    for n in 1..=8usize {
        let input = arith_nested(n);
        group.bench_with_input(BenchmarkId::from_parameter(n), &n, |b, _| {
            b.iter(|| parse_with_ctx(&grammar, &input, &Context::new()))
        });
    }
    group.finish();
}

criterion_group!(
    benches,
    bench_stlc_chain,
    bench_imp_block,
    bench_arith_flat,
    bench_arith_nested
);
criterion_main!(benches);
