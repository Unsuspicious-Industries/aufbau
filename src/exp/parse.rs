use crate::exp::safe::{self, SafeLimits};
use crate::logic::grammar::Grammar;
use crate::logic::partial::memo::clear_shared_memo;
use crate::logic::partial::{
    global_cache_stats, grammar_cache_stats, input_cache_entries, reset_global_store, Parser,
    ParserStats, Synthesizer,
};
use crate::logic::typing::Context;
use serde::Serialize;
use serde_json::{json, Value};
use std::fs;
use std::path::{Path, PathBuf};
use std::time::{Instant, SystemTime, UNIX_EPOCH};

type Suite = (&'static str, Grammar, usize, fn(usize) -> String);

#[derive(Debug, Clone)]
pub struct ExpConfig {
    pub include_safe: bool,
    pub max_n: usize,
    pub include_standard: bool,
    pub include_incremental: bool,
    pub max_prefixes: usize,
    pub include_drivers: bool,
    pub safe_max_steps: usize,
    pub safe_only: Option<String>,
    pub safe_max_step_ms: u64,
    pub safe_max_rss_kb: u64,
    pub output: Option<PathBuf>,
}

impl Default for ExpConfig {
    fn default() -> Self {
        Self {
            include_safe: true,
            max_n: 2,
            include_standard: false,
            include_incremental: false,
            max_prefixes: 2,
            include_drivers: true,
            safe_max_steps: 6,
            safe_only: None,
            safe_max_step_ms: 250,
            safe_max_rss_kb: 256 * 1024,
            output: None,
        }
    }
}

#[derive(Debug, Clone, Serialize)]
struct Sample {
    function: String,
    suite: String,
    mode: String,
    n: usize,
    input_len: usize,
    elapsed_us: u128,
    cache_entries: usize,
    node_count: usize,
    total_alternatives: usize,
    max_alternatives: usize,
    stats: Value,
}

pub fn run(config: ExpConfig) -> Value {
    let safe_report = config.include_safe.then(|| {
        safe::run(
            SafeLimits {
                max_steps: config.safe_max_steps.max(1),
                max_step_ms: config.safe_max_step_ms.max(1),
                max_rss_kb: config.safe_max_rss_kb.max(1),
            },
            config.safe_only.as_deref(),
        )
    });

    let samples = config
        .include_standard
        .then(|| {
            suites()
                .iter()
                .flat_map(|suite| run_suite(suite, config.max_n))
                .collect::<Vec<_>>()
        })
        .into_iter()
        .flatten()
        .chain(
            config
                .include_drivers
                .then(driver_samples)
                .into_iter()
                .flatten(),
        )
        .chain(
            (config.include_incremental)
                .then(|| {
                    incremental_suites()
                        .iter()
                        .flat_map(|suite| {
                            run_incremental_suite(suite, config.max_n, config.max_prefixes)
                        })
                        .collect::<Vec<_>>()
                })
                .into_iter()
                .flatten(),
        )
        .collect::<Vec<_>>();

    let summaries = summarize(&samples);
    let report = json!({
        "generated_at": now_unix(),
        "safe": safe_report,
        "samples": config.output.as_ref().map(|_| samples.clone()).unwrap_or_default(),
        "summaries": summaries,
        "global_cache": global_cache_stats(),
        "per_grammar_cache": grammar_cache_stats(),
        "input_cache_entries": config.output.as_ref().map(|_| input_cache_entries()).unwrap_or_default(),
    });

    if let Some(path) = &config.output {
        write_report(path, &report);
    }

    print_report(&report);
    report
}

fn suites() -> Vec<Suite> {
    vec![
        (
            "stlc_app_chain",
            load_example_grammar("stlc"),
            96,
            generate_stlc_app_chain,
        ),
        (
            "fun_let_chain",
            load_example_grammar("fun"),
            96,
            generate_fun_let_chain,
        ),
    ]
}

fn run_suite((name, grammar, max_recursion, mk_input): &Suite, max_n: usize) -> Vec<Sample> {
    (1..=max_n)
        .flat_map(|n| {
            let input = mk_input(n);
            [
                measure(
                    name,
                    grammar,
                    *max_recursion,
                    "cold",
                    &input,
                    false,
                    false,
                    n,
                ),
                measure(
                    name,
                    grammar,
                    *max_recursion,
                    "warm_reparse",
                    &input,
                    true,
                    true,
                    n,
                ),
            ]
        })
        .collect()
}

fn measure(
    name: &str,
    grammar: &Grammar,
    max_recursion: usize,
    mode: &str,
    input: &str,
    preserve_cache: bool,
    reparse: bool,
    n: usize,
) -> Sample {
    reset_global_store();
    clear_shared_memo();

    let mut parser = Parser::new(grammar.clone())
        .with_max_recursion(max_recursion)
        .with_preserve_cache_across_parses(preserve_cache);

    if reparse {
        let _ = parser.partial(input);
    }

    let started = Instant::now();
    let outcome = parser.partial(input);
    let elapsed_us = started.elapsed().as_micros();

    let (node_count, total_alternatives, max_alternatives) = outcome
        .ast()
        .map(|ast| {
            (
                ast.node_count(),
                ast.total_alternatives(),
                ast.max_alternatives(),
            )
        })
        .unwrap_or((0, 0, 0));
    let global = global_cache_stats();

    Sample {
        function: "parser.partial".to_string(),
        suite: name.to_string(),
        mode: mode.to_string(),
        n,
        input_len: input.len(),
        elapsed_us,
        cache_entries: parser.cache_entry_count(),
        node_count,
        total_alternatives,
        max_alternatives,
        stats: json!({
            "parser": parser_stats_json(parser.last_stats()),
            "global_nodes": global.total_nodes,
            "global_duplicates": global.duplicate_nodes,
        }),
    }
}

fn incremental_suites() -> Vec<Suite> {
    vec![(
        "fun_incremental_feed",
        load_example_grammar("fun"),
        96,
        generate_fun_let_chain,
    )]
}

fn driver_samples() -> Vec<Sample> {
    let ctx = Context::new();
    let fun = load_example_grammar("fun");
    let imp = load_example_grammar("imp");

    let mut out = Vec::new();
    out.extend(measure_driver_chain(
        "fun_operator_prefix",
        &fun,
        96,
        "1.0 +.",
        &ctx,
    ));
    out.extend(measure_driver_chain(
        "imp_declaration_prefix",
        &imp,
        96,
        "{ let x:Int = 1; x",
        &ctx,
    ));
    out
}

fn measure_driver_chain(
    suite: &str,
    grammar: &Grammar,
    max_recursion: usize,
    input: &str,
    ctx: &Context,
) -> Vec<Sample> {
    let mut out = Vec::new();
    out.push(measure_driver_step(suite, "parser.partial", input, || {
        reset_global_store();
        clear_shared_memo();
        let mut parser = Parser::new(grammar.clone()).with_max_recursion(max_recursion);
        let outcome = parser.partial(input);
        let (node_count, total_alternatives, max_alternatives) = outcome
            .ast()
            .map(|ast| {
                (
                    ast.node_count(),
                    ast.total_alternatives(),
                    ast.max_alternatives(),
                )
            })
            .unwrap_or((0, 0, 0));

        let stats = json!({
            "parser": parser_stats_json(parser.last_stats()),
            "cache_entries": parser.cache_entry_count(),
        });

        DriverStep {
            node_count,
            total_alternatives,
            max_alternatives,
            cache_entries: parser.cache_entry_count(),
            stats,
        }
    }));

    out.push(measure_driver_step(suite, "synth.feed", input, || {
        reset_global_store();
        clear_shared_memo();
        let mut synth = Synthesizer::new_with_max_depth(grammar.clone(), "", max_recursion);
        let tokens = synth.feed(input.to_string(), ctx);
        let (parse_hits, parse_misses, typed_hits, typed_misses) = synth.memo_stats();
        let (parse_cached, typed_node_count, approx_size) = synth.cache_stats();
        DriverStep {
            node_count: typed_node_count,
            total_alternatives: tokens.len(),
            max_alternatives: approx_size,
            cache_entries: synth.memo_entry_count(),
            stats: json!({
                "parse_memo_hits": parse_hits,
                "parse_memo_misses": parse_misses,
                "typed_memo_hits": typed_hits,
                "typed_memo_misses": typed_misses,
                "parse_cached_inputs": parse_cached,
                "completion_count": tokens.len(),
                "approx_cache_size": approx_size,
            }),
        }
    }));

    out.push(measure_driver_step(
        suite,
        "synth.partial_typed_ctx",
        input,
        || {
            reset_global_store();
            clear_shared_memo();
            let mut synth = Synthesizer::new_with_max_depth(grammar.clone(), input, max_recursion);
            let typed = synth.partial_typed_ctx(ctx).ok();
            let typed_roots = typed.as_ref().map(|tree| tree.len()).unwrap_or(0);
            let typed_complete = typed
                .as_ref()
                .map(|tree| usize::from(tree.is_complete()))
                .unwrap_or(0);
            let (parse_cached, typed_node_count, approx_size) = synth.cache_stats();
            DriverStep {
                node_count: typed_node_count,
                total_alternatives: typed_roots,
                max_alternatives: approx_size,
                cache_entries: synth.memo_entry_count(),
                stats: json!({
                    "typed_roots": typed_roots,
                    "typed_complete": typed_complete,
                    "parse_cached_inputs": parse_cached,
                    "approx_cache_size": approx_size,
                }),
            }
        },
    ));

    out.push(measure_driver_step(
        suite,
        "synth.completions_ctx",
        input,
        || {
            reset_global_store();
            clear_shared_memo();
            let mut synth = Synthesizer::new_with_max_depth(grammar.clone(), input, max_recursion);
            let tokens = synth.completions_ctx(ctx);
            let (parse_hits, parse_misses, typed_hits, typed_misses) = synth.memo_stats();
            let (parse_cached, typed_node_count, approx_size) = synth.cache_stats();
            DriverStep {
                node_count: typed_node_count,
                total_alternatives: tokens.len(),
                max_alternatives: approx_size,
                cache_entries: synth.memo_entry_count(),
                stats: json!({
                    "parse_memo_hits": parse_hits,
                    "parse_memo_misses": parse_misses,
                    "typed_memo_hits": typed_hits,
                    "typed_memo_misses": typed_misses,
                    "parse_cached_inputs": parse_cached,
                    "completion_count": tokens.len(),
                    "approx_cache_size": approx_size,
                }),
            }
        },
    ));

    out
}

#[derive(Debug, Clone)]
struct DriverStep {
    node_count: usize,
    total_alternatives: usize,
    max_alternatives: usize,
    cache_entries: usize,
    stats: Value,
}

fn measure_driver_step(
    suite: &str,
    function: &str,
    input: &str,
    f: impl FnOnce() -> DriverStep,
) -> Sample {
    let rss_before = current_rss_kb();
    let hwm_before = current_hwm_kb();
    let started = Instant::now();
    let step = f();
    let elapsed_us = started.elapsed().as_micros();
    let rss_after = current_rss_kb();
    let hwm_after = current_hwm_kb();
    let global = global_cache_stats();

    Sample {
        function: function.to_string(),
        suite: suite.to_string(),
        mode: "driver".to_string(),
        n: 1,
        input_len: input.len(),
        elapsed_us,
        cache_entries: step.cache_entries,
        node_count: step.node_count,
        total_alternatives: step.total_alternatives,
        max_alternatives: step.max_alternatives,
        stats: json!({
            "driver": step.stats,
            "rss_before_kb": rss_before,
            "rss_after_kb": rss_after,
            "rss_delta_kb": rss_after.saturating_sub(rss_before),
            "hwm_before_kb": hwm_before,
            "hwm_after_kb": hwm_after,
            "hwm_delta_kb": hwm_after.saturating_sub(hwm_before),
            "global_nodes": global.total_nodes,
            "global_duplicates": global.duplicate_nodes,
        }),
    }
}

fn run_incremental_suite(
    (name, grammar, max_recursion, mk_input): &Suite,
    max_n: usize,
    max_prefixes: usize,
) -> Vec<Sample> {
    let ctx = Context::new();
    let input = mk_input(max_n.max(1));
    let prefixes = sampled_prefixes(
        token_boundary_prefixes(grammar, &input),
        max_prefixes.max(1),
    );

    prefixes
        .iter()
        .enumerate()
        .flat_map(|(idx, prefix)| {
            [
                measure_feed(
                    name,
                    grammar,
                    *max_recursion,
                    prefix,
                    idx + 1,
                    &ctx,
                    false,
                    false,
                ),
                measure_feed(
                    name,
                    grammar,
                    *max_recursion,
                    prefix,
                    idx + 1,
                    &ctx,
                    true,
                    false,
                ),
                measure_feed(
                    name,
                    grammar,
                    *max_recursion,
                    prefix,
                    idx + 1,
                    &ctx,
                    true,
                    true,
                ),
            ]
        })
        .chain(measure_reused_feed_sequence(
            name,
            grammar,
            *max_recursion,
            &prefixes,
            &ctx,
        ))
        .collect()
}

fn measure_reused_feed_sequence(
    name: &str,
    grammar: &Grammar,
    max_recursion: usize,
    prefixes: &[String],
    ctx: &Context,
) -> Vec<Sample> {
    reset_global_store();
    clear_shared_memo();

    let mut synth = Synthesizer::new_with_max_depth(grammar.clone(), "", max_recursion);

    prefixes
        .iter()
        .enumerate()
        .map(|(idx, prefix)| {
            let started = Instant::now();
            let tokens = synth.feed(prefix.clone(), ctx);
            let elapsed_us = started.elapsed().as_micros();
            let (parse_hits, parse_misses, typed_hits, typed_misses) = synth.memo_stats();
            let (parse_cached, typed_node_count, approx_size) = synth.cache_stats();
            let global = global_cache_stats();

            Sample {
                function: "synth.feed".to_string(),
                suite: name.to_string(),
                mode: "reuse_prefix_feed".to_string(),
                n: idx + 1,
                input_len: prefix.len(),
                elapsed_us,
                cache_entries: synth.memo_entry_count(),
                node_count: typed_node_count,
                total_alternatives: tokens.len(),
                max_alternatives: approx_size,
                stats: json!({
                    "parse_memo_hits": parse_hits,
                    "parse_memo_misses": parse_misses,
                    "typed_memo_hits": typed_hits,
                    "typed_memo_misses": typed_misses,
                    "parse_cached_inputs": parse_cached,
                    "completion_count": tokens.len(),
                    "approx_cache_size": approx_size,
                    "global_nodes": global.total_nodes,
                    "global_duplicates": global.duplicate_nodes,
                }),
            }
        })
        .collect()
}

fn measure_feed(
    name: &str,
    grammar: &Grammar,
    max_recursion: usize,
    prefix: &str,
    n: usize,
    ctx: &Context,
    warm: bool,
    hot: bool,
) -> Sample {
    reset_global_store();
    clear_shared_memo();

    let mut synth = Synthesizer::new_with_max_depth(grammar.clone(), "", max_recursion);

    if warm {
        let _ = synth.feed(prefix.to_string(), ctx);
    }
    if hot {
        let _ = synth.feed(prefix.to_string(), ctx);
    }

    let started = Instant::now();
    let tokens = synth.feed(prefix.to_string(), ctx);
    let elapsed_us = started.elapsed().as_micros();
    let (parse_hits, parse_misses, typed_hits, typed_misses) = synth.memo_stats();
    let (parse_cached, typed_node_count, approx_size) = synth.cache_stats();
    let global = global_cache_stats();

    Sample {
        function: "synth.feed".to_string(),
        suite: name.to_string(),
        mode: if hot {
            "hot_feed"
        } else if warm {
            "warm_feed"
        } else {
            "cold_feed"
        }
        .to_string(),
        n,
        input_len: prefix.len(),
        elapsed_us,
        cache_entries: synth.memo_entry_count(),
        node_count: typed_node_count,
        total_alternatives: tokens.len(),
        max_alternatives: approx_size,
        stats: json!({
            "parse_memo_hits": parse_hits,
            "parse_memo_misses": parse_misses,
            "typed_memo_hits": typed_hits,
            "typed_memo_misses": typed_misses,
            "parse_cached_inputs": parse_cached,
            "completion_count": tokens.len(),
            "approx_cache_size": approx_size,
            "global_nodes": global.total_nodes,
            "global_duplicates": global.duplicate_nodes,
        }),
    }
}

fn summarize(samples: &[Sample]) -> Vec<Value> {
    let mut groups = std::collections::BTreeMap::<(String, String, String), Vec<&Sample>>::new();

    for sample in samples {
        groups
            .entry((
                sample.function.clone(),
                sample.suite.clone(),
                sample.mode.clone(),
            ))
            .or_default()
            .push(sample);
    }

    groups
        .into_iter()
        .map(|((function, suite, mode), group)| {
            let elapsed = collect(&group, |sample| sample.elapsed_us as f64);
            let nodes = collect(&group, |sample| sample.node_count as f64);
            let alternatives = collect(&group, |sample| sample.total_alternatives as f64);
            let clones = collect(&group, |sample| stat(sample, "clone_events"));

            let nt_hits = group
                .iter()
                .map(|sample| {
                    stat(sample, "nt_cache_hits") as usize
                        + stat(sample, "parse_memo_hits") as usize
                })
                .sum::<usize>();
            let nt_lookups = group
                .iter()
                .map(|sample| {
                    stat(sample, "nt_cache_hits") as usize
                        + stat(sample, "nt_cache_misses") as usize
                        + stat(sample, "parse_memo_hits") as usize
                        + stat(sample, "parse_memo_misses") as usize
                })
                .sum::<usize>();
            let suffix_hits = group
                .iter()
                .map(|sample| {
                    stat(sample, "suffix_cache_hits") as usize
                        + stat(sample, "typed_memo_hits") as usize
                })
                .sum::<usize>();
            let suffix_lookups = group
                .iter()
                .map(|sample| {
                    stat(sample, "suffix_cache_hits") as usize
                        + stat(sample, "suffix_cache_misses") as usize
                        + stat(sample, "typed_memo_hits") as usize
                        + stat(sample, "typed_memo_misses") as usize
                })
                .sum::<usize>();

            json!({
                "function": function,
                "suite": suite,
                "mode": mode,
                "samples": group.len(),
                "mean_us": mean(&elapsed),
                "mean_nodes": mean(&nodes),
                "mean_alternatives": mean(&alternatives),
                "mean_clone_events": mean(&clones),
                "cache_hit_rate": ratio(nt_hits, nt_lookups),
                "suffix_hit_rate": ratio(suffix_hits, suffix_lookups),
                "time_vs_nodes": correlation(&elapsed, &nodes),
                "time_vs_alternatives": correlation(&elapsed, &alternatives),
                "time_vs_clones": correlation(&elapsed, &clones),
            })
        })
        .collect()
}

fn collect<T>(group: &[&Sample], f: impl Fn(&Sample) -> T) -> Vec<T> {
    group.iter().map(|sample| f(sample)).collect()
}

fn stat(sample: &Sample, key: &str) -> f64 {
    sample.stats[key]
        .as_f64()
        .or_else(|| sample.stats[key].as_u64().map(|value| value as f64))
        .or_else(|| sample.stats["parser"][key].as_f64())
        .or_else(|| {
            sample.stats["parser"][key]
                .as_u64()
                .map(|value| value as f64)
        })
        .unwrap_or(0.0)
}

fn parser_stats_json(stats: &ParserStats) -> Value {
    json!({
        "nt_cache_hits": stats.nt_cache_hits,
        "nt_cache_misses": stats.nt_cache_misses,
        "nt_cache_stores": stats.nt_cache_stores,
        "suffix_cache_hits": stats.suffix_cache_hits,
        "suffix_cache_misses": stats.suffix_cache_misses,
        "cycle_cuts": stats.cycle_cuts,
        "clone_events": stats.clone_events,
    })
}

fn mean(values: &[f64]) -> f64 {
    (!values.is_empty())
        .then(|| values.iter().sum::<f64>() / values.len() as f64)
        .unwrap_or(0.0)
}

fn ratio(numerator: usize, denominator: usize) -> f64 {
    (denominator > 0)
        .then(|| numerator as f64 / denominator as f64)
        .unwrap_or(0.0)
}

fn correlation(xs: &[f64], ys: &[f64]) -> f64 {
    if xs.len() != ys.len() || xs.len() < 2 {
        return 0.0;
    }

    let mean_x = mean(xs);
    let mean_y = mean(ys);
    let (num, den_x, den_y) =
        xs.iter()
            .zip(ys.iter())
            .fold((0.0, 0.0, 0.0), |(num, den_x, den_y), (x, y)| {
                let dx = x - mean_x;
                let dy = y - mean_y;
                (num + dx * dy, den_x + dx * dx, den_y + dy * dy)
            });

    let den = den_x.sqrt() * den_y.sqrt();
    (den > f64::EPSILON).then(|| num / den).unwrap_or(0.0)
}

fn write_report(path: &Path, report: &Value) {
    if let Some(parent) = path.parent() {
        let _ = fs::create_dir_all(parent);
    }

    let file = fs::File::create(path).expect("failed to create experiment report");
    serde_json::to_writer_pretty(file, report).expect("failed to write experiment report");
}

fn current_rss_kb() -> u64 {
    proc_status_kb("VmRSS:")
}

fn current_hwm_kb() -> u64 {
    proc_status_kb("VmHWM:")
}

fn proc_status_kb(prefix: &str) -> u64 {
    fs::read_to_string("/proc/self/status")
        .ok()
        .and_then(|status| {
            status
                .lines()
                .find(|line| line.starts_with(prefix))
                .and_then(|line| line.split_whitespace().nth(1))
                .and_then(|value| value.parse::<u64>().ok())
        })
        .unwrap_or(0)
}

fn print_report(report: &Value) {
    if !report["safe"].is_null() {
        safe::print(&report["safe"]);
    }

    println!("== experiments ==");

    report["summaries"]
        .as_array()
        .into_iter()
        .flatten()
        .for_each(|summary| {
            println!(
                "{} {:>12} mean={:>8.1}us cache={:>5.1}% suffix={:>5.1}% nodes~{:>6.1} alts~{:>6.1} clones~{:>7.1}",
                format!(
                    "{}:{}",
                    summary["function"].as_str().unwrap_or("?"),
                    summary["suite"].as_str().unwrap_or("?")
                ),
                summary["mode"].as_str().unwrap_or("?"),
                summary["mean_us"].as_f64().unwrap_or(0.0),
                summary["cache_hit_rate"].as_f64().unwrap_or(0.0) * 100.0,
                summary["suffix_hit_rate"].as_f64().unwrap_or(0.0) * 100.0,
                summary["mean_nodes"].as_f64().unwrap_or(0.0),
                summary["mean_alternatives"].as_f64().unwrap_or(0.0),
                summary["mean_clone_events"].as_f64().unwrap_or(0.0),
            );
            println!(
                "  corr(time,nodes)={:+.2} corr(time,alts)={:+.2} corr(time,clones)={:+.2}",
                summary["time_vs_nodes"].as_f64().unwrap_or(0.0),
                summary["time_vs_alternatives"].as_f64().unwrap_or(0.0),
                summary["time_vs_clones"].as_f64().unwrap_or(0.0),
            );
        });

    println!(
        "global cache nodes={} unique={} duplicates={} input-cache={}",
        report["global_cache"]["total_nodes"].as_u64().unwrap_or(0),
        report["global_cache"]["unique_nodes"].as_u64().unwrap_or(0),
        report["global_cache"]["duplicate_nodes"]
            .as_u64()
            .unwrap_or(0),
        report["global_cache"]["input_cache_entries"]
            .as_u64()
            .unwrap_or(0),
    );
}

fn now_unix() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("system clock before unix epoch")
        .as_secs()
}

fn load_example_grammar(name: &str) -> Grammar {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("examples")
        .join(format!("{}.auf", name));
    let content = fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("Failed to read {}: {}", path.display(), e));
    Grammar::load(&content).unwrap_or_else(|e| panic!("Failed to load grammar '{}': {}", name, e))
}

fn generate_stlc_app_chain(n: usize) -> String {
    std::iter::once("f".to_string())
        .chain((0..n).map(|idx| format!("x{}", idx)))
        .collect::<Vec<_>>()
        .join(" ")
}

fn generate_fun_let_chain(n: usize) -> String {
    let prefix = (0..n)
        .map(|idx| format!("let x{}: Int = 1;", idx))
        .collect::<Vec<_>>()
        .join(" ");

    (n == 0)
        .then(|| "1".to_string())
        .unwrap_or_else(|| format!("{} x{}", prefix, n - 1))
}

fn token_boundary_prefixes(grammar: &Grammar, input: &str) -> Vec<String> {
    grammar
        .tokenize(input)
        .map(|segments| {
            let mut cuts = vec![0usize];
            cuts.extend(segments.iter().map(|segment| segment.end));
            if !cuts.contains(&input.len()) {
                cuts.push(input.len());
            }
            cuts.sort_unstable();
            cuts.dedup();
            cuts.into_iter()
                .map(|end| input[..end].to_string())
                .collect()
        })
        .unwrap_or_else(|_| {
            (0..=input.len())
                .map(|end| input[..end].to_string())
                .collect()
        })
}

fn sampled_prefixes(prefixes: Vec<String>, keep: usize) -> Vec<String> {
    if keep < 2 || prefixes.len() <= keep {
        return prefixes;
    }

    (0..keep)
        .map(|idx| idx * (prefixes.len() - 1) / (keep - 1))
        .map(|idx| prefixes[idx].clone())
        .collect()
}
