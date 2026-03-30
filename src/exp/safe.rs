use crate::logic::grammar::Grammar;
use crate::logic::partial::structure::{
    PackedAlternative, SppfChild, SppfForest, SppfNode, Terminal,
};
use crate::logic::partial::{
    global_cache_stats, reset_global_store, MetaParser, Parser, Synthesizer,
};
use crate::logic::typing::tree::{TypedAST, TypedNode};
use crate::logic::typing::Context;
use serde_json::{json, Value};
use std::fs;
use std::mem::size_of;
use std::path::Path;
use std::time::Instant;

#[derive(Debug, Clone, Copy)]
pub struct SafeLimits {
    pub max_steps: usize,
    pub max_step_ms: u64,
    pub max_rss_kb: u64,
}

pub fn run(limits: SafeLimits, only: Option<&str>) -> Value {
    let fun = load_example_grammar("fun");
    let stlc = load_example_grammar("stlc");

    let include = |name: &str| only.is_none_or(|selected| selected == name);

    json!({
        "limits": {
            "max_steps": limits.max_steps,
            "max_step_ms": limits.max_step_ms,
            "max_rss_kb": limits.max_rss_kb,
        },
        "layout": layout_report(),
        "append_only_parser": include("parser").then(|| append_only_parser_compare(&stlc, &limits)),
        "repeat_same_parse": include("repeat").then(|| repeat_same_parse_growth(&fun, &limits)),
        "append_only_feed": include("feed").then(|| append_only_feed_profile(&fun, &limits)),
        "stage_breakdown": include("stage").then(|| stage_breakdown(&fun, &limits)),
    })
}

pub fn print(report: &Value) {
    println!("== safe experiments ==");
    println!(
        "layout sppf_node={} packed_alt={} sppf_child={} terminal={} typed_node={}",
        report["layout"]["sppf_node_bytes"].as_u64().unwrap_or(0),
        report["layout"]["packed_alternative_bytes"]
            .as_u64()
            .unwrap_or(0),
        report["layout"]["sppf_child_bytes"].as_u64().unwrap_or(0),
        report["layout"]["terminal_bytes"].as_u64().unwrap_or(0),
        report["layout"]["typed_node_bytes"].as_u64().unwrap_or(0),
    );

    if !report["append_only_parser"].is_null() {
        print_last_step("append_only_parser", &report["append_only_parser"]);
    }
    if !report["repeat_same_parse"].is_null() {
        print_last_step("repeat_same_parse", &report["repeat_same_parse"]);
    }
    if !report["append_only_feed"].is_null() {
        print_last_step("append_only_feed", &report["append_only_feed"]);
    }

    if let Some(stage) = report["stage_breakdown"].as_object() {
        println!(
            "stage_breakdown parse={}us type={}us raw_complete={}us forest_nodes={} typed_nodes={} raw_completions={}",
            stage["parse_elapsed_us"].as_u64().unwrap_or(0),
            stage["type_elapsed_us"].as_u64().unwrap_or(0),
            stage["raw_completion_elapsed_us"].as_u64().unwrap_or(0),
            stage["forest"]["nodes"].as_u64().unwrap_or(0),
            stage["typed"]["nodes"].as_u64().unwrap_or(0),
            stage["raw_completion_count"].as_u64().unwrap_or(0),
        );
    }
}

fn print_last_step(name: &str, report: &Value) {
    let aborted = report["aborted"].as_bool().unwrap_or(false);
    let steps = report["steps"].as_array().cloned().unwrap_or_default();
    let last = steps.last().cloned().unwrap_or_else(|| json!({}));
    println!(
        "{} steps={} aborted={} rss={}kb elapsed={}us nodes={} dups={}",
        name,
        steps.len(),
        aborted,
        last["rss_kb"].as_u64().unwrap_or(0),
        last["elapsed_us"].as_u64().unwrap_or(0),
        last["global_nodes"].as_u64().unwrap_or(0),
        last["global_duplicate_nodes"].as_u64().unwrap_or(0),
    );
}

fn layout_report() -> Value {
    json!({
        "sppf_node_bytes": size_of::<SppfNode>(),
        "packed_alternative_bytes": size_of::<PackedAlternative>(),
        "sppf_child_bytes": size_of::<SppfChild>(),
        "terminal_bytes": size_of::<Terminal>(),
        "typed_node_bytes": size_of::<TypedNode>(),
        "typed_ast_bytes": size_of::<TypedAST>(),
    })
}

fn append_only_parser_compare(grammar: &Grammar, limits: &SafeLimits) -> Value {
    reset_global_store();

    let prefixes = sampled_prefixes(
        token_boundary_prefixes(grammar, &generate_stlc_app_chain(2)),
        limits.max_steps,
    )
    .into_iter()
    .filter(|prefix| !prefix.trim().is_empty())
    .collect::<Vec<_>>();
    let mut steps = Vec::new();
    let mut aborted = false;

    let mut incremental = Parser::new(grammar.clone()).with_max_recursion(96);
    let mut prev = None;

    for prefix in prefixes {
        let started = Instant::now();

        let fresh_stats = {
            let mut parser = Parser::new(grammar.clone()).with_max_recursion(96);
            let _ = parser.prefix(&prefix);
            parser.last_stats().clone()
        };

        let next = match prev.take() {
            Some(state) => incremental.advance_owned(state, &prefix),
            None => incremental.prefix(&prefix),
        };
        let incremental_stats = incremental.last_stats().clone();
        if let Ok(prefix_state) = next {
            prev = Some(prefix_state);
        }

        let elapsed_us = started.elapsed().as_micros() as u64;
        let global = global_cache_stats();
        let rss_kb = current_rss_kb();
        steps.push(json!({
            "prefix": prefix,
            "elapsed_us": elapsed_us,
            "fresh_nt_cache_stores": fresh_stats.nt_cache_stores,
            "incremental_nt_cache_stores": incremental_stats.nt_cache_stores,
            "fresh_nt_cache_hits": fresh_stats.nt_cache_hits,
            "incremental_nt_cache_hits": incremental_stats.nt_cache_hits,
            "global_nodes": global.total_nodes,
            "global_duplicate_nodes": global.duplicate_nodes,
            "rss_kb": rss_kb,
        }));

        if should_abort(elapsed_us, rss_kb, limits) {
            aborted = true;
            break;
        }
    }

    json!({ "aborted": aborted, "steps": steps })
}

fn repeat_same_parse_growth(grammar: &Grammar, limits: &SafeLimits) -> Value {
    reset_global_store();

    let input = "let x0: Int = 1; x0".to_string();
    let mut steps = Vec::new();
    let mut aborted = false;

    for iter in 0..limits.max_steps {
        let started = Instant::now();
        let mut parser = MetaParser::new(grammar.clone()).with_max_depth(96);
        let forest = parser.partial(&input).ok();
        let elapsed_us = started.elapsed().as_micros() as u64;
        let global = global_cache_stats();
        let rss_kb = current_rss_kb();
        let forest_stats = forest
            .as_ref()
            .map(forest_shape)
            .unwrap_or_else(|| json!({}));

        steps.push(json!({
            "iteration": iter + 1,
            "elapsed_us": elapsed_us,
            "rss_kb": rss_kb,
            "global_nodes": global.total_nodes,
            "global_duplicate_nodes": global.duplicate_nodes,
            "forest": forest_stats,
        }));

        if should_abort(elapsed_us, rss_kb, limits) {
            aborted = true;
            break;
        }
    }

    json!({ "input": input, "aborted": aborted, "steps": steps })
}

fn append_only_feed_profile(grammar: &Grammar, limits: &SafeLimits) -> Value {
    reset_global_store();

    let input = "let x0: Int = 1".to_string();
    let prefixes = sampled_prefixes(token_boundary_prefixes(grammar, &input), limits.max_steps);
    let prefixes = prefixes
        .into_iter()
        .filter(|prefix| !prefix.trim().is_empty())
        .collect::<Vec<_>>();
    let ctx = Context::new();
    let mut synth = Synthesizer::new_with_max_depth(grammar.clone(), "", 96);
    let mut steps = Vec::new();
    let mut aborted = false;

    for prefix in prefixes {
        let started = Instant::now();
        let completions = synth.feed(prefix.clone(), &ctx);
        let elapsed_us = started.elapsed().as_micros() as u64;
        let (parse_hits, parse_misses, typed_hits, typed_misses) = synth.memo_stats();
        let (parse_cached_inputs, typed_nodes, approx_cache_size) = synth.cache_stats();
        let global = global_cache_stats();
        let rss_kb = current_rss_kb();

        steps.push(json!({
            "prefix": prefix,
            "elapsed_us": elapsed_us,
            "rss_kb": rss_kb,
            "completion_count": completions.len(),
            "parse_memo_hits": parse_hits,
            "parse_memo_misses": parse_misses,
            "typed_memo_hits": typed_hits,
            "typed_memo_misses": typed_misses,
            "parse_cached_inputs": parse_cached_inputs,
            "typed_nodes": typed_nodes,
            "approx_cache_size": approx_cache_size,
            "global_nodes": global.total_nodes,
            "global_duplicate_nodes": global.duplicate_nodes,
        }));

        if should_abort(elapsed_us, rss_kb, limits) {
            aborted = true;
            break;
        }
    }

    json!({ "input": input, "aborted": aborted, "steps": steps })
}

fn stage_breakdown(grammar: &Grammar, _limits: &SafeLimits) -> Value {
    reset_global_store();

    let input = "let x0: Int = 1; x";
    let ctx = Context::new();

    let parse_started = Instant::now();
    let mut meta = MetaParser::new(grammar.clone()).with_max_depth(96);
    let forest = meta
        .partial(input)
        .expect("safe parse experiment should parse");
    let parse_elapsed_us = parse_started.elapsed().as_micros() as u64;

    let type_started = Instant::now();
    let typed = forest
        .typed_ctx(grammar, &ctx)
        .expect("safe type experiment should type");
    let type_elapsed_us = type_started.elapsed().as_micros() as u64;

    let raw_complete_started = Instant::now();
    let raw_completions = typed.completions(grammar);
    let raw_complete_elapsed_us = raw_complete_started.elapsed().as_micros() as u64;

    json!({
        "input": input,
        "parse_elapsed_us": parse_elapsed_us,
        "type_elapsed_us": type_elapsed_us,
        "raw_completion_elapsed_us": raw_complete_elapsed_us,
        "raw_completion_count": raw_completions.len(),
        "forest": forest_shape(&forest),
        "typed": typed_shape(&typed),
        "rss_kb": current_rss_kb(),
        "global": global_cache_stats(),
    })
}

fn forest_shape(forest: &SppfForest) -> Value {
    let nodes = forest.nodes();
    let alternatives = nodes
        .iter()
        .map(|node| node.alternatives.len())
        .sum::<usize>();
    let children = nodes
        .iter()
        .flat_map(|node| node.alternatives.iter())
        .map(|alt| alt.children.len())
        .sum::<usize>();

    json!({
        "nodes": forest.node_count(),
        "alternatives": alternatives,
        "children": children,
        "max_alternatives": forest.max_alternatives(),
        "lower_bound_bytes": forest.node_count() * size_of::<SppfNode>()
            + alternatives * size_of::<PackedAlternative>()
            + children * size_of::<SppfChild>(),
    })
}

fn typed_shape(typed: &TypedAST) -> Value {
    fn count(node: &TypedNode) -> (usize, usize) {
        match node {
            TypedNode::Term { .. } => (1, 0),
            TypedNode::Expr { children, .. } => {
                children.iter().fold((1, children.len()), |acc, child| {
                    let (nodes, edges) = count(child);
                    (acc.0 + nodes, acc.1 + edges)
                })
            }
        }
    }

    let (nodes, edges) = typed.roots.iter().fold((0usize, 0usize), |acc, root| {
        let (n, e) = count(root);
        (acc.0 + n, acc.1 + e)
    });

    json!({
        "roots": typed.roots.len(),
        "nodes": nodes,
        "edges": edges,
        "lower_bound_bytes": nodes * size_of::<TypedNode>(),
    })
}

fn should_abort(elapsed_us: u64, rss_kb: u64, limits: &SafeLimits) -> bool {
    elapsed_us > limits.max_step_ms * 1_000 || rss_kb > limits.max_rss_kb
}

fn current_rss_kb() -> u64 {
    proc_status_kb("VmRSS:")
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

fn load_example_grammar(name: &str) -> Grammar {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("examples")
        .join(format!("{}.auf", name));
    let content = fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("Failed to read {}: {}", path.display(), e));
    Grammar::load(&content).unwrap_or_else(|e| panic!("Failed to load grammar '{}': {}", name, e))
}

fn generate_stlc_app_chain(n: usize) -> String {
    std::iter::once("apply".to_string())
        .chain((0..n).map(|idx| format!("x{}", idx)))
        .collect::<Vec<_>>()
        .join(" ")
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
