use crate::logic::debug::{DebugLevel, add_module_filter, clear_module_filters, set_debug_input};
use crate::logic::partial::{Node, Terminal};
use crate::logic::typing::core::{Context, TreeStatus};
use crate::logic::typing::eval::{check_tree, check_tree_with_context};
use crate::{logic::Parser, logic::grammar::Grammar, set_debug_level};
use rouille::{Request, Response};
use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use std::fs;
use std::path::Path;
use std::time::Instant;

use super::graph::build_graph;

#[derive(Debug, Deserialize)]
pub struct GraphRequest {
    pub spec: String,
    pub input: String,
}

/// Richer analysis request for the new visualization UI.
///
/// Notes:
/// - `debug_level` controls server-side debug-printing (stdout). We also return
///   a structured `warnings` array for non-fatal issues.
/// - `context` is an initial typing context (variable -> type string).
#[derive(Debug, Deserialize)]
pub struct AnalyzeRequest {
    pub spec: String,
    pub input: String,
    #[serde(default)]
    pub debug_level: Option<String>,
    #[serde(default)]
    pub debug_modules: Vec<String>,
    #[serde(default)]
    pub context: Vec<ContextBinding>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContextBinding {
    pub name: String,
    pub ty: String,
}

#[derive(Debug, Serialize)]
pub struct ParseResponse {
    pub graph: super::graph::GraphData,
    pub completions: Vec<String>,     // Well-typed completions only
    pub all_completions: Vec<String>, // All syntactic completions (for reference)
    pub is_complete: bool,
    pub root_count: usize,
}

#[derive(Debug, Serialize)]
pub struct AnalyzeResponse {
    pub version: &'static str,
    pub ok: bool,
    pub warnings: Vec<String>,
    pub error: Option<String>,

    pub is_complete: bool,
    pub root_count: usize,

    pub tokens: Vec<TokenInfo>,
    pub ast_graph: super::graph::GraphData,

    pub typed_ast: Option<TypedAstResponse>,

    pub completions: Vec<String>,
    pub all_completions: Vec<String>,

    pub timings_ms: TimingsMs,
}

#[derive(Debug, Serialize)]
pub struct TimingsMs {
    pub grammar_load: u128,
    pub tokenize: u128,
    pub parse_partial: u128,
    pub build_graph: u128,
    pub completions: u128,
    pub typed_ast: u128,
    pub total: u128,
}

#[derive(Debug, Serialize)]
pub struct TokenInfo {
    pub text: String,
    pub start: usize,
    pub end: usize,
    pub index: usize,
    pub is_partial_special: bool,
}

#[derive(Debug, Serialize)]
pub struct TypedAstResponse {
    pub trees: Vec<TypedTreeInfo>,
    pub roots: Vec<TypedNodeResponse>,
}

#[derive(Debug, Serialize)]
pub struct TypedTreeInfo {
    pub index: usize,
    pub complete: bool,
    pub type_status: String,
    pub ty: String,
}

#[derive(Debug, Serialize)]
#[serde(tag = "kind")]
pub enum TypedNodeResponse {
    Term {
        val: String,
        ty: String,
    },
    Expr {
        name: String,
        ty: String,
        complete: bool,
        children: Vec<TypedNodeResponse>,
    },
}

pub fn handle_analyze_request(request: &Request) -> Response {
    let total_start = Instant::now();
    let body = match rouille::input::json_input::<AnalyzeRequest>(request) {
        Ok(b) => b,
        Err(e) => {
            return Response::json(&AnalyzeResponse {
                version: "v1",
                ok: false,
                warnings: vec![],
                error: Some(format!("bad json: {}", e)),
                is_complete: false,
                root_count: 0,
                tokens: vec![],
                ast_graph: super::graph::GraphData {
                    nodes: vec![],
                    edges: vec![],
                    trees: vec![],
                    reconstructed_inputs: vec![],
                },
                typed_ast: None,
                completions: vec![],
                all_completions: vec![],
                timings_ms: TimingsMs {
                    grammar_load: 0,
                    tokenize: 0,
                    parse_partial: 0,
                    build_graph: 0,
                    completions: 0,
                    typed_ast: 0,
                    total: 0,
                },
            })
            .with_status_code(400);
        }
    };

    let mut warnings: Vec<String> = Vec::new();

    // Debug configuration
    let level = body
        .debug_level
        .as_deref()
        .map(parse_debug_level)
        .unwrap_or(DebugLevel::None);
    set_debug_level(level);
    set_debug_input(Some(body.input.clone()));
    clear_module_filters();
    for module in &body.debug_modules {
        add_module_filter(module);
    }

    // Grammar
    let t0 = Instant::now();
    let grammar = match Grammar::load(&body.spec) {
        Ok(g) => g,
        Err(e) => {
            return Response::json(&AnalyzeResponse {
                version: "v1",
                ok: false,
                warnings,
                error: Some(format!("spec error: {}", e)),
                is_complete: false,
                root_count: 0,
                tokens: vec![],
                ast_graph: super::graph::GraphData {
                    nodes: vec![],
                    edges: vec![],
                    trees: vec![],
                    reconstructed_inputs: vec![],
                },
                typed_ast: None,
                completions: vec![],
                all_completions: vec![],
                timings_ms: TimingsMs {
                    grammar_load: t0.elapsed().as_millis(),
                    tokenize: 0,
                    parse_partial: 0,
                    build_graph: 0,
                    completions: 0,
                    typed_ast: 0,
                    total: total_start.elapsed().as_millis(),
                },
            })
            .with_status_code(400);
        }
    };
    let grammar_load_ms = t0.elapsed().as_millis();

    // Tokenize (for UI display)
    let t1 = Instant::now();
    let segments = match grammar.tokenize(&body.input) {
        Ok(s) => s,
        Err(e) => {
            return Response::json(&AnalyzeResponse {
                version: "v1",
                ok: false,
                warnings,
                error: Some(format!("tokenization failed: {:?}", e)),
                is_complete: false,
                root_count: 0,
                tokens: vec![],
                ast_graph: super::graph::GraphData {
                    nodes: vec![],
                    edges: vec![],
                    trees: vec![],
                    reconstructed_inputs: vec![],
                },
                typed_ast: None,
                completions: vec![],
                all_completions: vec![],
                timings_ms: TimingsMs {
                    grammar_load: grammar_load_ms,
                    tokenize: t1.elapsed().as_millis(),
                    parse_partial: 0,
                    build_graph: 0,
                    completions: 0,
                    typed_ast: 0,
                    total: total_start.elapsed().as_millis(),
                },
            })
            .with_status_code(400);
        }
    };
    let tokens: Vec<TokenInfo> = segments
        .iter()
        .map(|s| TokenInfo {
            text: s.text(),
            start: s.start,
            end: s.end,
            index: s.index,
            is_partial_special: s.is_partial_special,
        })
        .collect();
    let tokenize_ms = t1.elapsed().as_millis();

    // Parse
    let t2 = Instant::now();
    let mut parser = Parser::new(grammar.clone());
    let partial = match parser.partial(&body.input).into_result() {
        Ok(p) => p,
        Err(e) => {
            return Response::json(&AnalyzeResponse {
                version: "v1",
                ok: false,
                warnings,
                error: Some(format!("parse error: {}", e)),
                is_complete: false,
                root_count: 0,
                tokens,
                ast_graph: super::graph::GraphData {
                    nodes: vec![],
                    edges: vec![],
                    trees: vec![],
                    reconstructed_inputs: vec![],
                },
                typed_ast: None,
                completions: vec![],
                all_completions: vec![],
                timings_ms: TimingsMs {
                    grammar_load: grammar_load_ms,
                    tokenize: tokenize_ms,
                    parse_partial: t2.elapsed().as_millis(),
                    build_graph: 0,
                    completions: 0,
                    typed_ast: 0,
                    total: total_start.elapsed().as_millis(),
                },
            })
            .with_status_code(400);
        }
    };
    let parse_partial_ms = t2.elapsed().as_millis();

    // Graph
    let t3 = Instant::now();
    let ast_graph = build_graph(&partial, &grammar);
    let build_graph_ms = t3.elapsed().as_millis();

    // Completions
    let t4 = Instant::now();
    let (well_typed_completions, all_completion_strings) =
        compute_completions_for_partial(&body.input, &partial, &grammar);
    let completions_ms = t4.elapsed().as_millis();

    // Typed AST + per-tree info
    let t5 = Instant::now();
    let mut ctx = Context::new();
    // Parsing of types from strings is not implemented here; expose as raw atoms.
    // This keeps the endpoint robust, and the UI can still show/compare.
    for b in &body.context {
        ctx.add(
            b.name.clone(),
            crate::logic::typing::Type::Raw(b.ty.clone()),
        );
    }
    let typed_ast = match partial.typed_ctx(&grammar, &ctx) {
        Ok(typed) => {
            let trees: Vec<TypedTreeInfo> = partial
                .roots
                .iter()
                .enumerate()
                .map(|(i, root)| {
                    let status = check_tree_with_context(root, &grammar, &ctx);
                    let (type_status, ty) = match status {
                        TreeStatus::Valid(t) => ("valid", format!("{t}")),
                        TreeStatus::Partial(t) => ("partial", format!("{t}")),
                        TreeStatus::Malformed => ("malformed", "<malformed>".to_string()),
                        TreeStatus::TooDeep => ("too_deep", "<too_deep>".to_string()),
                    };
                    TypedTreeInfo {
                        index: i,
                        complete: root.is_complete(),
                        type_status: type_status.to_string(),
                        ty,
                    }
                })
                .collect();
            let roots = typed.roots.iter().map(typed_node_to_response).collect();
            Some(TypedAstResponse { trees, roots })
        }
        Err(e) => {
            warnings.push(format!("typed ast unavailable: {}", e));
            None
        }
    };
    let typed_ast_ms = t5.elapsed().as_millis();

    // Reset debug input to avoid leaking between requests
    set_debug_input(None);

    Response::json(&AnalyzeResponse {
        version: "v1",
        ok: true,
        warnings,
        error: None,
        is_complete: partial.is_complete(),
        root_count: partial.roots.len(),
        tokens,
        ast_graph,
        typed_ast,
        completions: well_typed_completions,
        all_completions: all_completion_strings,
        timings_ms: TimingsMs {
            grammar_load: grammar_load_ms,
            tokenize: tokenize_ms,
            parse_partial: parse_partial_ms,
            build_graph: build_graph_ms,
            completions: completions_ms,
            typed_ast: typed_ast_ms,
            total: total_start.elapsed().as_millis(),
        },
    })
}

#[derive(Debug, Serialize)]
pub struct SpecListResponse {
    pub ok: bool,
    pub specs: Vec<SpecEntry>,
    pub error: Option<String>,
}

#[derive(Debug, Serialize)]
pub struct SpecEntry {
    pub name: String,
    pub path: String,
}

/// Lists available grammar specs from the repository `examples/` directory.
///
/// This is intentionally narrow (no arbitrary file system browsing).
pub fn handle_list_specs(_request: &Request) -> Response {
    let base = Path::new("examples");

    let read_dir = match fs::read_dir(base) {
        Ok(r) => r,
        Err(e) => {
            return Response::json(&SpecListResponse {
                ok: false,
                specs: vec![],
                error: Some(format!("Failed to read examples/: {e}")),
            })
            .with_status_code(500);
        }
    };

    let mut specs: Vec<SpecEntry> = vec![];
    for ent in read_dir.flatten() {
        let path = ent.path();
        if !path.is_file() {
            continue;
        }
        if path.extension().and_then(|s| s.to_str()) != Some("spec") {
            continue;
        }
        let name = path
            .file_name()
            .and_then(|s| s.to_str())
            .unwrap_or("(unknown)")
            .to_string();
        specs.push(SpecEntry {
            name: name.clone(),
            path: format!("examples/{name}"),
        });
    }
    specs.sort_by(|a, b| a.name.cmp(&b.name));

    Response::json(&SpecListResponse {
        ok: true,
        specs,
        error: None,
    })
}

fn typed_node_to_response(node: &crate::logic::typing::tree::TypedNode) -> TypedNodeResponse {
    use crate::logic::typing::tree::TypedNode as TN;
    match node {
        TN::Term { val, ty } => TypedNodeResponse::Term {
            val: val.clone(),
            ty: format!("{ty}"),
        },
        TN::Expr {
            name,
            children,
            ty,
            complete,
        } => TypedNodeResponse::Expr {
            name: name.clone(),
            ty: format!("{ty}"),
            complete: *complete,
            children: children.iter().map(typed_node_to_response).collect(),
        },
    }
}

fn parse_debug_level(s: &str) -> DebugLevel {
    match s.to_ascii_lowercase().as_str() {
        "none" => DebugLevel::None,
        "error" => DebugLevel::Error,
        "warn" | "warning" => DebugLevel::Warn,
        "info" => DebugLevel::Info,
        "debug" => DebugLevel::Debug,
        "trace" => DebugLevel::Trace,
        _ => DebugLevel::None,
    }
}

pub fn handle_parser_viz_request(request: &Request) -> Response {
    let body = rouille::input::json_input::<GraphRequest>(request);
    let body = match body {
        Ok(b) => b,
        Err(e) => return Response::text(format!("bad json: {}", e)).with_status_code(400),
    };
    let spec = body.spec;
    let input = body.input;

    // Build grammar and partial AST
    let grammar = match Grammar::load(&spec) {
        Ok(g) => g,
        Err(e) => return Response::text(format!("spec error: {}", e)).with_status_code(400),
    };
    let mut parser = Parser::new(grammar.clone());
    set_debug_level(crate::logic::debug::DebugLevel::Trace);

    println!("Received input: '{}'", input);
    let partial = match parser.partial(&input).into_result() {
        Ok(p) => p,
        Err(e) => return Response::text(format!("parse error: {}", e)).with_status_code(400),
    };
    println!(
        "Parsed input: '{}', partial roots: {}",
        input,
        partial.roots.len()
    );

    let graph = build_graph(&partial, &grammar);

    // Compute completions for this partial AST
    let (well_typed_completions, all_completion_strings) =
        compute_completions_for_partial(&input, &partial, &grammar);

    let response = ParseResponse {
        graph,
        completions: well_typed_completions,
        all_completions: all_completion_strings,
        is_complete: partial.is_complete(),
        root_count: partial.roots.len(),
    };

    Response::json(&response)
}

/// Filter completions to only those that lead to at least one well-typed tree
fn filter_well_typed_completions(
    input: &str,
    completions: &[String],
    grammar: &Grammar,
) -> Vec<String> {
    completions
        .iter()
        .filter(|completion| {
            // Try extending the input with this completion
            let extended = format!("{}{}", input, completion);

            // Parse the extended input
            let mut parser = Parser::new(grammar.clone());
            match parser.partial(&extended).into_result() {
                Ok(partial) => {
                    // Check if any tree is well-typed
                    partial
                        .roots
                        .iter()
                        .any(|root| match check_tree(root, grammar) {
                            TreeStatus::Valid(_) | TreeStatus::Partial(_) => true,
                            TreeStatus::Malformed | TreeStatus::TooDeep => false,
                        })
                }
                Err(_) => false, // Parse error means invalid completion
            }
        })
        .cloned()
        .collect()
}

/// Extract all identifier tokens from the AST by traversing terminal nodes
fn extract_symbols_from_ast(ast: &crate::logic::partial::PartialAST) -> Vec<String> {
    let mut tokens = Vec::new();
    let mut seen = HashSet::new();

    for root in ast.roots() {
        collect_tokens_from_node(&Node::NonTerminal(root.clone()), &mut tokens, &mut seen);
    }

    tokens
}

fn collect_tokens_from_node(node: &Node, tokens: &mut Vec<String>, seen: &mut HashSet<String>) {
    match node {
        Node::Terminal(Terminal::Complete { value, .. }) => {
            if !value.is_empty()
                && !value.chars().all(|c| c.is_whitespace())
                && seen.insert(value.clone())
            {
                tokens.push(value.clone());
            }
        }
        Node::Terminal(Terminal::Partial { value, .. }) => {
            if !value.is_empty()
                && !value.chars().all(|c| c.is_whitespace())
                && seen.insert(value.clone())
            {
                tokens.push(value.clone());
            }
        }
        Node::NonTerminal(nt) => {
            for child in &nt.children {
                collect_tokens_from_node(child, tokens, seen);
            }
        }
    }
}

/// Compute completions for a parsed `PartialAST`.
///
/// Returns (well_typed_completions, all_completion_strings).
fn compute_completions_for_partial(
    input: &str,
    partial: &crate::logic::partial::PartialAST,
    grammar: &Grammar,
) -> (Vec<String>, Vec<String>) {
    let symbols = extract_symbols_from_ast(partial);
    let completion_regexes = partial.completions(grammar);
    let all_completion_strings: Vec<String> = completion_regexes
        .iter()
        .filter_map(|regex| find_working_completion(input, regex, &symbols, grammar))
        .collect();
    let well_typed = filter_well_typed_completions(input, &all_completion_strings, grammar);
    (well_typed, all_completion_strings)
}

/// Find the first working completion for a regex
/// Priority: symbols that match first, then generated examples
fn find_working_completion(
    input: &str,
    regex: &crate::regex::Regex,
    symbols: &[String],
    grammar: &Grammar,
) -> Option<String> {
    // First try symbols that match the regex
    for symbol in symbols {
        if regex.matches(symbol) {
            if try_completion_works(input, symbol, grammar) {
                return Some(symbol.clone());
            }
        }
    }

    // Then try generated examples
    let examples = regex.examples(10);
    for example in &examples {
        if try_completion_works(input, example, grammar) {
            return Some(example.clone());
        }
    }

    // Fallback: return first example even if it doesn't work perfectly
    examples
        .into_iter()
        .next()
        .or_else(|| Some(regex.to_pattern()))
}

/// Check if extending input with completion produces a valid partial parse
fn try_completion_works(input: &str, completion: &str, grammar: &Grammar) -> bool {
    // Add space delimiter like extend_tree does
    let extended = if input.is_empty() || input.ends_with(' ') || input.ends_with('\n') {
        format!("{}{}", input, completion)
    } else {
        format!("{} {}", input, completion)
    };

    let mut parser = Parser::new(grammar.clone());
    parser.partial(&extended).is_ok()
}
