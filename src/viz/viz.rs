use crate::{logic::Parser, logic::grammar::Grammar, set_debug_level};
use crate::logic::typing::eval::check_tree;
use crate::logic::typing::core::TreeStatus;
use rouille::{Request, Response};
use serde::{Deserialize, Serialize};

use super::graph::build_graph;

#[derive(Debug, Deserialize)]
pub struct GraphRequest {
    pub spec: String,
    pub input: String,
}

#[derive(Debug, Serialize)]
pub struct ParseResponse {
    pub graph: super::graph::GraphData,
    pub completions: Vec<String>,           // Well-typed completions only
    pub all_completions: Vec<String>,       // All syntactic completions (for reference)
    pub is_complete: bool,
    pub root_count: usize,
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
    set_debug_level(crate::logic::debug::DebugLevel::None);
    
    let partial = match parser.partial(&input) {
        Ok(p) => p,
        Err(e) => return Response::text(format!("parse error: {}", e)).with_status_code(400),
    };
    
    let graph = build_graph(&partial, &grammar);
    
    // Get syntactic completions
    let completions = partial.completions(&grammar);
    let all_completion_strings: Vec<String> = completions
        .iter()
        .map(|r| r.example().unwrap_or_else(|| r.to_pattern()))
        .collect();
    
    // Filter completions to only those that lead to well-typed trees
    let well_typed_completions = filter_well_typed_completions(
        &input, 
        &all_completion_strings, 
        &grammar
    );
    
    let response = ParseResponse {
        graph,
        completions: well_typed_completions,
        all_completions: all_completion_strings,
        is_complete: partial.complete(),
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
            match parser.partial(&extended) {
                Ok(partial) => {
                    // Check if any tree is well-typed
                    partial.roots.iter().any(|root| {
                        match check_tree(root, grammar) {
                            TreeStatus::Valid(_) | TreeStatus::Partial(_) => true,
                            TreeStatus::Malformed => false,
                        }
                    })
                }
                Err(_) => false, // Parse error means invalid completion
            }
        })
        .cloned()
        .collect()
}
