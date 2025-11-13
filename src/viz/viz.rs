use crate::{logic::Parser, logic::grammar::Grammar, set_debug_level};
use rouille::{Request, Response};
use serde::Deserialize;

use super::graph::build_graph;

#[derive(Debug, Deserialize)]
pub struct GraphRequest {
    pub spec: String,
    pub input: String,
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
    let mut parser = Parser::new(grammar);
    set_debug_level(crate::logic::debug::DebugLevel::Trace);
    let partial = match parser.partial(&input) {
        Ok(p) => p,
        Err(e) => return Response::text(format!("parse error: {}", e)).with_status_code(400),
    };
    let graph = build_graph(&partial);
    Response::json(&graph)
}
