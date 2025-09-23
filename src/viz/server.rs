use rouille::{router, Request, Response};
use serde::Serialize;
use std::sync::Arc;

use crate::logic::debug::{DebugLevel, set_debug_level};
use crate::logic::grammar::Grammar;
use crate::logic::parser::PartialParser;
use crate::viz::types::VizNode;

#[derive(Clone)]
struct AppState {
    grammar: Arc<Grammar>,
}

#[derive(Serialize)]
struct ParseResponse {
    ok: bool,
    error: Option<String>,
    root: Option<VizNode>,
}

pub fn serve<A: std::net::ToSocketAddrs>(addr: A, grammar: Grammar, debug: DebugLevel) {
    let state = AppState { grammar: Arc::new(grammar) };
    let state = Arc::new(state);

    // Ensure server emits parser traces according to the requested level
    set_debug_level(debug);

    rouille::start_server(addr, move |request: &Request| {
        // Basic request logging
        println!("[server] {} {}", request.method(), request.raw_url());
        let state = state.clone();
        router!(request,
            (GET) (/api/health) => {
                Response::json(&serde_json::json!({"ok": true}))
            },
            (POST) (/api/parse) => {
                handle_parse(request, state)
            },
            _ => {
                // Serve static UI
                match serve_static(request) {
                    Some(resp) => resp,
                    None => Response::empty_404(),
                }
            }
        )
    });
}

fn serve_static(request: &Request) -> Option<Response> {
    let path = request.url();
    let rel = if path == "/" { "/index.html" } else { &path };
    let base = include_str!("./static/index.html");
    if rel == "/index.html" {
        return Some(Response::from_data("text/html", base));
    }
    // Bundle simple JS and CSS via include_str!
    if rel == "/app.js" {
        let js = include_str!("./static/app.js");
        return Some(Response::from_data("application/javascript", js));
    }
    if rel == "/styles.css" {
        let css = include_str!("./static/styles.css");
        return Some(Response::from_data("text/css", css));
    }
    None
}

fn handle_parse(request: &Request, state: Arc<AppState>) -> Response {
    let body = match request.data() {
        Some(mut data) => {
            let mut buf = String::new();
            if std::io::Read::read_to_string(&mut data, &mut buf).is_err() { "".to_string() } else { buf }
        }
        None => "".to_string(),
    };
    #[derive(serde::Deserialize)]
    struct ParseInput { code: String, spec: Option<String> }
    let input: ParseInput = match serde_json::from_str(&body) {
        Ok(v) => v,
        Err(e) => return Response::json(&serde_json::json!({"ok": false, "error": format!("invalid json: {}", e)})),
    };

    // Choose grammar: uploaded spec in request takes precedence; otherwise use server grammar
    let grammar = if let Some(spec) = input.spec {
        match Grammar::load(&spec) {
            Ok(g) => g,
            Err(e) => return Response::json(&ParseResponse { ok: false, error: Some(format!("failed to parse spec: {}", e)), root: None }),
        }
    } else {
        (*state.grammar).clone()
    };
    let mut parser = PartialParser::new(grammar);
    match parser.partial(&input.code) {
        Ok(root) => {
            let node = VizNode::from_partial_ast(&root);
            Response::json(&ParseResponse { ok: true, error: None, root: Some(node) })
        }
        Err(e) => Response::json(&ParseResponse { ok: false, error: Some(e), root: None }),
    }
}


