use crate::{
    engine::rank::{DefaultRanker, Ranker, StlcRanker},
    engine::Synthesizer,
    logic::{
        grammar::Grammar,
        parser::Parser,
        partial::{CompletionSet, PartialAST},
    },
};
use rouille::{Request, Response};
use serde::{Deserialize, Serialize};
use std::sync::{Arc, Mutex};

use super::graph::{build_graph, GraphData};

#[derive(Debug, Deserialize)]
pub struct SynthRequest {
    pub spec: String,
    pub input: String,
    pub ranker: String,
    pub k: i32,
    pub max_steps: usize,
}

#[derive(Debug, Serialize, Clone)]
pub struct CompletionInfo {
    pub token_type: String,
    pub token_value: String,
    pub metadata: CompletionMetadataView,
}

#[derive(Debug, Serialize, Clone)]
pub struct CompletionMetadataView {
    pub branch: String,
    pub rule_name: Option<String>,
    pub type_hint: Option<String>,
    pub symbol_index: usize,
    pub origin: String,
}

#[derive(Debug, Serialize, Clone)]
pub struct SynthesisStep {
    pub step_number: usize,
    pub code: String,
    pub completions: Vec<CompletionInfo>,
    pub ast: Option<GraphData>,
}

#[derive(Debug, Serialize)]
pub struct SynthesisResult {
    pub steps: Vec<SynthesisStep>,
}

pub fn handle_synth_request(request: &Request) -> Response {
    let body = rouille::input::json_input::<SynthRequest>(request);
    let body = match body {
        Ok(b) => b,
        Err(e) => return Response::text(format!("bad json: {}", e)).with_status_code(400),
    };

    match run_synthesis_with_steps(body) {
        Ok(result) => Response::json(&result),
        Err(e) => Response::text(format!("synthesis error: {}", e)).with_status_code(400),
    }
}

fn run_synthesis_with_steps(req: SynthRequest) -> Result<SynthesisResult, String> {
    // Load grammar
    let grammar = Grammar::load(&req.spec)?;

    // Create ranker based on request
    let ranker: Box<dyn Ranker> = match req.ranker.as_str() {
        "stlc" => Box::new(StlcRanker),
        _ => Box::new(DefaultRanker),
    };

    // Create synthesizer with the actual engine
    let synthesizer = Synthesizer::new(&req.spec, ranker)?;

    // Shared state for capturing steps
    let steps = Arc::new(Mutex::new(Vec::new()));

    // Helper to parse code
    let parse_code = |code: &str| -> Result<PartialAST, String> {
        let mut parser = Parser::new(grammar.clone());
        parser.partial(code)
    };

    // Helper to get completions
    let get_completions = |code: &str| -> Vec<CompletionInfo> {
        match parse_code(code) {
            Ok(ast) => {
                let comp_set = ast.completions(&grammar, req.k as usize);
                convert_completions(&comp_set)
            }
            Err(_) => Vec::new(),
        }
    };

    // Capture initial state
    let mut code = req.input.clone();
    let initial_ast = match parse_code(&code) {
        Ok(ast) => Some(build_graph(&ast)),
        Err(_) => None,
    };
    let initial_completions = get_completions(&code);

    steps.lock().unwrap().push(SynthesisStep {
        step_number: 0,
        code: code.clone(),
        completions: initial_completions,
        ast: initial_ast,
    });

    // Run synthesis step-by-step using the actual synthesizer logic
    for step_number in 1..=req.max_steps {
        // Parse current state
        let ast_result = parse_code(&code);
        
        // Check if complete
        if let Ok(ref ast) = ast_result {
            if ast.complete() {
                break;
            }
        }

        // Get completions from the AST
        let completion_set = if let Ok(ref ast) = ast_result {
            ast.completions(&grammar, req.k as usize)
        } else {
            break;
        };

        // Get ranker proposals
        let mut proposals = match synthesizer.ranker.rank(&code) {
            Ok(p) => p,
            Err(_) => break,
        };

        if proposals.is_empty() {
            break;
        }

        // Sort proposals by score
        proposals.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));

        // Use synthesizer's filtering logic by checking completions
        let mut filtered_proposals: Vec<(String, f32)> = Vec::new();
        
        for (token, score) in proposals.iter().take(100) {
            let mut boost: f32 = 0.0;

            // Check if token matches any completion
            for candidate in completion_set.iter() {
                match &candidate.token {
                    crate::logic::partial::CompletionToken::Literal(lit) => {
                        if token == lit {
                            boost = boost.max(5.0); // Perfect match
                        } else if lit.starts_with(token) && !token.is_empty() {
                            boost = boost.max(3.0); // Token is prefix of completion
                        } else if token.starts_with(lit) && !lit.is_empty() {
                            boost = boost.max(2.0); // Completion is prefix of token
                        }
                    }
                    crate::logic::partial::CompletionToken::Regex(_pattern) => {
                        // Regex patterns get moderate boost
                        boost = boost.max(1.5);
                    }
                }
            }

            // Include delimiter tokens even without completion boost
            if token == " " || token == "\n" || token == "\t" {
                boost = boost.max(1.0);
            }

            // Only include tokens that have some relevance
            if boost > 0.5 {
                filtered_proposals.push((token.clone(), score * boost));
            }
        }

        if filtered_proposals.is_empty() {
            break;
        }

        // Sort by boosted score
        filtered_proposals.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));

        // Try to append the best valid token
        let mut advanced = false;
        for (token, _score) in filtered_proposals.iter().take(20) {
            let candidate = format!("{}{}", code, token);
            match parse_code(&candidate) {
                Ok(_new_ast) => {
                    code = candidate;
                    advanced = true;
                    break;
                }
                Err(_) => continue,
            }
        }

        if !advanced {
            break;
        }

        // Capture the new step
        let step_ast = match parse_code(&code) {
            Ok(ast) => Some(build_graph(&ast)),
            Err(_) => None,
        };
        let step_completions = get_completions(&code);

        steps.lock().unwrap().push(SynthesisStep {
            step_number,
            code: code.clone(),
            completions: step_completions,
            ast: step_ast,
        });
    }

    // Extract the captured steps
    let final_steps = match Arc::try_unwrap(steps) {
        Ok(mutex) => mutex.into_inner().unwrap(),
        Err(arc) => arc.lock().unwrap().clone(),
    };

    Ok(SynthesisResult {
        steps: final_steps,
    })
}

fn convert_completions(comp_set: &CompletionSet) -> Vec<CompletionInfo> {
    comp_set
        .iter()
        .map(|candidate| {
            let (token_type, token_value) = match &candidate.token {
                crate::logic::partial::CompletionToken::Literal(lit) => {
                    ("Literal".to_string(), lit.clone())
                }
                crate::logic::partial::CompletionToken::Regex(re) => {
                    ("Regex".to_string(), re.clone())
                }
            };

            let origin = format!("{:?}", candidate.metadata.origin);

            CompletionInfo {
                token_type,
                token_value,
                metadata: CompletionMetadataView {
                    branch: candidate.metadata.branch.clone(),
                    rule_name: candidate.metadata.rule_name.clone(),
                    type_hint: candidate.metadata.type_hint.clone(),
                    symbol_index: candidate.metadata.symbol_index,
                    origin,
                },
            }
        })
        .collect()
}
