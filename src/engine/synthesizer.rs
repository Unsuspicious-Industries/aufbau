use rouille::input;

use crate::logic::grammar::Grammar;
use crate::logic::parser::Parser;
use super::rank::Ranker;
use crate::logic::partial::PartialAST;

/// The synthesizer: pure/functional w.r.t. state; holds only grammar and ranker
pub struct Synthesizer {
    pub(crate) grammar: Grammar,
    pub(crate) ranker: Box<dyn Ranker>,
}

// I wanted to do an album with the sounds of the '50s
// The sounds of the '60s, of the '70s
// And then have a sound of the future
// And I said, "Wait a second, I know the synthesizer
// Why don't I use the synthesizer which is the sound of the future?"
impl Synthesizer {
    /// Parse input into a PartialAST with a fresh parser
    fn parse_partial(&self, input: &str) -> Result<PartialAST, String> {
        let mut parser = Parser::new(self.grammar.clone());
        parser.partial(input)
    }

    /// Attempt to append a token and return new code and AST if valid
    fn try_append(&self, code: &str, token: &str) -> Option<(String, PartialAST)> {
        let candidate = format!("{}{}", code, token);
        match self.parse_partial(&candidate) {
            Ok(ast) => Some((candidate, ast)),
            Err(_) => None,
        }
    }

    /// Log current parser state for debugging
    fn log_state(&self, step: usize, code: &str, ast: Option<&PartialAST>) {
        crate::debug_debug!("synthesizer", "=== Step {} | code_len={} code='{}' ===", step, code.len(), code);
        if let Some(a) = ast {
            crate::debug_debug!("synthesizer", "PartialAST present: complete={}", a.is_complete());
        } else {
            crate::debug_debug!("synthesizer", "No PartialAST available");
        }
    }

    pub fn new(grammar_spec: &str, ranker: Box<dyn Ranker>) -> Result<Self, String> {
        let grammar = Grammar::load(grammar_spec)?;
        Ok(Self { grammar, ranker })
    }

    // synthesie (run run n times)
    pub fn synthesize(&mut self, input: &str, k: i32, max_steps: usize, n: i32) -> Result<String, String> {
        let mut input = input.to_string();
        for _ in 0..n {
            match self.run_with(&input, k, max_steps) {
                Ok(prog) => {
                    input = prog.clone();
                }
                Err(e) => return Err(e),
            }
        }
        Ok(input)
    }

    /// Synthesize a well-typed program guided by the ranker.
    /// k: beam width
    pub fn run(&mut self, input: &str, k: i32) -> Result<String, String> {
        self.run_with(input, k, 128)
    }

    /// Synthesize with explicit max_steps using a greedy ranked approach.
    /// At each step, take the highest-ranked token that yields a valid partial parse.
    pub fn run_with(&mut self, input: &str, _k: i32, max_steps: usize) -> Result<String, String> {
        crate::debug_info!("synthesizer", "Running synthesizer (greedy) on input: {} (max_steps={})", input, max_steps);

        // Ensure grammar has a start symbol
        if self.grammar.start_nonterminal().is_none() {
            if let Some(first) = self.grammar.productions.keys().next().cloned() { self.grammar.set_start(first); }
        }

        // Local functional state
        let mut code = input.to_string();
        let mut ast = match self.parse_partial(&code) { Ok(a) => Some(a), Err(_) => None };
        
        for step in 0..max_steps {
            self.log_state(step, &code, ast.as_ref());
            
            // Rank proposals and try them in descending order; no expectation-based filtering
            let mut proposals = self.ranker.rank(&code);
            proposals.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap());
            
            if proposals.is_empty() { break; }
            crate::debug_debug!("synthesizer", "Top proposals at step {}: {:?}", step, &proposals[0..proposals.len().min(5)]);

            let mut advanced = false;
            let mut tried_this_step: std::collections::HashSet<String> = std::collections::HashSet::new();
            for (tok, score) in proposals.into_iter() {
                if tried_this_step.contains(&tok) { continue; }
                crate::debug_trace!("synthesizer", "Step {}: TRY token='{}' score={:.6}", step, tok, score);
                
                if let Some((new_code, new_ast)) = self.try_append(&code, &tok) {
                    crate::debug_trace!("synthesizer", "Step {}: SELECT token='{}' -> code='{}' (complete={})", 
                        step, tok, new_code, new_ast.is_complete());
                    code = new_code;
                    ast = Some(new_ast);
                    advanced = true;
                    break; // Take the first valid token
                } else {
                    crate::debug_trace!("synthesizer", "Step {}: REJECT token='{}' (parser error)", step, tok);
                    tried_this_step.insert(tok);
                }
            }

            if !advanced {
                crate::debug_info!("synthesizer", "No token led to progress at step {}, returning longest valid prefix", step);
                return Ok(code);
            }
        }

        // Step cap reached; return the current valid prefix
        Ok(code)
    }

}

// No saved state struct: synthesizer is kept functional/pure in operation
