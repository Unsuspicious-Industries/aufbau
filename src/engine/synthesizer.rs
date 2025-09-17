use crate::logic::ast::ASTNode;
use crate::logic::check::TypeChecker;
use crate::logic::grammar::Grammar;
use crate::logic::parser::Parser;
use super::rank::{Ranker, DefaultRanker};

/// The main synthesizer that performs partial parsing and caches results
pub struct Synthesizer {
    pub(crate) grammar: Grammar,
    pub(crate) parser: Parser,
    pub(crate) type_checker: TypeChecker,
    pub(crate) code: String,
    pub(crate) ranker: Box<dyn Ranker>,
    pub(crate) cache: Option<ASTNode>,
    pub(crate) remaining_tokens: Vec<String>, // tokens not yet consumed (for future synthesis expansion)
}

// I wanted to do an album with the sounds of the '50s
// The sounds of the '60s, of the '70s
// And then have a sound of the future
// And I said, "Wait a second, I know the synthesizer
// Why don't I use the synthesizer which is the sound of the future?"
impl Synthesizer {
    pub fn new(grammar_spec: &str, ranker: Box<dyn Ranker>) -> Result<Self, String> {
        let grammar = Grammar::load(grammar_spec)?;
        let parser = Parser::new(grammar.clone());
        let type_checker = TypeChecker::new();
        Ok(Self { 
            grammar, 
            parser, 
            type_checker, 
            code: String::new(), 
            cache: None, 
            ranker,
            remaining_tokens: Vec::new() 
        })
    }

    pub fn run(&mut self, input: &str, k:i32) -> Result<String, String> {
        crate::debug_info!("synthesizer", "Running synthesizer on input: {}", input);
        let ranked = self.ranker.rank(input);
        // sort and keep the top k
        let mut ranked: Vec<(String, f32)> = ranked.into_iter().take(k as usize).collect();
        ranked.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap());
        crate::debug_info!("synthesizer", "Top {} ranked tokens: {:?}", k, ranked);
        for (token, score) in ranked {
            let new_code = format!("{}{}", self.code, token);
            crate::debug_info!("synthesizer", "Trying token: '{}' with score {}", token, score);
        }

        Ok(String::new()) // placeholder
    }


}

/// Represents a saved state of the synthesizer
#[derive(Debug, Clone)]
pub(crate) struct SynthesizerState {
    pub code: String,
    pub cache: Option<ASTNode>,
    pub remaining_tokens: Vec<String>,
}
