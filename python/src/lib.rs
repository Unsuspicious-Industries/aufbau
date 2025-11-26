//! Python bindings for the p7 constrained completion engine.
//!
//! This module exposes the completion engine to Python for use in constrained
//! generation with language models (e.g., HuggingFace Transformers).

use p7::logic::grammar::Grammar as RustGrammar;
use p7::logic::partial::{CompletionSet, Parser};
use p7::regex::Regex as DerivativeRegex;
use pyo3::prelude::*;
use std::collections::HashMap;

/// A compiled grammar that can be used for constrained generation.
#[pyclass]
pub struct Grammar {
    inner: RustGrammar,
}

#[pymethods]
impl Grammar {
    /// Create a new Grammar from a spec string.
    ///
    /// The spec string follows the p7 grammar format. See examples/*.spec files.
    ///
    /// Example:
    /// ```python
    /// grammar = Grammar("""
    ///     Number ::= /[0-9]+/
    ///     Op ::= '+' | '-'
    ///     Expr ::= Number | Number Op Expr
    ///     start ::= Expr
    /// """)
    /// ```
    #[new]
    fn new(spec: &str) -> PyResult<Self> {
        let inner = RustGrammar::load(spec)
            .map_err(|e| PyErr::new::<pyo3::exceptions::PyValueError, _>(format!("Grammar parse error: {}", e)))?;
        Ok(Self { inner })
    }

    /// Get the start nonterminal name.
    fn start_nonterminal(&self) -> Option<String> {
        self.inner.start_nonterminal().cloned()
    }
}

/// A constrained generator that tracks parsing state and provides valid completions.
///
/// Use this class to incrementally generate tokens that conform to the grammar.
#[pyclass]
pub struct ConstrainedGenerator {
    grammar: RustGrammar,
    current_text: String,
    parser: Parser,
}

#[pymethods]
impl ConstrainedGenerator {
    /// Create a new ConstrainedGenerator from a Grammar.
    #[new]
    fn new(grammar: &Grammar) -> Self {
        let parser = Parser::new(grammar.inner.clone());
        Self {
            grammar: grammar.inner.clone(),
            current_text: String::new(),
            parser,
        }
    }

    /// Reset the generator to the initial state.
    fn reset(&mut self) {
        self.current_text = String::new();
        self.parser = Parser::new(self.grammar.clone());
    }

    /// Get the current accumulated text.
    fn current_text(&self) -> String {
        self.current_text.clone()
    }

    /// Feed a token to the generator and update state.
    ///
    /// Returns True if the token was valid, False otherwise.
    fn feed(&mut self, token: &str) -> PyResult<bool> {
        let new_text = if self.current_text.is_empty() {
            token.to_string()
        } else {
            format!("{} {}", self.current_text, token)
        };

        // Try parsing
        match self.parser.partial(&new_text) {
            Ok(_) => {
                self.current_text = new_text;
                Ok(true)
            }
            Err(_) => Ok(false),
        }
    }

    /// Feed raw text (without adding space).
    fn feed_raw(&mut self, text: &str) -> PyResult<bool> {
        let new_text = format!("{}{}", self.current_text, text);

        match self.parser.partial(&new_text) {
            Ok(_) => {
                self.current_text = new_text;
                Ok(true)
            }
            Err(_) => Ok(false),
        }
    }

    /// Check if the current state is complete (fully parsed).
    fn is_complete(&mut self) -> PyResult<bool> {
        match self.parser.partial(&self.current_text) {
            Ok(ast) => Ok(ast.complete()),
            Err(_) => Ok(false),
        }
    }

    /// Get valid completion tokens as regex patterns.
    ///
    /// Returns a list of regex pattern strings that represent valid next tokens.
    fn get_valid_patterns(&mut self) -> PyResult<Vec<String>> {
        let completions = self.get_completions()?;
        Ok(completions.iter().map(|r| r.to_pattern()).collect())
    }

    /// Check if a specific token/string is a valid next token.
    fn is_valid_next(&mut self, token: &str) -> PyResult<bool> {
        let completions = self.get_completions()?;
        Ok(completions.matches(token))
    }

    /// Get a token mask for a vocabulary.
    ///
    /// Given a list of tokens (vocabulary), returns a list of booleans indicating
    /// which tokens are valid next tokens.
    ///
    /// This is useful for constrained generation with language models.
    fn get_token_mask(&mut self, vocabulary: Vec<String>) -> PyResult<Vec<bool>> {
        let completions = self.get_completions()?;
        Ok(vocabulary
            .iter()
            .map(|token| completions.matches(token))
            .collect())
    }

    /// Get valid token indices for a vocabulary.
    ///
    /// Given a list of tokens (vocabulary), returns indices of valid next tokens.
    fn get_valid_token_indices(&mut self, vocabulary: Vec<String>) -> PyResult<Vec<usize>> {
        let completions = self.get_completions()?;
        Ok(vocabulary
            .iter()
            .enumerate()
            .filter_map(|(i, token)| {
                if completions.matches(token) {
                    Some(i)
                } else {
                    None
                }
            })
            .collect())
    }

    /// Check if any vocabulary token matches (useful for debugging).
    fn any_valid_token(&mut self, vocabulary: Vec<String>) -> PyResult<bool> {
        let completions = self.get_completions()?;
        Ok(vocabulary.iter().any(|token| completions.matches(token)))
    }

    /// Get completion info (for debugging).
    fn debug_completions(&mut self) -> PyResult<HashMap<String, Vec<String>>> {
        let completions = self.get_completions()?;
        let patterns: Vec<String> = completions.iter().map(|r| r.to_pattern()).collect();
        let examples: Vec<String> = completions
            .iter()
            .filter_map(|r| r.example())
            .collect();

        let mut info = HashMap::new();
        info.insert("patterns".to_string(), patterns);
        info.insert("examples".to_string(), examples);
        Ok(info)
    }
}

impl ConstrainedGenerator {
    fn get_completions(&mut self) -> PyResult<CompletionSet> {
        let ast = self
            .parser
            .partial(&self.current_text)
            .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("Parse error: {}", e)))?;
        Ok(ast.completions(&self.grammar))
    }
}

/// A LogitsProcessor for HuggingFace Transformers integration.
///
/// This class can be used with transformers' `LogitsProcessor` to mask invalid tokens.
#[pyclass]
pub struct ConstrainedLogitsProcessor {
    generator: ConstrainedGenerator,
    // Cache vocabulary strings
    token_to_str: Vec<String>,
    // Whether to allow EOS when complete
    eos_token_id: Option<usize>,
}

#[pymethods]
impl ConstrainedLogitsProcessor {
    /// Create a new ConstrainedLogitsProcessor.
    ///
    /// Args:
    ///     grammar: The Grammar to constrain to
    ///     eos_token_id: Optional EOS token ID to allow when parse is complete
    #[new]
    #[pyo3(signature = (grammar, eos_token_id=None))]
    fn new(grammar: &Grammar, eos_token_id: Option<usize>) -> PyResult<Self> {
        let generator = ConstrainedGenerator::new(grammar);
        Ok(Self {
            generator,
            token_to_str: Vec::new(), // Will be populated lazily
            eos_token_id,
        })
    }

    /// Initialize the vocabulary mapping from a tokenizer.
    ///
    /// Call this with your tokenizer's decode function to build the vocabulary cache.
    fn init_vocab(&mut self, tokens: Vec<String>) -> PyResult<()> {
        self.token_to_str = tokens;
        Ok(())
    }

    /// Reset the processor state.
    fn reset(&mut self) {
        self.generator.reset();
    }

    /// Feed a token by its string value.
    fn feed_token(&mut self, token_str: &str) -> PyResult<bool> {
        self.generator.feed_raw(token_str)
    }

    /// Get mask for all tokens (returns list of valid token indices).
    ///
    /// Returns indices of tokens that are valid continuations.
    fn get_allowed_tokens(&mut self) -> PyResult<Vec<usize>> {
        if self.token_to_str.is_empty() {
            return Err(PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(
                "Vocabulary not initialized. Call init_vocab first.",
            ));
        }

        let mut allowed = self.generator.get_valid_token_indices(self.token_to_str.clone())?;

        // If parse is complete and we have an EOS token, add it
        if let Some(eos_id) = self.eos_token_id {
            if self.generator.is_complete()? && !allowed.contains(&eos_id) {
                allowed.push(eos_id);
            }
        }

        Ok(allowed)
    }

    /// Get current accumulated text.
    fn current_text(&self) -> String {
        self.generator.current_text()
    }

    /// Check if current state is complete.
    fn is_complete(&mut self) -> PyResult<bool> {
        self.generator.is_complete()
    }
}

/// Utility function to check if a string matches a regex pattern.
#[pyfunction]
fn regex_matches(pattern: &str, text: &str) -> PyResult<bool> {
    let regex = DerivativeRegex::from_str(pattern)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyValueError, _>(format!("Invalid regex: {}", e)))?;
    Ok(regex.match_full(text))
}

/// Utility function to check if a string is a valid prefix for a regex pattern.
#[pyfunction]
fn regex_prefix_valid(pattern: &str, prefix: &str) -> PyResult<bool> {
    let regex = DerivativeRegex::from_str(pattern)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyValueError, _>(format!("Invalid regex: {}", e)))?;
    match regex.prefix_match(prefix) {
        p7::regex::PrefixStatus::NoMatch => Ok(false),
        _ => Ok(true),
    }
}

/// The p7_constrained Python module.
#[pymodule]
fn p7_constrained(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<Grammar>()?;
    m.add_class::<ConstrainedGenerator>()?;
    m.add_class::<ConstrainedLogitsProcessor>()?;
    m.add_function(wrap_pyfunction!(regex_matches, m)?)?;
    m.add_function(wrap_pyfunction!(regex_prefix_valid, m)?)?;
    Ok(())
}

