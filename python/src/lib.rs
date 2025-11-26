//! Python bindings for p7 constrainted completion engine

use p7::logic::grammar::Grammar as RustGrammar;
use p7::logic::partial::{CompletionSet, Parser};
use p7::logic::typing::core::{Context, TreeStatus};
use p7::logic::typing::eval::check_tree_with_context;
use p7::regex::Regex as DerivativeRegex;
use pyo3::prelude::*;
use pyo3::exceptions::PyTypeError;
use std::collections::HashMap;

#[pyclass]
pub struct Grammar {
    inner: RustGrammar,
}

#[pymethods]
impl Grammar {
    #[new]
    fn new(spec: &str) -> PyResult<Self> {
        let inner = RustGrammar::load(spec)
            .map_err(|e| PyErr::new::<pyo3::exceptions::PyValueError, _>(format!("Grammar parse error: {}", e)))?;
        Ok(Self { inner })
    }

    fn start_nonterminal(&self) -> Option<String> {
        self.inner.start_nonterminal().cloned()
    }
}

#[pyclass]
pub struct ConstrainedGenerator {
    grammar: RustGrammar,
    current_text: String,
    parser: Parser,
}

#[pymethods]
impl ConstrainedGenerator {
    #[new]
    fn new(grammar: &Grammar) -> Self {
        let parser = Parser::new(grammar.inner.clone());
        Self {
            grammar: grammar.inner.clone(),
            current_text: String::new(),
            parser,
        }
    }

    fn reset(&mut self) {
        self.current_text = String::new();
        self.parser = Parser::new(self.grammar.clone());
    }

    fn current_text(&self) -> String {
        self.current_text.clone()
    }

    fn feed(&mut self, token: &str) -> PyResult<bool> {
        let new_text = if self.current_text.is_empty() {
            token.to_string()
        } else {
            format!("{} {}", self.current_text, token)
        };

        match self.parser.partial(&new_text) {
            Ok(ast) => {
                let ctx = Context::new();
                let has_well_typed = ast.roots.iter().any(|root| {
                    match check_tree_with_context(root, &self.grammar, &ctx) {
                        TreeStatus::Valid(_) | TreeStatus::Partial(_) => true,
                        TreeStatus::Malformed => false,
                    }
                });

                if has_well_typed {
                    self.current_text = new_text;
                    Ok(true)
                } else {
                    Err(PyTypeError::new_err(format!(
                        "Type error: '{}' produces no well-typed parse trees",
                        new_text
                    )))
                }
            }
            Err(_) => Ok(false),
        }
    }

    fn feed_raw(&mut self, text: &str) -> PyResult<bool> {
        let new_text = format!("{}{}", self.current_text, text);

        match self.parser.partial(&new_text) {
            Ok(ast) => {
                let ctx = Context::new();
                let has_well_typed = ast.roots.iter().any(|root| {
                    match check_tree_with_context(root, &self.grammar, &ctx) {
                        TreeStatus::Valid(_) | TreeStatus::Partial(_) => true,
                        TreeStatus::Malformed => false,
                    }
                });

                if has_well_typed {
                    self.current_text = new_text;
                    Ok(true)
                } else {
                    Err(PyTypeError::new_err(format!(
                        "Type error: '{}' produces no well-typed parse trees",
                        new_text
                    )))
                }
            }
            Err(_) => Ok(false),
        }
    }

    fn is_complete(&mut self) -> PyResult<bool> {
        match self.parser.partial(&self.current_text) {
            Ok(ast) => Ok(ast.complete()),
            Err(_) => Ok(false),
        }
    }

    fn get_valid_patterns(&mut self) -> PyResult<Vec<String>> {
        let completions = self.get_completions()?;
        Ok(completions.iter().map(|r| r.to_pattern()).collect())
    }

    fn is_valid_next(&mut self, token: &str) -> PyResult<bool> {
        let completions = self.get_completions()?;
        Ok(completions.matches(token))
    }

    fn get_token_mask(&mut self, vocabulary: Vec<String>) -> PyResult<Vec<bool>> {
        let completions = self.get_completions()?;
        Ok(vocabulary
            .iter()
            .map(|token| completions.matches(token))
            .collect())
    }

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

    fn any_valid_token(&mut self, vocabulary: Vec<String>) -> PyResult<bool> {
        let completions = self.get_completions()?;
        Ok(vocabulary.iter().any(|token| completions.matches(token)))
    }

    fn check_completion(&mut self, completion: &str) -> PyResult<bool> {
        let test_text = format!("{}{}", self.current_text, completion);
        
        match self.parser.partial(&test_text) {
            Ok(ast) => {
                let ctx = Context::new();
                let has_well_typed = ast.roots.iter().any(|root| {
                    match check_tree_with_context(root, &self.grammar, &ctx) {
                        TreeStatus::Valid(_) | TreeStatus::Partial(_) => true,
                        TreeStatus::Malformed => false,
                    }
                });
                Ok(has_well_typed)
            }
            Err(_) => Ok(false),
        }
    }

    fn filter_completions(&mut self, completions: Vec<String>) -> PyResult<Vec<String>> {
        let ctx = Context::new();
        let valid: Vec<String> = completions
            .into_iter()
            .filter(|completion| {
                let test_text = format!("{}{}", self.current_text, completion);
                match self.parser.partial(&test_text) {
                    Ok(ast) => ast.roots.iter().any(|root| {
                        match check_tree_with_context(root, &self.grammar, &ctx) {
                            TreeStatus::Valid(_) | TreeStatus::Partial(_) => true,
                            TreeStatus::Malformed => false,
                        }
                    }),
                    Err(_) => false,
                }
            })
            .collect();
        Ok(valid)
    }

    fn filter_completion_indices(&mut self, vocabulary: Vec<String>) -> PyResult<Vec<usize>> {
        let ctx = Context::new();
        let valid_indices: Vec<usize> = vocabulary
            .iter()
            .enumerate()
            .filter_map(|(i, completion)| {
                let test_text = format!("{}{}", self.current_text, completion);
                match self.parser.partial(&test_text) {
                    Ok(ast) => {
                        let is_valid = ast.roots.iter().any(|root| {
                            match check_tree_with_context(root, &self.grammar, &ctx) {
                                TreeStatus::Valid(_) | TreeStatus::Partial(_) => true,
                                TreeStatus::Malformed => false,
                            }
                        });
                        if is_valid { Some(i) } else { None }
                    }
                    Err(_) => None,
                }
            })
            .collect();
        Ok(valid_indices)
    }

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

    fn well_typed_tree_count(&mut self) -> PyResult<usize> {
        if self.current_text.is_empty() {
            return Ok(0);
        }
        
        match self.parser.partial(&self.current_text) {
            Ok(ast) => {
                let ctx = Context::new();
                let count = ast.roots.iter().filter(|root| {
                    match check_tree_with_context(root, &self.grammar, &ctx) {
                        TreeStatus::Valid(_) | TreeStatus::Partial(_) => true,
                        TreeStatus::Malformed => false,
                    }
                }).count();
                Ok(count)
            }
            Err(_) => Ok(0),
        }
    }

    fn to_sexpr(&mut self) -> PyResult<String> {
        let ast = self
            .parser
            .parse(&self.current_text)
            .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("Parse error: {}", e)))?;
        
        let ctx = Context::new();
        let partial = self
            .parser
            .partial(&self.current_text)
            .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("Parse error: {}", e)))?;
        
        let has_valid = partial.roots.iter().any(|root| {
            root.is_complete() && matches!(
                check_tree_with_context(root, &self.grammar, &ctx),
                TreeStatus::Valid(_)
            )
        });
        
        if !has_valid {
            return Err(PyTypeError::new_err(format!(
                "Type error: '{}' has no complete well-typed parse",
                self.current_text
            )));
        }
        
        Ok(ast.pretty())
    }

    fn to_sexpr_compact(&mut self) -> PyResult<String> {
        let ast = self
            .parser
            .parse(&self.current_text)
            .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("Parse error: {}", e)))?;
        
        let ctx = Context::new();
        let partial = self
            .parser
            .partial(&self.current_text)
            .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("Parse error: {}", e)))?;
        
        let has_valid = partial.roots.iter().any(|root| {
            root.is_complete() && matches!(
                check_tree_with_context(root, &self.grammar, &ctx),
                TreeStatus::Valid(_)
            )
        });
        
        if !has_valid {
            return Err(PyTypeError::new_err(format!(
                "Type error: '{}' has no complete well-typed parse",
                self.current_text
            )));
        }
        
        Ok(ast.serialize())
    }

}

impl ConstrainedGenerator {
    // typed_completions filters out ill-typed parses before computing completions
    fn get_completions(&mut self) -> PyResult<CompletionSet> {
        let ast = self
            .parser
            .partial(&self.current_text)
            .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(format!("Parse error: {}", e)))?;
        Ok(ast.typed_completions(&self.grammar))
    }
}

#[pyclass]
pub struct ConstrainedLogitsProcessor {
    generator: ConstrainedGenerator,
    token_to_str: Vec<String>,
    eos_token_id: Option<usize>,
}

#[pymethods]
impl ConstrainedLogitsProcessor {
    #[new]
    #[pyo3(signature = (grammar, eos_token_id=None))]
    fn new(grammar: &Grammar, eos_token_id: Option<usize>) -> PyResult<Self> {
        let generator = ConstrainedGenerator::new(grammar);
        Ok(Self {
            generator,
            token_to_str: Vec::new(),
            eos_token_id,
        })
    }

    fn init_vocab(&mut self, tokens: Vec<String>) -> PyResult<()> {
        self.token_to_str = tokens;
        Ok(())
    }

    fn reset(&mut self) {
        self.generator.reset();
    }

    fn feed_token(&mut self, token_str: &str) -> PyResult<bool> {
        self.generator.feed_raw(token_str)
    }

    fn get_allowed_tokens(&mut self) -> PyResult<Vec<usize>> {
        if self.token_to_str.is_empty() {
            return Err(PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(
                "Vocabulary not initialized. Call init_vocab first.",
            ));
        }

        let mut allowed = self.generator.get_valid_token_indices(self.token_to_str.clone())?;

        if let Some(eos_id) = self.eos_token_id {
            if self.generator.is_complete()? && !allowed.contains(&eos_id) {
                allowed.push(eos_id);
            }
        }

        Ok(allowed)
    }

    fn current_text(&self) -> String {
        self.generator.current_text()
    }

    fn is_complete(&mut self) -> PyResult<bool> {
        self.generator.is_complete()
    }
}

#[pyfunction]
fn regex_matches(pattern: &str, text: &str) -> PyResult<bool> {
    let regex = DerivativeRegex::from_str(pattern)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyValueError, _>(format!("Invalid regex: {}", e)))?;
    Ok(regex.match_full(text))
}

#[pyfunction]
fn regex_prefix_valid(pattern: &str, prefix: &str) -> PyResult<bool> {
    let regex = DerivativeRegex::from_str(pattern)
        .map_err(|e| PyErr::new::<pyo3::exceptions::PyValueError, _>(format!("Invalid regex: {}", e)))?;
    match regex.prefix_match(prefix) {
        p7::regex::PrefixStatus::NoMatch => Ok(false),
        _ => Ok(true),
    }
}

#[pymodule]
fn proposition_7(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<Grammar>()?;
    m.add_class::<ConstrainedGenerator>()?;
    m.add_class::<ConstrainedLogitsProcessor>()?;
    m.add_function(wrap_pyfunction!(regex_matches, m)?)?;
    m.add_function(wrap_pyfunction!(regex_prefix_valid, m)?)?;
    Ok(())
}
