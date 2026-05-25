use pyo3::exceptions::{PyRuntimeError, PyValueError};
use pyo3::prelude::*;

use super::grammar::PyGrammar;
use super::parse::PyAst;
use crate::domains::typing::{Context, Type, TypingDomain, TypingRule, TypingSynth};
use crate::engine::grammar::SPG;
use crate::regex::{PrefixStatus, Regex};

// ═══════════════════════════════════════════════════════════════════════════════
// PyTypingRule — Read-only view of a typing rule
// ═══════════════════════════════════════════════════════════════════════════════

#[pyclass(unsendable, name = "TypingRule")]
pub struct PyTypingRule {
    inner: TypingRule,
}

#[pymethods]
impl PyTypingRule {
    /// Rule name.
    #[getter]
    fn name(&self) -> &str {
        &self.inner.name
    }

    /// Premise count.
    fn premise_count(&self) -> usize {
        self.inner.premises.len()
    }

    /// Pretty-printed rule text.
    fn pretty(&self, indent: usize) -> String {
        self.inner.pretty(indent)
    }

    /// Binding names referenced by this rule.
    fn bindings(&self) -> Vec<String> {
        self.inner
            .used_bindings()
            .into_iter()
            .map(|s| s.to_string())
            .collect()
    }

    fn __repr__(&self) -> String {
        format!("TypingRule('{}')", self.inner.name)
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// PySynthesizer — Type checker / parser
// ═══════════════════════════════════════════════════════════════════════════════

#[pyclass(unsendable, name = "Synthesizer")]
pub struct PySynthesizer {
    spec_source: String,
    synth: TypingSynth,
    ctx: Context,
}

#[pymethods]
impl PySynthesizer {
    #[new]
    #[pyo3(signature = (spec_source, input = ""))]
    fn new(spec_source: String, input: &str) -> PyResult<Self> {
        let grammar = SPG::<TypingDomain>::load(&spec_source)
            .map_err(|e| PyValueError::new_err(format!("failed to load grammar: {e}")))?;
        let synth = TypingSynth::new(grammar, input);
        Ok(Self {
            spec_source,
            synth,
            ctx: Context::new(),
        })
    }

    /// Re-create the internal synthesizer with new input.
    fn set_input(&mut self, input: &str) -> PyResult<()> {
        let grammar = SPG::<TypingDomain>::load(&self.spec_source)
            .map_err(|e| PyValueError::new_err(format!("failed to load grammar: {e}")))?;
        self.synth = TypingSynth::new(grammar, input);
        Ok(())
    }

    /// Current accumulated input.
    fn input(&self) -> String {
        self.synth.input().to_string()
    }

    /// Parse, returning an AST string.
    fn parse(&mut self) -> PyResult<String> {
        self.synth
            .parse_with(&self.ctx)
            .map(|ast| ast.to_string())
            .map_err(PyRuntimeError::new_err)
    }

    /// Feed one token (state-altering).
    fn feed(&mut self, token: &str) -> PyResult<String> {
        self.synth
            .feed_with(token, &self.ctx)
            .map(|ast| ast.to_string())
            .map_err(PyRuntimeError::new_err)
    }

    /// Try feeding one token without altering state.
    fn try_feed(&mut self, token: &str) -> PyResult<String> {
        self.synth
            .try_feed(token)
            .map(|ast| ast.to_string())
            .map_err(PyRuntimeError::new_err)
    }

    /// Add a variable to the typing context.
    fn add_to_ctx(&mut self, name: &str, ty: &str) -> PyResult<()> {
        let ty = Type::parse_raw(ty)
            .map_err(|e| PyValueError::new_err(format!("invalid type '{ty}': {e}")))?;
        self.ctx.add(name.to_string(), ty);
        Ok(())
    }

    /// Clear the typing context.
    fn clear_ctx(&mut self) {
        self.ctx = Context::new();
    }

    /// Return an AST string if parsing succeeded.
    fn ast_str(&mut self) -> Option<String> {
        self.synth.ast().ok().map(|a| a.to_string())
    }

    /// Whether the parsed tree is complete.
    fn is_complete(&mut self) -> bool {
        match self.synth.parse_with(&self.ctx) {
            Ok(ast) => ast.is_complete(),
            Err(_) => false,
        }
    }

    /// Expose the grammar for inspection.
    fn grammar(&self) -> PyGrammar {
        PyGrammar {
            inner: self.synth.grammar().clone(),
        }
    }

    /// Get a specific typing rule by name.
    fn get_rule(&self, name: &str) -> Option<PyTypingRule> {
        self.synth
            .grammar()
            .rules
            .get(name)
            .cloned()
            .map(|inner| PyTypingRule { inner })
    }

    /// Return the current AST as a structured object.
    fn ast(&mut self) -> PyResult<PyAst> {
        let fusion = self
            .synth
            .parse_with(&self.ctx)
            .map_err(|e| PyRuntimeError::new_err(format!("parse error: {e}")))?;
        let runtime = self.synth.runtime().clone();
        Ok(PyAst::from_fusion(&fusion, runtime))
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// PyRegex
// ═══════════════════════════════════════════════════════════════════════════════

#[pyclass(unsendable, name = "Regex")]
#[derive(Clone)]
pub struct PyRegex {
    regex: Regex,
}

#[pymethods]
impl PyRegex {
    #[new]
    fn new(pattern: &str) -> PyResult<Self> {
        let regex = Regex::from_str(pattern)
            .map_err(|e| PyValueError::new_err(format!("invalid regex: {e}")))?;
        Ok(Self { regex })
    }

    fn __repr__(&self) -> String {
        format!("Regex({})", self.regex.to_pattern())
    }

    fn __str__(&self) -> String {
        self.regex.to_pattern()
    }

    fn matches(&self, text: &str) -> bool {
        self.regex.matches(text)
    }

    fn prefix_match(&self, prefix: &str) -> PyPrefixStatus {
        PyPrefixStatus::from(self.regex.prefix_match(prefix))
    }

    fn derivative(&self, text: &str) -> Self {
        Self {
            regex: self.regex.derivative(text),
        }
    }

    fn deriv(&self, character: &str) -> PyResult<Self> {
        let mut chars = character.chars();
        let c = chars
            .next()
            .ok_or_else(|| PyValueError::new_err("character must be a non-empty string"))?;
        if chars.next().is_some() {
            return Err(PyValueError::new_err(
                "character must be a single Unicode character",
            ));
        }
        Ok(Self {
            regex: self.regex.deriv(c),
        })
    }

    fn is_empty(&self) -> bool {
        self.regex.is_empty()
    }

    fn is_nullable(&self) -> bool {
        self.regex.is_nullable()
    }

    fn match_len(&self, text: &str) -> Option<usize> {
        self.regex.match_len(text)
    }

    fn to_pattern(&self) -> String {
        self.regex.to_pattern()
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// PyPrefixStatus
// ═══════════════════════════════════════════════════════════════════════════════

#[pyclass(unsendable, name = "PrefixStatus")]
#[derive(Clone)]
pub struct PyPrefixStatus {
    kind: String,
    regex: Option<PyRegex>,
}

#[pymethods]
impl PyPrefixStatus {
    #[getter]
    fn kind(&self) -> &str {
        &self.kind
    }

    #[getter]
    fn regex(&self) -> Option<PyRegex> {
        self.regex.clone()
    }

    fn __repr__(&self) -> String {
        match &self.regex {
            Some(regex) => format!("PrefixStatus.{}({})", self.kind, regex.to_pattern()),
            None => format!("PrefixStatus.{}", self.kind),
        }
    }

    fn is_complete(&self) -> bool {
        matches!(self.kind.as_str(), "complete" | "extensible")
    }

    fn is_prefix(&self) -> bool {
        self.kind == "prefix"
    }

    fn is_extensible(&self) -> bool {
        self.kind == "extensible"
    }

    fn is_no_match(&self) -> bool {
        self.kind == "no_match"
    }
}

impl From<PrefixStatus> for PyPrefixStatus {
    fn from(status: PrefixStatus) -> Self {
        match status {
            PrefixStatus::Extensible(regex) => Self {
                kind: "extensible".to_string(),
                regex: Some(PyRegex { regex }),
            },
            PrefixStatus::Complete => Self {
                kind: "complete".to_string(),
                regex: None,
            },
            PrefixStatus::Prefix(regex) => Self {
                kind: "prefix".to_string(),
                regex: Some(PyRegex { regex }),
            },
            PrefixStatus::NoMatch => Self {
                kind: "no_match".to_string(),
                regex: None,
            },
        }
    }
}
