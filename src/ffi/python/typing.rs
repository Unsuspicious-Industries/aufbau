use pyo3::exceptions::{PyRuntimeError, PyValueError};
use pyo3::prelude::*;

use super::grammar::PyGrammar;
use super::parse::PyAst;
use crate::engine::grammar::SPG;
use crate::typing::{Context, Term, Type, TypingRule, TypingSynth};

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
// PyTerm — a type as a tree (the low-level object: Var | Con(label, kids) | Leaf)
// ═══════════════════════════════════════════════════════════════════════════════

#[pyclass(unsendable, name = "Term")]
#[derive(Clone)]
pub struct PyTerm {
    pub(crate) inner: Term,
}

#[pymethods]
impl PyTerm {
    fn __repr__(&self) -> String {
        format!("Term({})", self.inner)
    }
    fn __str__(&self) -> String {
        self.inner.to_string()
    }
    /// Constructor label (the nonterminal), or `None` for a hole or leaf.
    fn label(&self) -> Option<String> {
        match &self.inner {
            Term::Con(l, _) => Some(l.clone()),
            _ => None,
        }
    }
    /// Child terms of a constructor (empty for a hole or leaf).
    fn children(&self) -> Vec<PyTerm> {
        match &self.inner {
            Term::Con(_, kids) => kids.iter().cloned().map(|inner| PyTerm { inner }).collect(),
            _ => vec![],
        }
    }
    fn is_var(&self) -> bool {
        matches!(self.inner, Term::Var(_))
    }
    fn is_leaf(&self) -> bool {
        matches!(self.inner, Term::Leaf(_))
    }
    fn is_con(&self) -> bool {
        matches!(self.inner, Term::Con(..))
    }
    /// No unification variables: a fully determined type.
    fn is_ground(&self) -> bool {
        self.inner.is_ground()
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// PySynthesizer — Type checker / parser
// ═══════════════════════════════════════════════════════════════════════════════

#[pyclass(unsendable, name = "Synthesizer")]
pub struct PySynthesizer {
    synth: TypingSynth,
    ctx: Context,
}

#[pymethods]
impl PySynthesizer {
    #[new]
    #[pyo3(signature = (spec_source, input = ""))]
    fn new(spec_source: String, input: &str) -> PyResult<Self> {
        let grammar = SPG::load(&spec_source)
            .map_err(|e| PyValueError::new_err(format!("failed to load grammar: {e}")))?;
        Ok(Self {
            synth: TypingSynth::new(grammar, input),
            ctx: Context::new(),
        })
    }

    /// A synthesizer over an already-built grammar (no `.auf` re-parse).
    #[staticmethod]
    #[pyo3(signature = (grammar, input = ""))]
    fn from_grammar(grammar: &PyGrammar, input: &str) -> Self {
        Self {
            synth: TypingSynth::new(grammar.inner.clone(), input),
            ctx: Context::new(),
        }
    }

    /// Reset the input, keeping the loaded grammar.
    fn set_input(&mut self, input: &str) {
        self.synth.set_input(input);
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

    /// The constrained-generation mask: for each candidate continuation, can
    /// the current input still be extended by it? One Rust-side pass over the
    /// whole candidate set, no state change.
    fn mask(&mut self, candidates: Vec<String>) -> Vec<bool> {
        candidates
            .iter()
            .map(|t| self.synth.try_feed(t).is_ok())
            .collect()
    }

    /// In-scope names whose type unifies with `expected` (every name when
    /// `expected` is `None`). This is the var rule's membership constraint
    /// intersected with a type: the type-filtered set of identifiers a
    /// generator may emit at a hole, the masking signal the var rule denotes.
    #[pyo3(signature = (expected = None))]
    fn in_scope(&self, expected: Option<&str>) -> PyResult<Vec<String>> {
        let g = self.synth.grammar();
        let want = match expected {
            Some(s) => Some(
                Term::parse(g, s)
                    .map_err(|e| PyValueError::new_err(format!("invalid type '{s}': {e}")))?,
            ),
            None => None,
        };
        let norm = crate::typing::loader::normalizer(g);
        let mut names: Vec<String> = self
            .ctx
            .bindings
            .iter()
            .filter(|(_, ty)| match &want {
                None => true,
                Some(w) => {
                    let mut s = crate::typing::Subst::new();
                    crate::typing::unify_modulo(&norm, w, ty, &mut s, true)
                }
            })
            .map(|(name, _)| name.clone())
            .collect();
        names.sort();
        Ok(names)
    }

    /// The three-valued verdict on the current input: `"typed"` (a complete,
    /// well-typed parse), `"live"` (a completable prefix), or `"dead"`.
    fn status(&mut self) -> &'static str {
        match self.synth.parse_with(&self.ctx) {
            Ok(ast) if ast.is_complete() => "typed",
            Ok(_) => "live",
            Err(_) => "dead",
        }
    }

    /// The type of a complete root, as a term.
    fn root_type(&mut self) -> Option<PyTerm> {
        let ast = self.synth.parse_with(&self.ctx).ok()?;
        let rt = self.synth.runtime().clone();
        ast.roots()
            .filter(crate::engine::structure::FusionNode::is_complete)
            .find_map(|r| rt.evidence_of(r.evidence()))
            .map(|inner| PyTerm { inner })
    }

    /// Add a variable to the typing context. The type is parsed with the grammar
    /// into its tree, so a structured type (`A -> B`) becomes a constructor, not a
    /// flat leaf.
    fn add_to_ctx(&mut self, name: &str, ty: &str) -> PyResult<()> {
        let ty = Type::parse(self.synth.grammar(), ty)
            .map_err(|e| PyValueError::new_err(format!("invalid type '{ty}': {e}")))?;
        self.ctx.add(name.to_string(), ty);
        Ok(())
    }

    /// Clear the typing context.
    fn clear_ctx(&mut self) {
        self.ctx = Context::new();
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
