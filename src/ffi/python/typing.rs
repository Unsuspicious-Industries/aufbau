use std::collections::HashMap;

use pyo3::exceptions::{PyRuntimeError, PyValueError};
use pyo3::prelude::*;

use super::grammar::PyGrammar;
use super::parse::PyAst;
use crate::grammar::SPG;
use crate::synth::verification::Verification;
use crate::typing::{Context, Term, Type, TypingRule, TypingSynth, render};

// ═══════════════════════════════════════════════════════════════════════════════
// PyVerification — the result of Synthesizer.verify()
// ═══════════════════════════════════════════════════════════════════════════════

#[pyclass(unsendable, name = "Verification")]
pub struct PyVerification {
    pub(crate) inner: Verification,
}

#[pymethods]
impl PyVerification {
    /// `"typed"` | `"live"` | `"dead"`.
    #[getter]
    fn status(&self) -> &str {
        self.inner.status
    }

    /// Every distinct complete-root type, normalized and rendered. More than
    /// one entry means the complete roots disagree.
    #[getter]
    fn root_types(&self) -> Vec<String> {
        self.inner.root_types.clone()
    }

    /// Whether the goal type was met, or `None` when no goal was given.
    #[getter]
    fn goal_satisfied(&self) -> Option<bool> {
        self.inner.goal_satisfied
    }

    /// Whether the complete roots disagree on the type.
    fn is_ambiguous(&self) -> bool {
        self.inner.is_ambiguous()
    }

    fn __repr__(&self) -> String {
        format!(
            "Verification(status={:?}, root_types={:?}, goal_satisfied={:?})",
            self.inner.status, self.inner.root_types, self.inner.goal_satisfied
        )
    }
}

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
    fn __repr__(&self) -> &'static str {
        "Term(...)"
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
    /// The one authoritative context lives inside `synth`. Keeping a second
    /// copy here and passing it back on every call meant a mutation could be
    /// visible to one operation and not another, and it dropped the cached
    /// parse tree on every call because re-installing a context invalidates it.
    synth: TypingSynth,
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
        })
    }

    /// A synthesizer over an already-built grammar (no `.auf` re-parse).
    #[staticmethod]
    #[pyo3(signature = (grammar, input = ""))]
    fn from_grammar(grammar: &PyGrammar, input: &str) -> Self {
        Self {
            synth: TypingSynth::new(grammar.inner.clone(), input),
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
            .ast()
            .map(|ast| ast.to_string())
            .map_err(PyRuntimeError::new_err)
    }

    /// Feed one token (state-altering).
    fn feed(&mut self, token: &str) -> PyResult<String> {
        self.synth
            .feed(token)
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
            .synth
            .ctx()
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
        match self.synth.ast() {
            Ok(ast) if ast.is_complete() => "typed",
            Ok(_) => "live",
            Err(_) => "dead",
        }
    }

    /// The type of a complete root, as a term.
    fn root_type(&mut self) -> Option<PyTerm> {
        let ast = self.synth.ast().ok()?;
        let rt = self.synth.runtime().clone();
        ast.roots()
            .filter(crate::ast::FusionNode::is_complete)
            .find_map(|r| rt.evidence_of(r.evidence()))
            .map(|inner| PyTerm { inner })
    }

    /// Replace the entire typing context.
    ///
    /// Every type is parsed with the active grammar *before* anything is
    /// mutated, so the replacement either happens whole or not at all: if any
    /// binding fails to parse, the previous context still stands. Every
    /// subsequent operation — `mask`, `feed`, `parse`, `status`, `verify`,
    /// `ast`, `in_scope` — observes the new bindings immediately.
    ///
    /// A type is any term the grammar derives. Nothing here interprets names or
    /// dispatches on the shape of a type string.
    fn set_context(&mut self, bindings: HashMap<String, String>) -> PyResult<()> {
        let g = self.synth.grammar();
        let mut next = Context::new();
        // Sorted so a failure reports the same binding every run.
        let mut pairs: Vec<_> = bindings.iter().collect();
        pairs.sort();
        for (name, ty) in pairs {
            let parsed = Type::parse(g, ty).map_err(|e| {
                PyValueError::new_err(format!("invalid type '{ty}' for '{name}': {e}"))
            })?;
            next.add(name.clone(), parsed);
        }
        self.synth.set_context(next);
        Ok(())
    }

    /// The accumulated typing context, rendered by the active grammar. This
    /// round-trips through `set_context` so callers carry the engine's
    /// authoritative context forward without reimplementing effect application.
    fn context(&self) -> Vec<(String, String)> {
        let g = self.synth.grammar();
        let mut bindings: Vec<_> = self
            .synth
            .ctx()
            .bindings
            .iter()
            .map(|(name, ty)| (name.clone(), render(g, ty)))
            .collect();
        bindings.sort_by(|(left, _), (right, _)| left.cmp(right));
        bindings
    }

    /// Add one binding. Compatibility wrapper over `set_context`.
    fn add_to_ctx(&mut self, name: &str, ty: &str) -> PyResult<()> {
        let parsed = Type::parse(self.synth.grammar(), ty)
            .map_err(|e| PyValueError::new_err(format!("invalid type '{ty}': {e}")))?;
        let mut next = self.synth.ctx().clone();
        next.add(name.to_string(), parsed);
        self.synth.set_context(next);
        Ok(())
    }

    /// Clear the typing context. Compatibility wrapper over `set_context`.
    fn clear_ctx(&mut self) {
        self.synth.set_context(Context::new());
    }

    /// Verify the current input, optionally against a goal type.
    ///
    /// Unlike `root_type()`, which returns whichever complete root comes first,
    /// this reports *every* complete-root type. More than one entry in
    /// `root_types` means the roots genuinely disagree. State-free.
    #[pyo3(signature = (expected_type = None))]
    fn verify(&mut self, expected_type: Option<&str>) -> PyResult<PyVerification> {
        crate::synth::verification::verify(&mut self.synth, expected_type)
            .map(|inner| PyVerification { inner })
            .map_err(PyValueError::new_err)
    }

    /// Whether the parsed tree is complete.
    fn is_complete(&mut self) -> bool {
        match self.synth.ast() {
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
            .ast()
            .map_err(|e| PyRuntimeError::new_err(format!("parse error: {e}")))?;
        let runtime = self.synth.runtime().clone();
        Ok(PyAst::from_fusion(&fusion, runtime))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;
    use std::sync::OnceLock;

    fn ml() -> SPG {
        static GRAMMAR: OnceLock<SPG> = OnceLock::new();
        GRAMMAR
            .get_or_init(|| {
                SPG::load(include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/examples/ml.auf")))
                    .unwrap()
            })
            .clone()
    }

    fn type_source() -> impl Strategy<Value = String> {
        prop::sample::select(vec![
            "int".to_string(),
            "bool".to_string(),
            "int list".to_string(),
        ])
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(16))]

        #[test]
        fn context_round_trips_binding_sets(
            bindings in prop::collection::hash_map("[a-z][a-z0-9_]{0,5}", type_source(), 0..4),
        ) {
            let grammar = ml();
            let mut synth = PySynthesizer {
                synth: TypingSynth::new(grammar.clone(), ""),
            };
            synth.set_context(bindings.clone()).unwrap();
            let original = synth.synth.ctx().bindings.clone();

            let recovered = synth.context();
            prop_assert!(recovered.windows(2).all(|pair| pair[0].0 < pair[1].0));
            let mut expected_names: Vec<_> = bindings.keys().collect();
            expected_names.sort();
            prop_assert_eq!(
                recovered.iter().map(|(name, _)| name).collect::<Vec<_>>(),
                expected_names,
            );

            let normalizer = crate::typing::loader::normalizer(&grammar);
            synth.set_context(recovered.clone().into_iter().collect()).unwrap();
            for (name, rendered) in &recovered {
                let reparsed = &synth.synth.ctx().bindings[name];
                let mut subst = crate::typing::Subst::new();
                prop_assert!(crate::typing::unify_modulo(
                    &normalizer,
                    &original[name],
                    reparsed,
                    &mut subst,
                    true,
                ));
                prop_assert_eq!(render(&grammar, reparsed), rendered.as_str());
            }

            prop_assert_eq!(synth.context(), recovered);
        }
    }

    #[test]
    fn context_empty() {
        let mut synth = PySynthesizer {
            synth: TypingSynth::new(ml(), ""),
        };
        synth.set_context(HashMap::new()).unwrap();
        assert!(synth.context().is_empty());
    }

    #[test]
    fn context_renders_applied_type() {
        let mut synth = PySynthesizer {
            synth: TypingSynth::new(ml(), ""),
        };
        synth
            .set_context(HashMap::from([(String::from("paths"), String::from("int list"))]))
            .unwrap();
        assert_eq!(synth.context(), vec![(String::from("paths"), String::from("int list"))]);
    }
}
