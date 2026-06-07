use pyo3::exceptions::PyValueError;
use pyo3::prelude::*;
use std::collections::HashMap;

use super::typing::PyTerm;
use crate::engine::grammar::{Production, SPG, Segment, Symbol};
use crate::typing::{Subst, Term, compile, render, term, unify_modulo};

// ═══════════════════════════════════════════════════════════════════════════════
// PyGrammar — Grammar inspection
// ═══════════════════════════════════════════════════════════════════════════════

#[pyclass(unsendable, name = "SPG")]
pub struct PyGrammar {
    pub(crate) inner: SPG,
}

#[pymethods]
impl PyGrammar {
    #[new]
    fn new(source: &str) -> PyResult<Self> {
        let g = SPG::load(source)
            .map_err(|e| PyValueError::new_err(format!("grammar load error: {e}")))?;
        Ok(Self { inner: g })
    }

    #[getter]
    fn start(&self) -> Option<&str> {
        self.inner.start.as_deref()
    }

    fn nonterminals(&self) -> Vec<String> {
        self.inner.nonterminals.clone()
    }

    fn productions(&self, nt: &str) -> PyResult<Vec<PyProduction>> {
        let prods = self
            .inner
            .productions
            .get(nt)
            .ok_or_else(|| PyValueError::new_err(format!("unknown nonterminal: {nt}")))?;
        Ok(prods.iter().map(|p| PyProduction::from_inner(p)).collect())
    }

    /// Rule name attached to a nonterminal, if any.
    fn nt_rule(&self, nt: &str) -> Option<&str> {
        self.inner.nt_rule(nt).map(|s| s.as_str())
    }

    fn rule_names(&self) -> Vec<String> {
        self.inner.rules.keys().cloned().collect()
    }

    fn specials(&self) -> Vec<String> {
        self.inner.specials().map(|v| v.clone()).unwrap_or_default()
    }

    fn tokenize(&self, input: &str) -> PyResult<Vec<PySegment>> {
        let mut g = self.inner.clone();
        let segs = g
            .tokenize(input)
            .map_err(|e| PyValueError::new_err(format!("tokenize error: {e}")))?;
        Ok(segs.into_iter().map(PySegment::from_inner).collect())
    }

    // ── Type layer (the low-level objects) ──────────────────────────────────

    /// Parse a type string into its term (tree), using the grammar's structure.
    fn parse_type(&self, s: &str) -> PyResult<PyTerm> {
        Term::parse(&self.inner, s)
            .map(|inner| PyTerm { inner })
            .map_err(PyValueError::new_err)
    }

    /// Render a term back to the grammar's surface syntax.
    fn show(&self, t: &PyTerm) -> String {
        render(&self.inner, &t.inner)
    }

    /// Normal form of a type under the grammar's rewrite theory.
    fn normalize(&self, s: &str) -> PyResult<PyTerm> {
        let t = Term::parse(&self.inner, s).map_err(PyValueError::new_err)?;
        Ok(PyTerm {
            inner: self.inner.normalizer().normalize(&t),
        })
    }

    /// Unify two types in the free theory: `var → type` bindings, or `None` on
    /// clash.
    fn unify(&self, a: &str, b: &str) -> PyResult<Option<HashMap<String, String>>> {
        self.unify_impl(a, b, false)
    }

    /// Unify two types modulo the rewrite theory (normalize, then unify).
    fn unify_modulo(&self, a: &str, b: &str) -> PyResult<Option<HashMap<String, String>>> {
        self.unify_impl(a, b, true)
    }

    /// The declared rewrite theory, as `(lhs, rhs)` source pairs.
    fn rewrites(&self) -> Vec<(String, String)> {
        self.inner.rewrites.clone()
    }

    /// The type signature: every constructor (nonterminal) with an arity it
    /// appears at, sorted.
    fn signature(&self) -> Vec<(String, usize)> {
        let mut out: Vec<(String, usize)> = Vec::new();
        for (nt, prods) in &self.inner.productions {
            for p in prods {
                let arity = p
                    .rhs
                    .iter()
                    .filter(|s| matches!(s, Symbol::Nonterminal { .. }))
                    .count();
                let entry = (nt.clone(), arity);
                if !out.contains(&entry) {
                    out.push(entry);
                }
            }
        }
        out.sort();
        out
    }

    /// The compiled IR of a typing rule: the instruction stream it lowers to.
    fn ir(&self, rule: &str) -> PyResult<String> {
        let r = self
            .inner
            .rules
            .get(rule)
            .ok_or_else(|| PyValueError::new_err(format!("no rule '{rule}'")))?;
        Ok(compile(r, &self.inner.type_trees()).to_string())
    }
}

impl PyGrammar {
    fn unify_impl(
        &self,
        a: &str,
        b: &str,
        modulo: bool,
    ) -> PyResult<Option<HashMap<String, String>>> {
        let ta = Term::parse(&self.inner, a).map_err(PyValueError::new_err)?;
        let tb = Term::parse(&self.inner, b).map_err(PyValueError::new_err)?;
        let mut s = Subst::new();
        let ok = if modulo {
            unify_modulo(&self.inner.normalizer(), &ta, &tb, &mut s, true)
        } else {
            term::unify(&ta, &tb, &mut s, true)
        };
        if !ok {
            return Ok(None);
        }
        let map = s
            .iter()
            .map(|(k, v)| (k.clone(), render(&self.inner, &term::apply(v, &s))))
            .collect();
        Ok(Some(map))
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// PyProduction — Read-only production view
// ═══════════════════════════════════════════════════════════════════════════════

#[pyclass(unsendable, name = "Production")]
#[derive(Clone)]
pub struct PyProduction {
    symbols: Vec<PySymbol>,
}

impl PyProduction {
    pub(crate) fn from_inner(p: &Production) -> Self {
        Self {
            symbols: p.rhs.iter().map(|s| PySymbol::from_inner(s)).collect(),
        }
    }
}

#[pymethods]
impl PyProduction {
    /// RHS symbols as a list.
    #[getter]
    fn rhs(&self) -> Vec<PySymbol> {
        self.symbols.clone()
    }

    /// Number of symbols on the RHS.
    #[getter]
    fn len(&self) -> usize {
        self.symbols.len()
    }

    fn __repr__(&self) -> String {
        let parts: Vec<String> = self.symbols.iter().map(|s| s.__repr__()).collect();
        format!("Production({})", parts.join(" "))
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// PySymbol — Terminal or nonterminal symbol
// ═══════════════════════════════════════════════════════════════════════════════

#[pyclass(unsendable, name = "Symbol")]
#[derive(Clone)]
pub struct PySymbol {
    kind: String, // "terminal" or "nonterminal"
    name: String, // nonterminal name or regex pattern
    binding: Option<String>,
}

impl PySymbol {
    pub(crate) fn from_inner(s: &Symbol) -> Self {
        match s {
            Symbol::Nonterminal { name, binding } => Self {
                kind: "nonterminal".into(),
                name: name.clone(),
                binding: binding.clone(),
            },
            Symbol::Terminal { regex, binding } => Self {
                kind: "terminal".into(),
                name: regex.to_pattern(),
                binding: binding.clone(),
            },
        }
    }
}

#[pymethods]
impl PySymbol {
    /// "terminal" or "nonterminal".
    #[getter]
    fn kind(&self) -> &str {
        &self.kind
    }

    /// Nonterminal name, or regex pattern for terminals.
    #[getter]
    fn name(&self) -> &str {
        &self.name
    }

    /// Binding name if this symbol is annotated with one.
    #[getter]
    fn binding(&self) -> Option<&str> {
        self.binding.as_deref()
    }

    /// Whether this is a terminal symbol.
    fn is_terminal(&self) -> bool {
        self.kind == "terminal"
    }

    /// Whether this symbol has a binding annotation.
    fn has_binding(&self) -> bool {
        self.binding.is_some()
    }

    fn __repr__(&self) -> String {
        let bind = self
            .binding
            .as_ref()
            .map(|b| format!("[{}]", b))
            .unwrap_or_default();
        format!("{}{}{}", self.kind, bind, self.name)
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// PySegment — Tokenized segment
// ═══════════════════════════════════════════════════════════════════════════════

#[pyclass(unsendable, name = "Segment")]
#[derive(Clone)]
pub struct PySegment {
    text: String,
    start: usize,
    end: usize,
    index: usize,
}

impl PySegment {
    pub(crate) fn from_inner(s: Segment) -> Self {
        Self {
            text: s.text().to_string(),
            start: s.start,
            end: s.end,
            index: s.index,
        }
    }
}

#[pymethods]
impl PySegment {
    /// The text of this segment.
    #[getter]
    fn text(&self) -> &str {
        &self.text
    }

    /// Byte start position in the original input.
    #[getter]
    fn start(&self) -> usize {
        self.start
    }

    /// Byte end position in the original input.
    #[getter]
    fn end(&self) -> usize {
        self.end
    }

    /// Index in the token stream.
    #[getter]
    fn index(&self) -> usize {
        self.index
    }

    /// Length in bytes.
    #[getter]
    fn len(&self) -> usize {
        self.end.saturating_sub(self.start)
    }

    fn __repr__(&self) -> String {
        format!("Segment('{}' @ {})", self.text, self.index)
    }
}
