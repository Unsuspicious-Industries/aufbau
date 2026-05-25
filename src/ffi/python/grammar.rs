use pyo3::exceptions::PyValueError;
use pyo3::prelude::*;

use crate::domains::typing::TypingDomain;
use crate::engine::grammar::{Production, Segment, Symbol, SPG};

// ═══════════════════════════════════════════════════════════════════════════════
// PyGrammar — Grammar inspection
// ═══════════════════════════════════════════════════════════════════════════════

#[pyclass(unsendable, name = "SPG")]
pub struct PyGrammar {
    pub(crate) inner: SPG<TypingDomain>,
}

#[pymethods]
impl PyGrammar {
    #[new]
    fn new(source: &str) -> PyResult<Self> {
        let g = SPG::<TypingDomain>::load(source)
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

    /// Whether a nonterminal is transparent.
    fn is_transparent(&self, nt: &str) -> bool {
        self.inner.is_transparent_nt(nt)
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
