//! Python FFI bindings for aufbau.
//!
//! Exposes grammar inspection, tokenization, parsing, and the typing domain.
//! Future: `PythonDomain` for implementing `ConstraintDomain` in Python.

use pyo3::exceptions::{PyRuntimeError, PyValueError};
use pyo3::prelude::*;

use crate::domains::typing::TypingRuntime;
use crate::domains::typing::{Context, Type, TypingDomain, TypingRule, TypingSynth};
use crate::engine::grammar::{Production, Segment, Symbol, SPG};
use crate::engine::parse::arena::ParseArena;
use crate::engine::structure::ast::FusionAST;
use crate::regex::{PrefixStatus, Regex};

// ═══════════════════════════════════════════════════════════════════════════════
// PyGrammar — Grammar inspection
// ═══════════════════════════════════════════════════════════════════════════════

#[pyclass(unsendable, name = "SPG")]
pub struct PyGrammar {
    inner: SPG<TypingDomain>,
}

#[pymethods]
impl PyGrammar {
    /// Load a grammar from .auf source text.
    #[new]
    fn new(source: &str) -> PyResult<Self> {
        let g = SPG::<TypingDomain>::load(source)
            .map_err(|e| PyValueError::new_err(format!("grammar load error: {e}")))?;
        Ok(Self { inner: g })
    }

    /// Grammar name (derived from source or empty).
    #[getter]
    fn name(&self) -> &str {
        &self.inner.name
    }

    /// Start symbol, if set.
    #[getter]
    fn start(&self) -> Option<&str> {
        self.inner.start.as_deref()
    }

    #[getter]
    fn nts(&self) -> PyResult<Vec<String>>{
        self.inner.nonterminals
    }

    /// Productions for a given nonterminal.
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

    /// All rule names in the grammar.
    fn rule_names(&self) -> Vec<String> {
        self.inner.rules.keys().cloned().collect()
    }

    /// Tokenize an input string.
    fn tokenize(&self, input: &str) -> PyResult<Vec<PySegment>> {
        let mut g = self.inner.clone();
        let segs = g
            .tokenize(input)
            .map_err(|e| PyValueError::new_err(format!("tokenize error: {e}")))?;
        Ok(segs.into_iter().map(PySegment::from_inner).collect())
    }

    /// Whether a nonterminal is marked as a bridge.
    fn is_bridge(&self, nt: &str) -> bool {
        self.inner.is_bridge_nt(nt)
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
    fn from_inner(p: &Production) -> Self {
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
    fn from_inner(s: &Symbol) -> Self {
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
    fn from_inner(s: Segment) -> Self {
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
// PyAst — Owned parse tree with type resolution
// ═══════════════════════════════════════════════════════════════════════════════

#[pyclass(unsendable, name = "Ast")]
pub struct PyAst {
    roots: Vec<PyNode>,
    node_count: usize,
    is_complete: bool,
    input: String,
    runtime: TypingRuntime,
}

impl PyAst {
    fn from_fusion(ast: &FusionAST<TypingDomain>, runtime: TypingRuntime) -> Self {
        let arena = ast.arena();
        let grammar = ast.grammar();
        let roots: Vec<PyNode> = ast
            .roots()
            .map(|n| fold_node(arena, grammar, n.node_id()))
            .collect();
        Self {
            roots,
            node_count: ast.node_count(),
            is_complete: ast.is_complete(),
            input: ast.text().to_string(),
            runtime,
        }
    }
}

#[pymethods]
impl PyAst {
    /// Root nodes of the parse forest.
    #[getter]
    fn roots(&self) -> Vec<PyNode> {
        self.roots.clone()
    }

    /// Total arena node count.
    fn node_count(&self) -> usize {
        self.node_count
    }

    /// Whether any root is a complete parse.
    fn is_complete(&self) -> bool {
        self.is_complete
    }

    /// The input string that was parsed.
    #[getter]
    fn input(&self) -> &str {
        &self.input
    }

    /// Resolve an evidence ID to a type string.
    fn type_of(&self, evidence: usize) -> Option<String> {
        self.runtime.evidence_of(evidence).map(|ty| format!("{}", ty))
    }

    fn __repr__(&self) -> String {
        format!(
            "Ast(roots={}, nodes={}, complete={})",
            self.roots.len(),
            self.node_count,
            self.is_complete
        )
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// PyNode — Owned parse-tree node
// ═══════════════════════════════════════════════════════════════════════════════

#[pyclass(unsendable, name = "Node")]
#[derive(Clone)]
pub struct PyNode {
    node_id: usize,
    evidence: usize,
    is_complete: bool,
    text: String,
    nt_name: String,
    span_start: u32,
    span_end: u32,
    children: Vec<PyChild>,
    rhs_len: usize,
}

impl PyNode {
    fn empty() -> Self {
        Self {
            node_id: 0,
            evidence: 0,
            is_complete: false,
            text: String::new(),
            nt_name: String::new(),
            span_start: 0,
            span_end: 0,
            children: vec![],
            rhs_len: 0,
        }
    }
}

#[pymethods]
impl PyNode {
    #[getter]
    fn nodeid(&self) -> usize {
        self.node_id
    }

    #[getter]
    fn evidence(&self) -> usize {
        self.evidence
    }

    fn is_complete(&self) -> bool {
        self.is_complete
    }

    #[getter]
    fn text(&self) -> &str {
        &self.text
    }

    fn nt_name(&self) -> &str {
        &self.nt_name
    }

    #[getter]
    fn start(&self) -> u32 {
        self.span_start
    }

    #[getter]
    fn end(&self) -> u32 {
        self.span_end
    }

    fn child_count(&self) -> usize {
        self.children.len()
    }

    #[getter]
    fn rhs(&self) -> usize {
        self.rhs_len
    }

    #[getter]
    fn children(&self) -> Vec<PyChild> {
        self.children.clone()
    }

    fn __repr__(&self) -> String {
        format!(
            "Node(id={}, eid={}, complete={}, text='{}', children={})",
            self.node_id,
            self.evidence,
            self.is_complete,
            self.text,
            self.children.len()
        )
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// PyChild — Either a sub-node or a terminal leaf
// ═══════════════════════════════════════════════════════════════════════════════

#[pyclass(unsendable, name = "Child")]
#[derive(Clone)]
pub struct PyChild {
    /// "node" or "terminal".
    kind: String,
    /// The child node, if kind == "node".
    node: Option<PyNode>,
    /// Terminal text, if kind == "terminal".
    terminal_text: Option<String>,
    /// Whether this terminal is a complete match.
    terminal_complete: Option<bool>,
}

#[pymethods]
impl PyChild {
    #[getter]
    fn kind(&self) -> &str {
        &self.kind
    }

    #[getter]
    fn node(&self) -> Option<PyNode> {
        self.node.clone()
    }

    fn terminal_text(&self) -> Option<&str> {
        self.terminal_text.as_deref()
    }

    fn terminal_complete(&self) -> Option<bool> {
        self.terminal_complete
    }

    fn __repr__(&self) -> String {
        match self.kind.as_str() {
            "node" => format!(
                "Child::Node({})",
                self.node
                    .as_ref()
                    .map(|n| format!("{}", n.node_id))
                    .unwrap_or_default()
            ),
            "terminal" => format!(
                "Child::Terminal('{}')",
                self.terminal_text.as_deref().unwrap_or("")
            ),
            _ => "Child::Unknown".into(),
        }
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

// ═══════════════════════════════════════════════════════════════════════════════
// Module registration
// ═══════════════════════════════════════════════════════════════════════════════

// ── Tree-folding helpers ─────────────────────────────────────────────────────

fn fold_node(arena: &ParseArena, grammar: &SPG<TypingDomain>, node_id: usize) -> PyNode {
    let node = arena.node(node_id);
    let (evidence, is_complete, span_start, span_end, nt_name, children, rhs_len) =
        if let Some(ref n) = node {
            let ev = n.evidence;
            let comp = n.is_complete();
            let ss = n.span.start;
            let se = n.span.end;
            let nt = grammar.nt(n.nt).unwrap_or("?").to_string();
            let mut kids = vec![];
            let mut rlen = 0;
            if let Some(alts) = arena.alts_for(node_id) {
                if let Some(alt) = alts.first() {
                    rlen = grammar.prod(alt.prod).map(|p| p.rhs.len()).unwrap_or(0);
                    for child in &alt.children {
                        kids.push(fold_child(arena, grammar, child));
                    }
                }
            }
            (ev, comp, ss, se, nt, kids, rlen)
        } else {
            (0, false, 0u32, 0u32, "?".to_string(), vec![], 0usize)
        };

    // Reconstruct text from the arena
    let text = text_from_node(arena, &[], node_id);

    PyNode {
        node_id,
        evidence,
        is_complete,
        text,
        nt_name,
        span_start,
        span_end,
        children,
        rhs_len,
    }
}

fn fold_child(
    arena: &ParseArena,
    grammar: &SPG<TypingDomain>,
    child: &crate::engine::parse::arena::ChildRef,
) -> PyChild {
    match child {
        crate::engine::parse::arena::ChildRef::Node(id) => PyChild {
            kind: "node".into(),
            node: Some(fold_node(arena, grammar, *id)),
            terminal_text: None,
            terminal_complete: None,
        },
        crate::engine::parse::arena::ChildRef::Terminal(lexeme) => PyChild {
            kind: "terminal".into(),
            node: None,
            terminal_text: lexeme.value(&[]).or(Some(String::new())),
            terminal_complete: Some(lexeme.complete),
        },
    }
}

fn text_from_node(arena: &ParseArena, _segments: &[Segment], node_id: usize) -> String {
    let Some(alts) = arena.alts_for(node_id) else {
        return String::new();
    };
    let Some(alt) = alts.first() else {
        return String::new();
    };
    let mut parts = vec![];
    for child in &alt.children {
        match child {
            crate::engine::parse::arena::ChildRef::Node(cid) => {
                let s = text_from_node(arena, _segments, *cid);
                if !s.is_empty() {
                    parts.push(s);
                }
            }
            crate::engine::parse::arena::ChildRef::Terminal(lexeme) => {
                if lexeme.complete {
                    if let Some(val) = lexeme.value(_segments) {
                        if !val.is_empty() {
                            parts.push(val);
                        }
                    }
                }
            }
        }
    }
    parts.join(" ")
}

#[pyfunction]
fn version() -> String {
    env!("CARGO_PKG_VERSION").to_string()
}

#[pymodule]
fn aufbau(_py: Python<'_>, m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<PyGrammar>()?;
    m.add_class::<PyProduction>()?;
    m.add_class::<PySymbol>()?;
    m.add_class::<PySegment>()?;
    m.add_class::<PyTypingRule>()?;
    m.add_class::<PySynthesizer>()?;
    m.add_class::<PyRegex>()?;
    m.add_class::<PyPrefixStatus>()?;
    m.add_function(wrap_pyfunction!(version, m)?)?;
    Ok(())
}

// ═══════════════════════════════════════════════════════════════════════════════
// Tests
// ═══════════════════════════════════════════════════════════════════════════════

#[cfg(test)]
mod tests {
    use super::*;

    const SPEC: &str = "start ::= 'x' 'y'";
    const TYPED_SPEC: &str = r#"
        Identifier ::= /[a-z]+/
        Variable(var) ::= Identifier[x]
        Expr ::= Variable

        x ∈ Γ
        ----------- (var)
        Γ(x)
    "#;

    #[test]
    fn grammar_load_and_inspect() {
        let g = PyGrammar::new(TYPED_SPEC).unwrap();
        assert_eq!(g.start(), Some("Expr"));
        assert_eq!(g.ntcount(), 3);
        assert!(!g.nonterminals().is_empty());
        // "var" rule should be present
        assert!(g.rule_names().contains(&"var".to_string()));
    }

    #[test]
    fn grammar_productions_inspect() {
        let g = PyGrammar::new(TYPED_SPEC).unwrap();
        let prods = g.productions("Variable").unwrap();
        assert_eq!(prods.len(), 1);
        let rhs = prods[0].rhs();
        assert_eq!(rhs.len(), 1);
        assert_eq!(rhs[0].kind(), "terminal");
        assert_eq!(rhs[0].binding(), Some("x"));
    }

    #[test]
    fn grammar_tokenize() {
        let g = PyGrammar::new(TYPED_SPEC).unwrap();
        let segs = g.tokenize("hello world").unwrap();
        assert_eq!(segs.len(), 2);
        assert_eq!(segs[0].text(), "hello");
        assert_eq!(segs[1].text(), "world");
    }

    #[test]
    fn synthesis_grammar_access() {
        let mut s = PySynthesizer::new(TYPED_SPEC.to_string(), "x").unwrap();
        let g = s.grammar();
        assert_eq!(g.start(), Some("Expr"));
        // Should be able to get a rule
        let rule = s.get_rule("var");
        assert!(rule.is_some());
        assert_eq!(rule.unwrap().name(), "var");
    }

    #[test]
    fn python_synth_tokens_and_feed() {
        let mut s = PySynthesizer::new(SPEC.to_string(), "").unwrap();
        s.feed("x").unwrap();
        assert_eq!(s.input(), "x");
    }

    #[test]
    fn python_synth_set_input_and_complete() {
        let mut s = PySynthesizer::new(SPEC.to_string(), "").unwrap();
        s.set_input("x y").unwrap();
        assert!(s.is_complete());
    }

    #[test]
    fn python_synth_exported_as_module_class() {
        pyo3::prepare_freethreaded_python();
        Python::with_gil(|py| {
            let module = PyModule::new(py, "aufbau").unwrap();
            super::aufbau(py, &module).unwrap();

            let synth_class = module.getattr("Synthesizer").unwrap();
            let instance = synth_class.call1((SPEC, "")).unwrap();

            let input = instance.call_method0("input").unwrap();
            assert_eq!(input.extract::<String>().unwrap(), "");
        });
    }

    #[test]
    fn python_regex_helpers_work() {
        let regex = PyRegex::new("a*b").unwrap();
        assert!(regex.matches("ab"));

        let prefix_status = regex.prefix_match("a");
        assert!(prefix_status.is_prefix());
        assert!(!prefix_status.is_no_match());

        let derived = regex.derivative("ab");
        assert!(derived.is_nullable());
    }
}
