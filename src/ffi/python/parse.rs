use pyo3::prelude::*;

use crate::domains::typing::{TypingRuntime};
use crate::engine::grammar::{Segment, SPG};
use crate::engine::parse::arena::{ChildRef, ParseArena};
use crate::engine::structure::ast::FusionAST;

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
    pub(crate) fn from_fusion(ast: &FusionAST, runtime: TypingRuntime) -> Self {
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
        self.runtime
            .evidence_of(evidence)
            .map(|ty| format!("{}", ty))
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

// ── Tree-folding helpers ─────────────────────────────────────────────────────

fn fold_node(arena: &ParseArena, grammar: &SPG, node_id: usize) -> PyNode {
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

fn fold_child(arena: &ParseArena, grammar: &SPG, child: &ChildRef) -> PyChild {
    match child {
        ChildRef::Node(id) => PyChild {
            kind: "node".into(),
            node: Some(fold_node(arena, grammar, *id)),
            terminal_text: None,
            terminal_complete: None,
        },
        ChildRef::Terminal(lexeme) => PyChild {
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
            ChildRef::Node(cid) => {
                let s = text_from_node(arena, _segments, *cid);
                if !s.is_empty() {
                    parts.push(s);
                }
            }
            ChildRef::Terminal(lexeme) => {
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
