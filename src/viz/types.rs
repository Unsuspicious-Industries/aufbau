use serde::Serialize;
use crate::logic::partial::{PartialAST, PartialNonTerminal, PartialProduction, PartialTerminal};
use crate::logic::ast::SourceSpan;

#[derive(Debug, Clone, Serialize)]
pub struct VizSpan {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "snake_case")] 
pub enum VizNodeKind {
    Terminal,
    NonTerminal,
    Mismatch,
}

#[derive(Debug, Clone, Serialize)]
pub struct VizNode {
    pub id: String,
    pub kind: VizNodeKind,
    pub label: String,
    pub binding: Option<String>,
    pub span: Option<VizSpan>,
    pub production_len: Option<usize>,
    pub parsed_symbols: Option<usize>,
    pub is_complete: Option<bool>,
    pub is_dead_end: Option<bool>,
    pub children: Vec<VizNode>,
}

impl VizNode {
    pub fn from_partial_ast(ast: &PartialAST) -> VizNode {
        build_viz_node(ast, "root".to_string())
    }
}

fn to_viz_span(span: &Option<SourceSpan>) -> Option<VizSpan> {
    span.as_ref().map(|s| VizSpan { start: s.start, end: s.end })
}

fn build_viz_node(ast: &PartialAST, id_prefix: String) -> VizNode {
    match ast {
        PartialAST::Mismatch => VizNode {
            id: id_prefix,
            kind: VizNodeKind::Mismatch,
            label: "mismatch".to_string(),
            binding: None,
            span: None,
            production_len: None,
            parsed_symbols: None,
            is_complete: Some(false),
            is_dead_end: Some(true),
            children: vec![],
        },
        PartialAST::Terminal(t) => build_terminal_node(t, id_prefix),
        PartialAST::NonTerminal(parallels) => {
            // Represent NonTerminal as a synthetic node with parallel children
            let mut children = Vec::new();
            for (idx, nt) in parallels.iter().enumerate() {
                children.push(build_nonterminal_branch(nt, format!("{}:p{}", id_prefix, idx)));
            }
            VizNode {
                id: id_prefix,
                kind: VizNodeKind::NonTerminal,
                label: parallels.get(0).map(|p| p.value().clone()).unwrap_or_else(|| "<nt>".to_string()),
                binding: parallels.get(0).and_then(|p| p.binding().clone()),
                span: parallels.get(0).and_then(|p| to_viz_span(p.span())),
                production_len: None,
                parsed_symbols: None,
                is_complete: None,
                is_dead_end: Some(false),
                children,
            }
        }
    }
}

fn build_terminal_node(t: &PartialTerminal, id_prefix: String) -> VizNode {
    VizNode {
        id: id_prefix,
        kind: VizNodeKind::Terminal,
        label: t.value.clone(),
        binding: t.binding.clone(),
        span: to_viz_span(&t.span),
        production_len: None,
        parsed_symbols: Some(1),
        is_complete: Some(true),
        is_dead_end: Some(false),
        children: vec![],
    }
}

fn build_nonterminal_branch(nt: &PartialNonTerminal, id_prefix: String) -> VizNode {
    let production_len = nt.production().rhs_len();
    let parsed_symbols = nt.production().cursor_value();
    let is_complete = nt.production().is_complete_state(nt.children().len());
    let is_dead_end = !is_complete && nt.stopped() && nt.children().is_empty();
    let mut children = Vec::new();
    for (idx, ch) in nt.children().iter().enumerate() {
        children.push(build_viz_node(ch, format!("{}:c{}", id_prefix, idx)));
    }
    VizNode {
        id: id_prefix,
        kind: VizNodeKind::NonTerminal,
        label: format!("{}", nt.value()),
        binding: nt.binding().clone(),
        span: to_viz_span(nt.span()),
        production_len: Some(production_len),
        parsed_symbols: Some(parsed_symbols),
        is_complete: Some(is_complete),
        is_dead_end: Some(is_dead_end),
        children,
    }
}

// Helper methods exported from PartialProduction
trait PartialProductionIntrospect {
    fn production_len(&self) -> usize;
    fn cursor(&self) -> usize;
    fn is_complete(&self, children: usize) -> bool;
}

impl PartialProductionIntrospect for PartialProduction {
    fn production_len(&self) -> usize { self.rhs_len() }
    fn cursor(&self) -> usize { self.cursor_value() }
    fn is_complete(&self, children: usize) -> bool { self.is_complete_state(children) }
}

