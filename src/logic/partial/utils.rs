use crate::logic::ast::{ASTNode, SourceSpan};
use crate::logic::grammar::Production;
use crate::logic::parser::Parser;

use super::{PartialProduction, PartialState, PartialASTNode, PartialNonTerminal};

impl Parser {
    /// Build a partial AST node for a nonterminal with the given children
    pub(super) fn build_partial_ast(
        &self,
        value: &str,
        binding: Option<String>,
        children: &[PartialASTNode],
        start_pos: usize,
        end_pos: usize,
    ) -> PartialASTNode {
        let span = if start_pos < end_pos {
            self.span_from(start_pos, end_pos)
        } else {
            None
        };
        PartialASTNode::Nonterminal(PartialNonTerminal {
            value: value.to_string(),
            span,
            children: children.to_vec(),
            binding,
        })
    }


    /// Match a terminal symbol against a token
    pub(super) fn match_terminal(&self, sym_val: &str, token: &str) -> bool {
        if sym_val.starts_with('"') && sym_val.ends_with('"') {
            // support quoted? (fallback)
            sym_val.trim_matches('"') == token
        } else if sym_val.starts_with('\'') && sym_val.ends_with('\'') {
            sym_val.trim_matches('\'') == token
        } else if sym_val.starts_with('/') && sym_val.ends_with('/') {
            if self.grammar.special_tokens.contains(&token.to_string()) {
                false
            } else {
                regex::Regex::new(sym_val.trim_matches('/'))
                    .map(|re| re.is_match(token))
                    .unwrap_or(false)
            }
        } else {
            sym_val == token
        }
    }

    /// Get the span for a single token
    pub(super) fn token_span(&self, idx: usize) -> Option<SourceSpan> {
        self.token_spans
            .get(idx)
            .map(|(s, e)| SourceSpan { start: *s, end: *e })
    }

    /// Create a span from a range of token positions
    pub(super) fn span_from(&self, start_token: usize, end_token: usize) -> Option<SourceSpan> {
        if start_token >= self.token_spans.len() || end_token == 0 {
            return None;
        }
        let start = self.token_spans.get(start_token).map(|p| p.0)?;
        let end = if end_token - 1 < self.token_spans.len() {
            self.token_spans[end_token - 1].1
        } else {
            start
        };
        Some(SourceSpan { start, end })
    }

    /// Calculate the span that encompasses all children
    pub(super) fn children_span_ast(&self, children: &[ASTNode]) -> Option<SourceSpan> {
        if children.is_empty() { return None; }
        let first = children.first()?.span()?.start;
        let last = children.last()?.span()?.end;
        Some(SourceSpan { start: first, end: last })
    }

    /// Calculate the span that encompasses all partial children
    pub(super) fn children_span_partial(&self, children: &[PartialASTNode]) -> Option<SourceSpan> {
        let mut first: Option<usize> = None;
        let mut last: Option<usize> = None;
        for ch in children.iter() {
            if let Some(sp) = ch.span() {
                if first.map(|f| sp.start < f).unwrap_or(true) { first = Some(sp.start); }
                if last.map(|l| sp.end > l).unwrap_or(true) { last = Some(sp.end); }
            }
        }
        match (first, last) {
            (Some(s), Some(e)) => Some(SourceSpan { start: s, end: e }),
            _ => None,
        }
    }
}
