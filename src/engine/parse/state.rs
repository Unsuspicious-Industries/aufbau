//! Parser-level state objects for prefix parsing and continuation.

use crate::engine::grammar::Symbol;
use crate::engine::parse::Item;
use crate::engine::parse::arena::{NodeId, Span};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Next {
    pub node: NodeId,
    pub path: Vec<(usize, usize)>,
    pub symbol: Symbol,
}

impl Default for Next {
    fn default() -> Self {
        Self {
            node: 0,
            path: Vec::new(),
            symbol: Symbol::Nonterminal {
                name: String::new(),
                binding: None,
            },
        }
    }
}

/// One chosen root together with the frontier needed for continuation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct State {
    pub span: Span,
    pub root: NodeId,
    pub next: Next,
    pub frontier: Option<Vec<Item>>,
}

impl Default for State {
    fn default() -> Self {
        Self {
            span: Span { start: 0, end: 0 },
            root: 0,
            next: Next::default(),
            frontier: None,
        }
    }
}
