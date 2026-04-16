use crate::logic::grammar::Symbol;
use crate::logic::parse::arena::{NodeId, Span};
use crate::logic::parse::Item;

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

/// The parser state tracking one chosen root, frontier for continuation.
///
/// Note that the ParseArena is tracked separately by the parser struct,
/// while roots and frontier are returned here.
/// This state allows checking completion or starting an incremental extension
/// by monotonic operations.
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


// Weird might wanna remove
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PrefixError {
    pub input_len: usize,
    pub message: String,
}

impl std::error::Error for PrefixError {}

impl PrefixError {
    /// Time: O(1). Space: O(1).
    pub fn rejected(input_len: usize, message: impl Into<String>) -> Self {
        Self {
            input_len,
            message: message.into(),
        }
    }
}