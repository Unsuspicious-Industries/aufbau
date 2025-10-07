use std::collections::HashMap;

use crate::logic::ast::SourceSpan;
use crate::logic::bind::BoundTypingRule;
use crate::logic::grammar::{Production, Symbol, RepetitionKind};
use crate::logic::partial::production::PartialSymbol;
use crate::logic::partial::{PartialTerminal};

/// A node that is already fully parsed (terminal or non-terminal complete subtree).
#[derive(Clone, Debug)]
pub enum ParsedNode {
    Terminal(PartialTerminal),
    NonTerminal {
        name: String,
        alts: Vec<Alt>,
    },
}

/// A slot represents the state of one RHS symbol of a production.
#[derive(Clone, Debug)]
pub enum Slot {
    /// X – the symbol is satisfied. For plain symbols this means a single
    /// `ParsedNode`; for `*` or `+` repetitions it can hold many.
    Filled(Vec<ParsedNode>),
    /// P – we have consumed some input inside this symbol but it is not yet
    /// finished (prefix / in-flight).
    Partial(PartialSymbol),
}

impl Slot {
    pub fn is_filled(&self) -> bool {
        matches!(self, Slot::Filled(_))
    }
    pub fn is_partial(&self) -> bool {
        matches!(self, Slot::Partial(_))
    }
}

/// One concrete alternative of a non-terminal.
#[derive(Clone, Debug)]
pub struct Alt {
    /// Grammar production this alternative implements.
    pub production: Production,
    /// Tape of slots; length == production.rhs.len().
    pub slots: HashMap<usize, Vec<Slot>>,
    /// Span covering consumed bytes from first Filled/Partial to last.
    pub span: Option<SourceSpan>,
    /// Bound typing rule – present when every mandatory slot is Filled.
    pub bound: Option<BoundTypingRule>,
}

impl Alt {

    // ------------------------
    // Symbols helper functions 
    // ------------------------

    pub fn symbol_slots(self, idx: usize) -> Option<Vec<Slot>> {
        self.slots.get(&idx).cloned()
    }
    pub fn symbol_empty(&self, idx: usize) -> bool {
        self.clone().symbol_slots(idx).map(|slots| {
            slots.iter().all(|slot| match slot {
                Slot::Filled(nodes) => nodes.is_empty(),
                Slot::Partial(_) => false,
            })
        }).unwrap_or(true)
    }
    pub fn symbol_partial(&self, idx: usize) -> bool {
        self.clone().symbol_slots(idx).map(|slots| {
            slots.iter().last().map(|slot| slot.is_partial()).unwrap_or(false)
        }).unwrap_or(false)
    }
    pub fn symbol_complete(&self, idx: usize) -> bool {

        let sym = self.production.rhs.get(idx).unwrap();
        match sym {
            Symbol::Single { repetition, .. } | 
            Symbol::Group {  repetition,.. } 
                => match repetition.as_ref().unwrap() {
                    RepetitionKind::ZeroOrMore | RepetitionKind::ZeroOrOne => {
                        true
                    }
                    RepetitionKind::OneOrMore => {
                        !self.symbol_empty(idx) && !self.symbol_partial(idx)
                    }
            },
            _ => {
                !self.symbol_empty(idx) && !self.symbol_partial(idx)
            }
        }
    }

    // Alt utils
    pub fn is_complete(&self) -> bool {
        self.production.rhs
            .iter()
            .enumerate()
            .all(|(idx, _)| self.symbol_complete(idx))
    }

    // Manipulations functions
    pub fn flatten_slots(&self) -> Vec<Slot> {
        let mut result = Vec::new();
        for idx in 0..self.production.rhs.len() {
            if let Some(slots) = self.slots.get(&idx) {
                result.extend_from_slice(slots);
            }
        }

        // ensure validity : 
        // [FFFFFFF]P is valid
        // FFFFFFFFFFFFF is valid if all symbols are filled
        // [F]P[F] is invalid
        // [F]P[P] is invalid
        assert!(
            self.is_complete()
            || result
                .iter()
                .rposition(
                    |s| s.is_partial()
                ).map_or(true, |p| p == result.len() - 1)
        );

        result
    }


    /// Cursor = number of *leading* symbols that are definitively satisfied.
    /// For optional symbols (ZeroOrMore / ZeroOrOne) an `Empty` counts as
    /// satisfied, keeping the mapping injective.
    pub fn cursor(&self) -> usize {
        for (idx, slot) in self.production.rhs.iter().enumerate() {
            if !self.symbol_complete(idx) {
                return idx;
            }
        }
        self.production.rhs.len()
    }
}


// ------------------------
// Test helpers
// ------------------------

/// Create a dummy [`Alt`] value for unit tests.
///
/// The `layout` argument describes the slots for each right-hand-side symbol
/// of the production. The outer vector index corresponds to the symbol index.
/// Each inner vector enumerates the slots for that symbol in the order they
/// would appear on the parsing tape.
///
/// * `false` – insert a [`Slot::Filled`] slot containing a single dummy
///   terminal node.
/// * `true`  – insert a [`Slot::Partial`] slot representing a symbol that is
///   currently in flight.
///
/// This is intended purely for white-box tests and therefore keeps the
/// implementation minimal: every RHS symbol is a generic non-terminal like
/// `S0`, `S1`, …, and filled slots hold one dummy terminal with an empty span.
#[cfg(test)]
pub fn dummy_alt(layout: Vec<Vec<bool>>) -> Alt {
    use crate::logic::partial::ParsedNode;
    use std::collections::HashMap;

    // Construct a minimal RHS where every symbol is a unique expression
    // (non-terminal) so that it is as unconstrained as possible.
    let rhs: Vec<Symbol> = layout
        .iter()
        .enumerate()
        .map(|(i, _)| Symbol::Expression(format!("S{}", i)))
        .collect();

    let production = Production { rule: None, rhs };

    // Build the slots map according to the requested layout.
    let mut slots: HashMap<usize, Vec<Slot>> = HashMap::new();
    for (idx, kinds) in layout.into_iter().enumerate() {
        let mut vec = Vec::new();
        for is_partial in kinds {
            if is_partial {
                vec.push(Slot::Partial(PartialSymbol::Other {
                    symbol_index: idx,
                }));
            } else {
                vec.push(Slot::Filled(vec![ParsedNode::Terminal(PartialTerminal {
                    value: format!("t{}", idx),
                    span: None,
                    binding: None,
                })]));
            }
        }
        slots.insert(idx, vec);
    }

    Alt {
        production,
        slots,
        span: None,
        bound: None,
    }
}


mod tests {
    use super::*;

    #[test]
    fn symbol_helpers_basic() {
        // One symbol, filled
        let alt = dummy_alt(vec![vec![false]]);
        assert!(!alt.symbol_empty(0));
        assert!(!alt.symbol_partial(0));
        assert!(alt.symbol_complete(0));
        assert_eq!(alt.cursor(), 1);

        // One symbol, partial
        let alt = dummy_alt(vec![vec![true]]);
        assert!(!alt.symbol_empty(0));
        assert!(alt.symbol_partial(0));
        assert!(!alt.symbol_complete(0));
        assert_eq!(alt.cursor(), 0);

        // One symbol, empty (no slots)
        let alt = dummy_alt(vec![vec![]]);
        assert!(alt.symbol_empty(0));
        assert!(!alt.symbol_partial(0));
        assert!(!alt.symbol_complete(0));
        assert_eq!(alt.cursor(), 0);
    }

    #[test]
    fn cursor_advances_across_symbols() {
        // Two symbols, first complete, second partial
        let alt = dummy_alt(vec![vec![false], vec![true]]);
        assert_eq!(alt.cursor(), 1);
        println!("Alt: {:#?}", alt);

        // Two symbols, first empty, second filled
        let alt = dummy_alt(vec![vec![], vec![false]]);
        assert_eq!(alt.cursor(), 0);

        // Two symbols, both complete
        let alt = dummy_alt(vec![vec![false], vec![false]]);
        assert_eq!(alt.cursor(), 2);
    }
}