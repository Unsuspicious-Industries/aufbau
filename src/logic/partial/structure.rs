use std::collections::HashMap;
use crate::logic::ast::SourceSpan;
use crate::logic::grammar::{Production, RepetitionKind};
use crate::logic::partial::production::PartialSymbol;

/// Top-level partial AST result
#[derive(Clone, Debug)]
pub struct PartialAST {
    pub root: NonTerminal,
    pub input: String,
}

impl PartialAST {
    pub fn new(root: NonTerminal, input: String) -> Self { 
        Self { root, input } 
    }
    
    pub fn root(&self) -> &NonTerminal { 
        &self.root 
    }
    
    pub fn input(&self) -> &str { 
        &self.input 
    }
    
    /// Check if the AST is complete (has at least one complete alternative)
    pub fn complete(&self) -> bool {
        self.root.is_complete()
    }
    
    /// Convert to a completed AST by selecting a fully matched alternative path
    pub fn into_complete(self) -> Result<crate::logic::ast::ASTNode, String> {
        use crate::logic::ast::{ASTNode, NonTerminal as FullNT};

        fn pick_complete_alt<'a>(alts: &'a [Alt]) -> Option<&'a Alt> {
            alts.iter().find(|alt| alt.is_complete())
        }

        fn convert_nonterminal(nt: &NonTerminal) -> Result<crate::logic::ast::NonTerminal, String> {
            let alt = pick_complete_alt(&nt.alts)
                .ok_or_else(|| format!("No complete alternative for nonterminal '{}'", nt.name))?;
            let mut children: Vec<ASTNode> = Vec::new();

            let mut indices: Vec<usize> = alt.slots.keys().cloned().collect();
            indices.sort_unstable();
            for idx in indices {
                if let Some(slots) = alt.slots.get(&idx) {
                    for slot in slots {
                        match slot {
                            Slot::Filled(nodes) => {
                                for node in nodes {
                                    match node {
                                        ParsedNode::Terminal(t) => {
                                            children.push(ASTNode::Terminal(crate::logic::ast::Terminal {
                                                value: t.value.clone(),
                                                span: t.span.clone(),
                                                binding: t.binding.clone(),
                                            }));
                                        }
                                        ParsedNode::NonTerminal(child_nt) => {
                                            children.push(ASTNode::Nonterminal(convert_nonterminal(child_nt)?));
                                        }
                                    }
                                }
                            }
                            Slot::Partial { .. } => {
                                return Err(format!(
                                    "Incomplete symbol remained in complete alternative '{}' at slot {}",
                                    nt.name, idx
                                ));
                            }
                        }
                    }
                }
            }

            let full = FullNT {
                value: nt.name.clone(),
                span: nt.span.clone(),
                children,
                binding: nt.binding.clone(),
                bound_typing_rule: None,
            };

            Ok(full)
        }

        let root = convert_nonterminal(&self.root)?;
        Ok(ASTNode::Nonterminal(root))
    }
}

/// A nonterminal node with parallel alternatives
#[derive(Clone, Debug)]
pub struct NonTerminal {
    /// Name of the nonterminal (e.g., "Expr", "start")
    pub name: String,
    /// Parallel alternatives (different ways to parse this nonterminal)
    pub alts: Vec<Alt>,
    /// Optional binding from grammar
    pub binding: Option<String>,
    /// Span covering the parsed input
    pub span: Option<SourceSpan>,
}

#[derive(Clone, Debug)]
pub struct Terminal {
    pub value: String,
    pub span: Option<SourceSpan>,
    pub binding: Option<String>,
}


impl NonTerminal {
    pub fn new(name: String, alts: Vec<Alt>, binding: Option<String>) -> Self {
        Self { 
            name, 
            alts, 
            binding,
            span: None,
        }
    }
    
    pub fn with_span(mut self, span: Option<SourceSpan>) -> Self {
        self.span = span;
        self
    }
    
    /// Check if any alternative is complete
    pub fn is_complete(&self) -> bool {
        self.alts.iter().any(|alt| alt.is_complete())
    }
    
    /// Check if any alternative has progressed
    pub fn is_progressing(&self) -> bool {
        self.alts.iter().any(|alt| alt.is_progressing())
    }
}

/// A node that is already fully parsed (terminal or non-terminal complete subtree).
#[derive(Clone, Debug)]
pub enum ParsedNode {
    Terminal(Terminal),
    NonTerminal(NonTerminal),
}


/// A slot represents the state of one RHS symbol of a production.
#[derive(Clone, Debug)]
pub enum Slot {
    /// Filled – the symbol is satisfied. For plain symbols this means a single
    /// `ParsedNode`; for `*` or `+` repetitions it can hold many.
    Filled(Vec<ParsedNode>),
    /// Partial – we have consumed some input inside this symbol but it is not yet
    /// finished (prefix / in-flight). Contains the partially parsed node (if any)
    /// and metadata about what's incomplete.
    Partial {
        /// The partially parsed node (e.g., a NonTerminal with some children)
        node: Option<ParsedNode>,
        /// Metadata about what symbol is incomplete
        partial_symbol: PartialSymbol,
    },
}

impl Slot {
    pub fn is_filled(&self) -> bool {
        matches!(self, Slot::Filled(_))
    }
    pub fn is_partial(&self) -> bool {
        matches!(self, Slot::Partial { .. })
    }
}

/// One concrete alternative of a non-terminal.
#[derive(Clone, Debug)]
pub struct Alt {
    /// Grammar production this alternative implements.
    pub production: Production,
    /// Tape of slots; HashMap allows sparse representation (missing = zero matches for optional symbols)
    pub slots: HashMap<usize, Vec<Slot>>,
    /// Span covering consumed bytes from first Filled/Partial to last.
    pub span: Option<SourceSpan>,
    /// Typing rule name (copied from production, not bound yet)
    pub typing_rule: Option<String>,
}

impl Alt {
    pub fn new(production: Production) -> Self {
        Self {
            typing_rule: production.rule.clone(),
            production,
            slots: HashMap::new(),
            span: None,
        }
    }

    // ------------------------
    // Slot access functions 
    // ------------------------
    
    /// Get slots for a given symbol index (None if no entry = zero matches)
    pub fn get_slots(&self, idx: usize) -> Option<&Vec<Slot>> {
        self.slots.get(&idx)
    }
    
    /// Get mutable slots for a given symbol index
    pub fn get_slots_mut(&mut self, idx: usize) -> &mut Vec<Slot> {
        self.slots.entry(idx).or_insert_with(Vec::new)
    }
    
    /// Check if symbol at index has no slots (or all empty Filled slots)
    pub fn symbol_empty(&self, idx: usize) -> bool {
        self.slots.get(&idx).map(|slots| {
            slots.is_empty() || slots.iter().all(|slot| match slot {
                Slot::Filled(nodes) => nodes.is_empty(),
                Slot::Partial { .. } => false,
            })
        }).unwrap_or(true)
    }
    
    /// Check if symbol at index has a Partial slot (in-flight)
    pub fn symbol_partial(&self, idx: usize) -> bool {
        self.slots.get(&idx).map(|slots| {
            slots.iter().any(|slot| slot.is_partial())
        }).unwrap_or(false)
    }
    
    /// Check if symbol at index is complete (satisfied based on repetition kind)
    pub fn symbol_complete(&self, idx: usize) -> bool {
        let Some(sym) = self.production.rhs.get(idx) else {
            return false;
        };
        
        let rep = sym.repetition();
        match rep {
            Some(RepetitionKind::ZeroOrMore | RepetitionKind::ZeroOrOne) => {
                // Optional symbols are always complete (even if empty)
                !self.symbol_partial(idx)
            }
            Some(RepetitionKind::OneOrMore) => {
                // Must have at least one filled node and no partial
                !self.symbol_empty(idx) && !self.symbol_partial(idx)
            }
            None => {
                // Required symbol must be filled and not partial
                !self.symbol_empty(idx) && !self.symbol_partial(idx)
            }
        }
    }
    
    /// Add a filled node to symbol at index
    pub fn add_filled(&mut self, idx: usize, node: ParsedNode) {
        let slots = self.get_slots_mut(idx);
        // Find or create the Filled slot for this symbol
        if let Some(Slot::Filled(nodes)) = slots.last_mut() {
            nodes.push(node);
        } else {
            slots.push(Slot::Filled(vec![node]));
        }
    }
    
    /// Set a partial symbol at index (in-flight) with optional partial node
    pub fn set_partial(&mut self, idx: usize, node: Option<ParsedNode>, partial_symbol: PartialSymbol) {
        let slots = self.get_slots_mut(idx);
        slots.push(Slot::Partial { node, partial_symbol });
    }

    // ------------------------
    // Alt state queries
    // ------------------------
    
    /// Check if this alternative is complete (all symbols satisfied)
    pub fn is_complete(&self) -> bool {
        self.production.rhs
            .iter()
            .enumerate()
            .all(|(idx, _)| self.symbol_complete(idx))
    }
    
    /// Check if this alternative has progressed (consumed some input)
    pub fn is_progressing(&self) -> bool {
        self.slots.values().any(|slots| {
            !slots.is_empty() && slots.iter().any(|s| match s {
                Slot::Filled(nodes) => !nodes.is_empty(),
                Slot::Partial { .. } => true,
            })
        })
    }

    /// Get the "cursor" - the index of the first incomplete symbol
    pub fn cursor(&self) -> usize {
        for idx in 0..self.production.rhs.len() {
            if !self.symbol_complete(idx) {
                return idx;
            }
        }
        self.production.rhs.len()
    }
    
    /// Get all parsed nodes in order (flattening slots)
    pub fn get_all_nodes(&self) -> Vec<&ParsedNode> {
        let mut nodes = Vec::new();
        for idx in 0..self.production.rhs.len() {
            if let Some(slots) = self.slots.get(&idx) {
                for slot in slots {
                    match slot {
                        Slot::Filled(slot_nodes) => {
                            nodes.extend(slot_nodes.iter());
                        }
                        Slot::Partial { node: Some(n), .. } => {
                            nodes.push(n);
                        }
                        Slot::Partial { node: None, .. } => {
                            // No node for this partial
                        }
                    }
                }
            }
        }
        nodes
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
    use std::collections::HashMap;

    // Construct a minimal RHS where every symbol is a unique expression
    // (non-terminal) so that it is as unconstrained as possible.
    use crate::logic::grammar::Symbol;

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
                vec.push(Slot::Partial {
                    node: None,
                    partial_symbol: PartialSymbol::Other {
                        symbol_index: idx,
                    },
                });
            } else {
                vec.push(Slot::Filled(vec![ParsedNode::Terminal(Terminal {
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
        typing_rule: None,
    }
}


mod tests {
    #[test]
    fn symbol_helpers_basic() {
        // One symbol, filled
        let alt = super::dummy_alt(vec![vec![false]]);
        assert!(!alt.symbol_empty(0));
        assert!(!alt.symbol_partial(0));
        assert!(alt.symbol_complete(0));
        assert_eq!(alt.cursor(), 1);

        // One symbol, partial
    let alt = super::dummy_alt(vec![vec![true]]);
        assert!(!alt.symbol_empty(0));
        assert!(alt.symbol_partial(0));
        assert!(!alt.symbol_complete(0));
        assert_eq!(alt.cursor(), 0);

        // One symbol, empty (no slots)
    let alt = super::dummy_alt(vec![vec![]]);
        assert!(alt.symbol_empty(0));
        assert!(!alt.symbol_partial(0));
        assert!(!alt.symbol_complete(0));
        assert_eq!(alt.cursor(), 0);
    }

    #[test]
    fn cursor_advances_across_symbols() {
        // Two symbols, first complete, second partial
    let alt = super::dummy_alt(vec![vec![false], vec![true]]);
        assert_eq!(alt.cursor(), 1);
        println!("Alt: {:#?}", alt);

        // Two symbols, first empty, second filled
    let alt = super::dummy_alt(vec![vec![], vec![false]]);
        assert_eq!(alt.cursor(), 0);

        // Two symbols, both complete
    let alt = super::dummy_alt(vec![vec![false], vec![false]]);
        assert_eq!(alt.cursor(), 2);
    }
}