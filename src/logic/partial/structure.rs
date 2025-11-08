use serde::Serialize;

use crate::logic::ast::SegmentRange;
use crate::logic::bind::BoundTypingRule;
use crate::logic::grammar::{Production, RepetitionKind};
use crate::logic::partial::production::PartialSymbol;
use crate::logic::typing::Type;
use crate::regex::Regex as DerivativeRegex;
use std::collections::HashMap;

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

        fn convert_slot_nodes(slot: &Slot, children: &mut Vec<ASTNode>) -> Result<(), String> {
            match slot {
                Slot::Filled { nodes, .. } => {
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
                    return Err("Incomplete symbol in complete alternative".to_string());
                }
                Slot::Group { iterations, .. } => {
                    for iteration in iterations {
                        for inner_slot in iteration {
                            convert_slot_nodes(inner_slot, children)?;
                        }
                    }
                }
            }
            Ok(())
        }

        fn convert_nonterminal(nt: &NonTerminal) -> Result<crate::logic::ast::NonTerminal, String> {
            let alt = pick_complete_alt(&nt.alts)
                .ok_or_else(|| format!("No complete alternative for nonterminal '{}'", nt.name))?;
            let mut children: Vec<ASTNode> = Vec::new();

            let mut indices: Vec<usize> = alt.slots.keys().cloned().collect();
            indices.sort_unstable();
            for idx in indices {
                if let Some(slot) = alt.slots.get(&idx) {
                    match slot {
                        Slot::Filled { nodes, .. } => {
                            for node in nodes {
                                match node {
                                    ParsedNode::Terminal(t) => {
                                        children.push(ASTNode::Terminal(
                                            crate::logic::ast::Terminal {
                                                value: t.value.clone(),
                                                span: t.span.clone(),
                                                binding: t.binding.clone(),
                                            },
                                        ));
                                    }
                                    ParsedNode::NonTerminal(child_nt) => {
                                        children.push(ASTNode::Nonterminal(convert_nonterminal(
                                            child_nt,
                                        )?));
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
                        Slot::Group { iterations, .. } => {
                            // Flatten all complete iterations
                            for iteration in iterations {
                                for inner_slot in iteration {
                                    // Recursively extract and convert nodes from each slot
                                    convert_slot_nodes(inner_slot, &mut children)?;
                                }
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
    pub span: Option<SegmentRange>,
}

#[derive(Clone, Debug)]
pub struct Terminal {
    pub value: String,
    pub span: Option<SegmentRange>,
    pub binding: Option<String>,
    pub extension: Option<DerivativeRegex>,
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

    pub fn with_span(mut self, span: Option<SegmentRange>) -> Self {
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
    /// Filled - the symbol is satisfied. For plain symbols this means a single
    /// `ParsedNode`; for `*` or `+` repetitions it can hold many.
    /// `extensible` is true if the symbol has a repetition and can accept more matches.
    Filled {
        nodes: Vec<ParsedNode>,
        extensible: bool,
        // type
        type_constraint: Option<Type>,
    },
    /// Partial - we have consumed some input inside this symbol but it is not yet
    /// finished (prefix / in-flight). Contains the partially parsed node (if any)
    /// and metadata about what's incomplete.
    /// Partial slots are never extensible - they must be completed before extension.
    Partial {
        /// The partially parsed node (e.g., a NonTerminal with some children)
        node: Option<ParsedNode>,
        /// Metadata about what symbol is incomplete
        partial_symbol: PartialSymbol,
        // type constraint
        type_constraint: Option<Type>,
    },
    /// Group - a grouped sequence of symbols with optional repetition.
    /// This preserves the structure: repetitions are in the outer Vec,
    /// and group member boundaries are preserved in the inner Vec<Box<Slot>>.
    /// `extensible` is true if the group has a repetition and can accept more iterations.
    ///
    /// `(A B C)+` parsing "A B C A B ":
    /// - iterations[0] = [Slot::Filled(A), Slot::Filled(B), Slot::Filled(C)]
    /// - partial_iteration = Some([Slot::Filled(A), Slot::Filled(B), Slot::Partial(C)])
    ///
    Group {
        /// Complete iterations of the group (all symbols satisfied)
        iterations: Vec<Vec<Box<Slot>>>,
        /// Current partial iteration being parsed
        /// Contains filled slots up to current position + optional partial slot
        partial_iteration: Option<Vec<Box<Slot>>>,
        /// Number of symbols in each group (for validation)
        group_size: usize,
        /// True if the group has a repetition and can accept more iterations
        extensible: bool,
    },
}

impl Slot {
    pub fn is_filled(&self) -> bool {
        matches!(self, Slot::Filled { .. })
    }
    pub fn is_partial(&self) -> bool {
        matches!(self, Slot::Partial { .. })
    }
    pub fn is_extensible(&self) -> bool {
        match self {
            Slot::Filled { extensible, .. } | Slot::Group { extensible, .. } => *extensible,
            Slot::Partial { .. } => false,
        }
    }
    pub fn is_group(&self) -> bool {
        matches!(self, Slot::Group { .. })
    }
}

/// A typing rule reference with optional bound instance
#[derive(Clone, Debug)]
pub struct Rule {
    /// Name of the typing rule from grammar
    pub name: String,
    /// Bound instance (set during binding phase)
    pub bound: Option<BoundTypingRule>,
}

/// One concrete alternative of a non-terminal.
#[derive(Clone, Debug)]
pub struct Alt {
    /// Grammar production this alternative implements.
    pub production: Production,
    /// Tape of slots; HashMap allows sparse representation (missing = zero matches for optional symbols)
    pub slots: HashMap<usize, Slot>,
    /// Span covering consumed segments from first Filled/Partial to last.
    pub span: Option<SegmentRange>,
    /// Typing rule reference (copied from production, bound during binding phase)
    pub typing_rule: Option<Rule>,
}

impl Alt {
    pub fn new(production: Production) -> Self {
        Self {
            typing_rule: production.rule.as_ref().map(|name| Rule {
                name: name.clone(),
                bound: None,
            }),
            production,
            slots: HashMap::new(),
            span: None,
        }
    }

    // ------------------------
    // Slot access functions
    // ------------------------

    /// Get slot for a given symbol index (None if no entry = zero matches)
    pub fn get_slot(&self, idx: usize) -> Option<&Slot> {
        self.slots.get(&idx)
    }

    /// Get mutable slot for a given symbol index
    pub fn get_slot_mut(&mut self, idx: usize) -> &mut Slot {
        self.slots.entry(idx).or_insert_with(|| Slot::Filled {
            nodes: Vec::new(),
            extensible: false,
            type_constraint: None,
        })
    }

    /// Check if symbol at index has no slot (or empty Filled slot)
    pub fn symbol_empty(&self, idx: usize) -> bool {
        self.slots
            .get(&idx)
            .map(|slot| match slot {
                Slot::Filled { nodes, .. } => nodes.is_empty(),
                Slot::Partial { .. } => false,
                Slot::Group {
                    iterations,
                    partial_iteration,
                    ..
                } => iterations.is_empty() && partial_iteration.is_none(),
            })
            .unwrap_or(true)
    }

    /// Check if symbol at index has a Partial slot (in-flight)
    pub fn symbol_partial(&self, idx: usize) -> bool {
        self.slots
            .get(&idx)
            .map(|slot| slot.is_partial())
            .unwrap_or(false)
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
        let slot = self.slots.entry(idx).or_insert_with(|| Slot::Filled {
            nodes: Vec::new(),
            extensible: false,
            type_constraint: None,
        });
        match slot {
            Slot::Filled { nodes, .. } => {
                nodes.push(node);
            }
            _ => {
                // Replace with a Filled slot containing the node
                *slot = Slot::Filled {
                    nodes: vec![node],
                    extensible: false,
                    type_constraint: None,
                };
            }
        }
    }

    /// Set a partial symbol at index (in-flight) with optional partial node
    pub fn set_partial(
        &mut self,
        idx: usize,
        node: Option<ParsedNode>,
        partial_symbol: PartialSymbol,
    ) {
        self.slots.insert(
            idx,
            Slot::Partial {
                node,
                partial_symbol,
                type_constraint: None,
            },
        );
    }

    // ------------------------
    // Alt state queries
    // ------------------------

    /// Check if this alternative is complete (all symbols satisfied)
    pub fn is_complete(&self) -> bool {
        self.production
            .rhs
            .iter()
            .enumerate()
            .all(|(idx, _)| self.symbol_complete(idx))
    }

    /// Check if this alternative has progressed (consumed some input)
    pub fn is_progressing(&self) -> bool {
        self.slots.values().any(|slot| match slot {
            Slot::Filled { nodes, .. } => !nodes.is_empty(),
            Slot::Partial { .. } => true,
            Slot::Group {
                iterations,
                partial_iteration,
                ..
            } => !iterations.is_empty() || partial_iteration.is_some(),
        })
    }

    /// Get the "cursor" - the index of the partial symbol
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
            if let Some(slot) = self.slots.get(&idx) {
                match slot {
                    Slot::Filled {
                        nodes: slot_nodes, ..
                    } => {
                        nodes.extend(slot_nodes.iter());
                    }
                    Slot::Partial { node: n, .. } => {
                        if let Some(n) = n {
                            nodes.push(n);
                        }
                    }
                    Slot::Group {
                        iterations,
                        partial_iteration,
                        ..
                    } => {
                        // Flatten all complete iterations
                        for iteration in iterations {
                            for inner_slot in iteration {
                                // Recursively extract nodes from each slot
                                extract_nodes_from_slot(inner_slot, &mut nodes);
                            }
                        }
                        // Also include nodes from partial iteration if any
                        if let Some(partial) = partial_iteration {
                            for inner_slot in partial {
                                extract_nodes_from_slot(inner_slot, &mut nodes);
                            }
                        }
                    }
                }
            }
        }
        nodes
    }
}

/// Helper function to recursively extract nodes from a slot
fn extract_nodes_from_slot<'a>(slot: &'a Slot, nodes: &mut Vec<&'a ParsedNode>) {
    match slot {
        Slot::Filled {
            nodes: slot_nodes, ..
        } => {
            nodes.extend(slot_nodes.iter());
        }
        Slot::Partial { node: n, .. } => {
            if let Some(n) = n {
                nodes.push(n);
            }
        }
        Slot::Group {
            iterations,
            partial_iteration,
            ..
        } => {
            for iteration in iterations {
                for inner_slot in iteration {
                    extract_nodes_from_slot(inner_slot, nodes);
                }
            }
            if let Some(partial) = partial_iteration {
                for inner_slot in partial {
                    extract_nodes_from_slot(inner_slot, nodes);
                }
            }
        }
    }
}