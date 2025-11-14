 

use crate::logic::ast::SegmentRange;
use crate::logic::grammar::Production;
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

        let root = FullNT::from_partial(&self.root)?;
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
}

#[derive(Clone, Debug)]
pub enum Terminal {
    Complete {
        value: String,
        binding: Option<String>,
        extension: Option<DerivativeRegex>,
    },
    Partial {
        value: String,
        binding: Option<String>,
        remainder: Option<DerivativeRegex>,
    },
}

impl Terminal {
    /// Get the length (in bytes) that this terminal matches.
    /// 
    /// Returns the byte length of the matched value string.
    /// This handles cases where terminals may match partial segments or
    /// multi-byte characters.
    pub fn lens(&self) -> Vec<usize> {
        let value = match self {
            Terminal::Complete { value, .. } => value,
            Terminal::Partial { value, .. } => value,
        };
        vec![value.len()]
    }
}

impl NonTerminal {
    pub fn new(name: String, alts: Vec<Alt>, binding: Option<String>) -> Self {
        Self {
            name,
            alts,
            binding,
        }
    }

    /// Check if any alternative is complete
    pub fn is_complete(&self, segments: &[crate::logic::tokenizer::Segment]) -> bool {
        self.alts.iter().any(|alt| alt.is_complete(segments))
    }
    pub fn pick_complete_alt(&self) -> Option<&Alt> {
        self.alts.iter().find(|alt| alt.is_complete())
    }

    /// Get the segment range covered by the complete alternative for this nonterminal.
    ///
    /// By assumption, a nonterminal has at most one complete alternative
    /// in a partial AST, so the range is well-defined.
    pub fn complete_len(&self, segments: &[crate::logic::tokenizer::Segment]) -> Option<SegmentRange> {
        self.pick_complete_alt()
            .and_then(|alt| alt.complete_len(segments))
    }

    /// Get all possible lengths (in bytes) that this nonterminal can match,
    /// including both complete and partial alternatives.
    /// 
    /// Returns a Vec of possible lengths. May grow exponentially with nested
    /// alternatives and repetitions, but provides a complete picture of the
    /// partial parse state.
    pub fn lens(&self) -> Vec<usize> {
        let mut lengths = Vec::new();
        for alt in &self.alts {
            lengths.extend(alt.lens());
        }
        // Deduplicate and sort for clarity
        lengths.sort_unstable();
        lengths.dedup();
        lengths
    }

    /// Get the "best" length for this nonterminal across all alternatives.
    /// 
    /// Sorting criteria (in order of priority):
    /// 1. Longer is better (prefer alternatives that match more input)
    /// 2. Complete is better (prefer complete over partial alternatives)
    /// 
    /// Returns the best length based on these criteria.
    pub fn best_len(&self) -> Option<usize> {
        if self.alts.is_empty() {
            return None;
        }
        // Collect all alternatives with their best length and completeness status
        let mut alt_info: Vec<(usize, bool)> = self.alts
            .iter()
            .filter_map(|alt| alt.best_len().map(|len| (len, alt.is_complete())))
            .collect();

        if alt_info.is_empty() {
            return None;
        }
        // Sort by:
        // 1. Length (descending - longer is better)
        // 2. Completeness (descending - complete is better)
        alt_info.sort_by(|a, b| {
            match b.0.cmp(&a.0) {
                std::cmp::Ordering::Equal => b.1.cmp(&a.1),
                other => other,
            }
        });
        // Return the best length
        Some(alt_info[0].0)
    }

    /// Get the "best" length floored to the nearest complete segment boundary.
    /// Returns the segment range from the start (segment 0) to the last complete segment
    /// that fits within the best byte length, or None if no best length exists.
    pub fn floor_best_len(&self, segments: &[crate::logic::tokenizer::Segment]) -> Option<SegmentRange> {
        let best_byte_len = self.best_len()?;
        
        // Count how many complete segments fit within the best byte length
        let mut accumulated_bytes = 0;
        let mut last_segment_index = None;
        
        for (i, segment) in segments.iter().enumerate() {
            let segment_len = segment.len();
            if accumulated_bytes + segment_len <= best_byte_len {
                accumulated_bytes += segment_len;
                last_segment_index = Some(i);
            } else {
                break;
            }
        }
        
        last_segment_index.map(|end| SegmentRange::new(0, end))
    }
}

#[derive(Clone, Debug)]
pub enum Node {
    NonTerminal(NonTerminal),
    Terminal(Terminal),
}

/// A slot represents the state of one RHS symbol of a production.
#[derive(Clone, Debug)]
pub struct Slot {
    pub nodes: Vec<Node>, // for repetitions 
    pub repetition: (usize, Option<usize>),
}
impl Slot {

    pub fn nodes(&self) -> &[Node] {
        &self.nodes
    }

    pub fn is_complete(&self) -> bool {
        let (min, _) = self.repetition;
        let count = self.nodes.len();
        match count {
            c if (c < min) => false,
            c if (c == min) => match self.nodes.last() {
                Some(Node::NonTerminal(nt)) => nt.is_complete(),
                Some(Node::Terminal(t)) => match t {
                    Terminal::Complete { .. } => true,
                    Terminal::Partial { .. } => false,
                },
                None => true, // nullable repetition
            },
            _ => true,
        }

    }

    /// Get all possible lengths (in bytes) that this slot can match.
    /// 
    /// A slot may contain multiple nodes (due to repetitions), each of which
    /// contributes to the total length. Returns all possible combinations.
    pub fn lens(&self) -> Vec<usize> {
        if self.nodes.is_empty() {
            return vec![0];
        }

        // Start with length 0
        let mut current_lengths = vec![0];

        // Process each node in the slot
        for node in &self.nodes {
            let node_lens = match node {
                Node::Terminal(term) => term.lens(),
                Node::NonTerminal(nt) => nt.lens(),
            };

            // Combine current lengths with this node's lengths
            let mut new_lengths = Vec::new();
            for &curr_len in &current_lengths {
                for &node_len in &node_lens {
                    new_lengths.push(curr_len + node_len);
                }
            }
            current_lengths = new_lengths;
        }

        // Deduplicate and sort
        current_lengths.sort_unstable();
        current_lengths.dedup();
        current_lengths
    }
}


/// One concrete alternative of a non-terminal.
#[derive(Clone, Debug)]
pub struct Alt {
    /// Grammar production this alternative implements.
    pub production: Production,
    /// Tape of slots; HashMap allows sparse representation (missing = zero matches for optional symbols)
    pub slots: HashMap<usize, Slot>,
}

impl Alt {
    pub fn new(production: Production) -> Self {
        Self {
            production,
            slots: HashMap::new()
        }
    }

    /// Get slot for a given symbol index (None if no entry = zero matches)
    pub fn get_slot(&self, idx: usize) -> Option<&Slot> {
        self.slots.get(&idx)
    }

    /// Get mutable slot for a given symbol index
    pub fn get_slot_mut(&mut self, idx: usize) -> Option<&mut Slot> {
        self.slots.get_mut(&idx)
    }

    /// Check if symbol at index has no slot
    pub fn symbol_empty(&self, idx: usize) -> bool {
        self.slots.get(&idx).is_none()
    }

    // ------------------------
    // Alt state queries
    // ------------------------

    /// Check if this alternative is complete (all symbols satisfied)
    pub fn is_complete(&self,segments: &[crate::logic::tokenizer::Segment]) -> bool {
        self.production
            .rhs
            .iter()
            .enumerate()
            .all(|(idx, _)| 
                match self.get_slot(idx) {
                    Some(slot) => slot.is_complete(),
                    None => false,
                }
            ) && (
            self.complete_len(segments) == Some(SegmentRange::new(0, segments.len() - 1))
        )
    }

    pub fn is_empty(&self) -> bool {
        self.slots.is_empty()
    }

    pub fn last(&self) -> Option<&Slot> {
        if self.is_empty() {
            return None;
        } else if self.is_complete() {
            let last_idx = self.production.rhs.len() - 1;
            self.slots.get(&last_idx)
        } else {
            let last_idx = self.slots.keys().max()?;
            self.slots.get(last_idx)
        }
    }

    // THEOREM:
    // - We only have one complete alternative per nonterminal in a partial AST.
    // Might be false but if its false its too hard
    //
    // UPDATE: Idk if the theorem holds but we have another important issue
    // Sometimes, complete isn't better:
    // Complete productions that match less input than partial production 
    // are leaving essential uncomsumed input
    // and thus directing the whole parse towards failure. 
    // a good example is the Lambda type production: 
    /// BaseType ::= Identifier | '(' Type ')'
    /// Type ::= BaseType '->' Type | BaseType
    // Here, in most incomplete cases we are gonna have 
    // Type (BaseType (Identifier)) complete with remaning input '->'
    // Type (BaseType '->' Type) partial with remaning 'Type' and no remaning input
    // Thats bad

    pub fn complete_len(&self, segments: &[crate::logic::tokenizer::Segment]) -> Option<SegmentRange> {
        if !self.is_complete() {
            return None;
        }

        // Walk all slots in order and collect segment ranges from complete terminals
        // and nonterminals along the unique complete path.
        let mut min_seg: Option<usize> = None;
        let mut max_seg: Option<usize> = None;
        
        // Sort slot indices to process in order
        let mut slot_indices: Vec<usize> = self.slots.keys().copied().collect();
        slot_indices.sort_unstable();
        
        for idx in slot_indices {
            if let Some(slot) = self.slots.get(&idx) {
                // Each slot may have multiple nodes (for repetitions)
                for node in slot.nodes() {
                    match node {
                        Node::Terminal(Terminal::Complete { value, .. }) => {
                            // Find which segment(s) this terminal value corresponds to
                            // by searching through the segments for a matching value
                            for seg in segments {
                                if seg.text() == *value {
                                    let seg_idx = seg.index;
                                    min_seg = Some(min_seg.map_or(seg_idx, |m| m.min(seg_idx)));
                                    max_seg = Some(max_seg.map_or(seg_idx, |m| m.max(seg_idx)));
                                    break;
                                }
                            }
                        }
                        Node::Terminal(Terminal::Partial { .. }) => {
                            // Partial terminals should not appear in a complete alt
                            return None;
                        }
                        Node::NonTerminal(nt) => {
                            // Recursively get the segment range of the complete child nonterminal
                            if let Some(child_range) = nt.complete_len(segments) {
                                min_seg = Some(min_seg.map_or(child_range.start, |m| m.min(child_range.start)));
                                max_seg = Some(max_seg.map_or(child_range.end, |m| m.max(child_range.end)));
                            } else {
                                return None;
                            }
                        }
                    }
                }
            }
        }
        
        match (min_seg, max_seg) {
            (Some(start), Some(end)) => Some(SegmentRange::new(start, end)),
            _ => None,
        }
    }

    /// Get all possible lengths (in bytes) that this alternative can match.
    /// 
    /// Computes the length by summing up contributions from all slots.
    /// For slots with repetitions, considers all nodes within the slot.
    /// Returns a Vec of possible lengths due to alternative paths within the parse.
    pub fn lens(&self) -> Vec<usize> {
        // If the alternative is empty (no slots), it matches 0 segments
        if self.slots.is_empty() {
            return vec![0];
        }

        // Start with a single possibility: length 0
        let mut current_lengths = vec![0];

        // Process each slot in order
        let mut slot_indices: Vec<usize> = self.slots.keys().copied().collect();
        slot_indices.sort_unstable();

        for idx in slot_indices {
            if let Some(slot) = self.slots.get(&idx) {
                let slot_lens = slot.lens();
                
                // For each current accumulated length, add each possible slot length
                let mut new_lengths = Vec::new();
                for &curr_len in &current_lengths {
                    for &slot_len in &slot_lens {
                        new_lengths.push(curr_len + slot_len);
                    }
                }
                current_lengths = new_lengths;
            }
        }

        // Deduplicate and sort
        current_lengths.sort_unstable();
        current_lengths.dedup();
        current_lengths
    }

    /// Get the "best" length for this alternative.
    /// In order to avoid problem as mentionned in some of the comments above we try 
    /// to select our return length based on non blocking stuff that should work
    /// 
    /// Returns the maximum length if complete, or the maximum length if partial.
    pub fn best_len(&self) -> Option<usize> {
        let lengths = self.lens();
        if lengths.is_empty() {
            return None;
        }
        // Get the maximum length
        lengths.into_iter().max()
    }

    /// Get the "best" length floored to the nearest complete segment boundary.
    /// 
    /// This computes the best byte length using `best_len()`, then returns the
    /// segment range from the start (segment 0) to the last complete segment that
    /// fits within that length.
    /// 
    /// # Arguments
    /// * `segments` - The tokenized input segments
    /// 
    /// # Returns
    /// The segment range from 0 to the last segment that fits, or None if no best length exists.
    pub fn floor_best_len(&self, segments: &[crate::logic::tokenizer::Segment]) -> Option<SegmentRange> {
        let best_byte_len = self.best_len()?;
        
        // Count how many complete segments fit within the best byte length
        let mut accumulated_bytes = 0;
        let mut last_segment_index = None;
        
        for (i, segment) in segments.iter().enumerate() {
            let segment_len = segment.len();
            if accumulated_bytes + segment_len <= best_byte_len {
                accumulated_bytes += segment_len;
                last_segment_index = Some(i);
            } else {
                break;
            }
        }
        
        last_segment_index.map(|end| SegmentRange::new(0, end))
    }
}
