use std::collections::HashMap;

use crate::logic::partial::ast::{PartialAST, PartialASTNode, PartialNonTerminal};
use crate::logic::partial::structure::{
    Alt as NewAlt, 
    ParsedNode as NewParsedNode, 
    NonTerminal as NewNonTerminal, 
    PartialAST as NewPartialAST, 
    Slot as NewSlot
};

/// Temporary conversion from old PartialAST to new structure
/// TODO: Remove once parser is updated
pub fn convert_partial_to_alt(past: &PartialAST) -> NewPartialAST {
    let root = convert_node_to_nt(past.root());
    NewPartialAST::new(root, past.input.clone())
}

fn convert_node_to_nt(node: &PartialASTNode) -> NewNonTerminal {
    match node {
        PartialASTNode::Terminal(_) => {
            // Shouldn't happen at root, but handle gracefully
            NewNonTerminal::new("?".to_string(), Vec::new(), None)
        }
        PartialASTNode::NonTerminal(alts) => {
            let name = alts.first().map(|n| n.value.clone()).unwrap_or_else(|| "?".to_string());
            let binding = alts.first().and_then(|n| n.binding.clone());
            let alts_conv = alts.iter().map(convert_nt).collect::<Vec<_>>();
            NewNonTerminal::new(name, alts_conv, binding)
        }
    }
}

fn convert_node(node: &PartialASTNode) -> NewParsedNode {
    match node {
        PartialASTNode::Terminal(t) => NewParsedNode::Terminal(t.clone()),
        PartialASTNode::NonTerminal(alts) => {
            let name = alts.first().map(|n| n.value.clone()).unwrap_or_else(|| "?".to_string());
            let binding = alts.first().and_then(|n| n.binding.clone());
            let alts_conv = alts.iter().map(convert_nt).collect::<Vec<_>>();
            NewParsedNode::NonTerminal(NewNonTerminal::new(name, alts_conv, binding))
        }
    }
}

fn convert_nt(nt: &PartialNonTerminal) -> NewAlt {
    let rhs_len = nt.production.rhs_len();
    let mut slots: HashMap<usize, Vec<NewSlot>> = HashMap::new();

    // Distribute children in order to first rhs_len symbols (best-effort)
    for (i, ch) in nt.children.iter().enumerate() {
        let idx = i.min(rhs_len.saturating_sub(1));
        slots.entry(idx).or_default().push(NewSlot::Filled(vec![convert_node(ch)]));
    }

    // Mark fully parsed symbols with at least one filled placeholder if missing
    let full = nt.production.fully_parsed_symbols_count();
    for idx in 0..full.min(rhs_len) {
        slots.entry(idx).or_default();
        if slots.get(&idx).map(|v| v.is_empty()).unwrap_or(true) {
            slots.insert(idx, vec![NewSlot::Filled(Vec::new())]);
        }
    }

    // Mark partial in-flight symbol if any
    if let Some(ps) = nt.production.partial_symbol_ref() {
        slots.entry(ps.symbol_index()).or_default().push(NewSlot::Partial {
            node: None,
            partial_symbol: ps.clone(),
        });
    }

    NewAlt {
        production: nt.production.production.clone(),
        slots,
        span: nt.span.clone(),
        typing_rule: nt.production.rule_name().cloned(),
    }
}
