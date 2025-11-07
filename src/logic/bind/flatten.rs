use std::collections::HashMap;
use super::typing::{
    BoundConclusion, BoundConclusionContext, BoundConclusionKind, BoundPremise, BoundType,
    BoundTypeAscription, BoundTypeSetting, BoundTypingJudgment, BoundTypingRule,
};

use crate::logic::partial::{Alt, NonTerminal, ParsedNode, Slot};
use crate::logic::typing::rule::ConclusionKind;
use crate::logic::typing::{Type, TypingJudgment, TypingRule};


impl NonTerminal {
    /// Recursively flatten the tree into a forest, where each tree represents
    /// a single path through all alternatives (top-level and nested).
    pub fn into_forest(self) -> Vec<NonTerminal> {
        self.alts
            .into_iter()
            .flat_map(|alt| {
                flatten_alt(alt)
                    .into_iter()
                    .map(|flattened_alt| {
                        NonTerminal::new(
                            self.name.clone(),
                            vec![flattened_alt],
                            self.binding.clone(),
                        )
                    })
            })
            .collect()
    }
}

/// Recursively flatten an alternative by expanding all nested NonTerminal alternatives.
/// Returns a vector of alternatives where each contains only single-alternative NonTerminals.
fn flatten_alt(alt: Alt) -> Vec<Alt> {
    // Collect all nodes from the alternative
    let nodes: Vec<_> = alt.get_all_nodes().into_iter().cloned().collect();
    
    // Find which nodes have multiple alternatives
    let has_multi_alts = nodes.iter().any(|node| match node {
        ParsedNode::NonTerminal(nt) => nt.alts.len() > 1,
        _ => false,
    });
    
    // If no multi-alternatives, recursively flatten single-alt NonTerminals and return
    if !has_multi_alts {
        return vec![flatten_alt_single_path(alt)];
    }
    
    // Expand nodes into all possible combinations
    let node_combinations = expand_node_combinations(&nodes);
    
    // Build new alternatives for each combination
    node_combinations
        .into_iter()
        .map(|combo| rebuild_alt_with_nodes(alt.clone(), combo))
        .collect()
}

/// Flatten an alternative that has no multi-alternative nested NonTerminals.
/// This recursively flattens single-alternative children without creating branches.
fn flatten_alt_single_path(mut alt: Alt) -> Alt {
    for (idx, slot) in alt.slots.clone().iter() {
        let flattened_slot = match slot {
            Slot::Filled { nodes, extensible, type_constraint } => {
                let flattened_nodes: Vec<_> = nodes
                    .iter()
                    .map(|node| match node {
                        ParsedNode::NonTerminal(nt) if nt.alts.len() == 1 => {
                            let flattened_nt = nt.clone();
                            let forest = flattened_nt.into_forest();
                            ParsedNode::NonTerminal(forest.into_iter().next().unwrap())
                        }
                        other => other.clone(),
                    })
                    .collect();
                Slot::Filled {
                    nodes: flattened_nodes,
                    extensible: *extensible,
                    type_constraint: type_constraint.clone(),
                }
            }
            Slot::Group {
                iterations,
                partial_iteration,
                group_size,
                extensible,
            } => Slot::Group {
                iterations: flatten_group_iterations_single(iterations),
                partial_iteration: partial_iteration
                    .as_ref()
                    .map(|p| flatten_iteration_single(p)),
                group_size: *group_size,
                extensible: *extensible,
            },
            other => other.clone(),
        };
        alt.slots.insert(*idx, flattened_slot);
    }
    alt
}

/// Expand a list of nodes into all possible combinations based on nested alternatives.
fn expand_node_combinations(nodes: &[ParsedNode]) -> Vec<Vec<ParsedNode>> {
    if nodes.is_empty() {
        return vec![vec![]];
    }
    
    let mut combinations = vec![vec![]];
    
    for node in nodes {
        let mut new_combinations = Vec::new();
        
        match node {
            ParsedNode::Terminal(t) => {
                // Terminals don't branch - add to all current combinations
                for combo in &combinations {
                    let mut new_combo = combo.clone();
                    new_combo.push(ParsedNode::Terminal(t.clone()));
                    new_combinations.push(new_combo);
                }
            }
            ParsedNode::NonTerminal(nt) => {
                // Expand the nonterminal into its forest
                let forest = nt.clone().into_forest();
                for combo in &combinations {
                    for single_nt in &forest {
                        let mut new_combo = combo.clone();
                        new_combo.push(ParsedNode::NonTerminal(single_nt.clone()));
                        new_combinations.push(new_combo);
                    }
                }
            }
        }
        
        combinations = new_combinations;
    }
    
    combinations
}

/// Rebuild an alternative with a new set of nodes, preserving slot structure.
fn rebuild_alt_with_nodes(alt: Alt, nodes: Vec<ParsedNode>) -> Alt {
    let mut node_iter = nodes.into_iter();
    let mut new_slots = HashMap::new();
    
    // Iterate through all slots in order
    let mut indices: Vec<usize> = alt.slots.keys().copied().collect();
    indices.sort_unstable();
    
    for idx in indices {
        if let Some(slot) = alt.slots.get(&idx) {
            let new_slot = match slot {
                Slot::Filled { extensible, nodes: original_nodes, type_constraint } => {
                    // Consume exactly as many nodes as the original slot had
                    let node_count = original_nodes.len();
                    let new_nodes: Vec<_> = (0..node_count)
                        .filter_map(|_| node_iter.next())
                        .collect();
                    
                    Slot::Filled {
                        nodes: new_nodes,
                        extensible: *extensible,
                        type_constraint: type_constraint.clone(),
                    }
                }
                Slot::Partial { partial_symbol, type_constraint, node } => {
                    let node = node_iter.next();
                    Slot::Partial {
                        node,
                        partial_symbol: partial_symbol.clone(),
                        type_constraint: type_constraint.clone(),
                    }
                }
                Slot::Group {
                    iterations,
                    partial_iteration,
                    group_size,
                    extensible,
                } => {
                    // For groups, reconstruct iterations with new nodes
                    let total_nodes_in_group = count_nodes_in_group(iterations, partial_iteration.as_ref());
                    
                    // Consume nodes for this group
                    let group_nodes: Vec<_> = (0..total_nodes_in_group)
                        .filter_map(|_| node_iter.next())
                        .collect();
                    
                    // Rebuild iterations with new nodes
                    let (new_iterations, new_partial) = rebuild_group_iterations(
                        iterations,
                        partial_iteration.as_ref(),
                        group_nodes,
                    );
                    
                    Slot::Group {
                        iterations: new_iterations,
                        partial_iteration: new_partial,
                        group_size: *group_size,
                        extensible: *extensible,
                    }
                }
            };
            new_slots.insert(idx, new_slot);
        }
    }
    
    Alt {
        production: alt.production,
        slots: new_slots,
        span: alt.span,
        typing_rule: alt.typing_rule,
    }
}

/// Count total nodes in a group (iterations + partial iteration)
fn count_nodes_in_group(
    iterations: &[Vec<Box<Slot>>],
    partial_iteration: Option<&Vec<Box<Slot>>>,
) -> usize {
    let mut count = 0;
    
    for iteration in iterations {
        for slot in iteration {
            count += count_nodes_in_slot(slot);
        }
    }
    
    if let Some(partial) = partial_iteration {
        for slot in partial {
            count += count_nodes_in_slot(slot);
        }
    }
    
    count
}

/// Count nodes in a single slot
fn count_nodes_in_slot(slot: &Slot) -> usize {
    match slot {
        Slot::Filled { nodes, .. } => nodes.len(),
        Slot::Partial { node, .. } => if node.is_some() { 1 } else { 0 },
        Slot::Group { iterations, partial_iteration, .. } => {
            count_nodes_in_group(iterations, partial_iteration.as_ref())
        }
    }
}

/// Rebuild group iterations with new nodes
fn rebuild_group_iterations(
    old_iterations: &[Vec<Box<Slot>>],
    old_partial: Option<&Vec<Box<Slot>>>,
    nodes: Vec<ParsedNode>,
) -> (Vec<Vec<Box<Slot>>>, Option<Vec<Box<Slot>>>) {
    let mut node_iter = nodes.into_iter();
    let mut new_iterations = Vec::new();
    
    for iteration in old_iterations {
        let mut new_iteration = Vec::new();
        for old_slot in iteration {
            let new_slot = rebuild_slot_with_nodes(old_slot.as_ref(), &mut node_iter);
            new_iteration.push(Box::new(new_slot));
        }
        new_iterations.push(new_iteration);
    }
    
    let new_partial = if let Some(partial) = old_partial {
        let mut new_partial_iteration = Vec::new();
        for old_slot in partial {
            let new_slot = rebuild_slot_with_nodes(old_slot.as_ref(), &mut node_iter);
            new_partial_iteration.push(Box::new(new_slot));
        }
        Some(new_partial_iteration)
    } else {
        None
    };
    
    (new_iterations, new_partial)
}

/// Rebuild a single slot with nodes from the iterator
fn rebuild_slot_with_nodes<I>(slot: &Slot, node_iter: &mut I) -> Slot 
where
    I: Iterator<Item = ParsedNode>
{
    match slot {
        Slot::Filled { extensible, nodes: original_nodes, type_constraint } => {
            let node_count = original_nodes.len();
            let new_nodes: Vec<_> = (0..node_count)
                .filter_map(|_| node_iter.next())
                .collect();
            
            Slot::Filled {
                nodes: new_nodes,
                extensible: *extensible,
                type_constraint: type_constraint.clone(),
            }
        }
        Slot::Partial { partial_symbol, type_constraint, node } => {
            let node = node_iter.next();
            Slot::Partial {
                node,
                partial_symbol: partial_symbol.clone(),
                type_constraint: type_constraint.clone(),
            }
        }
        Slot::Group { iterations, partial_iteration, group_size, extensible } => {
            let total_nodes = count_nodes_in_group(iterations, partial_iteration.as_ref());
            let group_nodes: Vec<_> = (0..total_nodes)
                .filter_map(|_| node_iter.next())
                .collect();
            
            let (new_iterations, new_partial) = rebuild_group_iterations(
                iterations,
                partial_iteration.as_ref(),
                group_nodes,
            );
            
            Slot::Group {
                iterations: new_iterations,
                partial_iteration: new_partial,
                group_size: *group_size,
                extensible: *extensible,
            }
        }
    }
}

/// Flatten iterations for single-path (no branching).
fn flatten_group_iterations_single(iterations: &[Vec<Box<Slot>>]) -> Vec<Vec<Box<Slot>>> {
    iterations.iter()
        .map(|iteration| flatten_iteration_single(iteration))
        .collect()
}

/// Flatten a single iteration for single-path (no branching).
fn flatten_iteration_single(iteration: &[Box<Slot>]) -> Vec<Box<Slot>> {
    iteration.iter()
        .map(|slot| {
            Box::new(match slot.as_ref() {
                Slot::Filled { nodes, extensible, type_constraint } => {
                    let flattened_nodes: Vec<_> = nodes
                        .iter()
                        .map(|node| match node {
                            ParsedNode::Terminal(t) => ParsedNode::Terminal(t.clone()),
                            ParsedNode::NonTerminal(nt) if nt.alts.len() == 1 => {
                                let forest = nt.clone().into_forest();
                                ParsedNode::NonTerminal(forest.into_iter().next().unwrap())
                            }
                            ParsedNode::NonTerminal(nt) => ParsedNode::NonTerminal(nt.clone()),
                        })
                        .collect();
                    Slot::Filled {
                        nodes: flattened_nodes,
                        extensible: *extensible,
                        type_constraint: type_constraint.clone(),
                    }
                }
                Slot::Group { iterations, partial_iteration, group_size, extensible } => {
                    Slot::Group {
                        iterations: flatten_group_iterations_single(iterations),
                        partial_iteration: partial_iteration.as_ref().map(|p| flatten_iteration_single(p)),
                        group_size: *group_size,
                        extensible: *extensible,
                    }
                }
                other => other.clone(),
            })
        })
        .collect()
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_flatten_parse() {
        let spec = r#"
        U ::= /b/ /a/ /r/ /c/ /b/ /a/ /r/ /c/ /u/
        A ::= /a/
        B ::= /b/ A /r/
        start ::= U | (B /c/ )+ | /t/
        "#;
        let g = crate::logic::grammar::Grammar::load(spec).unwrap();
        let mut p = crate::logic::partial::parse::Parser::new(g);
        let input = "b a";
        let ast = p.partial(input).unwrap();
        let forest = ast.root.into_forest();
        assert_eq!(forest.len(), 2, "Should have exactly 1 tree in forest");
        
        // Verify structure is preserved
        let tree = &forest[0];
        assert_eq!(tree.name, "start");
        assert_eq!(tree.alts.len(), 1);

        for tree in &forest {
            println!("{:#?}", tree);
        }

    }
}