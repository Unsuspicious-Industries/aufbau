// ASSUME: Partial AST edges correspond bijectively to grammar-path steps with
// (𝒫 = ℕ* optional alternative annotations)
// Using the formal spec docs/challenges.md. Example: resolving binding `e`
// in `Application(app)` follows path `[1]`, so this module trusts that every non-terminal
// stores the selected alternative index used by the parser.
use super::*;
use crate::logic::grammar::Grammar;

use crate::logic::partial::structure::{Node, NonTerminal, Terminal};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ResolutionError {
    AlternativeMismatch,
    MissingNode,
}

#[derive(Debug, Clone)]
pub enum ResolutionResult<'a> {
    Match(&'a Node),
    Partial(&'a Node),
}

impl<'a> ResolutionResult<'a> {
    pub fn node(&self) -> &'a Node {
        match self {
            ResolutionResult::Match(n) => n,
            ResolutionResult::Partial(n) => n,
        }
    }

    pub fn is_match(&self) -> bool {
        matches!(self, ResolutionResult::Match(_))
    }
}

// Resolve a binding for a given non-terminal node in the partial tree.
// Used stored grammar paths
pub fn resolve_binding<'a>(
    root: &'a Node,
    binding_name: &str,
    rule_name: &str,
    grammar: &Grammar,
) -> Vec<&'a Node> {
    let mut results = Vec::new();

    // Get the grammar paths for this binding
    if let Some(paths) = grammar.binding_map.get(binding_name, rule_name) {
        for path in paths {
            if let Ok(resolutions) = resolve_binding_path(root, path) {
                for res in resolutions {
                    results.push(res.node());
                }
            }
        }
    }

    results
}

/// Resolve a specific grammar path against a non-terminal node.
pub fn resolve_binding_path<'a>(
    root: &'a Node,
    path: &GrammarPath,
) -> Result<Vec<ResolutionResult<'a>>, ResolutionError> {
    let mut results = Vec::new();
    collect_nodes_from_node(root, path.steps(), &mut results)?;
    Ok(results)
}

fn collect_nodes_from_nt<'a>(
    nt: &'a NonTerminal,
    nt_node: &'a Node,
    steps: &[PathStep],
    results: &mut Vec<ResolutionResult<'a>>,
) -> Result<(), ResolutionError> {
    if steps.is_empty() {
        // We reached the target node, but we are at a NonTerminal.
        // We can't return &Node from &NonTerminal.
        // This case should be handled by the caller (collect_nodes_from_node) pushing the node before recursing if steps become empty.
        // this is a big issue in our design thing
        // Thats why we droll
        return Ok(());
    }

    let step = &steps[0];
    let remaining_steps = &steps[1..];

    if let Some(child) = nt.children.get(step.i) {
        // Check alternative constraint on the child
        if let Some(expected_alt) = step.a {
            match child {
                Node::NonTerminal(child_nt) => {
                    if child_nt.alternative_index != expected_alt {
                        return Err(ResolutionError::AlternativeMismatch);
                    }
                }
                Node::Terminal(_) => {
                    // Terminals don't have alternatives.
                    // If we expected an alternative, this is a mismatch (or invalid path).
                    return Err(ResolutionError::AlternativeMismatch);
                }
            }
        }

        collect_nodes_from_node(child, remaining_steps, results)
    } else {
        // Missing child. Return parent (nt_node) as partial result.
        results.push(ResolutionResult::Partial(nt_node));
        Ok(())
    }
}

fn collect_nodes_from_node<'a>(
    current_node: &'a Node,
    steps: &[PathStep],
    results: &mut Vec<ResolutionResult<'a>>,
) -> Result<(), ResolutionError> {
    if steps.is_empty() {
        // We reached the target node
        if current_node.is_complete() {
            results.push(ResolutionResult::Match(current_node));
        } else {
            results.push(ResolutionResult::Partial(current_node));
        }
        return Ok(());
    }

    if let Node::NonTerminal(nt) = current_node {
        collect_nodes_from_nt(nt, current_node, steps, results)
    } else {
        // If we are at a Terminal (partial or complete) and have steps remaining,
        // we can't go deeper.
        // If it's Terminal::Partial, return it.
        // Beware heuristic might be incorrect
        if let Node::Terminal(Terminal::Partial { .. }) = current_node {
            results.push(ResolutionResult::Partial(current_node));
            return Ok(());
        }
        Err(ResolutionError::MissingNode)
    }
}
