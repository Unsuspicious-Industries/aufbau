// ASSUME: Partial AST edges correspond bijectively to grammar-path steps (𝒫 = ℕ* with
// optional alternative annotations) from docs/challenges.md. Example: resolving binding `e`
// in `Application(app)` follows path `[1]`, so this module trusts that every non-terminal
// stores the selected alternative index used by the parser.
// ASSUME: Partial AST edges correspond bijectively to the grammar paths 𝒫 = ℕ* (with
// alternative annotations) defined in docs/challenges.md. Example: resolving binding `e`
// in `Application(app)` follows path `[1]`, so we reject mismatched alternative indices.
use crate::logic::grammar::{
    Grammar,
    binding::{GrammarPath, PathStep},
};
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

/// Resolve a binding for a given non-terminal node in the partial tree.
///
/// # Arguments
/// * `root` - The non-terminal node where the binding is defined (or associated with).
/// * `binding_name` - The name of the binding to resolve.
/// * `rule_name` - The name of the rule (production) that defines the context for the binding.
/// * `grammar` - The grammar containing the binding map.
///
/// # Returns
/// A list of nodes that are bound to the given name.
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
pub fn resolve_binding_path<'a>(root: &'a Node, path: &GrammarPath) -> Result<Vec<ResolutionResult<'a>>, ResolutionError> {
    let mut results = Vec::new();
    collect_nodes_from_node(root, path.steps(), &mut results)?;
    Ok(results)
}

fn collect_nodes_from_nt<'a>(nt: &'a NonTerminal, nt_node: &'a Node, steps: &[PathStep], results: &mut Vec<ResolutionResult<'a>>) -> Result<(), ResolutionError> {
    if steps.is_empty() {
        // We reached the target node, but we are at a NonTerminal.
        // We can't return &Node from &NonTerminal.
        // This case should be handled by the caller (collect_nodes_from_node) pushing the node before recursing if steps become empty.
        // If we are here, it means we started with empty steps or something went wrong.
        return Ok(());
    }

    let step = &steps[0];
    let remaining_steps = &steps[1..];

    if let Some(child) = nt.children.get(step.child_index) {
        // Check alternative constraint on the child
        if let Some(expected_alt) = step.alternative_index {
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
        if let Node::Terminal(Terminal::Partial { .. }) = current_node {
             results.push(ResolutionResult::Partial(current_node));
             return Ok(());
        }
        Err(ResolutionError::MissingNode)
    }
}
