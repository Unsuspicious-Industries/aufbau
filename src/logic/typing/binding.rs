// ============================================================================
// Binding Resolution
// ============================================================================
// We need a complete overhaul of this binding resolution process
// We need something that works
// We need something that can be tested and validated with proper unit tests
// TODO: fix this shit
// Then fix the type syste
// Then the parser ig.

use crate::logic::binding::resolve_binding_path;
use crate::logic::grammar::Grammar;
use crate::logic::partial::structure::{Node, NonTerminal, Terminal};
use crate::logic::typing::core::{Context, Substitution, TreeStatus, subst, unify};
use crate::logic::typing::rule::{ConclusionKind, TypeOperation};
use crate::logic::typing::{Type, TypingJudgment, TypingRule};
use std::collections::HashMap;

pub enum BindError {
    AtFrontier,
    Malformed,
}

pub fn resolve_bindings(
    nt: &NonTerminal,
    rule_name: &str,
    grammar: &Grammar,
) -> Result<HashMap<String, Node>, BindError> {
    let root = Node::NonTerminal(nt.clone());
    let mut bound = HashMap::new();

    // Get the set of bindings actually USED by the typing rule
    let required_bindings = if let Some(rule) = grammar.typing_rules.get(rule_name) {
        rule.used_bindings()
    } else {
        // No rule defined - no required bindings
        std::collections::HashSet::new()
    };

    for (name, paths) in grammar.binding_map.bindings_for_rule(rule_name) {
        match resolve_one(&root, nt, paths) {
            // instead of resolving for and returning a node
            // Apply a **validation** operation to the grammar paths
            // and return **tree paths** meaning
            // Grammar paths stripped of their Alt information
            // TODO: fix this
            Some(node) => {
                bound.insert(name.to_string(), node);
            }
            None => {
                // Only fail if this binding is REQUIRED by the rule
                if required_bindings.contains(name) {
                    if is_at_frontier(&root) {
                        return Err(BindError::AtFrontier);
                    } else {
                        return Err(BindError::Malformed);
                    }
                }
            }
        }
    }

    Ok(bound)
}

fn resolve_one(
    root: &Node,
    nt: &NonTerminal,
    paths: &[crate::logic::binding::GrammarPath],
) -> Option<Node> {
    use crate::logic::binding::ResolutionError;

    for path in paths {
        match resolve_binding_path(root, path) {
            Ok(results) => {
                if let Some(res) = results.iter().find(|r| r.is_match()).or(results.first()) {
                    return Some(res.node().clone());
                }
            }
            Err(ResolutionError::AlternativeMismatch) => continue,
            Err(ResolutionError::MissingNode) => {
                // Check if beyond frontier
                if is_path_beyond_frontier(nt, path) {
                    return None; // Will trigger AtFrontier
                }
                continue; // Try other paths
            }
        }
    }
    None
}

pub fn extract_type_bindings(bound: &HashMap<String, Node>) -> Substitution {
    let mut s = Substitution::new();
    for (name, node) in bound {
        if let Some(text) = node_text(node) {
            if let Ok(ty) = Type::parse(&text) {
                s.insert(name.clone(), ty);
            }
        }
    }
    s
}

// ============================================================================
// Frontier Detection
// ============================================================================
pub fn is_at_frontier(node: &Node) -> bool {
    match node {
        Node::Terminal(Terminal::Partial { .. }) => true,
        Node::Terminal(Terminal::Complete { .. }) => false,
        Node::NonTerminal(nt) => {
            nt.children.len() < nt.production.rhs.len()
                || nt.children.last().map_or(false, is_at_frontier)
        }
    }
}

fn is_path_beyond_frontier(nt: &NonTerminal, path: &crate::logic::binding::GrammarPath) -> bool {
    let steps = path.steps();
    if steps.is_empty() {
        return is_at_frontier(&Node::NonTerminal(nt.clone()));
    }

    let idx = steps[0].i;
    idx >= nt.children.len()
}

// ============================================================================
// Utilities
// ============================================================================
// This is suspicious
// Maybe the most stupid part of the code here
pub fn node_text(node: &Node) -> Option<String> {
    match node {
        Node::Terminal(Terminal::Complete { value, .. }) => Some(value.clone()),
        Node::Terminal(Terminal::Partial { value, .. }) if !value.is_empty() => Some(value.clone()),
        Node::Terminal(Terminal::Partial { .. }) => None,
        Node::NonTerminal(nt) => {
            let mut s = String::new();
            for child in &nt.children {
                s.push_str(&node_text(child)?);
            }
            Some(s)
        }
    }
}
