use crate::logic::typing::Type;
use crate::logic::ast::{ASTNode, NonTerminal};
use crate::logic::debug::DebugUtils;
use crate::{debug_trace, debug_debug, debug_warn};

fn collect_terminals(node: &ASTNode, out: &mut Vec<String>) {
    match node {
        ASTNode::Terminal(t) => out.push(t.value.clone()),
        ASTNode::Nonterminal(nt) => {
            for ch in &nt.children { collect_terminals(ch, out); }
        }
    }
}

fn type_from_type_ast(node: &ASTNode) -> Option<Type> {
    // The value of a type nonterminal is the concatenation of all its terminal tokens
    let mut toks: Vec<String> = Vec::new();
    collect_terminals(node, &mut toks);
    if toks.is_empty() { return None; }
    let s: String = toks.concat();
    Type::parse(&s).ok()
}

pub fn  get_type_binding(node: &NonTerminal, type_var: Type) -> Option<Type> {
    debug_trace!("bind::utils", "get_type_binding: looking for type_var={}", DebugUtils::type_summary(&type_var));
    match type_var {
        Type::Atom(var) => {
            // Resolve the variable to an AST node, then parse the full type from that subtree
            if let Some(nt) = get_nt_binding(&node, var.clone()) {
                if let Some(full_ty) = type_from_type_ast(&nt.as_node()) {
                    debug_trace!("bind::utils", "get_type_binding: found structured type={}", DebugUtils::type_summary(&full_ty));
                    return Some(full_ty);
                } else {
                    debug_warn!("bind::utils", "get_type_binding: failed to parse structured type from subtree");
                    return None;
                }
            }
            
            // Special handling for lambda rules with simple parameter types
            // If we can't find τ₁ or τ₂, try to infer from the parameter type τ
            if var == "τ₁" || var == "τ₂" {
                if let Some(param_type_node) = get_nt_binding(&node, "τ".to_string()) {
                    if let Some(param_type) = type_from_type_ast(&param_type_node.as_node()) {
                        // For simple types (non-arrow), both τ₁ and τ₂ should be the same
                        match param_type {
                            Type::Arrow(ref t1, ref t2) => {
                                if var == "τ₁" {
                                    debug_trace!("bind::utils", "get_type_binding: inferred τ₁ from arrow type: {}", DebugUtils::type_summary(t1));
                                    return Some((**t1).clone());
                                } else if var == "τ₂" {
                                    debug_trace!("bind::utils", "get_type_binding: inferred τ₂ from arrow type: {}", DebugUtils::type_summary(t2));
                                    return Some((**t2).clone());
                                }
                            }
                            _ => {
                                // For simple types, both τ₁ and τ₂ are the parameter type
                                debug_trace!("bind::utils", "get_type_binding: inferred {} from simple parameter type: {}", var, DebugUtils::type_summary(&param_type));
                                return Some(param_type);
                            }
                        }
                    }
                }
            }
            
            debug_debug!("bind::utils", "get_type_binding: no node binding found for {}", var);
            None
        }
        Type::Arrow(t1, t2) => {
            let b1 = get_type_binding(node, *t1)?;
            let b2 = get_type_binding(node, *t2)?;
            Some(Type::Arrow(Box::new(b1), Box::new(b2)))
        }
        Type::Pointer(t) => {
            let b = get_type_binding(node, *t)?;
            Some(Type::Pointer(Box::new(b)))
        }
        Type::Array(t, size) => {
            let b = get_type_binding(node, *t)?;
            Some(Type::Array(Box::new(b), size))
        }
        Type::Empty => Some(Type::Empty),
        Type::Not(t) => {
            let b = get_type_binding(node, *t)?;
            Some(Type::Not(Box::new(b)))
        }
        Type::Intersection(t1, t2) => {
            let b1 = get_type_binding(node, *t1)?;
            let b2 = get_type_binding(node, *t2)?;
            Some(Type::Intersection(Box::new(b1), Box::new(b2)))
        }
        Type::Union(t1, t2) => {
            let b1 = get_type_binding(node, *t1)?;
            let b2 = get_type_binding(node, *t2)?;
            Some(Type::Union(Box::new(b1), Box::new(b2)))
        }
        Type::Refinement { base, predicate } => {
            let b = get_type_binding(node, *base)?;
            Some(Type::Refinement { base: Box::new(b), predicate })
        }
        Type::Universe => Some(Type::Universe),
    }
}

/// Bind a variable to a type in the current context
pub fn get_nt_binding(node: &NonTerminal,var: String) -> Option<NonTerminal> {
    debug_trace!("bind::utils", "get_nt_binding: looking for {} in node {}", var, node.value);
    
    // First pass: check if any direct children have the binding we're looking for
    for child in &node.nonterminal_children() {
        debug_trace!("bind::utils", "get_nt_binding: checking direct child {} (binding: {:?})", child.value, child.binding);
        if let Some(binding) = &child.binding {
            if *binding == var {
                debug_trace!("bind::utils", "get_nt_binding: DIRECT MATCH! Found {} in child {}", var, child.value);
                return Some(child.clone());
            }
        }
    }
    
    // Second pass: if no direct match, recurse into children
    for child in &node.nonterminal_children() {
        debug_trace!("bind::utils", "get_nt_binding: recursing into child {}", child.value);
        if let Some(binding) = child.bind(var.clone()) {
            debug_trace!("bind::utils", "get_nt_binding: found binding in child {}", child.value);
            return Some(binding);
        }
    }
    debug_trace!("bind::utils", "get_nt_binding: no binding found for {}", var);
    None
}

pub fn get_var_binding(node: &NonTerminal,var: String) -> Result<Option<String>, String> {
    if let Some(binding) = get_nt_binding(node, var) {
        if let Some(value) = extract_terminal_value(&binding.as_node()) {
            return Ok(Some(value));
        } else {
            return Err("Failed to extract terminal value, malformed AST".into());
        }
    }
    Ok(None)
}

pub fn extract_terminal_value(node: &ASTNode) -> Option<String> {
    match node {
        ASTNode::Terminal(t) => Some(t.value.clone()),
        ASTNode::Nonterminal(nt) => {
            // Special handling for parenthesized expressions like '(' Term ')'
            if nt.value == "BaseTerm" && nt.children.len() == 3 {
                if let (Some(ASTNode::Terminal(open_t)), Some(middle), Some(ASTNode::Terminal(close_t))) = (nt.children.get(0), nt.children.get(1), nt.children.get(2)) {
                    if open_t.value == "(" && close_t.value == ")" {
                        // Skip the parentheses and extract from the middle term
                        return extract_terminal_value(middle);
                    }
                }
            }
            
            let terminal_children = nt.children
                .iter()
                .filter(|c| matches!(c, ASTNode::Terminal(_)))
                .collect::<Vec<_>>();
            
            if terminal_children.len() == 1 {
                return extract_terminal_value(terminal_children[0]);
            }
            
            // Skip structural tokens like parentheses and look for meaningful content
            for child in &nt.children {
                match child {
                    ASTNode::Terminal(t) if t.value == "(" || t.value == ")" => continue,
                    _ => {
                        if let Some(value) = extract_terminal_value(child) {
                            return Some(value);
                        }
                    }
                }
            }
            
            None
        }
    }
}

impl NonTerminal {
    pub fn bind(&self, var: String) -> Option<Self> {
        debug_trace!("bind::utils", "NonTerminal::bind: checking node {} for binding {}", self.value, var);
        if let Some(binding) = self.binding() {
            debug_trace!("bind::utils", "NonTerminal::bind: {} =? {}", binding, var);
            if *binding == var {
                debug_trace!("bind::utils", "NonTerminal::bind: MATCH! Returning node {}", self.value);
                return Some(self.clone());
            }
        }
        for child in &self.nonterminal_children() {
            if let Some(binding) = child.bind(var.clone()) {
                debug_trace!("bind::utils", "NonTerminal::bind: found {} in child, returning from {}", var, self.value);
                return Some(binding);
            }
        }
        None
    }
}
