use super::typing::BoundType;
use crate::logic::typing::Type;
use crate::logic::ast::{ASTNode, NonTerminal};
use crate::{debug_trace, debug_debug, debug_warn};

fn collect_terminals(node: &ASTNode, out: &mut Vec<String>) {
    match node {
        ASTNode::Terminal(t) => out.push(t.value.clone()),
        ASTNode::Nonterminal(nt) => {
            for ch in &nt.children { collect_terminals(ch, out); }
        }
    }
}

/// Find the shallowest NonTerminal in the subtree of `root` that has one or more
/// direct nonterminal children with the requested binding name. Returns that parent.
fn find_parent_with_binding_level(root: &NonTerminal, var: &str) -> Option<NonTerminal> {
    // If current node has any direct children with binding = var, return it
    let direct = root.nonterminal_children();
    let has_any = direct.iter().any(|ch| ch.binding.as_deref() == Some(var));
    if has_any { return Some(root.clone()); }
    // Otherwise recurse; prefer the first shallowest occurrence in preorder
    for ch in direct {
        if let Some(p) = find_parent_with_binding_level(&ch, var) { return Some(p); }
    }
    None
}

/// Collect all direct child nonterminals of the discovered parent that share the same binding name.
pub fn collect_nt_bindings_same_level(root: &NonTerminal, var: &str) -> Vec<NonTerminal> {
    if let Some(parent) = find_parent_with_binding_level(root, var) {
        parent
            .nonterminal_children()
            .into_iter()
            .filter(|ch| ch.binding.as_deref() == Some(var))
            .collect()
    } else {
        Vec::new()
    }
}

/// Collect and parse types from all nodes at the same AST level sharing the binding name.
pub fn collect_types_same_level(root: &NonTerminal, var: &str) -> Vec<BoundType> {
    collect_nt_bindings_same_level(root, var)
        .into_iter()
        .filter_map(|nt| get_type_value(&nt))
        .collect()
}

pub fn get_type_value(nt: &NonTerminal) -> Option<BoundType> {
    extract_terminal_value(&nt.as_node()).map(BoundType::Atom)
}

pub fn bind_type(node: &NonTerminal, type_var: Type) -> Option<BoundType> {
    debug_trace!("bind::utils", "get_type_binding: looking for type_var={:?}", type_var);
    match type_var {
        Type::Atom(var) => {
            // Check if this is a quoted concrete type (e.g., 'int', 'float')
            if var.starts_with('\'') && var.ends_with('\'') {
                let concrete_type = &var[1..var.len()-1]; // Remove quotes
                debug_trace!("bind::utils", "get_type_binding: found concrete type '{}'", concrete_type);
                return Some(BoundType::Atom(concrete_type.to_string()));
            }
            
            // Single binding resolution path
            if let Some(nt) = get_nt_binding(&node, var.clone()) {
                if let Some(full_ty) = get_type_value(&nt) {
                    debug_trace!("bind::utils", "get_type_binding: found structured type={:?}", full_ty);
                    return Some(full_ty);
                } else {
                    debug_warn!("bind::utils", "get_type_binding: failed to parse structured type from subtree");
                    return None;
                }
            }
            debug_debug!("bind::utils", "get_type_binding: no node binding found for {}", var);
            None
        }
        Type::Raw(concrete_type) => {
            // Raw/concrete types are bound directly without variable resolution
            debug_trace!("bind::utils", "get_type_binding: binding raw type '{}'", concrete_type);
            Some(BoundType::Atom(concrete_type.clone()))
        }
        Type::Arrow(t1, t2) => {
            // Otherwise resolve both as Arrow if both sides resolve
            let b1 = bind_type(node, *t1)?;
            let b2 = bind_type(node, *t2)?;
            Some(BoundType::Arrow(Box::new(b1), Box::new(b2)))
        }
        Type::Tuple(v) => {
            let elements = collect_nt_bindings_same_level(node, &v);
            if elements.is_empty() {
                debug_debug!("bind::utils", "get_type_binding: no tuple elements found for {}, returning empty tuple", v);
                return Some(BoundType::Tuple(Vec::new()));
            }
            let tuple_elem_types: Vec<BoundType> = elements
                .into_iter()
                .filter_map(|nt| get_type_value(&nt))
                .collect();
            Some(BoundType::Tuple(tuple_elem_types))
        }
        Type::Pointer(t) => {
            let b = bind_type(node, *t)?;
            Some(BoundType::Pointer(Box::new(b)))
        }
        Type::Array(t, size) => {
            let b = bind_type(node, *t)?;
            // Try to resolve symbolic size variables via get_var_binding
            let size_str = get_var_binding(node, size);
            if size_str.is_err() {
                debug_warn!("bind::utils", "get_type_binding: failed to resolve array size variable");
                return None;
            }
            // parse as u64
            let size_u64 = if let Some(s) = size_str.unwrap() {
                s.parse::<u64>().ok()
            } else {
                None
            };
            match size_u64 {
                    Some(n) => Some(BoundType::Array(Box::new(b), n)),
                    None => {
                        debug_warn!("bind::utils", "get_type_binding: failed to parse array size as u64");
                        None
                    }
                }

        }
        Type::Empty => Some(BoundType::Empty),
        Type::Not(t) => {
            let b = bind_type(node, *t)?;
            Some(BoundType::Not(Box::new(b)))
        }
        Type::Intersection(t1, t2) => {
            let b1 = bind_type(node, *t1)?;
            let b2 = bind_type(node, *t2)?;
            Some(BoundType::Intersection(Box::new(b1), Box::new(b2)))
        }
        Type::Union(t1, t2) => {
            let b1 = bind_type(node, *t1)?;
            let b2 = bind_type(node, *t2)?;
            Some(BoundType::Union(Box::new(b1), Box::new(b2)))
        }
        Type::Universe => Some(BoundType::Universe),
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

pub fn extract_terminals(node: &ASTNode) -> Vec<String> {
    match node {
        ASTNode::Terminal(t) => vec![t.value.clone()],
        ASTNode::Nonterminal(nt) => {
            
            let mut terminals = vec![];
            for child in &nt.children {
                // all extract_terminals on all childrens and merge vecs
                let extracted = extract_terminals(child);
                terminals.extend(extracted);
            }
            
            terminals
        }
    }
}

// extract ONE terminal value. errors if more
pub fn extract_terminal_value(node: &ASTNode) -> Option<String> {
    let terms = extract_terminals(node);
    // Filter out structural parentheses
    let filtered: Vec<String> = terms
        .into_iter()
        .filter(|t| t != "(" && t != ")")
        .collect();
    match filtered.len() {
        1 => Some(filtered[0].clone()),
        _ => None,
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
