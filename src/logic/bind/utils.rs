use crate::logic::typing::{Type, ArraySize};
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
pub fn collect_type_bindings_same_level(root: &NonTerminal, var: &str) -> Vec<Type> {
    collect_nt_bindings_same_level(root, var)
        .into_iter()
        .filter_map(|nt| type_from_type_ast(&nt.as_node()))
        .collect()
}

pub fn  get_type_binding(node: &NonTerminal, type_var: Type) -> Option<Type> {
    debug_trace!("bind::utils", "get_type_binding: looking for type_var={}", DebugUtils::type_summary(&type_var));
    match type_var {
        Type::Atom(var) => {
            // Check if repeated same-level occurrences exist
            let many = collect_type_bindings_same_level(node, &var);
            if many.len() > 1 {
                // Ambiguous in a scalar position; caller must model list explicitly (e.g., via Arrow-list→Fn path)
                debug_warn!("bind::utils", "Ambiguous repeated type binding '{}' at same level ({} items) in scalar position", var, many.len());
                return None;
            }
            if many.len() == 1 { return Some(many[0].clone()); }

            // Single binding resolution path
            if let Some(nt) = get_nt_binding(&node, var.clone()) {
                if let Some(full_ty) = type_from_type_ast(&nt.as_node()) {
                    debug_trace!("bind::utils", "get_type_binding: found structured type={}", DebugUtils::type_summary(&full_ty));
                    return Some(full_ty);
                } else {
                    debug_warn!("bind::utils", "get_type_binding: failed to parse structured type from subtree");
                    return None;
                }
            }
            
            debug_debug!("bind::utils", "get_type_binding: no node binding found for {}", var);
            None
        }
        Type::Arrow(t1, t2) => {
            // If the domain variable denotes a same-level repeated list, synthesize Fn(params, ret)
            if let Type::Atom(var) = *t1.clone() {
                let params = collect_type_bindings_same_level(node, &var);
                if !params.is_empty() {
                    let ret = get_type_binding(node, *t2)?;
                    return Some(Type::Fn { params, ret: Box::new(ret) });
                }
            }
            // Otherwise resolve both as Arrow if both sides resolve
            let b1 = get_type_binding(node, *t1)?;
            let b2 = get_type_binding(node, *t2)?;
            Some(Type::Arrow(Box::new(b1), Box::new(b2)))
        }
        Type::Fn { params, ret } => {
            // Resolve inner types generically
            let mut new_params = Vec::with_capacity(params.len());
            for p in params {
                let rp = get_type_binding(node, p)?;
                new_params.push(rp);
            }
            let new_ret = get_type_binding(node, *ret)?;
            Some(Type::Fn { params: new_params, ret: Box::new(new_ret) })
        }
        Type::Pointer(t) => {
            let b = get_type_binding(node, *t)?;
            Some(Type::Pointer(Box::new(b)))
        }
        Type::Array(t, size) => {
            let b = get_type_binding(node, *t)?;
            // Try to resolve symbolic size variables via get_var_binding
            let resolved_size = match size {
                ArraySize::Var(var) => {
                    if let Ok(Some(val)) = get_var_binding(node, var.clone()) {
                        if let Ok(n) = val.parse::<u64>() {
                            ArraySize::Const(n)
                        } else {
                            // Keep as symbolic if cannot parse
                            ArraySize::Var(var)
                        }
                    } else {
                        ArraySize::Var(var)
                    }
                }
                other => other,
            };
            Some(Type::Array(Box::new(b), resolved_size))
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
