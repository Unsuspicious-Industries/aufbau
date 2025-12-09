//! Symbol Gathering - Collect all context symbols from a partial AST
//!
//! Runs a mock typechecking pass to collect all variable names and types
//! that would appear in typing contexts.

use crate::logic::grammar::Grammar;
use crate::logic::partial::binding::resolve_binding_path;
use crate::logic::partial::structure::{Node, NonTerminal, Terminal};
use crate::logic::typing::core::{Context, Substitution, subst};
use crate::logic::typing::rule::ConclusionKind;
use crate::logic::typing::{Type, TypingJudgment, TypingRule};
use moka::sync::Cache;
use once_cell::sync::Lazy;
use std::collections::HashMap;
use std::sync::Arc;

// ============================================================================
// Symbol Gathering Cache
// ============================================================================

/// Cache key: (node text representation, grammar hash)
type SymbolCacheKey = (String, u64);

/// Global cache for gather_symbols() results.
/// Caches symbol lists by the text representation of the NonTerminal.
static SYMBOLS_CACHE: Lazy<Cache<SymbolCacheKey, Arc<Vec<String>>>> = Lazy::new(|| {
    Cache::builder()
        .max_capacity(10_000)
        .build()
});

/// Clear the symbols cache (for testing)
#[allow(dead_code)]
pub fn clear_symbols_cache() {
    SYMBOLS_CACHE.invalidate_all();
}

/// Get a simple hash for the grammar (for cache key)
fn grammar_hash(grammar: &Grammar) -> u64 {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};
    let mut hasher = DefaultHasher::new();
    // Hash the start symbol as a simple grammar identifier
    grammar.start.hash(&mut hasher);
    grammar.productions.len().hash(&mut hasher);
    hasher.finish()
}

// ============================================================================
// Public API
// ============================================================================

/// Gather all context symbols (keys and values) from a partial AST.
/// Runs a mock typechecking pass to collect variable names and types.
/// Results are cached based on the node's text representation.
pub fn gather_symbols(root: &NonTerminal, grammar: &Grammar) -> Vec<String> {

    // Create cache key from node text
    let node_text = node_text(&Node::NonTerminal(root.clone())).unwrap_or_default();
    let cache_key = (node_text, grammar_hash(grammar));
    
    // Check cache first
    if let Some(cached) = SYMBOLS_CACHE.get(&cache_key) {
        return (*cached).clone();
    }
    
    // Compute
    let symbols = gather_symbols_with_context(root, grammar, 50, &Context::new());
    
    // Cache result
    SYMBOLS_CACHE.insert(cache_key, Arc::new(symbols.clone()));
    
    symbols
    
}

/// Gather symbols with an initial context
pub fn gather_symbols_with_context(
    root: &NonTerminal,
    grammar: &Grammar,
    max_depth: usize,
    ctx: &Context,
) -> Vec<String> {
    let mut symbols = Vec::new();
    
    // Add initial context symbols
    for (name, ty) in &ctx.bindings {
        symbols.push(name.clone());
        collect_type_symbols(ty, &mut symbols);
    }
    
    // Traverse the tree and collect symbols
    gather_from_node(&Node::NonTerminal(root.clone()), grammar, ctx, 0, max_depth, &mut symbols);
    
    // Deduplicate while preserving order
    let mut seen = std::collections::HashSet::new();
    symbols.retain(|s| seen.insert(s.clone()));
    
    symbols
}

// ============================================================================
// Node Traversal
// ============================================================================

fn gather_from_node(
    node: &Node,
    grammar: &Grammar,
    ctx: &Context,
    depth: usize,
    max_depth: usize,
    symbols: &mut Vec<String>,
) {
    if depth > max_depth {
        return;
    }

    match node {
        Node::Terminal(t) => gather_from_terminal(t, ctx, symbols),
        Node::NonTerminal(nt) => gather_from_nt(nt, grammar, ctx, depth, max_depth, symbols),
    }
}

fn gather_from_terminal(term: &Terminal, ctx: &Context, symbols: &mut Vec<String>) {
    match term {
        Terminal::Complete { value, .. } => {
            // If this terminal is in the context, record the lookup
            if let Some(ty) = ctx.lookup(value) {
                symbols.push(value.clone());
                collect_type_symbols(ty, symbols);
            }
        }
        Terminal::Partial { value, .. } if !value.is_empty() => {
            // Partial terminals might match context entries
            if let Some(ty) = ctx.lookup(value) {
                symbols.push(value.clone());
                collect_type_symbols(ty, symbols);
            }
        }
        _ => {}
    }
}

fn gather_from_nt(
    nt: &NonTerminal,
    grammar: &Grammar,
    ctx: &Context,
    depth: usize,
    max_depth: usize,
    symbols: &mut Vec<String>,
) {
    // If this production has a typing rule, gather from it
    if let Some(rule_name) = &nt.production.rule {
        if let Some(rule) = grammar.typing_rules.get(rule_name) {
            gather_from_rule(nt, rule, grammar, ctx, depth, max_depth, symbols);
            return;
        }
    }
    
    // No rule - drill into children
    gather_from_children(nt, grammar, ctx, depth, max_depth, symbols);
}

fn gather_from_children(
    nt: &NonTerminal,
    grammar: &Grammar,
    ctx: &Context,
    depth: usize,
    max_depth: usize,
    symbols: &mut Vec<String>,
) {
    let nt_children: Vec<_> = nt.children.iter()
        .filter(|c| matches!(c, Node::NonTerminal(_)))
        .collect();
    
    // Check each child, potentially propagating context
    let mut current_ctx = ctx.clone();
    
    for child in &nt_children {
        gather_from_node(child, grammar, &current_ctx, depth + 1, max_depth, symbols);
        
        // Check if this child produces a context transform
        if let Node::NonTerminal(child_nt) = child {
            if let Some(new_ctx) = get_context_transform(child_nt, grammar, &current_ctx, depth + 1) {
                // Collect newly added symbols
                for (name, ty) in &new_ctx.bindings {
                    if current_ctx.lookup(name).is_none() {
                        symbols.push(name.clone());
                        collect_type_symbols(ty, symbols);
                    }
                }
                current_ctx = new_ctx;
            }
        }
    }
}

// ============================================================================
// Rule Processing
// ============================================================================

fn gather_from_rule(
    nt: &NonTerminal,
    rule: &TypingRule,
    grammar: &Grammar,
    ctx: &Context,
    depth: usize,
    max_depth: usize,
    symbols: &mut Vec<String>,
) {
    // Resolve bindings
    let bound = match resolve_bindings(nt, &rule.name, grammar) {
        Ok(b) => b,
        Err(_) => return,
    };

    // Initialize substitution
    let subst_map = extract_type_bindings(&bound);

    // Check each premise and gather symbols from context extensions
    for premise in &rule.premises {
        // Build extended context from premise setting
        let premise_ctx = match &premise.setting {
            Some(setting) => {
                match gather_from_setting(setting, &bound, ctx, &subst_map, symbols) {
                    Some(c) => c,
                    None => ctx.clone(),
                }
            }
            None => ctx.clone(),
        };

        // Gather from judgment
        if let Some(judgment) = &premise.judgment {
            match judgment {
                TypingJudgment::Ascription((term_var, expected_ty)) => {
                    if let Some(node) = bound.get(term_var) {
                        gather_from_node(node, grammar, &premise_ctx, depth + 1, max_depth, symbols);
                    }
                    // Collect type symbols from expected type
                    collect_type_symbols(expected_ty, symbols);
                }
                TypingJudgment::Membership(var_name, _) => {
                    if let Some(name) = bound.get(var_name).and_then(node_text) {
                        symbols.push(name.clone());
                        if let Some(ty) = premise_ctx.lookup(&name) {
                            collect_type_symbols(ty, symbols);
                        }
                    }
                }
            }
        }
    }

    // Gather from conclusion
    match &rule.conclusion.kind {
        ConclusionKind::Type(ty) => {
            let resolved = subst(ty, &subst_map);
            collect_type_symbols(&resolved, symbols);
        }
        ConclusionKind::ContextLookup(_, var_name) => {
            if let Some(name) = bound.get(var_name).and_then(node_text) {
                symbols.push(name.clone());
                if let Some(ty) = ctx.lookup(&name) {
                    collect_type_symbols(ty, symbols);
                }
            }
        }
    }

    // Gather from output context transform
    if let Some(output) = &rule.conclusion.context.output {
        for (var_name, ty_expr) in &output.extensions {
            if let Some(name) = bound.get(var_name).and_then(node_text) {
                symbols.push(name.clone());
                let ty = subst(ty_expr, &subst_map);
                collect_type_symbols(&ty, symbols);
            }
        }
    }
}

fn gather_from_setting(
    setting: &crate::logic::typing::rule::TypeSetting,
    bound: &HashMap<String, Node>,
    base: &Context,
    subst_map: &Substitution,
    symbols: &mut Vec<String>,
) -> Option<Context> {
    let mut ctx = base.clone();
    
    for (var_name, ty_expr) in &setting.extensions {
        let node = bound.get(var_name)?;
        let name = node_text(node)?;
        let ty = subst(ty_expr, subst_map);
        
        // Collect the symbol and type
        symbols.push(name.clone());
        collect_type_symbols(&ty, symbols);
        
        ctx = ctx.extend(name, ty);
    }
    
    Some(ctx)
}

// ============================================================================
// Context Transform Extraction
// ============================================================================

fn get_context_transform(
    nt: &NonTerminal,
    grammar: &Grammar,
    ctx: &Context,
    depth: usize,
) -> Option<Context> {
    if depth > 50 {
        return None;
    }
    
    if let Some(rule_name) = &nt.production.rule {
        if let Some(rule) = grammar.typing_rules.get(rule_name) {
            let bound = resolve_bindings(nt, &rule.name, grammar).ok()?;
            let subst_map = extract_type_bindings(&bound);
            return extract_context_transform(&rule.conclusion, &bound, ctx, &subst_map);
        }
    }
    
    // Check children for context transform
    for child in &nt.children {
        if let Node::NonTerminal(child_nt) = child {
            if let Some(new_ctx) = get_context_transform(child_nt, grammar, ctx, depth + 1) {
                return Some(new_ctx);
            }
        }
    }
    
    None
}

fn extract_context_transform(
    conc: &crate::logic::typing::Conclusion,
    bound: &HashMap<String, Node>,
    base_ctx: &Context,
    subst_map: &Substitution,
) -> Option<Context> {
    let output = conc.context.output.as_ref()?;
    let mut new_ctx = base_ctx.clone();
    
    for (var_name, ty_expr) in &output.extensions {
        let node = bound.get(var_name)?;
        let name = node_text(node)?;
        let ty = subst(ty_expr, subst_map);
        new_ctx = new_ctx.extend(name, ty);
    }
    
    Some(new_ctx)
}

// ============================================================================
// Binding Resolution (simplified from eval.rs)
// ============================================================================

enum BindError {
    AtFrontier,
    Malformed,
}

fn resolve_bindings(
    nt: &NonTerminal,
    rule_name: &str,
    grammar: &Grammar,
) -> Result<HashMap<String, Node>, BindError> {
    let root = Node::NonTerminal(nt.clone());
    let mut bound = HashMap::new();

    let required_bindings = if let Some(rule) = grammar.typing_rules.get(rule_name) {
        rule.used_bindings()
    } else {
        std::collections::HashSet::new()
    };

    for (name, paths) in grammar.binding_map.bindings_for_rule(rule_name) {
        match resolve_one(&root, nt, paths) {
            Some(node) => { bound.insert(name.to_string(), node); }
            None => {
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
    paths: &[crate::logic::grammar::binding::GrammarPath],
) -> Option<Node> {
    use crate::logic::partial::binding::ResolutionError;

    for path in paths {
        match resolve_binding_path(root, path) {
            Ok(results) => {
                if let Some(res) = results.iter().find(|r| r.is_match()).or(results.first()) {
                    return Some(res.node().clone());
                }
            }
            Err(ResolutionError::AlternativeMismatch) => continue,
            Err(ResolutionError::MissingNode) => {
                if is_path_beyond_frontier(nt, path) {
                    return None;
                }
                continue;
            }
        }
    }
    None
}

fn extract_type_bindings(bound: &HashMap<String, Node>) -> Substitution {
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
// Utilities
// ============================================================================

fn node_text(node: &Node) -> Option<String> {
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

fn is_at_frontier(node: &Node) -> bool {
    match node {
        Node::Terminal(Terminal::Partial { .. }) => true,
        Node::Terminal(Terminal::Complete { .. }) => false,
        Node::NonTerminal(nt) => {
            nt.children.len() < nt.production.rhs.len() ||
            nt.children.last().map_or(false, is_at_frontier)
        }
    }
}

fn is_path_beyond_frontier(
    nt: &NonTerminal,
    path: &crate::logic::grammar::binding::GrammarPath,
) -> bool {
    let steps = path.steps();
    if steps.is_empty() {
        return is_at_frontier(&Node::NonTerminal(nt.clone()));
    }
    
    let idx = steps[0].child_index;
    idx >= nt.children.len()
}

/// Collect symbol strings from a Type (atom names, raw names, etc.)
fn collect_type_symbols(ty: &Type, symbols: &mut Vec<String>) {
    match ty {
        Type::Atom(name) => symbols.push(name.clone()),
        Type::Raw(name) => symbols.push(name.clone()),
        Type::Tuple(name) => symbols.push(name.clone()),
        Type::Arrow(l, r) => {
            collect_type_symbols(l, symbols);
            collect_type_symbols(r, symbols);
        }
        Type::Not(t) => collect_type_symbols(t, symbols),
        Type::Intersection(l, r) | Type::Union(l, r) => {
            collect_type_symbols(l, symbols);
            collect_type_symbols(r, symbols);
        }
        Type::ContextCall(ctx_name, var_name) => {
            symbols.push(ctx_name.clone());
            symbols.push(var_name.clone());
        }
        Type::Universe | Type::Empty => {}
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::logic::grammar::Grammar;
    use crate::logic::partial::parse::Parser;

    fn parse_one(spec: &str, input: &str) -> (NonTerminal, Grammar) {
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g.clone());
        let ast = p.partial(input).unwrap();
        let root = ast.roots.iter()
            .find(|r| r.is_complete())
            .or_else(|| ast.roots.first())
            .cloned()
            .expect("need at least one tree");
        (root, g)
    }

    #[test]
    fn test_gather_symbols_empty_context() {
        let spec = "start ::= 'hello'";
        let (root, g) = parse_one(spec, "hello");
        let symbols = gather_symbols(&root, &g);
        // No typing rules, should be empty or minimal
        assert!(symbols.is_empty() || symbols.iter().all(|s| s.is_empty() == false));
    }

    #[test]
    fn test_gather_symbols_with_context() {
        let spec = r#"
            Var(v) ::= /[a-z]+/
            start ::= Var
            x ∈ Γ
            -------------- (v)
            Γ(x)
        "#;
        let (root, g) = parse_one(spec, "x");
        let ctx = Context::new().extend("x".into(), Type::Atom("Int".into()));
        let symbols = gather_symbols_with_context(&root, &g, 20,&ctx);
        
        assert!(symbols.contains(&"x".to_string()));
        assert!(symbols.contains(&"Int".to_string()));
    }

    #[test]
    fn test_gather_symbols_lambda() {
        let spec = r#"
            Identifier ::= /[a-z]+/
            Variable(var) ::= Identifier[x]
            Lambda(lam) ::= 'λ' Identifier[x] '.' Variable[e]
            start ::= Lambda
            
            x ∈ Γ
            -------------- (var)
            Γ(x)
            
            Γ[x:'int'] ⊢ e : ?B
            -------------- (lam)
            'int' → ?B
        "#;
        let (root, g) = parse_one(spec, "λ x . x");
        let symbols = gather_symbols(&root, &g);
        
        // Should contain 'x' (bound by lambda) and 'int' (type annotation)
        assert!(symbols.contains(&"x".to_string()));
        assert!(symbols.contains(&"int".to_string()));
    }
}


