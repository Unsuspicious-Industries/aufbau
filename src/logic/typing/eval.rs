//! Type Evaluation - Single Tree Checker
//!
//! Clean API: `check_tree` evaluates ONE tree, returning its status.
//! Forest filtering is done at a higher level by the caller.

use crate::logic::grammar::Grammar;
use crate::logic::partial::binding::resolve_binding_path;
use crate::logic::partial::structure::{Node, NonTerminal, Terminal};
use crate::logic::typing::core::{Context, Substitution, TreeStatus, subst, unify};
use crate::logic::typing::rule::ConclusionKind;
use crate::logic::typing::{Type, TypingJudgment, TypingRule};
use std::collections::HashMap;

// ============================================================================
// Core API - single tree checking
// ============================================================================

/// Check a single tree → TreeStatus
pub fn check_tree(root: &NonTerminal, grammar: &Grammar) -> TreeStatus {
    check_tree_with_context(root, grammar, &Context::new())
}

/// Check with initial context
pub fn check_tree_with_context(root: &NonTerminal, grammar: &Grammar, ctx: &Context) -> TreeStatus {
    check_node(&Node::NonTerminal(root.clone()), grammar, ctx, 0)
}

/// Predicate: any tree in forest is well-typed?
pub fn evaluate_typing(roots: &[NonTerminal], grammar: &Grammar) -> bool {
    roots.iter().any(|r| check_tree(r, grammar).is_ok())
}

// ============================================================================
// Node Checking (recursive descent)
// ============================================================================

fn check_node(node: &Node, grammar: &Grammar, ctx: &Context, depth: usize) -> TreeStatus {
    if depth > 50 {
        return TreeStatus::Partial(Type::Universe);
    }

    match node {
        Node::Terminal(t) => check_terminal(t, ctx),
        Node::NonTerminal(nt) => check_nt(nt, grammar, ctx, depth),
    }
}

fn check_terminal(term: &Terminal, ctx: &Context) -> TreeStatus {
    match term {
        Terminal::Complete { value, .. } => {
            // Try context lookup for identifiers
            let ty = ctx.lookup(value).cloned().unwrap_or(Type::Universe);
            TreeStatus::Valid(ty)
        }
        Terminal::Partial { .. } => {
            TreeStatus::Partial(Type::Universe)
        }
    }
}

fn check_nt(nt: &NonTerminal, grammar: &Grammar, ctx: &Context, depth: usize) -> TreeStatus {
    // If this production has a typing rule, apply it
    if let Some(rule_name) = &nt.production.rule {
        if let Some(rule) = grammar.typing_rules.get(rule_name) {
            return apply_rule(nt, rule, grammar, ctx, depth);
        }
    }
    
    // No rule - check for transparent wrapper pattern
    // HEURISTIC: Only-child drilling
    //   - If production has exactly ONE non-terminal child, drill through it
    //   - This handles wrapper productions like `Term ::= BaseTerm`
    //   - Productions with multiple children or only terminals return Universe
    drill_only_child(nt, grammar, ctx, depth)
}

/// Drill through "only-child" wrapper productions
/// 
/// SEMANTICS:
/// - Productions without typing rules are "transparent" if they have exactly
///   one non-terminal child
/// - The type of the production is the type of that single child
/// - If there are 0 or 2+ non-terminal children, check in sequence
///   propagating context transforms from rules like `let`
/// - Terminals (like ')', literals) have no type significance
fn drill_only_child(
    nt: &NonTerminal,
    grammar: &Grammar,
    ctx: &Context,
    depth: usize,
) -> TreeStatus {
    let (status, _) = drill_only_child_with_context(nt, grammar, ctx, depth);
    status
}

/// Check a node and return both its status AND any context transform it produces
fn check_node_with_context_output(
    node: &Node,
    grammar: &Grammar,
    ctx: &Context,
    depth: usize,
) -> (TreeStatus, Option<Context>) {
    if depth > 50 {
        return (TreeStatus::Partial(Type::Universe), None);
    }

    match node {
        Node::Terminal(t) => (check_terminal(t, ctx), None),
        Node::NonTerminal(nt) => check_nt_with_context_output(nt, grammar, ctx, depth),
    }
}

/// Check non-terminal and return any context transform
fn check_nt_with_context_output(
    nt: &NonTerminal,
    grammar: &Grammar,
    ctx: &Context,
    depth: usize,
) -> (TreeStatus, Option<Context>) {
    // If this production has a typing rule, apply it and check for context transform
    if let Some(rule_name) = &nt.production.rule {
                if let Some(rule) = grammar.typing_rules.get(rule_name) {
            return apply_rule_with_context_output(nt, rule, grammar, ctx, depth);
        }
    }
    
    // No rule - try to drill and propagate any context transform from children
    drill_only_child_with_context(nt, grammar, ctx, depth)
}

/// Drill through productions, propagating context transforms
fn drill_only_child_with_context(
    nt: &NonTerminal,
    grammar: &Grammar,
    ctx: &Context,
    depth: usize,
) -> (TreeStatus, Option<Context>) {
    // Collect non-terminal children
    let nt_children: Vec<_> = nt.children.iter()
        .filter(|c| matches!(c, Node::NonTerminal(_)))
        .collect();
    
    // Only drill if exactly one non-terminal child - propagate context transform
    if nt_children.len() == 1 {
        return check_node_with_context_output(nt_children[0], grammar, ctx, depth + 1);
    }
    
    // Zero NT children (only terminals)
    if nt_children.is_empty() {
        if is_at_frontier(&Node::NonTerminal(nt.clone())) {
            return (TreeStatus::Partial(Type::Universe), None);
        } else {
            return (TreeStatus::Valid(Type::Universe), None);
        }
    }
    
    // Multiple NT children - check in sequence with context propagation
    let (status, final_ctx) = check_children_with_context_propagation_full(&nt_children, grammar, ctx, depth);
    (status, final_ctx)
}

/// Check children in sequence, returning final context
fn check_children_with_context_propagation_full(
    children: &[&Node],
    grammar: &Grammar,
    initial_ctx: &Context,
    depth: usize,
) -> (TreeStatus, Option<Context>) {
    let mut current_ctx = initial_ctx.clone();
    let mut has_partial = false;
    let mut ctx_changed = false;
    
    for child in children.iter() {
        // Check the child with the current context
        let (status, new_ctx) = check_node_with_context_output(child, grammar, &current_ctx, depth + 1);
        
        match status {
            TreeStatus::Malformed => return (TreeStatus::Malformed, None),
            TreeStatus::Partial(_) => has_partial = true,
            TreeStatus::Valid(_) => {}
        }
        
        // Update context for next child if this child transformed it
        if let Some(ctx) = new_ctx {
            current_ctx = ctx;
            ctx_changed = true;
        }
    }
    
    let status = if has_partial || children.iter().any(|c| is_at_frontier(c)) {
        TreeStatus::Partial(Type::Universe)
    } else {
        TreeStatus::Valid(Type::Universe)
    };
    
    // Return the transformed context if any child changed it
    if ctx_changed {
        (status, Some(current_ctx))
    } else {
        (status, None)
    }
}

// ============================================================================
// Rule Application
// ============================================================================

fn apply_rule(
    nt: &NonTerminal,
    rule: &TypingRule,
    grammar: &Grammar,
    ctx: &Context,
    depth: usize,
) -> TreeStatus {
    let (status, _) = apply_rule_with_context_output(nt, rule, grammar, ctx, depth);
    status
}

/// Apply a rule and return both the status and any context transform
fn apply_rule_with_context_output(
    nt: &NonTerminal,
    rule: &TypingRule,
    grammar: &Grammar,
    ctx: &Context,
    depth: usize,
) -> (TreeStatus, Option<Context>) {
    // 1. Resolve all bindings
    let bound = match resolve_bindings(nt, &rule.name, grammar) {
        Ok(b) => b,
        Err(BindError::AtFrontier) => return (TreeStatus::Partial(Type::Universe), None),
        Err(BindError::Malformed) => return (TreeStatus::Malformed, None),
    };

    // 2. Initialize substitution from type bindings
    let mut subst_map = extract_type_bindings(&bound);

    // 3. Check all premises
    for premise in &rule.premises {
        match check_premise(premise, &bound, grammar, ctx, depth, &mut subst_map) {
            PremiseResult::Ok => {}
            PremiseResult::Fail => return (TreeStatus::Malformed, None),
            PremiseResult::Partial => return (TreeStatus::Partial(Type::Universe), None),
        }
    }

    // 4. Evaluate conclusion and extract context transform
    let status = eval_conclusion(&rule.conclusion, &bound, ctx, &subst_map);
    
    // 5. Check for context transform in conclusion (e.g., Γ -> Γ[x:τ])
    let new_ctx = extract_context_transform(&rule.conclusion, &bound, ctx, &subst_map);
    
    (status, new_ctx)
}

/// Extract context transform from a conclusion like `Γ -> Γ[x:τ] ⊢ τ`
fn extract_context_transform(
    conc: &crate::logic::typing::Conclusion,
    bound: &HashMap<String, Node>,
    base_ctx: &Context,
    subst_map: &Substitution,
) -> Option<Context> {
    // Check if conclusion has an output context transform
    let output = conc.context.output.as_ref()?;
    
    // Build extended context from output setting
    let mut new_ctx = base_ctx.clone();
    
    for (var_name, ty_expr) in &output.extensions {
        // Resolve the variable name binding to get actual name
        let node = bound.get(var_name)?;
        let name = node_text(node)?;
        
        // Apply substitution to type expression
        let ty = subst(ty_expr, subst_map);
        
        new_ctx = new_ctx.extend(name, ty);
    }
    
    Some(new_ctx)
}

// ============================================================================
// Binding Resolution
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

    // Get the set of bindings actually USED by the typing rule
    let required_bindings = if let Some(rule) = grammar.typing_rules.get(rule_name) {
        rule.used_bindings()
                        } else {
        // No rule defined - no required bindings
        std::collections::HashSet::new()
    };

    for (name, paths) in grammar.binding_map.bindings_for_rule(rule_name) {
        match resolve_one(&root, nt, paths) {
            Some(node) => { bound.insert(name.to_string(), node); }
            None => {
                // Only fail if this binding is REQUIRED by the rule
                if required_bindings.contains(name) {
                    if is_at_frontier(&root) {
                        return Err(BindError::AtFrontier);
                    } else {
                        return Err(BindError::Malformed);
                    }
                }
                // Else: binding not used by rule, OK to skip
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
// Premise Checking
// ============================================================================

enum PremiseResult {
    Ok,
    Fail,
    Partial,
}

fn check_premise(
    premise: &crate::logic::typing::Premise,
    bound: &HashMap<String, Node>,
    grammar: &Grammar,
    base_ctx: &Context,
    depth: usize,
    subst_map: &mut Substitution,
) -> PremiseResult {
    // Build extended context from setting
    let ctx = match &premise.setting {
        Some(setting) => {
            match build_ctx_extension(setting, bound, base_ctx, subst_map) {
                Some(c) => c,
                None => return PremiseResult::Partial, // Can't build context yet
            }
        }
        None => base_ctx.clone(),
    };

    // Check judgment
    match &premise.judgment {
        Some(TypingJudgment::Ascription((term_var, expected_ty))) => {
            let node = match bound.get(term_var) {
                Some(n) => n,
                None => return PremiseResult::Partial,
            };

            match check_node(node, grammar, &ctx, depth + 1) {
                TreeStatus::Valid(actual) => {
                    if unify(&actual, expected_ty, subst_map) {
                        PremiseResult::Ok
                        } else {
                        PremiseResult::Fail
                    }
                }
                TreeStatus::Partial(_) => PremiseResult::Partial,
                TreeStatus::Malformed => PremiseResult::Fail,
            }
        }

        Some(TypingJudgment::Membership(var_name, _)) => {
            let name = match bound.get(var_name).and_then(node_text) {
                        Some(n) => n,
                None => return PremiseResult::Partial,
            };
            
            if ctx.lookup(&name).is_some() {
                PremiseResult::Ok
                            } else {
                PremiseResult::Fail
            }
        }

        None => PremiseResult::Ok, // Setting-only premise
    }
}

fn build_ctx_extension(
    setting: &crate::logic::typing::rule::TypeSetting,
    bound: &HashMap<String, Node>,
    base: &Context,
    subst_map: &Substitution,
) -> Option<Context> {
    let mut ctx = base.clone();
    
    for (var_name, ty_expr) in &setting.extensions {
        let node = bound.get(var_name)?;
        let name = node_text(node)?;
        let ty = subst(ty_expr, subst_map);
        ctx = ctx.extend(name, ty);
    }
    
    Some(ctx)
}

// ============================================================================
// Conclusion Evaluation
// ============================================================================

fn eval_conclusion(
    conc: &crate::logic::typing::Conclusion,
    bound: &HashMap<String, Node>,
    ctx: &Context,
    subst_map: &Substitution,
) -> TreeStatus {
    let ty = match &conc.kind {
        ConclusionKind::Type(t) => subst(t, subst_map),
        
        ConclusionKind::ContextLookup(_, var_name) => {
            let node = match bound.get(var_name) {
                Some(n) => n,
                None => return TreeStatus::Partial(Type::Universe),
            };
            let name = match node_text(node) {
                Some(n) => n,
                None => return TreeStatus::Partial(Type::Universe),
            };
            match ctx.lookup(&name) {
                Some(t) => t.clone(),
                None => return TreeStatus::Malformed,
            }
        }
    };

    TreeStatus::Valid(ty)
}

// ============================================================================
// Frontier Detection
// ============================================================================

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

// ============================================================================
// Unit Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::logic::grammar::Grammar;
    use crate::logic::partial::parse::Parser;

    fn parse_one(spec: &str, input: &str) -> NonTerminal {
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g);
        let ast = p.partial(input).unwrap();
        ast.roots.iter()
            .find(|r| r.is_complete())
            .or_else(|| ast.roots.first())
            .cloned()
            .expect("need at least one tree")
    }

    #[test]
    fn test_no_rule_complete() {
        let spec = "start ::= 'a' 'b'";
        let root = parse_one(spec, "a b");
        let g = Grammar::load(spec).unwrap();
        
        match check_tree(&root, &g) {
            TreeStatus::Valid(_) => {}
            other => panic!("Expected Valid, got {:?}", other),
        }
    }

    #[test]
    fn test_no_rule_partial() {
        let spec = "start ::= 'a' 'b'";
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g.clone());
        let ast = p.partial("a").unwrap();
        let root = &ast.roots[0];
        
        match check_tree(root, &g) {
            TreeStatus::Partial(_) => {}
            other => panic!("Expected Partial, got {:?}", other),
        }
    }

    #[test]
    fn test_simple_rule_complete() {
        // Use the real STLC grammar to test
        use std::path::PathBuf;
        let path = PathBuf::from("examples/stlc.spec");
        let content = std::fs::read_to_string(&path).expect("read stlc.spec");
        let g = Grammar::load(&content).unwrap();
        
        // Parse a simple variable - needs to be in context for dec rule
        let mut p = Parser::new(g.clone());
        let ast = p.partial("x").unwrap();
        
        // Should have at least one complete tree
        assert!(ast.complete(), "Should have complete parse");
        
        let root = ast.roots.iter().find(|r| r.is_complete()).unwrap();
        
        // With x in context, should succeed
        let ctx = Context::new().extend("x".to_string(), Type::Atom("Int".to_string()));
        let status = check_tree_with_context(root, &g, &ctx);
        
        match status {
            TreeStatus::Valid(ty) => {
                assert_eq!(ty, Type::Atom("Int".to_string()));
            }
            other => panic!("Expected Valid(Int), got {:?}", other),
        }
    }

    #[test]
    fn test_no_rule_returns_universe() {
        // Productions without typing rules return Universe
        // This is the "only-child drilling" heuristic
        let spec = r#"
            Num ::= /[0-9]+/
            start ::= Num
        "#;
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g.clone());
        let ast = p.partial("123").unwrap();
        let root = ast.roots.iter().find(|r| r.is_complete()).unwrap();
        
        // No typing rules means Universe type
        match check_tree(root, &g) {
            TreeStatus::Valid(ty) => {
                assert_eq!(ty, Type::Universe);
            }
            other => panic!("Expected Valid(Universe), got {:?}", other),
        }
    }

    #[test]
    fn test_partial_tree() {
        let spec = r#"
            start ::= 'a' 'b' 'c'
        "#;
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g.clone());
        let ast = p.partial("a b").unwrap();
        let root = &ast.roots[0];
        
        // Partial tree should return Partial status
        match check_tree(root, &g) {
            TreeStatus::Partial(_) => {}
            other => panic!("Expected Partial, got {:?}", other),
        }
    }

    #[test]
    fn test_forest_has_valid() {
        let spec = r#"
            A(ruleA) ::= 'x'
            B(ruleB) ::= 'x' 'y'
            start ::= A | B
            
            -------------- (ruleA)
            'typeA'
            
            -------------- (ruleB)
            'typeB'
        "#;
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g.clone());
        let ast = p.partial("x").unwrap();
        
        // Both A (complete) and B (partial) should be valid
        assert!(evaluate_typing(&ast.roots, &g));
    }
}















