//! Symbol Gathering - Collect terminals from AST and type symbols from typing rules

use crate::logic::grammar::Grammar;
use crate::logic::partial::structure::{Node, NonTerminal, Terminal};
use crate::logic::typing::Type;
use crate::logic::typing::rule::{ConclusionKind, TypingJudgment, TypingRule};

// =============================================================================
// Terminal Gathering from AST
// =============================================================================

/// Collect all terminal values from a partial AST.
pub fn gather_terminals(root: &NonTerminal) -> Vec<String> {
    let mut terminals = Vec::new();
    collect_terminals_from_node(&Node::NonTerminal(root.clone()), &mut terminals);
    terminals
}

/// Collect all terminals as Terminal structs (preserving complete/partial info)
pub fn gather_terminal_nodes(root: &NonTerminal) -> Vec<Terminal> {
    let mut terminals = Vec::new();
    collect_terminal_nodes_from_node(&Node::NonTerminal(root.clone()), &mut terminals);
    terminals
}

fn collect_terminals_from_node(node: &Node, out: &mut Vec<String>) {
    match node {
        Node::Terminal(Terminal::Complete { value, .. }) => {
            out.push(value.clone());
        }
        Node::Terminal(Terminal::Partial { value, .. }) => {
            out.push(value.clone());
        }
        Node::NonTerminal(nt) => {
            for child in &nt.children {
                collect_terminals_from_node(child, out);
            }
        }
    }
}

fn collect_terminal_nodes_from_node(node: &Node, out: &mut Vec<Terminal>) {
    match node {
        Node::Terminal(t) => {
            out.push(t.clone());
        }
        Node::NonTerminal(nt) => {
            for child in &nt.children {
                collect_terminal_nodes_from_node(child, out);
            }
        }
    }
}

// =============================================================================
// Type Symbol Gathering from Typing Rules
// =============================================================================

/// Collect all type symbols (Raw, Atom, Meta) from all typing rules in the grammar.
/// Returns deduplicated symbols.
pub fn gather_type_symbols(grammar: &Grammar) -> Vec<String> {
    let mut symbols = Vec::new();

    for rule in grammar.typing_rules.values() {
        collect_symbols_from_rule(rule, &mut symbols);
    }

    // Deduplicate while preserving order
    let mut seen = std::collections::HashSet::new();
    symbols.retain(|s| seen.insert(s.clone()));

    symbols
}

/// Collect all Type::Raw values from typing rules (concrete type literals like 'int', 'bool')
pub fn gather_raw_types(grammar: &Grammar) -> Vec<String> {
    let mut raws = Vec::new();

    for rule in grammar.typing_rules.values() {
        collect_raws_from_rule(rule, &mut raws);
    }

    // Deduplicate
    let mut seen = std::collections::HashSet::new();
    raws.retain(|s| seen.insert(s.clone()));

    raws
}

fn collect_symbols_from_rule(rule: &TypingRule, out: &mut Vec<String>) {
    // From premises
    for premise in &rule.premises {
        // Setting extensions
        if let Some(setting) = &premise.setting {
            for (_, ty) in &setting.extensions {
                collect_symbols_from_type(ty, out);
            }
        }
        // Judgments
        if let Some(judgment) = &premise.judgment {
            match judgment {
                TypingJudgment::Ascription((_, ty)) => {
                    collect_symbols_from_type(ty, out);
                }
                TypingJudgment::Operation { left, right, .. } => {
                    collect_symbols_from_type(left, out);
                    collect_symbols_from_type(right, out);
                }
                TypingJudgment::Membership(_, _) => {}
            }
        }
    }

    // From conclusion
    match &rule.conclusion.kind {
        ConclusionKind::Type(ty) => {
            collect_symbols_from_type(ty, out);
        }
        ConclusionKind::ContextLookup(_, _) => {}
    }

    // From conclusion context output
    if let Some(output) = &rule.conclusion.context.output {
        for (_, ty) in &output.extensions {
            collect_symbols_from_type(ty, out);
        }
    }
}

fn collect_raws_from_rule(rule: &TypingRule, out: &mut Vec<String>) {
    // From premises
    for premise in &rule.premises {
        if let Some(setting) = &premise.setting {
            for (_, ty) in &setting.extensions {
                collect_raws_from_type(ty, out);
            }
        }
        if let Some(judgment) = &premise.judgment {
            match judgment {
                TypingJudgment::Ascription((_, ty)) => {
                    collect_raws_from_type(ty, out);
                }
                TypingJudgment::Operation { left, right, .. } => {
                    collect_raws_from_type(left, out);
                    collect_raws_from_type(right, out);
                }
                TypingJudgment::Membership(_, _) => {}
            }
        }
    }

    // From conclusion
    match &rule.conclusion.kind {
        ConclusionKind::Type(ty) => {
            collect_raws_from_type(ty, out);
        }
        ConclusionKind::ContextLookup(_, _) => {}
    }

    if let Some(output) = &rule.conclusion.context.output {
        for (_, ty) in &output.extensions {
            collect_raws_from_type(ty, out);
        }
    }
}

/// Recursively collect all symbol strings from a Type
fn collect_symbols_from_type(ty: &Type, out: &mut Vec<String>) {
    match ty {
        Type::Atom(name) => out.push(name.clone()),
        Type::Meta(name) => out.push(format!("?{}", name)),
        Type::Raw(name) => out.push(name.clone()),
        Type::Arrow(l, r) => {
            collect_symbols_from_type(l, out);
            collect_symbols_from_type(r, out);
        }
        Type::Not(t) => collect_symbols_from_type(t, out),
        Type::ContextCall(ctx, var) => {
            out.push(ctx.clone());
            out.push(var.clone());
        }
        Type::Partial(t, _) => collect_symbols_from_type(t, out),
        Type::PathOf(t, _) => collect_symbols_from_type(t, out),
        Type::Any | Type::None | Type::Path(_) => {}
    }
}

/// Recursively collect only Type::Raw values
fn collect_raws_from_type(ty: &Type, out: &mut Vec<String>) {
    match ty {
        Type::Raw(name) => out.push(name.clone()),
        Type::Arrow(l, r) => {
            collect_raws_from_type(l, out);
            collect_raws_from_type(r, out);
        }
        Type::Not(t) => collect_raws_from_type(t, out),
        Type::Partial(t, _) => collect_raws_from_type(t, out),
        Type::PathOf(t, _) => collect_raws_from_type(t, out),
        Type::Atom(_) | Type::Meta(_) | Type::ContextCall(_, _) => {}
        Type::Any | Type::None | Type::Path(_) => {}
    }
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::logic::partial::parse::Parser;

    fn parse_one(spec: &str, input: &str) -> NonTerminal {
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g);
        let ast = p.partial(input).unwrap();
        ast.roots
            .iter()
            .find(|r| r.is_complete())
            .or_else(|| ast.roots.first())
            .cloned()
            .expect("need at least one tree")
    }

    #[test]
    fn test_gather_terminals_simple() {
        let spec = "start ::= 'hello' 'world'";
        let root = parse_one(spec, "hello world");
        let terminals = gather_terminals(&root);
        assert_eq!(terminals, vec!["hello", "world"]);
    }

    #[test]
    fn test_gather_terminals_nested() {
        let spec = r#"
            A ::= 'a' B
            B ::= 'b' 'c'
            start ::= A
        "#;
        let root = parse_one(spec, "a b c");
        let terminals = gather_terminals(&root);
        assert_eq!(terminals, vec!["a", "b", "c"]);
    }

    #[test]
    fn test_gather_terminal_nodes() {
        let spec = "start ::= 'x' 'y'";
        let root = parse_one(spec, "x y");
        let nodes = gather_terminal_nodes(&root);
        assert_eq!(nodes.len(), 2);
        assert!(matches!(&nodes[0], Terminal::Complete { value, .. } if value == "x"));
        assert!(matches!(&nodes[1], Terminal::Complete { value, .. } if value == "y"));
    }

    #[test]
    fn test_gather_raw_types() {
        let spec = r#"
            Var(v) ::= /[a-z]+/
            Lambda(lam) ::= 'λ' Var[x] '.' Var[e]
            start ::= Lambda

            x ∈ Γ
            -------------- (v)
            Γ(x)

            Γ[x:'int'] ⊢ e : ?B
            -------------- (lam)
            'int' → ?B
        "#;
        let g = Grammar::load(spec).unwrap();
        let raws = gather_raw_types(&g);
        assert!(raws.contains(&"int".to_string()));
    }

    #[test]
    fn test_gather_type_symbols() {
        let spec = r#"
            Num(num) ::= /[0-9]+/
            start ::= Num

            -------------- (num)
            'number'
        "#;
        let g = Grammar::load(spec).unwrap();
        let symbols = gather_type_symbols(&g);
        assert!(symbols.contains(&"number".to_string()));
    }
}
