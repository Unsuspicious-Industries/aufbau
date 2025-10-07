use super::bind::BoundTypingRule;
use crate::logic::grammar::Grammar;
use crate::logic::partial::ParsedNode;
use crate::logic::{partial, PartialAST};
use std::collections::HashSet;
use std::path::Path;
use std::{fs, io};

pub mod serialize;
use serialize::*;
pub mod utils;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceSpan {
    pub start: usize,
    pub end: usize,
    //pub full: bool,
}

/// Nonterminal-specific data from an ASTNode
#[derive(Debug, Clone)]
pub struct NonTerminal {
    pub value: String,
    pub span: Option<SourceSpan>,
    pub children: Vec<ASTNode>,
    pub binding: Option<String>,
    pub bound_typing_rule: Option<Box<BoundTypingRule>>,
}

impl NonTerminal {
    /// Get the typing rule name if present
    pub fn rule_name(&self) -> Option<&str> {
        self.bound_typing_rule.as_ref().map(|r| r.name.as_str())
    }

    /// Check if this nonterminal has a specific rule
    pub fn has_rule(&self, rule_name: &str) -> bool {
        self.rule_name() == Some(rule_name)
    }

    /// Get terminal children of this nonterminal
    pub fn terminal_children(&self) -> Vec<Terminal> {
        self.children
            .iter()
            .filter_map(|c| {
                if let ASTNode::Terminal(t) = c {
                    Some(t.clone())
                } else {
                    None
                }
            })
            .collect()
    }

    /// Get nonterminal children of this nonterminal  
    pub fn nonterminal_children(&self) -> Vec<NonTerminal> {
        self.children
            .iter()
            .filter_map(|c| {
                if let ASTNode::Nonterminal(nt) = c {
                    Some(nt.clone())
                } else {
                    None
                }
            })
            .collect()
    }

    /// Get the binding if present
    pub fn binding(&self) -> Option<&String> {
        self.binding.as_ref()
    }

    pub fn as_node(&self) -> ASTNode {
        ASTNode::Nonterminal(self.clone())
    }
}

/// Terminal-specific data from an ASTNode
#[derive(Debug, Clone)]
pub struct Terminal {
    pub value: String,
    pub span: Option<SourceSpan>,
    pub binding: Option<String>,
}

impl Terminal {
    /// Get the binding if present
    pub fn binding(&self) -> Option<&String> {
        self.binding.as_ref()
    }

    pub fn as_node(&self) -> ASTNode {
        ASTNode::Terminal(self.clone())
    }
}

#[derive(Debug, Clone)]
pub enum ASTNode {
    Terminal(Terminal),
    Nonterminal(NonTerminal),
}

impl ASTNode {

    // TODO: remove legacy from_partial once new conversion lives on PartialAST

    pub fn span(&self) -> Option<&SourceSpan> {
        match self {
            ASTNode::Terminal(t) => t.span.as_ref(),
            ASTNode::Nonterminal(nt) => nt.span.as_ref(),
        }
    }

    pub fn set_span(&mut self, new_span: SourceSpan) {
        match self {
            ASTNode::Terminal(t) => t.span = Some(new_span),
            ASTNode::Nonterminal(nt) => nt.span = Some(new_span),
        }
    }

    pub fn binding(&self) -> Option<&String> {
        match self {
            ASTNode::Terminal(t) => t.binding.as_ref(),
            ASTNode::Nonterminal(nt) => nt.binding.as_ref(),
        }
    }

    pub fn set_binding(&mut self, new_binding: Option<String>) {
        match self {
            ASTNode::Terminal(t) => t.binding = new_binding,
            ASTNode::Nonterminal(nt) => nt.binding = new_binding,
        }
    }

    pub fn rules(&self) -> HashSet<String> {
        let mut out = HashSet::new();
        match self {
            ASTNode::Nonterminal(nt) => {
                if let Some(r) = &nt.bound_typing_rule {
                    out.insert(r.name.clone());
                }
                for child in &nt.children {
                    out.extend(child.rules());
                }
            }
            _ => {}
        }
        out
    }

    pub fn terminal_children(&self) -> Vec<Terminal> {
        match self {
            ASTNode::Nonterminal(nt) => nt
                .children
                .iter()
                .filter_map(|c| {
                    if let ASTNode::Terminal(t) = c {
                        Some(t.clone())
                    } else {
                        None
                    }
                })
                .collect(),
            _ => vec![],
        }
    }

    pub fn nonterminal_children(&self) -> Vec<NonTerminal> {
        match self {
            ASTNode::Nonterminal(nt) => nt
                .children
                .iter()
                .filter_map(|c| {
                    if let ASTNode::Nonterminal(n) = c {
                        Some(n.clone())
                    } else {
                        None
                    }
                })
                .collect(),
            _ => vec![],
        }
    }
    /// Get a reference to this node as a Terminal if it is one
    pub fn as_terminal(&self) -> Option<Terminal> {
        if let ASTNode::Terminal(t) = self {
            Some(t.clone())
        } else {
            None
        }
    }

    /// Get a reference to this node as a NonTerminal if it is one
    pub fn as_nonterminal(&self) -> Option<NonTerminal> {
        if let ASTNode::Nonterminal(nt) = self {
            Some(nt.clone())
        } else {
            None
        }
    }

    /// Access children directly for compatibility
    pub fn children(&self) -> Option<&Vec<ASTNode>> {
        match self {
            ASTNode::Nonterminal(nt) => Some(&nt.children),
            _ => None,
        }
    }

    pub fn value(&self) -> &str {
        match self {
            ASTNode::Terminal(t) => &t.value,
            ASTNode::Nonterminal(nt) => &nt.value,
        }
    }

    /// Calculate the depth of this AST node (maximum depth of any subtree)
    pub fn depth(&self) -> usize {
        match self {
            ASTNode::Terminal(_) => 1,
            ASTNode::Nonterminal(nt) => {
                if nt.children.is_empty() {
                    1
                } else {
                    1 + nt
                        .children
                        .iter()
                        .map(|child| child.depth())
                        .max()
                        .unwrap_or(0)
                }
            }
        }
    }

    /// Count the total number of nodes in this AST (including this node)
    pub fn node_count(&self) -> usize {
        match self {
            ASTNode::Terminal(_) => 1,
            ASTNode::Nonterminal(nt) => {
                1 + nt
                    .children
                    .iter()
                    .map(|child| child.node_count())
                    .sum::<usize>()
            }
        }
    }

    // ---- Lisp-style serialization API as methods ----
    pub fn serialize(&self) -> String {
        fn esc(s: &str) -> String {
            s.replace('\\', "\\\\").replace('"', "\\\"")
        }
        fn go(node: &ASTNode, out: &mut String) {
            match node {
                ASTNode::Terminal(t) => {
                    out.push_str(&format!("(T \"{}\"", esc(&t.value)));
                    if let Some(b) = &t.binding {
                        out.push_str(&format!("(b {})", b));
                    }
                    out.push(')');
                }
                ASTNode::Nonterminal(nt) => {
                    out.push_str(&format!("(N {}", nt.value));
                    if let Some(rule) = &nt.bound_typing_rule {
                        out.push_str(&format!("(rule {})", rule.name));
                    }
                    if let Some(b) = &nt.binding {
                        out.push_str(&format!("(b {})", b));
                    }
                    for ch in &nt.children {
                        go(ch, out);
                    }
                    out.push(')');
                }
            }
        }
        let mut s = String::new();
        go(self, &mut s);
        s
    }

    /// Pretty-print the AST as an indented S-expression for debugging
    pub fn pretty(&self) -> String {
        fn esc(s: &str) -> String {
            s.replace('\\', "\\\\").replace('"', "\\\"")
        }
        fn go(node: &ASTNode, indent: usize, out: &mut String) {
            let pad = "  ".repeat(indent);
            match node {
                ASTNode::Terminal(t) => {
                    out.push_str(&format!("{}(T \"{}\"", pad, esc(&t.value)));
                    if let Some(b) = &t.binding {
                        out.push_str(&format!(" (b {}))", b));
                    } else {
                        out.push(')');
                    }
                }
                ASTNode::Nonterminal(nt) => {
                    out.push_str(&format!("{}(N {}", pad, nt.value));
                    if let Some(rule) = &nt.bound_typing_rule {
                        out.push_str(&format!(" (rule {})", rule.name));
                    }
                    if let Some(b) = &nt.binding {
                        out.push_str(&format!(" (b {})", b));
                    }
                    if nt.children.is_empty() {
                        out.push(')');
                    } else {
                        out.push('\n');
                        for (i, ch) in nt.children.iter().enumerate() {
                            go(ch, indent + 1, out);
                            if i + 1 < nt.children.len() {
                                out.push('\n');
                            }
                        }
                        out.push_str(&format!("\n{})", pad));
                    }
                }
            }
        }
        let mut s = String::new();
        go(self, 0, &mut s);
        s
    }

    pub fn save<P: AsRef<Path>>(self, path: P) -> io::Result<()> {
        let rules = self.rules();
        let mut header = String::new();
        header.push_str(";!ast 1\n");
        if !rules.is_empty() {
            let mut v: Vec<_> = rules.into_iter().collect();
            v.sort();
            header.push_str(&format!(";!rules: {}\n", v.join(", ")));
        }
        header.push('\n');
        let body = self.serialize();
        fs::write(path, format!("{}{}\n", header, body))
    }

    /// Parse an AST S-expression with the help of a Grammar (for rule name resolution).
    pub fn parse(input: &str, grammar: &Grammar) -> Result<ASTNode, String> {
        let body = strip_headers(input);
        let sexpr = parse_sexpr(body)?;
        sexpr_to_ast(&sexpr, grammar)
    }

    /// Load an AST from a file that includes headers, resolving rule names with the provided Grammar.
    pub fn load<P: AsRef<Path>>(path: P, grammar: &Grammar) -> Result<ASTNode, String> {
        let content = fs::read_to_string(path).map_err(|e| e.to_string())?;
        Self::parse(&content, grammar)
    }

    // Syntactic equality
    pub fn syneq(&self, other: &ASTNode) -> bool {
        match (self, other) {
            (ASTNode::Terminal(t1), ASTNode::Terminal(t2)) => {
                t1.value == t2.value && t1.binding == t2.binding
            }
            (ASTNode::Nonterminal(nt1), ASTNode::Nonterminal(nt2)) => {
                nt1.value == nt2.value
                    && nt1.binding == nt2.binding
                    && nt1.children.len() == nt2.children.len()
                    && nt1
                        .children
                        .iter()
                        .zip(nt2.children.iter())
                        .all(|(a, b)| a.syneq(b))
            }
            _ => false,
        }
    }
    pub fn show_simple(&self) -> String {
        fn go(node: &ASTNode, indent: usize, out: &mut String) {
            for _ in 0..indent {
                out.push_str("  ");
            }
            match node {
                ASTNode::Terminal(t) => out.push_str(&t.value),
                ASTNode::Nonterminal(nt) => out.push_str(&nt.value),
            }
            out.push('\n');
            if let ASTNode::Nonterminal(nt) = node {
                for child in &nt.children {
                    go(child, indent + 1, out);
                }
            }
        }
        let mut s = String::new();
        go(self, 0, &mut s);
        s
    }
}
