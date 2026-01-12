//! Serialization and deserialization for partial ASTs.
//!
//! This module provides S-expression based serialization for the partial tree system.
//! 
//! # Format
//! 
//! ## Symbols
//! - `@N` - Alternative index (which production rule was used)
//! - `$name` - Binding name
//! - `#N` - Consumed segments count
//! - `+regex` - Extension regex (for complete terminals)
//! - `~regex` - Remainder regex (for partial terminals)
//! 
//! ## Terminals
//! - Complete: `(T "value" $bind +ext)` - all inline
//! - Partial: `(T~ "value" $bind ~rem)` - all inline
//! 
//! ## Nonterminals
//! - Format: `(Name @alt $bind #segs child1 child2 ...)`
//! - Metadata symbols are inline, children are indented on new lines
//! 
//! ## Example
//! ```text
//! (Expr @0 $result #3
//!   (T "x" $var)
//!   (T "+")
//!   (T "y" $var))
//! ```
//! 
//! This represents an Expr nonterminal using alternative 0, bound as "result",
//! consuming 3 segments, with three terminal children (all inline).

use crate::logic::grammar::Grammar;
use crate::logic::partial::structure::{Node, NonTerminal, PartialAST, Terminal};
use crate::regex::Regex as DerivativeRegex;

// ---------------- S-expression parsing helpers ----------------

#[derive(Debug, Clone, PartialEq)]
pub enum SExpr {
    Atom(String),
    Str(String),
    List(Vec<SExpr>),
}

pub fn strip_headers(input: &str) -> &str {
    let mut offset = 0usize;
    for line in input.lines() {
        let trimmed = line.trim_start();
        if trimmed.starts_with(';') || trimmed.is_empty() {
            offset += line.len();
            offset += 1; // account for newline
            continue;
        } else {
            return &input[offset..];
        }
    }
    input
}

pub fn parse_sexpr(input: &str) -> Result<SExpr, String> {
    enum Tok<'a> {
        LParen,
        RParen,
        Atom(&'a str),
        Str(String),
    }

    fn tokenize(s: &str) -> Result<Vec<Tok<'_>>, String> {
        let mut toks = Vec::new();
        let mut chars = s.char_indices().peekable();

        while let Some((byte_idx, ch)) = chars.next() {
            match ch {
                c if c.is_whitespace() => {
                    continue;
                }
                ';' => {
                    // comment to end of line
                    while let Some((_, ch)) = chars.peek() {
                        if *ch == '\n' {
                            break;
                        }
                        chars.next();
                    }
                }
                '(' => {
                    toks.push(Tok::LParen);
                }
                ')' => {
                    toks.push(Tok::RParen);
                }
                '"' => {
                    let mut buf = String::new();
                    let mut escaped = false;
                    while let Some((_, ch)) = chars.next() {
                        if escaped {
                            match ch {
                                '"' => buf.push('"'),
                                '\\' => buf.push('\\'),
                                'n' => buf.push('\n'),
                                't' => buf.push('\t'),
                                other => buf.push(other),
                            }
                            escaped = false;
                        } else if ch == '\\' {
                            escaped = true;
                        } else if ch == '"' {
                            break;
                        } else {
                            buf.push(ch);
                        }
                    }
                    toks.push(Tok::Str(buf));
                }
                _ => {
                    let start_byte = byte_idx;
                    let mut end_byte = byte_idx + ch.len_utf8();

                    while let Some((byte_idx, ch)) = chars.peek() {
                        if ch.is_whitespace() || *ch == '(' || *ch == ')' || *ch == '"' {
                            break;
                        }
                        end_byte = *byte_idx + ch.len_utf8();
                        chars.next();
                    }

                    let atom = &s[start_byte..end_byte];
                    toks.push(Tok::Atom(atom));
                }
            }
        }
        Ok(toks)
    }

    fn parse_list<'a>(toks: &[Tok<'a>], pos: &mut usize) -> Result<SExpr, String> {
        let mut items = Vec::new();
        while *pos < toks.len() {
            match toks[*pos] {
                Tok::RParen => {
                    *pos += 1;
                    break;
                }
                Tok::LParen => {
                    *pos += 1;
                    items.push(parse_list(toks, pos)?);
                }
                Tok::Atom(a) => {
                    *pos += 1;
                    items.push(SExpr::Atom(a.to_string()));
                }
                Tok::Str(ref s) => {
                    *pos += 1;
                    items.push(SExpr::Str(s.clone()));
                }
            }
        }
        Ok(SExpr::List(items))
    }

    let toks = tokenize(input)?;
    let mut pos = 0;
    match toks.get(pos) {
        Some(Tok::LParen) => {
            pos += 1;
            parse_list(&toks, &mut pos)
        }
        _ => Err("Expected '('".into()),
    }
}

// ---------------- Serialization: AST -> S-expression ----------------

/// Serialize a PartialAST to a more convenient S-expression format
pub fn ast_to_sexpr(ast: &PartialAST) -> SExpr {
    if ast.roots.len() == 1 {
        // Single root: just serialize the tree directly
        nt_to_sexpr(&ast.roots[0])
    } else {
        // Multiple roots: wrap in a list
        SExpr::List(
            ast.roots
                .iter()
                .map(|root| nt_to_sexpr(root))
                .collect()
        )
    }
}

/// Serialize a NonTerminal to S-expression
pub fn nt_to_sexpr(nt: &NonTerminal) -> SExpr {
    let mut items = vec![
        SExpr::Atom(nt.name.clone()),
        SExpr::Atom(format!("@{}", nt.alternative_index)),
    ];
    
    // Add binding if present (using $symbol)
    if let Some(binding) = &nt.binding {
        items.push(SExpr::Atom(format!("${}", binding)));
    }
    
    // Add consumed_segments metadata (using #symbol)
    items.push(SExpr::Atom(format!("#{}", nt.consumed_segments)));
    
    // Add children
    for child in &nt.children {
        items.push(node_to_sexpr(child));
    }
    
    SExpr::List(items)
}

/// Serialize a Node to S-expression
pub fn node_to_sexpr(node: &Node) -> SExpr {
    match node {
        Node::NonTerminal(nt) => nt_to_sexpr(nt),
        Node::Terminal(t) => terminal_to_sexpr(t),
    }
}

/// Serialize a Terminal to S-expression
pub fn terminal_to_sexpr(t: &Terminal) -> SExpr {
    match t {
        Terminal::Complete { value, binding, extension } => {
            let mut items = vec![
                SExpr::Atom("T".to_string()),
                SExpr::Str(value.clone()),
            ];
            
            // Add binding (using $symbol)
            if let Some(binding) = binding {
                items.push(SExpr::Atom(format!("${}", binding)));
            }
            
            // Add extension (using + followed by quoted pattern)
            if let Some(ext) = extension {
                items.push(SExpr::Atom("+".to_string()));
                items.push(SExpr::Str(ext.to_pattern()));
            }
            
            SExpr::List(items)
        }
        Terminal::Partial { value, binding, remainder } => {
            let mut items = vec![
                SExpr::Atom("T~".to_string()),
                SExpr::Str(value.clone()),
            ];
            
            // Add binding (using $symbol)
            if let Some(binding) = binding {
                items.push(SExpr::Atom(format!("${}", binding)));
            }
            
            // Add remainder (using ~ followed by quoted pattern)
            if let Some(rem) = remainder {
                items.push(SExpr::Atom("~".to_string()));
                items.push(SExpr::Str(rem.to_pattern()));
            }
            
            SExpr::List(items)
        }
    }
}

/// Pretty-print an S-expression
pub fn sexpr_to_string(sexpr: &SExpr) -> String {
    sexpr_to_string_indent(sexpr, 0)
}

fn sexpr_to_string_indent(sexpr: &SExpr, indent: usize) -> String {
    match sexpr {
        SExpr::Atom(s) => s.clone(),
        SExpr::Str(s) => format!("\"{}\"", escape_string(s)),
        SExpr::List(items) if items.is_empty() => "()".to_string(),
        SExpr::List(items) => {
            let indent_str = "  ".repeat(indent);
            let mut out = String::from("(");
            
            // Check if this is a terminal node (T or T~)
            let is_terminal = if let Some(SExpr::Atom(tag)) = items.first() {
                tag == "T" || tag == "T~"
            } else {
                false
            };
            
            // For terminals, put everything inline
            if is_terminal {
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        out.push(' ');
                    }
                    out.push_str(&sexpr_to_string_indent(item, indent + 1));
                }
                out.push(')');
                return out;
            }
            
            // For non-terminals, find where metadata ends (symbols starting with @, $, #, +, ~)
            let mut metadata_end = 0;
            for (i, item) in items.iter().enumerate() {
                if i == 0 {
                    // First item is always the tag/name
                    metadata_end = 1;
                } else if let SExpr::Atom(s) = item {
                    if s.starts_with('@') || s.starts_with('$') || s.starts_with('#') || 
                       s.starts_with('+') || s.starts_with('~') {
                        metadata_end = i + 1;
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            }
            
            for (i, item) in items.iter().enumerate() {
                if i > 0 {
                    if i < metadata_end {
                        // Metadata: keep inline with a space
                        out.push(' ');
                    } else {
                        // Children: put on new line
                        out.push('\n');
                        out.push_str(&indent_str);
                        out.push_str("  ");
                    }
                }
                out.push_str(&sexpr_to_string_indent(item, indent + 1));
            }
            
            out.push(')');
            out
        }
    }
}

fn escape_string(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\t', "\\t")
}

// ---------------- Deserialization: S-expression -> AST ----------------

pub fn sexpr_to_ast(sexpr: &SExpr, grammar: &Grammar, input: String) -> Result<PartialAST, String> {
    match sexpr {
        SExpr::List(items) if !items.is_empty() => {
            // Check if this is a single tree or multiple roots
            if let SExpr::Atom(name) = &items[0] {
                if grammar.productions.contains_key(name) {
                    // Single tree
                    let root = sexpr_to_nt(sexpr, grammar)?;
                    return Ok(PartialAST::new(vec![root], input));
                }
            }
            
            // Multiple roots
            let mut roots = Vec::new();
            for item in items {
                roots.push(sexpr_to_nt(item, grammar)?);
            }
            Ok(PartialAST::new(roots, input))
        }
        _ => Err("Expected list for AST".into()),
    }
}

pub fn sexpr_to_nt(sexpr: &SExpr, grammar: &Grammar) -> Result<NonTerminal, String> {
    match sexpr {
        SExpr::List(items) if items.len() >= 2 => {
            let name = sexpr_atom(&items[0])?;
            
            // Parse alternative index (e.g., "@0", "@1")
            let alt_str = sexpr_atom(&items[1])?;
            let alternative_index = if alt_str.starts_with('@') {
                alt_str[1..].parse::<usize>()
                    .map_err(|_| format!("Invalid alternative index: {}", alt_str))?
            } else {
                return Err(format!("Expected alternative index (e.g., @0), got: {}", alt_str));
            };
            
            // Look up the production in grammar
            let productions = grammar.productions.get(&name)
                .ok_or_else(|| format!("Unknown nonterminal: {}", name))?;
            
            if alternative_index >= productions.len() {
                return Err(format!(
                    "Alternative index {} out of bounds for nonterminal {} (has {} alternatives)",
                    alternative_index, name, productions.len()
                ));
            }
            
            let production = productions[alternative_index].clone();
            
            let mut binding: Option<String> = None;
            let mut consumed_segments: Option<usize> = None;
            let mut children: Vec<Node> = Vec::new();
            
            // Parse remaining items (metadata and children)
            for item in &items[2..] {
                match item {
                    SExpr::Atom(s) if s.starts_with('$') => {
                        // Binding: $name
                        binding = Some(s[1..].to_string());
                    }
                    SExpr::Atom(s) if s.starts_with('#') => {
                        // Consumed segments: #N
                        let segs_str = &s[1..];
                        consumed_segments = Some(segs_str.parse::<usize>()
                            .map_err(|_| format!("Invalid consumed_segments value: #{}", segs_str))?);
                    }
                    other => {
                        children.push(sexpr_to_node(other, grammar)?);
                    }
                }
            }
            
            // Use provided consumed_segments or fall back to children.len()
            let consumed_segments = consumed_segments.unwrap_or_else(|| children.len());
            
            Ok(NonTerminal::new(
                name,
                production,
                alternative_index,
                children,
                binding,
                consumed_segments,
            ))
        }
        _ => Err("Invalid S-expression for nonterminal; expected (Name @alt ...)".into()),
    }
}

pub fn sexpr_to_node(sexpr: &SExpr, grammar: &Grammar) -> Result<Node, String> {
    match sexpr {
        SExpr::List(items) if !items.is_empty() => {
            match &items[0] {
                SExpr::Atom(tag) if tag == "T" => {
                    Ok(Node::Terminal(sexpr_to_terminal(sexpr, false)?))
                }
                SExpr::Atom(tag) if tag == "T~" => {
                    Ok(Node::Terminal(sexpr_to_terminal(sexpr, true)?))
                }
                SExpr::Atom(_) => {
                    // Assume it's a nonterminal
                    Ok(Node::NonTerminal(sexpr_to_nt(sexpr, grammar)?))
                }
                _ => Err("Invalid node S-expression".into()),
            }
        }
        _ => Err("Expected list for node".into()),
    }
}

pub fn sexpr_to_terminal(sexpr: &SExpr, is_partial: bool) -> Result<Terminal, String> {
    match sexpr {
        SExpr::List(items) if items.len() >= 2 => {
            let value = sexpr_atom_or_str(&items[1])?;
            let mut binding: Option<String> = None;
            let mut extension: Option<DerivativeRegex> = None;
            let mut remainder: Option<DerivativeRegex> = None;
            
            // Parse metadata
            let mut i = 2;
            while i < items.len() {
                match &items[i] {
                    SExpr::Atom(s) if s.starts_with('$') => {
                        // Binding: $name
                        binding = Some(s[1..].to_string());
                        i += 1;
                    }
                    SExpr::Atom(s) if s == "+" => {
                        // Extension: + followed by pattern string
                        i += 1;
                        if i < items.len() {
                            let pattern = sexpr_atom_or_str(&items[i])?;
                            extension = Some(DerivativeRegex::new(&pattern)
                                .map_err(|e| format!("Failed to parse extension regex: {:?}", e))?);
                            i += 1;
                        } else {
                            return Err("Expected pattern after '+'".into());
                        }
                    }
                    SExpr::Atom(s) if s == "~" => {
                        // Remainder: ~ followed by pattern string
                        i += 1;
                        if i < items.len() {
                            let pattern = sexpr_atom_or_str(&items[i])?;
                            remainder = Some(DerivativeRegex::new(&pattern)
                                .map_err(|e| format!("Failed to parse remainder regex: {:?}", e))?);
                            i += 1;
                        } else {
                            return Err("Expected pattern after '~'".into());
                        }
                    }
                    SExpr::Atom(s) if s.starts_with('+') => {
                        // Legacy format: +pattern in one atom
                        let pattern = &s[1..];
                        extension = Some(DerivativeRegex::new(pattern)
                            .map_err(|e| format!("Failed to parse extension regex: {:?}", e))?);
                        i += 1;
                    }
                    SExpr::Atom(s) if s.starts_with('~') => {
                        // Legacy format: ~pattern in one atom
                        let pattern = &s[1..];
                        remainder = Some(DerivativeRegex::new(pattern)
                            .map_err(|e| format!("Failed to parse remainder regex: {:?}", e))?);
                        i += 1;
                    }
                    _ => {
                        i += 1;
                    }
                }
            }
            
            if is_partial {
                Ok(Terminal::Partial {
                    value,
                    binding,
                    remainder,
                })
            } else {
                Ok(Terminal::Complete {
                    value,
                    binding,
                    extension,
                })
            }
        }
        _ => Err("Invalid terminal S-expression; expected (T \"value\" ...) or (T~ \"value\" ...)".into()),
    }
}

pub fn sexpr_atom(s: &SExpr) -> Result<String, String> {
    match s {
        SExpr::Atom(a) => Ok(a.clone()),
        _ => Err("expected atom".into()),
    }
}

pub fn sexpr_atom_or_str(s: &SExpr) -> Result<String, String> {
    match s {
        SExpr::Atom(a) => Ok(a.clone()),
        SExpr::Str(t) => Ok(t.clone()),
        _ => Err("expected atom or string".into()),
    }
}

// ---------------- Convenient impl blocks for types ----------------

impl PartialAST {
    /// Serialize this PartialAST to a string
    pub fn serialize(&self) -> String {
        let sexpr = ast_to_sexpr(self);
        sexpr_to_string(&sexpr)
    }

    /// Deserialize a PartialAST from a string
    pub fn deserialize(s: &str, grammar: &Grammar, input: String) -> Result<Self, String> {
        let sexpr = parse_sexpr(&strip_headers(s))?;
        sexpr_to_ast(&sexpr, grammar, input)
    }
}

impl NonTerminal {
    /// Serialize this NonTerminal to a string
    pub fn serialize(&self) -> String {
        let sexpr = nt_to_sexpr(self);
        sexpr_to_string(&sexpr)
    }

    /// Deserialize a NonTerminal from a string
    pub fn deserialize(s: &str, grammar: &Grammar) -> Result<Self, String> {
        let sexpr = parse_sexpr(&strip_headers(s))?;
        sexpr_to_nt(&sexpr, grammar)
    }
}

impl Terminal {
    /// Serialize this Terminal to a string
    pub fn serialize(&self) -> String {
        let sexpr = terminal_to_sexpr(self);
        sexpr_to_string(&sexpr)
    }

    /// Deserialize a Terminal from a string
    pub fn deserialize(s: &str) -> Result<Self, String> {
        let sexpr = parse_sexpr(&strip_headers(s))?;
        let is_partial = if let SExpr::List(items) = &sexpr {
            if let Some(SExpr::Atom(tag)) = items.first() {
                tag == "T~"
            } else {
                false
            }
        } else {
            false
        };
        sexpr_to_terminal(&sexpr, is_partial)
    }
}

impl Node {
    /// Serialize this Node to a string
    pub fn serialize(&self) -> String {
        let sexpr = node_to_sexpr(self);
        sexpr_to_string(&sexpr)
    }

    /// Deserialize a Node from a string
    pub fn deserialize(s: &str, grammar: &Grammar) -> Result<Self, String> {
        let sexpr = parse_sexpr(&strip_headers(s))?;
        sexpr_to_node(&sexpr, grammar)
    }
    
    /// Serialize this Node without derivative information (extensions/remainders).
    /// This is useful for structural comparison that ignores derivative data.
    pub fn serialize_structure(&self) -> String {
        match self {
            Node::NonTerminal(nt) => nt.serialize_structure(),
            Node::Terminal(Terminal::Complete { value, binding, .. }) => {
                format!("(T {:?} {:?})", value, binding)
            }
            Node::Terminal(Terminal::Partial { value, binding, .. }) => {
                format!("(T~ {:?} {:?})", value, binding)
            }
        }
    }
}

// ---------------- Structure-only serialization ----------------

impl NonTerminal {
    /// Serialize a NonTerminal without derivative information (extensions/remainders).
    /// This is useful for structural comparison purposes.
    /// 
    /// The format omits extension and remainder regex information but includes
    /// everything else: name, alternative index, binding, consumed segments, and children.
    pub fn serialize_structure(&self) -> String {
        let children: Vec<String> = self.children.iter()
            .map(|node| node.serialize_structure())
            .collect();
        
        format!(
            "({} @{} {:?} #{}{})",
            self.name,
            self.alternative_index,
            self.binding,
            self.consumed_segments,
            if children.is_empty() {
                String::new()
            } else {
                format!(" {}", children.join(" "))
            }
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_roundtrip_simple() {
        let grammar_str = r#"
        start ::= "hello"
        "#;
        
        let _grammar = Grammar::load(grammar_str).unwrap();
        
        // Create a simple complete terminal
        let terminal = Terminal::Complete {
            value: "hello".to_string(),
            binding: None,
            extension: None,
        };
        
        let sexpr = terminal_to_sexpr(&terminal);
        let roundtrip = sexpr_to_terminal(&sexpr, false).unwrap();
        
        assert_eq!(terminal, roundtrip);
    }

    #[test]
    fn test_nt_serialization() {
        let grammar_str = r#"
        start ::= "hello" "world"
        "#;
        
        let grammar = Grammar::load(grammar_str).unwrap();
        let production = grammar.productions.get("start").unwrap()[0].clone();
        
        let nt = NonTerminal::new(
            "start".to_string(),
            production,
            0,
            vec![
                Node::Terminal(Terminal::Complete {
                    value: "hello".to_string(),
                    binding: None,
                    extension: None,
                }),
                Node::Terminal(Terminal::Complete {
                    value: "world".to_string(),
                    binding: None,
                    extension: None,
                }),
            ],
            None,
            2,
        );
        
        let sexpr = nt_to_sexpr(&nt);
        let roundtrip = sexpr_to_nt(&sexpr, &grammar).unwrap();
        
        assert_eq!(nt.name, roundtrip.name);
        assert_eq!(nt.alternative_index, roundtrip.alternative_index);
        assert_eq!(nt.children.len(), roundtrip.children.len());
    }

    #[test]
    fn test_partial_ast_serialization() {
        let grammar_str = r#"
        start ::= "hello"
        "#;
        
        let grammar = Grammar::load(grammar_str).unwrap();
        let production = grammar.productions.get("start").unwrap()[0].clone();
        
        let root = NonTerminal::new(
            "start".to_string(),
            production,
            0,
            vec![
                Node::Terminal(Terminal::Partial {
                    value: "hel".to_string(),
                    binding: None,
                    remainder: None,
                }),
            ],
            None,
            1,
        );
        
        let ast = PartialAST::new(vec![root], "hel".to_string());
        let sexpr = ast_to_sexpr(&ast);
        let s = sexpr_to_string(&sexpr);
        
        println!("Serialized:\n{}", s);
        
        // Parse it back
        let parsed_sexpr = parse_sexpr(&s).unwrap();
        let roundtrip = sexpr_to_ast(&parsed_sexpr, &grammar, "hel".to_string()).unwrap();
        
        assert_eq!(ast.roots.len(), roundtrip.roots.len());
    }

    #[test]
    fn test_load_complete_terminal() {
        let grammar_str = r#"
        start ::= "hello"
        "#;
        
        let _grammar = Grammar::load(grammar_str).unwrap();
        
        let s = r#"(T "hello")"#;
        let sexpr = parse_sexpr(s).unwrap();
        let terminal = sexpr_to_terminal(&sexpr, false).unwrap();
        
        match terminal {
            Terminal::Complete { value, binding, extension } => {
                assert_eq!(value, "hello");
                assert_eq!(binding, None);
                assert_eq!(extension, None);
            }
            _ => panic!("Expected complete terminal"),
        }
    }

    #[test]
    fn test_load_partial_terminal() {
        let grammar_str = r#"
        start ::= "hello"
        "#;
        
        let _grammar = Grammar::load(grammar_str).unwrap();
        
        let s = r#"(T~ "hel")"#;
        let sexpr = parse_sexpr(s).unwrap();
        let terminal = sexpr_to_terminal(&sexpr, true).unwrap();
        
        match terminal {
            Terminal::Partial { value, binding, remainder } => {
                assert_eq!(value, "hel");
                assert_eq!(binding, None);
                assert_eq!(remainder, None);
            }
            _ => panic!("Expected partial terminal"),
        }
    }

    #[test]
    fn test_load_terminal_with_binding() {
        let grammar_str = r#"
        start ::= "hello"
        "#;
        
        let _grammar = Grammar::load(grammar_str).unwrap();
        
        let s = r#"(T "hello" $greeting)"#;
        let sexpr = parse_sexpr(s).unwrap();
        let terminal = sexpr_to_terminal(&sexpr, false).unwrap();
        
        match terminal {
            Terminal::Complete { value, binding, .. } => {
                assert_eq!(value, "hello");
                assert_eq!(binding, Some("greeting".to_string()));
            }
            _ => panic!("Expected complete terminal"),
        }
    }

    #[test]
    fn test_load_nonterminal() {
        let grammar_str = r#"
        start ::= "hello" "world"
        "#;
        
        let grammar = Grammar::load(grammar_str).unwrap();
        
        let s = r#"(start @0 (T "hello") (T "world"))"#;
        let sexpr = parse_sexpr(s).unwrap();
        let nt = sexpr_to_nt(&sexpr, &grammar).unwrap();
        
        assert_eq!(nt.name, "start");
        assert_eq!(nt.alternative_index, 0);
        assert_eq!(nt.children.len(), 2);
        assert!(nt.is_complete());
    }

    #[test]
    fn test_load_partial_nonterminal() {
        let grammar_str = r#"
        start ::= "hello" "world"
        "#;
        
        let grammar = Grammar::load(grammar_str).unwrap();
        
        let s = r#"(start @0 (T "hello") (T~ "wor"))"#;
        let sexpr = parse_sexpr(s).unwrap();
        let nt = sexpr_to_nt(&sexpr, &grammar).unwrap();
        
        assert_eq!(nt.name, "start");
        assert_eq!(nt.alternative_index, 0);
        assert_eq!(nt.children.len(), 2);
        assert!(!nt.is_complete());
    }

    #[test]
    fn test_load_nested_tree() {
        let grammar_str = r#"
        start ::= expr
        expr ::= "x" "+" "y"
        "#;
        
        let grammar = Grammar::load(grammar_str).unwrap();
        
        let s = r#"
        (start @0
          (expr @0
            (T "x")
            (T "+")
            (T "y")))
        "#;
        
        let sexpr = parse_sexpr(s).unwrap();
        let nt = sexpr_to_nt(&sexpr, &grammar).unwrap();
        
        assert_eq!(nt.name, "start");
        assert_eq!(nt.children.len(), 1);
        assert!(nt.is_complete());
        
        // Check nested expression
        if let Node::NonTerminal(expr) = &nt.children[0] {
            assert_eq!(expr.name, "expr");
            assert_eq!(expr.children.len(), 3);
        } else {
            panic!("Expected nonterminal child");
        }
    }

    #[test]
    fn test_load_partial_ast_single_root() {
        let grammar_str = r#"
        start ::= "hello"
        "#;
        
        let grammar = Grammar::load(grammar_str).unwrap();
        
        let s = r#"(start @0 (T~ "hel"))"#;
        let sexpr = parse_sexpr(s).unwrap();
        let ast = sexpr_to_ast(&sexpr, &grammar, "hel".to_string()).unwrap();
        
        assert_eq!(ast.roots.len(), 1);
        assert!(!ast.is_complete());
        assert_eq!(ast.input(), "hel");
    }

    #[test]
    fn test_load_partial_ast_multiple_roots() {
        let grammar_str = r#"
        start ::= "hello"
        other ::= "world"
        "#;
        
        let grammar = Grammar::load(grammar_str).unwrap();
        
        let s = r#"
        ((start @0 (T "hello"))
         (other @0 (T "world")))
        "#;
        
        let sexpr = parse_sexpr(s).unwrap();
        let ast = sexpr_to_ast(&sexpr, &grammar, "input".to_string()).unwrap();
        
        assert_eq!(ast.roots.len(), 2);
        assert_eq!(ast.roots[0].name, "start");
        assert_eq!(ast.roots[1].name, "other");
    }

    #[test]
    fn test_load_with_alternative_index() {
        let grammar_str = r#"
        start ::= "hello" | "hi" | "hey"
        "#;
        
        let grammar = Grammar::load(grammar_str).unwrap();
        
        // Test alternative 0
        let s0 = r#"(start @0 (T "hello"))"#;
        let sexpr0 = parse_sexpr(s0).unwrap();
        let nt0 = sexpr_to_nt(&sexpr0, &grammar).unwrap();
        assert_eq!(nt0.alternative_index, 0);
        
        // Test alternative 1
        let s1 = r#"(start @1 (T "hi"))"#;
        let sexpr1 = parse_sexpr(s1).unwrap();
        let nt1 = sexpr_to_nt(&sexpr1, &grammar).unwrap();
        assert_eq!(nt1.alternative_index, 1);
        
        // Test alternative 2
        let s2 = r#"(start @2 (T "hey"))"#;
        let sexpr2 = parse_sexpr(s2).unwrap();
        let nt2 = sexpr_to_nt(&sexpr2, &grammar).unwrap();
        assert_eq!(nt2.alternative_index, 2);
    }

    #[test]
    fn test_roundtrip_complex_tree() {
        let grammar_str = r#"
        start ::= expr
        expr ::= term "+" expr | term
        term ::= "x" | "y" | "(" expr ")"
        "#;
        
        let grammar = Grammar::load(grammar_str).unwrap();
        
        // Create a complex tree: (x + y)
        let production_start = grammar.productions.get("start").unwrap()[0].clone();
        let production_expr = grammar.productions.get("expr").unwrap()[0].clone();
        let production_term_x = grammar.productions.get("term").unwrap()[0].clone();
        let production_term_y = grammar.productions.get("term").unwrap()[1].clone();
        
        let term_x = NonTerminal::new(
            "term".to_string(),
            production_term_x,
            0,
            vec![Node::Terminal(Terminal::Complete {
                value: "x".to_string(),
                binding: None,
                extension: None,
            })],
            None,
            1,
        );
        
        let term_y = NonTerminal::new(
            "term".to_string(),
            production_term_y,
            1,
            vec![Node::Terminal(Terminal::Complete {
                value: "y".to_string(),
                binding: None,
                extension: None,
            })],
            None,
            1,
        );
        
        let expr_inner = NonTerminal::new(
            "expr".to_string(),
            production_expr.clone(),
            0,
            vec![
                Node::NonTerminal(term_x),
                Node::Terminal(Terminal::Complete {
                    value: "+".to_string(),
                    binding: None,
                    extension: None,
                }),
                Node::NonTerminal(term_y),
            ],
            None,
            3,
        );
        
        let start = NonTerminal::new(
            "start".to_string(),
            production_start,
            0,
            vec![Node::NonTerminal(expr_inner)],
            None,
            1,
        );
        
        let ast = PartialAST::new(vec![start], "x+y".to_string());
        
        // Serialize and deserialize
        let sexpr = ast_to_sexpr(&ast);
        let s = sexpr_to_string(&sexpr);
        
        println!("Complex tree serialized:\n{}", s);
        
        let parsed_sexpr = parse_sexpr(&s).unwrap();
        let roundtrip = sexpr_to_ast(&parsed_sexpr, &grammar, "x+y".to_string()).unwrap();
        
        assert_eq!(ast.roots.len(), roundtrip.roots.len());
        assert_eq!(ast.roots[0].name, roundtrip.roots[0].name);
        assert!(roundtrip.is_complete());
    }

    #[test]
    fn test_consumed_segments_preserved() {
        let grammar_str = r#"
        start ::= "a" "b" "c"
        "#;
        
        let grammar = Grammar::load(grammar_str).unwrap();
        let production = grammar.productions.get("start").unwrap()[0].clone();
        
        // Create a node with consumed_segments = 5 (different from children.len())
        let nt = NonTerminal::new(
            "start".to_string(),
            production,
            0,
            vec![
                Node::Terminal(Terminal::Complete {
                    value: "a".to_string(),
                    binding: None,
                    extension: None,
                }),
                Node::Terminal(Terminal::Complete {
                    value: "b".to_string(),
                    binding: None,
                    extension: None,
                }),
            ],
            None,
            5, // Different from children.len() which is 2
        );
        
        // Serialize
        let sexpr = nt_to_sexpr(&nt);
        let s = sexpr_to_string(&sexpr);
        
        println!("Serialized with consumed_segments=5:\n{}", s);
        
        // Deserialize
        let parsed_sexpr = parse_sexpr(&s).unwrap();
        let roundtrip = sexpr_to_nt(&parsed_sexpr, &grammar).unwrap();
        
        // Verify consumed_segments is preserved
        assert_eq!(nt.consumed_segments, roundtrip.consumed_segments);
        assert_eq!(roundtrip.consumed_segments, 5);
        assert_ne!(roundtrip.consumed_segments, roundtrip.children.len());
    }

    #[test]
    fn test_all_terminal_fields_preserved() {
        let _grammar_str = r#"
        start ::= "hello"
        "#;
        
        // Complete terminal with all fields
        let terminal = Terminal::Complete {
            value: "hello".to_string(),
            binding: Some("greeting".to_string()),
            extension: Some(DerivativeRegex::literal("world")),
        };
        
        let sexpr = terminal_to_sexpr(&terminal);
        let s = sexpr_to_string(&sexpr);
        let parsed_sexpr = parse_sexpr(&s).unwrap();
        let roundtrip = sexpr_to_terminal(&parsed_sexpr, false).unwrap();
        
        assert_eq!(terminal, roundtrip);
        
        // Partial terminal with all fields
        let partial = Terminal::Partial {
            value: "hel".to_string(),
            binding: Some("partial_greeting".to_string()),
            remainder: Some(DerivativeRegex::literal("lo")),
        };
        
        let sexpr2 = terminal_to_sexpr(&partial);
        let s2 = sexpr_to_string(&sexpr2);
        let parsed_sexpr2 = parse_sexpr(&s2).unwrap();
        let roundtrip2 = sexpr_to_terminal(&parsed_sexpr2, true).unwrap();
        
        assert_eq!(partial, roundtrip2);
    }

    #[test]
    fn test_complete_roundtrip_no_information_loss() {
        // This test verifies that EVERY field is preserved through serialization
        let grammar_str = r#"
        start ::= expr
        expr ::= term "+" expr | term
        term ::= "x" | "y"
        "#;
        
        let grammar = Grammar::load(grammar_str).unwrap();
        
        let production_start = grammar.productions.get("start").unwrap()[0].clone();
        let production_expr = grammar.productions.get("expr").unwrap()[0].clone();
        let production_term = grammar.productions.get("term").unwrap()[0].clone();
        
        // Build a tree with all possible field variations
        let term = NonTerminal::new(
            "term".to_string(),
            production_term,
            0,
            vec![Node::Terminal(Terminal::Complete {
                value: "x".to_string(),
                binding: Some("var".to_string()),
                extension: Some(DerivativeRegex::literal("_suffix")),
            })],
            Some("bound_term".to_string()),
            3, // Intentionally different from children.len()
        );
        
        let partial_term = NonTerminal::new(
            "term".to_string(),
            grammar.productions.get("term").unwrap()[1].clone(),
            1,
            vec![Node::Terminal(Terminal::Partial {
                value: "y".to_string(),
                binding: Some("partial_var".to_string()),
                remainder: Some(DerivativeRegex::literal("_rest")),
            })],
            None,
            2,
        );
        
        let expr = NonTerminal::new(
            "expr".to_string(),
            production_expr,
            0,
            vec![
                Node::NonTerminal(term),
                Node::Terminal(Terminal::Complete {
                    value: "+".to_string(),
                    binding: None,
                    extension: None,
                }),
                Node::NonTerminal(partial_term),
            ],
            Some("bound_expr".to_string()),
            7,
        );
        
        let start = NonTerminal::new(
            "start".to_string(),
            production_start,
            0,
            vec![Node::NonTerminal(expr)],
            None,
            10,
        );
        
        let ast = PartialAST::new(vec![start.clone()], "x+y".to_string());
        
        // Serialize and deserialize
        let sexpr = ast_to_sexpr(&ast);
        let s = sexpr_to_string(&sexpr);
        
        println!("\n=== Full serialization ===\n{}\n", s);
        
        let parsed_sexpr = parse_sexpr(&s).unwrap();
        let roundtrip = sexpr_to_ast(&parsed_sexpr, &grammar, "x+y".to_string()).unwrap();
        
        // Verify ALL fields match
        assert_eq!(ast.roots.len(), roundtrip.roots.len());
        assert_eq!(ast.input(), roundtrip.input());
        
        // Check root
        let original_root = &ast.roots[0];
        let roundtrip_root = &roundtrip.roots[0];
        assert_eq!(original_root.name, roundtrip_root.name);
        assert_eq!(original_root.alternative_index, roundtrip_root.alternative_index);
        assert_eq!(original_root.binding, roundtrip_root.binding);
        assert_eq!(original_root.consumed_segments, roundtrip_root.consumed_segments);
        assert_eq!(original_root.children.len(), roundtrip_root.children.len());
        
        // Check nested expr
        if let (Node::NonTerminal(orig_expr), Node::NonTerminal(rt_expr)) = 
            (&original_root.children[0], &roundtrip_root.children[0]) {
            assert_eq!(orig_expr.name, rt_expr.name);
            assert_eq!(orig_expr.alternative_index, rt_expr.alternative_index);
            assert_eq!(orig_expr.binding, rt_expr.binding);
            assert_eq!(orig_expr.consumed_segments, rt_expr.consumed_segments);
            
            // Check term with binding and extension
            if let (Node::NonTerminal(orig_term), Node::NonTerminal(rt_term)) = 
                (&orig_expr.children[0], &rt_expr.children[0]) {
                assert_eq!(orig_term.consumed_segments, rt_term.consumed_segments);
                assert_eq!(orig_term.binding, rt_term.binding);
                
                if let (Node::Terminal(orig_t), Node::Terminal(rt_t)) = 
                    (&orig_term.children[0], &rt_term.children[0]) {
                    assert_eq!(orig_t, rt_t);
                }
            }
            
            // Check partial term
            if let (Node::NonTerminal(orig_pt), Node::NonTerminal(rt_pt)) = 
                (&orig_expr.children[2], &rt_expr.children[2]) {
                assert_eq!(orig_pt.consumed_segments, rt_pt.consumed_segments);
                assert_eq!(orig_pt.alternative_index, rt_pt.alternative_index);
                
                if let (Node::Terminal(orig_t), Node::Terminal(rt_t)) = 
                    (&orig_pt.children[0], &rt_pt.children[0]) {
                    assert_eq!(orig_t, rt_t);
                }
            }
        }
        
        println!("✓ All fields preserved through serialization!");
    }

    #[test]
    fn test_partial_ast_serialize_method() {
        let grammar_str = r#"
        start ::= "hello" "world"
        "#;
        
        let grammar = Grammar::load(grammar_str).unwrap();
        let production = grammar.productions.get("start").unwrap()[0].clone();
        
        let root = NonTerminal::new(
            "start".to_string(),
            production,
            0,
            vec![
                Node::Terminal(Terminal::Complete {
                    value: "hello".to_string(),
                    binding: None,
                    extension: None,
                }),
                Node::Terminal(Terminal::Partial {
                    value: "wor".to_string(),
                    binding: None,
                    remainder: None,
                }),
            ],
            None,
            2,
        );
        
        let ast = PartialAST::new(vec![root], "hellowor".to_string());
        
        // Use the convenient serialize method
        let serialized = ast.serialize();
        println!("Serialized using .serialize():\n{}", serialized);
        
        // Deserialize using the convenient method
        let deserialized = PartialAST::deserialize(&serialized, &grammar, "hellowor".to_string()).unwrap();
        
        assert_eq!(ast.roots.len(), deserialized.roots.len());
        assert_eq!(ast.input(), deserialized.input());
    }

    #[test]
    fn test_nonterminal_serialize_method() {
        let grammar_str = r#"
        expr ::= "x" "+" "y"
        "#;
        
        let grammar = Grammar::load(grammar_str).unwrap();
        let production = grammar.productions.get("expr").unwrap()[0].clone();
        
        let nt = NonTerminal::new(
            "expr".to_string(),
            production,
            0,
            vec![
                Node::Terminal(Terminal::Complete {
                    value: "x".to_string(),
                    binding: Some("left".to_string()),
                    extension: None,
                }),
                Node::Terminal(Terminal::Complete {
                    value: "+".to_string(),
                    binding: None,
                    extension: None,
                }),
                Node::Terminal(Terminal::Complete {
                    value: "y".to_string(),
                    binding: Some("right".to_string()),
                    extension: None,
                }),
            ],
            Some("expr_binding".to_string()),
            3,
        );
        
        // Use the convenient serialize method
        let serialized = nt.serialize();
        println!("NonTerminal serialized:\n{}", serialized);
        
        // Deserialize
        let deserialized = NonTerminal::deserialize(&serialized, &grammar).unwrap();
        
        assert_eq!(nt.name, deserialized.name);
        assert_eq!(nt.binding, deserialized.binding);
        assert_eq!(nt.consumed_segments, deserialized.consumed_segments);
        assert_eq!(nt.children.len(), deserialized.children.len());
    }

    #[test]
    fn test_terminal_serialize_method() {
        // Complete terminal
        let complete = Terminal::Complete {
            value: "test".to_string(),
            binding: Some("var".to_string()),
            extension: Some(DerivativeRegex::literal("_ext")),
        };
        
        let serialized = complete.serialize();
        println!("Complete terminal:\n{}", serialized);
        
        let deserialized = Terminal::deserialize(&serialized).unwrap();
        assert_eq!(complete, deserialized);
        
        // Partial terminal
        let partial = Terminal::Partial {
            value: "tes".to_string(),
            binding: Some("partial_var".to_string()),
            remainder: Some(DerivativeRegex::literal("t")),
        };
        
        let serialized2 = partial.serialize();
        println!("Partial terminal:\n{}", serialized2);
        
        let deserialized2 = Terminal::deserialize(&serialized2).unwrap();
        assert_eq!(partial, deserialized2);
    }

    #[test]
    fn test_node_serialize_method() {
        let grammar_str = r#"
        term ::= "value"
        "#;
        
        let grammar = Grammar::load(grammar_str).unwrap();
        
        // Terminal node
        let term_node = Node::Terminal(Terminal::Complete {
            value: "value".to_string(),
            binding: None,
            extension: None,
        });
        
        let serialized = term_node.serialize();
        let deserialized = Node::deserialize(&serialized, &grammar).unwrap();
        
        match (&term_node, &deserialized) {
            (Node::Terminal(t1), Node::Terminal(t2)) => assert_eq!(t1, t2),
            _ => panic!("Expected terminal nodes"),
        }
        
        // NonTerminal node
        let production = grammar.productions.get("term").unwrap()[0].clone();
        let nt_node = Node::NonTerminal(NonTerminal::new(
            "term".to_string(),
            production,
            0,
            vec![Node::Terminal(Terminal::Complete {
                value: "value".to_string(),
                binding: None,
                extension: None,
            })],
            None,
            1,
        ));
        
        let serialized2 = nt_node.serialize();
        let deserialized2 = Node::deserialize(&serialized2, &grammar).unwrap();
        
        match (&nt_node, &deserialized2) {
            (Node::NonTerminal(nt1), Node::NonTerminal(nt2)) => {
                assert_eq!(nt1.name, nt2.name);
                assert_eq!(nt1.consumed_segments, nt2.consumed_segments);
            }
            _ => panic!("Expected nonterminal nodes"),
        }
    }

    #[test]
    fn test_serialize_with_comments() {
        let grammar_str = r#"
        start ::= "hello"
        "#;
        
        let grammar = Grammar::load(grammar_str).unwrap();
        
        // Test that strip_headers works with comments
        let s_with_comments = r#"
; This is a comment
; Another comment

(start @0 #1
  (T "hello"))
        "#;
        
        let ast = PartialAST::deserialize(s_with_comments, &grammar, "hello".to_string()).unwrap();
        assert_eq!(ast.roots.len(), 1);
        assert_eq!(ast.roots[0].name, "start");
    }

    #[test]
    fn test_new_symbol_format() {
        let grammar_str = r#"
        start ::= name | value
        name ::= /[a-z]+/
        value ::= /[0-9]+/
        "#;
        
        let grammar = Grammar::load(grammar_str).unwrap();
        
        // Test with all symbol features:
        // @ = alternative index
        // $ = binding
        // # = consumed segments
        // + = extension (for complete terminals)
        // ~ = remainder (for partial terminals)
        
        let s = r#"(start @1 $result #2
  (T "123" $num +[0-9]+)
  (T~ "45" $partial ~[0-9]+))"#;
        
        let parsed = parse_sexpr(s).unwrap();
        let ast = sexpr_to_ast(&parsed, &grammar, "12345".to_string()).unwrap();
        
        assert_eq!(ast.roots.len(), 1);
        let root = &ast.roots[0];
        assert_eq!(root.name, "start");
        assert_eq!(root.alternative_index, 1);
        assert_eq!(root.binding, Some("result".to_string()));
        assert_eq!(root.consumed_segments, 2);
        
        // Check first child (complete terminal)
        match &root.children[0] {
            Node::Terminal(Terminal::Complete { value, binding, extension }) => {
                assert_eq!(value, "123");
                assert_eq!(binding, &Some("num".to_string()));
                assert!(extension.is_some());
            }
            _ => panic!("Expected complete terminal"),
        }
        
        // Check second child (partial terminal)
        match &root.children[1] {
            Node::Terminal(Terminal::Partial { value, binding, remainder }) => {
                assert_eq!(value, "45");
                assert_eq!(binding, &Some("partial".to_string()));
                assert!(remainder.is_some());
            }
            _ => panic!("Expected partial terminal"),
        }
    }
}

