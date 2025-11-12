use std::fmt::{self, Display};

// Helper functions for indented display

fn format_nonterminal(nt: &super::NonTerminal, indent: usize) -> String {
    let mut result = format!("{}[{} alts]:\n", nt.name, nt.alts.len());
    for alt in &nt.alts {
        result.push_str(&format_alt(alt, indent + 1));
    }
    result
}

fn format_alt(alt: &super::Alt, indent: usize) -> String {
    let mut result = String::new();
    
    // Display type rule info
    if let Some(rule) = &alt.production.rule {
        result.push_str(&format!("[@{}] ", rule));
    }
    
    // Get cursor position
    let cursor = alt.cursor();
    let total_symbols = alt.production.rhs.len();
    
    // Display all slots in order
    let mut indices: Vec<_> = alt.slots.keys().cloned().collect();
    indices.sort_unstable();
    
    let mut slot_displays = Vec::new();
    for idx in &indices {
        if let Some(slot) = alt.slots.get(idx) {
            let formatted = format_slot_detailed(slot, *idx, indent);
            slot_displays.push(formatted);
        }
    }
    
    // Join parts
    if !slot_displays.is_empty() {
        result.push_str(&slot_displays.join(" "));
    }
    
    // Add status indicators
    if alt.is_complete() {
        result.push_str(" ✓");
    } else if alt.is_progressing() {
        result.push_str(&format!(" [progress:{}/{}]", cursor, total_symbols));
    } else {
        result.push_str(" [stopped]");
    }
    
    // Show span if present
    if let Some(span) = &alt.span {
        result.push_str(&format!(" (span:{}..{})", span.start, span.end));
    }
    
    result
}

fn format_slot(slot: &super::Slot, indent: usize) -> String {
    format_slot_detailed(slot, 0, indent)
}

fn format_slot_detailed(slot: &super::Slot, slot_index: usize, indent: usize) -> String {
    match slot {
        super::Slot::Filled { nodes, .. } => {
            if nodes.is_empty() {
                format!("[{}:empty]", slot_index)
            } else if nodes.len() == 1 {
                format!("[{}:{}]", slot_index, format_node(&nodes[0], indent))
            } else {
                let inner: Vec<_> = nodes.iter().map(|n| format_node(n, indent)).collect();
                format!("[{}:[{}]]", slot_index, inner.join(" "))
            }
        }
        super::Slot::Partial { node, partial_symbol, .. } => {
            if let Some(n) = node {
                // If we have a node, show what's completed and what's pending
                format!("[{}:{}+partial:{}]", slot_index, format_node(n, indent), partial_symbol)
            } else {
                // No node yet, show what we're waiting for
                format!("[{}:waiting:{}]", slot_index, partial_symbol)
            }
        }
        super::Slot::Group { iterations, partial_iteration, .. } => {
            if iterations.is_empty() && partial_iteration.is_none() {
                format!("[{}:group:empty]", slot_index)
            } else {
                let mut parts = Vec::new();
                for (i, iter) in iterations.iter().enumerate() {
                    let slots: Vec<_> = iter.iter().map(|s| format_slot(s, indent)).collect();
                    parts.push(format!("iter{}:[{}]", i, slots.join(" ")));
                }
                if let Some(partial) = partial_iteration {
                    let slots: Vec<_> = partial.iter().map(|s| format_slot(s, indent)).collect();
                    parts.push(format!("partial:[{}]", slots.join(" ")));
                }
                format!("[slot{}:group:[{}]]", slot_index, parts.join(", "))
            }
        }
    }
}

fn format_node(node: &super::ParsedNode, indent: usize) -> String {
    match node {
        super::ParsedNode::Terminal(t) => t.to_string(),
        super::ParsedNode::NonTerminal(nt) => {
            // For nested nonterminals, format with proper indentation
            let mut result = String::new();
            result.push('\n');
            result.push_str(&" ".repeat(indent));
            result.push_str(&nt.name);
            if !nt.alts.is_empty() {
                result.push_str(&format!("[{} alts]:", nt.alts.len()));
                for alt in &nt.alts {
                    result.push('\n');
                    result.push_str(&" ".repeat(indent + 1));
                    result.push_str(&format_alt(alt, indent + 1));
                }
            }
            result
        }
    }
}

impl Display for super::PartialAST {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", format_nonterminal(&self.root, 0))
    }
}

impl Display for super::NonTerminal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", format_nonterminal(self, 0))
    }
}

impl Display for super::Terminal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\"{}\"", self.value)?;
        if self.extension.is_some() {
            write!(f, "<{}>", self.extension.as_ref().unwrap().to_pattern())?;
        }
        Ok(())
    }
}

// Simple Display implementations for use in debug/trace output
impl Display for super::ParsedNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            super::ParsedNode::Terminal(t) => write!(f, "{}", t),
            super::ParsedNode::NonTerminal(nt) => write!(f, "{}", nt.name),
        }
    }
}

impl Display for super::Alt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", format_alt(self, 0))
    }
}

impl Display for super::Slot {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", format_slot(self, 0))
    }
}

impl Display for super::production::PartialSymbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            super::production::PartialSymbol::Terminal { derivative, .. } => {
                write!(f, "{}", derivative.to_pattern())
            }
            super::production::PartialSymbol::NonTerminal { nt, .. } => {
                write!(f, "{}", nt)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::logic::grammar::Grammar;
    use crate::logic::partial::Parser;

    #[test]
    fn test_display_simple_complete() {
        let spec = r#"
        start ::= 'hello'
        "#;
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g);
        
        let ast = p.partial("hello").unwrap();
        let display = format!("{}", ast);
        
        println!("\n=== Simple Complete ===");
        println!("{}", display);
        
        assert!(display.contains("start"));
        assert!(display.contains("\"hello\""));
        assert!(display.contains("✓")); // Should show complete
    }

    #[test]
    fn test_display_partial_literal() {
        let spec = r#"
        start ::= 'hello'
        "#;
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g);
        
        let ast = p.partial("hel").unwrap();
        let display = format!("{}", ast);
        
        println!("\n=== Partial Literal ===");
        println!("{}", display);
        
        assert!(display.contains("start"));
        assert!(display.contains("awaiting") || display.contains("pending")); // Should show what we're waiting for
    }

    #[test]
    fn test_display_alternatives() {
        let spec = r#"
        A ::= 'a' 
        B ::= 'b'
        start ::= A | B
        "#;
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g);
        
        let ast = p.partial("a").unwrap();
        let display = format!("{}", ast);
        
        println!("\n=== Alternatives (matched A) ===");
        println!("{}", display);
        
        assert!(display.contains("start"));
        assert!(display.contains("alts")); // Should show multiple alternatives
    }

    #[test]
    fn test_display_repetition() {
        let spec = r#"
        A ::= 'a'
        start ::= A*
        "#;
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g);
        
        let ast = p.partial("a a a").unwrap();
        let display = format!("{}", ast);
        
        println!("\n=== Repetition (star) ===");
        println!("{}", display);
        
        assert!(display.contains("start"));
        assert!(display.contains("\"a\"")); // Should show repeated elements
    }

    #[test]
    fn test_display_nesting() {
        let spec = r#"
        A ::= 'a' 
        B ::= 'b' A 'r'
        start ::= B 'c'
        "#;
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g);
        
        let ast = p.partial("barc").unwrap();
        let display = format!("{}", ast);
        
        println!("\n=== Nested Nonterminals ===");
        println!("{}", display);
        
        assert!(display.contains("start"));
        assert!(display.contains("B")); // Should show nested nonterminal
        assert!(display.contains("A")); // Should show deeply nested
    }

    #[test]
    fn test_display_partial_nesting() {
        let spec = r#"
        A ::= 'a' 
        B ::= 'b' A 'r'
        start ::= B 'c'
        "#;
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g);
        
        let ast = p.partial("ba").unwrap();
        let display = format!("{}", ast);
        
        println!("\n=== Partial Nested ===");
        println!("{}", display);
        
        assert!(display.contains("start"));
        assert!(display.contains("B"));
    }

    #[test]
    fn test_display_complex_alternatives() {
        let spec = r#"
        U ::= 'barcbarcu'
        A ::= 'a' 
        B ::= 'b' A 'r'
        start ::= U | (B 'c')+ | 't' 
        "#;
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g);
        
        let ast = p.partial("barcbarc").unwrap();
        let display = format!("{}", ast);
        
        println!("\n=== Complex Alternatives ===");
        println!("{}", display);
        
        assert!(display.contains("start"));
        assert!(display.contains("alts")); // Multiple alternatives
    }

    #[test]
    fn test_display_empty_input() {
        let spec = r#"
        A ::= 'a'
        start ::= A*
        "#;
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g);
        
        let ast = p.partial("").unwrap();
        let display = format!("{}", ast);
        
        println!("\n=== Empty Input (valid for star) ===");
        println!("{}", display);
        
        assert!(display.contains("start"));
    }

    #[test]
    fn test_display_partial_in_repetition() {
        let spec = r#"
        A ::= 'x'
        start ::= A+
        "#;
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g);
        
        // Complete multiple As
        let ast = p.partial("x x x").unwrap();
        let display = format!("{}", ast);
        
        println!("\n=== Partial in Repetition ===");
        println!("{}", display);
        
        assert!(display.contains("start"));
        assert!(display.contains("\"x\""));
    }

    #[test]
    fn test_display_with_span() {
        let spec = r#"
        start ::= 'a' 'b' 'c'
        "#;
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g);
        
        let ast = p.partial("a b c").unwrap();
        let display = format!("{}", ast);
        
        println!("\n=== With Span Information ===");
        println!("{}", display);
        
        assert!(display.contains("start"));
        // Spans are shown as (start..end)
        assert!(display.contains("(") && display.contains(")"));
    }

    #[test]
    fn test_display_readability_comprehensive() {
        // Test that creates a moderately complex AST to check for readability
        let spec = r#"
        Expr ::= Term ('+' Term)*
        Term ::= Factor ('*' Factor)*
        Factor ::= '(' Expr ')' | 'x' | 'y'
        start ::= Expr
        "#;
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g);
        
        let ast = p.partial("x + y").unwrap();
        let display = format!("{}", ast);
        
        println!("\n=== Comprehensive Readability Test ===");
        println!("{}", display);
        
        // Check that major elements are visible
        assert!(display.contains("start"));
        assert!(display.contains("Expr"));
        
        // Check that it's not too verbose (rough heuristic)
        let line_count = display.lines().count();
        println!("Line count: {}", line_count);
        assert!(line_count < 100, "Display output too verbose: {} lines", line_count);
        
        // Check that each line is reasonably short (not too cluttered)
        for (i, line) in display.lines().enumerate() {
            assert!(line.len() < 200, "Line {} too long ({} chars): {}", i, line.len(), line);
        }
    }

    #[test]
    fn test_display_all_features() {
        // Test showing all major display features in one example
        let spec = r#"
        U ::= 'completed'
        A ::= 'a' 
        B ::= 'b' A 'r'
        start ::= U | (B 'c')+ | 't' 
        "#;
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g);
        
        let ast = p.partial("b a r c").unwrap();
        let display = format!("{}", ast);
        
        println!("\n=== All Display Features ===");
        println!("{}", display);
        println!("\nFeatures demonstrated:");
        println!("  ✓ Multiple alternatives shown");
        println!("  ✓ Nested nonterminals (B contains A)");
        println!("  ✓ Repetition groups (B 'c')+");
        println!("  ✓ Complete status markers (✓)");
        println!("  ✓ Span information (start..end)");
        println!("  ✓ Hierarchical indentation");
        
        // Verify key elements are present
        assert!(display.contains("start"));
        assert!(display.contains("alts")); // Multiple alternatives
        assert!(display.contains("B")); // Nonterminal
        assert!(display.contains("A")); // Nested nonterminal
        assert!(display.contains("✓")); // Completion marker
        assert!(display.contains("(")); // Span indicators
    }
}
