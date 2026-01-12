use std::fmt::{self, Display};

// Helper functions for compact, *consistent* tree display.

fn indent(level: usize) -> String {
    const INDENT: &str = "  ";
    INDENT.repeat(level)
}

fn format_nonterminal(nt: &super::NonTerminal, level: usize) -> String {
    let mut out = String::new();

    out.push_str(&indent(level));
    out.push_str(&format!("{} [alt {}]", nt.name, nt.alternative_index));

    if nt.is_complete() {
        out.push_str(" ✓");
    } else {
        out.push_str(" (partial)");
    }

    for child in &nt.children {
        out.push('\n');
        out.push_str(&format_node(child, level + 1));
    }

    out
}

fn format_node(node: &super::Node, level: usize) -> String {
    match node {
        super::Node::Terminal(t) => {
            let mut out = indent(level);
            out.push_str(&t.to_string());
            out
        }
        super::Node::NonTerminal(nt) => format_nonterminal(nt, level),
    }
}

impl Display for super::PartialAST {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "PartialAST ({} roots):", self.roots.len())?;
        for (i, root) in self.roots.iter().enumerate() {
            writeln!(f, "Root {}:", i)?;
            write!(f, "{}", format_nonterminal(root, 1))?;
            if i < self.roots.len() - 1 {
                writeln!(f)?;
            }
        }
        Ok(())
    }
}

impl Display for super::NonTerminal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", format_nonterminal(self, 0))
    }
}

impl Display for super::Terminal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            super::Terminal::Complete {
                value, extension, ..
            } => {
                write!(f, "\"{}\"", value)?;
                if let Some(ext) = extension {
                    write!(f, "<{}>", ext.to_pattern())?;
                }
                Ok(())
            }
            super::Terminal::Partial {
                value, remainder, ..
            } => {
                write!(f, "\"{}\"", value)?;
                if let Some(rem) = remainder {
                    write!(f, "~{}", rem.to_pattern())?;
                }
                Ok(())
            }
        }
    }
}

// Simple Display implementations for use in debug/trace output
impl Display for super::Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            super::Node::Terminal(t) => write!(f, "{}", t),
            super::Node::NonTerminal(nt) => write!(f, "{}", nt.name),
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
}
