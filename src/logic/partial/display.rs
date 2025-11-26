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

// ============================================================================
// TypedAST Display (IDE-style type annotations)
// ============================================================================

impl super::TypedNode {
    fn fmt_tree(&self, f: &mut fmt::Formatter<'_>, prefix: &str, is_last: bool) -> fmt::Result {
        use crate::logic::typing::Type;
        let branch = if is_last { "└─ " } else { "├─ " };
        let ty_str = match self.ty() {
            Type::Universe => String::new(),
            t => format!(" : {}", t),
        };
        match self {
            Self::Term { val, .. } => writeln!(f, "{}{}{}{}", prefix, branch, val, ty_str),
            Self::Expr { name, children, .. } => {
                writeln!(f, "{}{}{}{}", prefix, branch, name, ty_str)?;
                let child_prefix = format!("{}{}", prefix, if is_last { "   " } else { "│  " });
                for (i, child) in children.iter().enumerate() {
                    child.fmt_tree(f, &child_prefix, i == children.len() - 1)?;
                }
                Ok(())
            }
        }
    }
}

impl Display for super::TypedNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { self.fmt_tree(f, "", true) }
}

impl Display for super::TypedAST {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Input: \"{}\"", self.input)?;
        for (i, root) in self.roots.iter().enumerate() {
            writeln!(f, "\nTree {}:", i)?;
            write!(f, "{}", root)?;
        }
        Ok(())
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
