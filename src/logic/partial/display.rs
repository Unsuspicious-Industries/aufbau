use std::fmt::{self, Display};

impl Display for super::PartialAST {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.root)
    }
}

impl Display for super::NonTerminal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)?;
        if self.alts.len() > 1 {
            write!(f, "[{} alts] : {}", self.alts.len(), self.alts.iter().map(|alt| alt.to_string()).collect::<Vec<_>>().join(", "))?;
        }
        Ok(())
    }
}

impl Display for super::Terminal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\"{}\"", self.value)?;
        if self.extension.is_some() {
            write!(f, "+")?;
        }
        Ok(())
    }
}

impl Display for super::ParsedNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            super::ParsedNode::Terminal(t) => write!(f, "{}", t),
            super::ParsedNode::NonTerminal(nt) => write!(f, "{}", nt),
        }
    }
}

impl Display for super::Alt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Display production rule name if present
        if let Some(rule) = &self.production.rule {
            write!(f, "[@{}] ", rule)?;
        }
        
        // Get cursor position (index of first incomplete symbol)
        let cursor = self.cursor();
        let total_symbols = self.production.rhs.len();
        
        // Display all slots in order
        let mut indices: Vec<_> = self.slots.keys().cloned().collect();
        indices.sort_unstable();
        
        for (i, idx) in indices.iter().enumerate() {
            if i > 0 {
                write!(f, " ")?;
            }
            if let Some(slot) = self.slots.get(idx) {
                write!(f, "{}", slot)?;
            }
        }
        
        // Add status indicators
        if self.is_complete() {
            write!(f, " ✓")?;
        } else if self.is_progressing() {
            // Show cursor position for in-progress alternatives
            write!(f, " ⋯[{}/{}]", cursor, total_symbols)?;
        } else {
            // Not started yet
            write!(f, " ○")?;
        }
        
        // Optionally show span if present
        if let Some(span) = &self.span {
            write!(f, " ({}..{})", span.start, span.end)?;
        }
        
        Ok(())
    }
}

impl Display for super::Slot {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            super::Slot::Filled { nodes, .. } => {
                if nodes.is_empty() {
                    write!(f, "∅")
                } else if nodes.len() == 1 {
                    write!(f, "{}", nodes[0])
                } else {
                    write!(f, "[")?;
                    for (i, node) in nodes.iter().enumerate() {
                        if i > 0 {
                            write!(f, " ")?;
                        }
                        write!(f, "{}", node)?;
                    }
                    write!(f, "]")
                }
            }
            super::Slot::Partial { node, partial_symbol,.. } => {
                if let Some(n) = node {
                    write!(f, "{}… {}", n, partial_symbol)
                } else {
                    write!(f, "⋯")
                }
            }
            super::Slot::Group { iterations, partial_iteration, .. } => {
                if iterations.is_empty() && partial_iteration.is_none() {
                    write!(f, "∅")
                } else {
                    write!(f, "(")?;
                    for (i, iter) in iterations.iter().enumerate() {
                        if i > 0 {
                            write!(f, " ")?;
                        }
                        for (j, slot) in iter.iter().enumerate() {
                            if j > 0 {
                                write!(f, " ")?;
                            }
                            write!(f, "{}", slot)?;
                        }
                    }
                    if let Some(partial) = partial_iteration {
                        if !iterations.is_empty() {
                            write!(f, " ")?;
                        }
                        for (j, slot) in partial.iter().enumerate() {
                            if j > 0 {
                                write!(f, " ")?;
                            }
                            write!(f, "{}", slot)?;
                        }
                        write!(f, "…")?;
                    }
                    write!(f, ")")
                }
            }
        }
    }
}

impl Display for super::production::PartialSymbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            super::production::PartialSymbol::Terminal { re, derivative, .. } => {
                // Show the derivative regex to indicate what's expected
                if re == derivative {
                    write!(f, "⟨{}⟩", re.to_pattern())
                } else {
                    write!(f, "⟨{}⟩", derivative.to_pattern())
                }
            }
            super::production::PartialSymbol::NonTerminal { nt, .. } => {
                write!(f, "⟨{}⟩", nt)
            }
        }
    }
}