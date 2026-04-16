use super::Symbol;

/// A single production rule `left ::= right₀ right₁ …`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Production {
    pub rule: Option<String>,
    pub rhs: Vec<Symbol>,
}

fn fmt_symbol(s: &Symbol) -> String {
    match s {
        Symbol::Nonterminal { name, .. } => name.clone(),
        Symbol::Terminal { regex, .. } => format!("/{}/", regex.to_pattern()),
    }
}

impl std::fmt::Display for Production {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let symbols: Vec<String> = self.rhs.iter().map(|s| fmt_symbol(s)).collect();
        write!(f, "{}", symbols.join(" "))
    }
}
