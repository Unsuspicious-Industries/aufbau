use super::Symbol;

/// A single production rule `left ::= right₀ right₁ …`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Production {
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
        let symbols: Vec<String> = self.rhs.iter().map(fmt_symbol).collect();
        write!(f, "{}", symbols.join(" "))
    }
}

// make the production directly indexable by rhs indice
impl std::ops::Index<usize> for Production {
    type Output = Symbol;

    fn index(&self, index: usize) -> &Self::Output {
        &self.rhs[index]
    }
}

// .len() for production
impl Production {
    #[must_use]
    pub fn len(&self) -> usize {
        self.rhs.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.rhs.is_empty()
    }
}
