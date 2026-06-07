use super::{SPG, Symbol};
use std::path::Path;

impl SPG {
    /// Produce the textual specification string.
    #[must_use]
    pub fn to_spec_string(&self) -> String {
        let mut out = String::new();
        // Preserve original declaration order; fall back to sorted for any missing
        let nt_list: Vec<&String> = self.productions.keys().collect();

        // ---------- Productions ----------
        out.push_str("// --- Productions ---\n");
        for nt in nt_list {
            if let Some(alts) = self.productions.get(nt) {
                let mut first = true;
                for prod in alts {
                    let lhs = if let Some(rule_name) = self.nt_rule(nt) {
                        format!("{nt}({rule_name})")
                    } else {
                        nt.clone()
                    };

                    let rhs = self.format_rhs(&prod.rhs);

                    if first {
                        out.push_str(&format!("{lhs} ::= {rhs}"));
                        first = false;
                    } else {
                        out.push_str(&format!(" | {rhs}"));
                    }
                }
                out.push('\n');
            }
        }
        out.push('\n');

        out.push_str(&crate::typing::loader::save(self));

        out
    }

    /// Helper to format the right-hand side of a production
    fn format_rhs(&self, rhs_symbols: &[Symbol]) -> String {
        rhs_symbols
            .iter()
            .map(|s| self.format_symbol(s))
            .collect::<Vec<_>>()
            .join(" ")
    }

    fn format_symbol(&self, symbol: &Symbol) -> String {
        match symbol {
            Symbol::Nonterminal { name, binding, .. } => {
                if let Some(b) = binding {
                    format!("{name}[{b}]")
                } else {
                    name.clone()
                }
            }
            Symbol::Terminal { regex, binding } => {
                let base = format!("/{}/", regex.to_pattern());
                if let Some(b) = binding {
                    format!("{base}[{b}]")
                } else {
                    base
                }
            }
        }
    }

    /// Write the textual specification to a file on disk.
    pub fn save<P: AsRef<Path>>(&self, path: P) -> std::io::Result<()> {
        std::fs::write(path, self.to_spec_string())
    }
}
