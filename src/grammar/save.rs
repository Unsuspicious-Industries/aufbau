use super::{SPG, Symbol};
use std::path::Path;

impl SPG {
    /// Produce the textual specification string.
    #[must_use]
    pub fn to_spec_string(&self) -> String {
        let mut out = String::new();
        // Declaration order, with the start block last: a reload's last-NT-is-
        // start convention then recovers the same start symbol.
        let mut nt_list: Vec<&String> = self
            .nonterminals
            .iter()
            .filter(|nt| self.productions.contains_key(*nt) && self.start.as_ref() != Some(nt))
            .collect();
        nt_list.extend(self.start.iter().filter(|s| self.productions.contains_key(*s)));

        // ---------- Productions ----------
        out.push_str("// --- Productions ---\n");
        for nt in nt_list {
            if let Some(alts) = self.productions.get(nt) {
                let mut first = true;
                for prod in alts {
                    let lhs = if self.ty.as_ref() == Some(nt) {
                        format!("{nt}*")
                    } else if let Some(rule_name) = self.nt_rule(nt) {
                        format!("{nt}({rule_name})")
                    } else {
                        nt.clone()
                    };

                    let rhs = prod
                        .rhs
                        .iter()
                        .map(|s| self.format_symbol(s))
                        .collect::<Vec<_>>()
                        .join(" ");

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
                // A pure literal renders quoted, so a reload re-registers it as
                // a tokenizer special, exactly as the original source did.
                let base = match regex.literal_value() {
                    Some(lit) => format!("'{lit}'"),
                    None => format!("/{}/", regex.to_pattern()),
                };
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
