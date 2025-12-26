use super::{Grammar, Symbol};
use crate::logic::typing::Conclusion;
use std::path::Path;

impl Grammar {
    /// Produce the textual specification string.
    pub fn to_spec_string(&self) -> String {
        let mut out = String::new();
        // Preserve original declaration order; fall back to sorted for any missing
        let mut nt_list: Vec<&String> = self.productions.keys().collect();

        // ---------- Productions ----------
        out.push_str("// --- Production Rules ---\n");
        for nt in nt_list {
            if let Some(alts) = self.productions.get(nt) {
                let mut first = true;
                for prod in alts {
                    let lhs = if let Some(rule_name) = &prod.rule {
                        format!("{}({})", nt, rule_name)
                    } else {
                        nt.clone()
                    };

                    let rhs = self.format_rhs(&prod.rhs);

                    if first {
                        out.push_str(&format!("{} ::= {}", lhs, rhs));
                        first = false;
                    } else {
                        out.push_str(&format!(" | {}", rhs));
                    }
                }
                out.push('\n');
            }
        }
        out.push_str("\n");

        // ---------- Typing rules ----------
        if !self.typing_rules.is_empty() {
            out.push_str("// --- Typing Rules ---\n");
            let mut rule_list: Vec<_> = self.typing_rules.values().collect();
            rule_list.sort_by_key(|r| &r.name);

            for rule in rule_list {
                out.push_str(&format_premises(&rule.premises));
                out.push('\n');
                let concl_str = format_conclusion(&rule.conclusion);
                let line = "-".repeat(std::cmp::max(20, concl_str.len() + 5));
                out.push_str(&format!("{} ({})\n", line, rule.name));
                out.push_str(&concl_str);
                out.push_str("\n\n");
            }
        }

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
            Symbol::Nonterminal { name, binding } => {
                if let Some(b) = binding {
                    format!("{}[{}]", name, b)
                } else {
                    name.to_string()
                }
            }
            Symbol::Terminal { regex, binding } => {
                let base = format!("/{}/", regex.to_pattern());
                if let Some(b) = binding {
                    format!("{}[{}]", base, b)
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

/// Helper to format a list of premises as a string
fn format_premises(premises: &[crate::logic::typing::Premise]) -> String {
    use crate::logic::typing::TypingJudgment;
    
    premises
        .iter()
        .map(|p| match (&p.setting, &p.judgment) {
            (Some(setting), Some(TypingJudgment::Ascription((term, ty)))) => {
                if setting.extensions.is_empty() {
                    format!("{} ⊢ {} : {}", setting.name, term, ty)
                } else {
                    let exts = setting
                        .extensions
                        .iter()
                        .map(|(v, t)| format!("[{}:{}]", v, t))
                        .collect::<Vec<_>>()
                        .join("");
                    format!("{}{} ⊢ {} : {}", setting.name, exts, term, ty)
                }
            }
            (None, Some(TypingJudgment::Ascription((term, ty)))) => {
                format!("{} : {}", term, ty)
            }
            (None, Some(TypingJudgment::Membership(var, ctx))) => {
                format!("{} ∈ {}", var, ctx)
            }
            (Some(_), Some(TypingJudgment::Membership(var, ctx))) => {
                // Membership with setting doesn't make sense in current design, but handle it
                format!("{} ∈ {}", var, ctx)
            }
            (_, Some(TypingJudgment::Operation { left, op, right })) => {
                format!("{} {} {}", left, op, right)
            }
            (Some(setting), None) => format!("{}", setting.name),
            (None, None) => String::new(),
        })
        .collect::<Vec<_>>()
        .join(", ")
}

fn format_conclusion(conclusion: &Conclusion) -> String {
    format!("{}", conclusion)
}
