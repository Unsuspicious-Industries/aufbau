use std::path::Path;
use super::{Grammar, Symbol, RepetitionKind};
use crate::logic::typing::Conclusion;


impl Grammar {
    /// Produce the textual specification string.
    pub fn to_spec_string(&self) -> String {
        let mut out = String::new();
        // Preserve original declaration order; fall back to sorted for any missing
        let mut nt_list: Vec<_> = self.production_order.clone();
        for k in self.productions.keys() {
            if !nt_list.contains(k) {
                nt_list.push(k.clone());
            }
        }

        // ---------- Productions ----------
        out.push_str("// --- Production Rules ---\n");
        for nt in nt_list {
            if let Some(alts) = self.productions.get(&nt) {
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
        rhs_symbols.iter().map(|s| self.format_symbol(s)).collect::<Vec<_>>().join(" ")
    }

    fn format_symbol(&self, symbol: &Symbol) -> String {
        match symbol {
            Symbol::Group { symbols, repetition } => {
                let inner = symbols.iter().map(|s| self.format_symbol(s)).collect::<Vec<_>>().join(" ");
                let rep = match repetition { Some(RepetitionKind::ZeroOrMore)=>"*", Some(RepetitionKind::OneOrMore)=>"+", Some(RepetitionKind::ZeroOrOne)=>"?", None=>"" };
                format!("({}){}", inner, rep)
            }
            Symbol::Simple { value, binding, repetition } => {
                let base = self.base_symbol_str(value);
                let rep = match repetition { Some(RepetitionKind::ZeroOrMore)=>"*", Some(RepetitionKind::OneOrMore)=>"+", Some(RepetitionKind::ZeroOrOne)=>"?", None=>"" };
                if let Some(b) = binding { format!("{}[{}]{}", base, b, rep) } else { format!("{}{}", base, rep) }
            }
        }
    }

    fn base_symbol_str(&self, value: &str) -> String {
        if value.starts_with('/') && value.ends_with('/') { return value.to_string(); }
        let is_nt = self.productions.contains_key(value);
        if is_nt { return value.to_string(); }
        if value.starts_with('\'') && value.ends_with('\'') { return value.to_string(); }
        if value.starts_with('"') && value.ends_with('"') { return value.to_string(); }
        format!("'{}'", value)
    }

    /// Write the textual specification to a file on disk.
    pub fn save<P: AsRef<Path>>(&self, path: P) -> std::io::Result<()> {
        std::fs::write(path, self.to_spec_string())
    }
}

/// Helper to format a list of premises as a string
fn format_premises(premises: &[crate::logic::typing::Premise]) -> String {
    premises.iter().map(|p| match (&p.setting, &p.judgment) {
        (Some(setting), Some(crate::logic::typing::TypingJudgment::Ascription((term, ty)))) => {
            if setting.extensions.is_empty() {
                format!("{} ⊢ {} : {}", setting.name, term, ty)
            } else {
                let exts = setting.extensions.iter()
                    .map(|(v, t)| format!("[{}:{}]", v, t))
                    .collect::<Vec<_>>()
                    .join("");
                format!("{}{} ⊢ {} : {}", setting.name, exts, term, ty)
            }
        }
        (None, Some(crate::logic::typing::TypingJudgment::Ascription((term, ty)))) => {
            format!("{} : {}", term, ty)
        }
        (None, Some(crate::logic::typing::TypingJudgment::Membership(var, ctx))) => {
            format!("{} ∈ {}", var, ctx)
        }
        (Some(_), Some(crate::logic::typing::TypingJudgment::Membership(var, ctx))) => {
            // Membership with setting doesn't make sense in current design, but handle it
            format!("{} ∈ {}", var, ctx)
        }
        (Some(setting), None) => format!("{}", setting.name),
        (None, None) => String::new(),
    }).collect::<Vec<_>>().join(", ")
}


fn format_conclusion(conclusion: &Conclusion) -> String {
    format!("{}", conclusion)
}

