//! Typing rule loader — domain-specific half of grammar loading.
//!
//! Implements `ConstraintLoader` for `TypingDomain`.
//! Parses non-EBNF blocks from `.auf` source into a rule table.
//! The `NT → rule-name` mapping is handled separately by `Grammar::load`.

use std::collections::HashMap;

use crate::domains::typing::Conclusion;
use crate::domains::typing::TypingRule;
use crate::engine::grammar::utils::parse_inference_rule;
use crate::engine::grammar::SPG;
use crate::semantics::loader::ConstraintLoader;

use super::domain::TypingDomain;

#[derive(Default)]
pub struct TypingRuleLoader;

impl ConstraintLoader for TypingRuleLoader {
    type Domain = TypingDomain;

    fn load(blocks: &[&str]) -> Result<HashMap<String, TypingRule>, String> {
        let mut rules = HashMap::new();
        for block in blocks {
            let lines: Vec<&str> = block
                .lines()
                .map(str::trim)
                .filter(|l| !l.is_empty() && !l.starts_with("//"))
                .collect();
            if lines.is_empty() {
                continue;
            }
            if lines.iter().any(|l| l.contains("::=")) {
                continue;
            }

            let (premises, conclusion, name) = parse_inference_rule(&lines)?;
            let rule = TypingRule::new(premises, conclusion, name.clone())?;
            rules.insert(name, rule);
        }
        Ok(rules)
    }

    fn save(g: &SPG<Self::Domain>) -> String {
        let mut out = String::new();
        if g.rules.is_empty() {
            return String::new();
        }
        out.push_str("// --- Rules ---\n");
        let mut rule_list: Vec<_> = g.rules.values().collect();
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
        out
    }
}

/// Helper to format a list of premises as a string
fn format_premises(premises: &[crate::domains::typing::Premise]) -> String {
    use crate::domains::typing::TypingJudgment;

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
                        .map(|(v, t)| format!("[{v}:{t}]"))
                        .collect::<String>();
                    format!("{}{} ⊢ {} : {}", setting.name, exts, term, ty)
                }
            }
            (None, Some(TypingJudgment::Ascription((term, ty)))) => {
                format!("{term} : {ty}")
            }
            (None, Some(TypingJudgment::Membership(var, ctx))) => {
                format!("{var} ∈ {ctx}")
            }
            (Some(_), Some(TypingJudgment::Membership(var, ctx))) => {
                // Membership with setting doesn't make sense in current design, but handle it
                format!("{var} ∈ {ctx}")
            }
            (_, Some(TypingJudgment::Operation { left, op, right })) => {
                format!("{left} {op} {right}")
            }
            (_, Some(TypingJudgment::Equality { left, right })) => {
                format!("{left} = {right}")
            }
            (Some(setting), None) => setting.name.clone(),
            (None, None) => String::new(),
        })
        .collect::<Vec<_>>()
        .join(", ")
}

fn format_conclusion(conclusion: &Conclusion) -> String {
    format!("{conclusion}")
}
