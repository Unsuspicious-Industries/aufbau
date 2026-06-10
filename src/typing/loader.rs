//! Typing rule loader — domain-specific half of grammar loading.
//!
//! Parses the non-EBNF blocks of an `.auf` source into a rule table. The
//! `NT → rule-name` mapping is handled separately by `SPG::load`.

use std::collections::HashMap;

use crate::engine::grammar::SPG;
use crate::engine::grammar::utils::parse_inference_rule;
use crate::typing::Conclusion;
use crate::typing::TypingRule;

/// Parse the rule-body blocks into a `rule-name → TypingRule` table and the list
/// of type-rewrite rules (`lhs ⇝ rhs`, the normalization theory).
#[allow(clippy::type_complexity)] // the real tuple reads clearer than an alias
pub fn load(
    blocks: &[&str],
) -> Result<(HashMap<String, TypingRule>, Vec<(String, String)>), String> {
    let mut rules = HashMap::new();
    let mut rewrites = Vec::new();
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
        if lines.iter().any(|l| l.contains('⇝') || l.contains("~>")) {
            for line in &lines {
                let Some(pair) = split_rewrite(line) else {
                    return Err(format!("malformed rewrite rule: '{line}'"));
                };
                rewrites.push(pair);
            }
            continue;
        }

        let (premises, conclusion, name) = parse_inference_rule(&lines)?;
        let rule = TypingRule::new(premises, conclusion, name.clone())?;
        rules.insert(name, rule);
    }
    Ok((rules, rewrites))
}

/// Reject a grammar whose type-level declarations cannot mean what they say:
/// a rule pattern with no unique parse (it would silently weaken to `⊤`), or a
/// rewrite that does not parse or invents variables on its right side (which
/// would let normalization un-ground a ground term).
pub fn check(g: &SPG) -> Result<(), String> {
    for rule in g.rules.values() {
        let bindings = g.rule_bindings(&rule.name);
        for te in rule.type_exprs() {
            crate::typing::TyExpr::build(g, te, &bindings)
                .map_err(|e| format!("rule '{}': {e}", rule.name))?;
        }
    }
    for (l, r) in &g.rewrites {
        let (lhs, rhs) = (
            crate::typing::Term::parse(g, l)?,
            crate::typing::Term::parse(g, r)?,
        );
        let lv = lhs.vars();
        if !rhs.vars().iter().all(|v| lv.contains(v)) {
            return Err(format!("rewrite '{l} ⇝ {r}' invents a variable"));
        }
    }
    Ok(())
}

/// Split a `lhs ⇝ rhs` (or `lhs ~> rhs`) line into its two non-empty sides.
fn split_rewrite(line: &str) -> Option<(String, String)> {
    for sep in ["⇝", "~>"] {
        if let Some((l, r)) = line.split_once(sep) {
            let (l, r) = (l.trim(), r.trim());
            if !l.is_empty() && !r.is_empty() {
                return Some((l.to_string(), r.to_string()));
            }
        }
    }
    None
}

/// Render the rule table back to `.auf` source.
pub fn save(g: &SPG) -> String {
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

/// Helper to format a list of premises as a string
fn format_premises(premises: &[crate::typing::Premise]) -> String {
    use crate::typing::TypingJudgment;

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
            (Some(setting), None) => setting.name.clone(),
            (None, None) => String::new(),
        })
        .collect::<Vec<_>>()
        .join(", ")
}

fn format_conclusion(conclusion: &Conclusion) -> String {
    format!("{conclusion}")
}
