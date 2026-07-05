//! Typing rule loader — domain-specific half of grammar loading.
//!
//! Parses the non-EBNF blocks of an `.auf` source into a rule table. The
//! `NT → rule-name` mapping is handled separately by `SPG::load`.

use std::collections::HashMap;

use crate::engine::grammar::SPG;
use crate::engine::grammar::utils::parse_inference_rule;
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

/// The rewrite theory parsed against `g`. Empty ⇒ the normalizer is the
/// identity. A side that fails to parse is dropped; [`check`] reports it.
#[must_use]
pub fn normalizer(g: &SPG) -> crate::typing::Normalizer {
    let rules = g
        .rewrites
        .iter()
        .filter_map(|(l, r)| {
            Some(crate::typing::RewriteRule {
                lhs: crate::typing::Term::parse(g, l).ok()?,
                rhs: crate::typing::Term::parse(g, r).ok()?,
            })
        })
        .collect();
    crate::typing::Normalizer::from_rules(rules)
}

/// Each rule type-expression parsed into its tree once. A pattern that cannot
/// be built is left out and reads as unresolved; [`check`] reports it.
#[must_use]
pub fn type_trees(g: &SPG) -> crate::typing::domain::Trees {
    let mut trees = crate::typing::domain::Trees::new();
    for rule in g.rules.values() {
        let bindings = g.rule_bindings(&rule.name);
        for te in rule.type_exprs() {
            if !trees.contains_key(te)
                && let Ok(ty) = crate::typing::TyExpr::build(g, te, &bindings)
            {
                trees.insert(te.clone(), ty);
            }
        }
    }
    trees
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
        let premises: Vec<String> = rule.premises.iter().map(ToString::to_string).collect();
        out.push_str(&premises.join(", "));
        out.push('\n');
        let concl_str = rule.conclusion.to_string();
        let line = "-".repeat(std::cmp::max(20, concl_str.len() + 5));
        out.push_str(&format!("{} ({})\n", line, rule.name));
        out.push_str(&concl_str);
        out.push_str("\n\n");
    }
    out
}
