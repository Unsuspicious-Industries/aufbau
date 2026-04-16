use crate::logic::grammar::Grammar;
use crate::logic::typing::{gather_raw_types, Context};
use crate::regex::Regex;
use std::collections::HashSet;

pub fn collect_atoms(grammar: &Grammar) -> Vec<String> {
    let mut out = Vec::new();
    let mut seen = HashSet::new();

    for raw in gather_raw_types(grammar) {
        if seen.insert(raw.clone()) {
            out.push(raw);
        }
    }

    for seed in ["a", "x", "0", "1"] {
        if seen.insert(seed.to_string()) {
            out.push(seed.to_string());
        }
    }

    out
}

pub fn gather_candidates(
    token: &Regex,
    grammar: &Grammar,
    bound_texts: Vec<String>,
    other_completions: Vec<Regex>,
    ctx: &Context,
    atoms: &[String],
) -> Vec<String> {
    let example = token.example();
    let allowed =
        |s: &str| !grammar.specials().iter().any(|t| t == s) || example.as_deref() == Some(s);

    let mut out = Vec::new();
    let mut seen = HashSet::new();

    let mut push = |cand: String| {
        if allowed(&cand) && token.matches(&cand) && seen.insert(cand.clone()) {
            out.push(cand);
        }
    };

    if let Some(ex) = token.example() {
        push(ex);
    }
    for name in ctx.bindings.keys() {
        push(name.clone());
    }
    for text in bound_texts {
        push(text);
    }
    for t in other_completions {
        if let Some(ex) = t.example() {
            push(ex);
        }
    }
    for atom in atoms {
        push(atom.clone());
    }

    out
}
