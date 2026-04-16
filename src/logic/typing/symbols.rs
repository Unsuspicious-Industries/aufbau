//! Symbol Gathering - Collecting type symbols from typing rules

use crate::logic::grammar::Grammar;
use crate::logic::typing::rule::{ConclusionKind, TypingJudgment, TypingRule};
use crate::logic::typing::Type;

/// Collect all type symbols (Raw, Atom, Meta) from all typing rules in the grammar.
pub fn gather_type_symbols(grammar: &Grammar) -> Vec<String> {
    let mut symbols = Vec::new();

    for rule in grammar.rules().values() {
        collect_symbols_from_rule(rule, &mut symbols);
    }

    let mut seen = std::collections::HashSet::new();
    symbols.retain(|s| seen.insert(s.clone()));

    symbols
}

/// Collect all Type::Raw values from typing rules
pub fn gather_raw_types(grammar: &Grammar) -> Vec<String> {
    let mut raws = Vec::new();

    for rule in grammar.rules().values() {
        collect_raws_from_rule(rule, &mut raws);
    }

    let mut seen = std::collections::HashSet::new();
    raws.retain(|s| seen.insert(s.clone()));

    raws
}

fn collect_symbols_from_rule(rule: &TypingRule, out: &mut Vec<String>) {
    for premise in &rule.premises {
        if let Some(setting) = &premise.setting {
            for (_, ty) in &setting.extensions {
                collect_symbols_from_type(ty, out);
            }
        }
        if let Some(judgment) = &premise.judgment {
            match judgment {
                TypingJudgment::Ascription((_, ty)) => {
                    collect_symbols_from_type(ty, out);
                }
                TypingJudgment::Operation { left, right, .. } => {
                    collect_symbols_from_type(left, out);
                    collect_symbols_from_type(right, out);
                }
                TypingJudgment::Membership(_, _) => {}
                TypingJudgment::Check(_) => {}
            }
        }
    }

    match &rule.conclusion.kind {
        ConclusionKind::Type(ty) => {
            collect_symbols_from_type(ty, out);
        }
        ConclusionKind::ContextLookup(_, _) => {}
    }

    if let Some(output) = &rule.conclusion.context.output {
        for (_, ty) in &output.extensions {
            collect_symbols_from_type(ty, out);
        }
    }
}

fn collect_raws_from_rule(rule: &TypingRule, out: &mut Vec<String>) {
    for premise in &rule.premises {
        if let Some(setting) = &premise.setting {
            for (_, ty) in &setting.extensions {
                collect_raws_from_type(ty, out);
            }
        }
        if let Some(judgment) = &premise.judgment {
            match judgment {
                TypingJudgment::Ascription((_, ty)) => {
                    collect_raws_from_type(ty, out);
                }
                TypingJudgment::Operation { left, right, .. } => {
                    collect_raws_from_type(left, out);
                    collect_raws_from_type(right, out);
                }
                TypingJudgment::Membership(_, _) => {}
                TypingJudgment::Check(_) => {}
            }
        }
    }

    match &rule.conclusion.kind {
        ConclusionKind::Type(ty) => {
            collect_raws_from_type(ty, out);
        }
        ConclusionKind::ContextLookup(_, _) => {}
    }

    if let Some(output) = &rule.conclusion.context.output {
        for (_, ty) in &output.extensions {
            collect_raws_from_type(ty, out);
        }
    }
}

fn collect_symbols_from_type(ty: &Type, out: &mut Vec<String>) {
    match ty {
        Type::Meta(name) => out.push(name.clone()),
        Type::Raw(name) => out.push(name.clone()),
        Type::Arrow(l, r) => {
            collect_symbols_from_type(l, out);
            collect_symbols_from_type(r, out);
        }
        Type::Array(inner) => collect_symbols_from_type(inner, out),
        Type::Union(parts) => {
            for p in parts {
                collect_symbols_from_type(p, out);
            }
        }
        Type::Not(t) => collect_symbols_from_type(t, out),
        Type::ContextCall(ctx, var) => {
            out.push(ctx.clone());
            out.push(var.clone());
        }
        Type::Partial(t, _) => collect_symbols_from_type(t, out),
        Type::PathOf(t, _) => collect_symbols_from_type(t, out),
        Type::Any | Type::None | Type::Path(_) => {}
    }
}

fn collect_raws_from_type(ty: &Type, out: &mut Vec<String>) {
    match ty {
        Type::Raw(name) => out.push(name.clone()),
        Type::Arrow(l, r) => {
            collect_raws_from_type(l, out);
            collect_raws_from_type(r, out);
        }
        Type::Array(inner) => collect_raws_from_type(inner, out),
        Type::Union(parts) => {
            for p in parts {
                collect_raws_from_type(p, out);
            }
        }
        Type::Not(t) => collect_raws_from_type(t, out),
        Type::Partial(t, _) => collect_raws_from_type(t, out),
        Type::PathOf(t, _) => collect_raws_from_type(t, out),
        Type::Meta(_) | Type::ContextCall(_, _) => {}
        Type::Any | Type::None | Type::Path(_) => {}
    }
}
