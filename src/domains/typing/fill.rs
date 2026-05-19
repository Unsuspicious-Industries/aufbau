use std::collections::HashSet;

use crate::domains::typing::rule::{
    Conclusion, ConclusionContext, Premise, TypingJudgment, TypingRule,
};
use crate::domains::typing::TypeExpr;
use crate::domains::typing::TypingDomain;
use crate::engine::grammar::{Production, Symbol, SPG};

/// Post-load processing: fills bridge rules for transparent nonterminals
/// and compiles all rules through `CompilationPass`.
pub fn fill_and_compile(grammar: &mut SPG<TypingDomain>) -> Result<(), String> {
    let nonterminals: Vec<String> = grammar.nonterminals.clone();

    for nt in nonterminals {
        if grammar.nt_rule(&nt).is_some() {
            continue;
        }

        let should_bridge = grammar.productions.get(&nt).is_some_and(|productions| {
            !productions.is_empty()
                && productions
                    .iter()
                    .all(|production| transparent_child_index(production).is_some())
        });

        if !should_bridge {
            continue;
        }

        let binding = bridge_binding(grammar, &nt).ok_or_else(|| {
            format!(
                "nonterminal '{}' is a transparent wrapper with no explicit rule \
                 but child bindings are inconsistent across alternatives",
                nt
            )
        })?;

        if let Some(productions) = grammar.productions.get_mut(&nt) {
            for production in productions {
                let child_idx = transparent_child_index(production)
                    .expect("checked transparent nonterminal wrapper");
                match &mut production.rhs[child_idx] {
                    Symbol::Nonterminal { binding: slot, .. } => {
                        if slot.is_none() {
                            *slot = Some(binding.clone());
                        }
                    }
                    Symbol::Terminal { .. } => {
                        unreachable!("checked transparent nonterminal wrapper")
                    }
                }
            }
        }

        let rule_name = synthetic_rule_name(&nt);
        let rule = TypingRule {
            name: rule_name.clone(),
            premises: vec![Premise {
                setting: None,
                judgment: Some(TypingJudgment::Ascription((
                    binding.clone(),
                    TypeExpr::Meta(binding.clone()),
                ))),
            }],
            conclusion: Conclusion {
                context: ConclusionContext::default(),
                kind: TypeExpr::TypeOf(binding.clone()),
            },
        };

        grammar.add_rule(rule_name.clone(), rule);
        grammar
            .set_nt_rule(nt.clone(), rule_name)
            .map_err(|e| e.to_string())?;
    }

    Ok(())
}

fn transparent_child_index(production: &Production) -> Option<usize> {
    let mut child_index = None;

    for (index, symbol) in production.rhs.iter().enumerate() {
        match symbol {
            Symbol::Nonterminal { .. } => {
                if child_index.replace(index).is_some() {
                    return None;
                }
            }
            Symbol::Terminal {
                binding: Some(_), ..
            } => return None,
            Symbol::Terminal { .. } => {}
        }
    }

    child_index
}

fn bridge_binding(grammar: &SPG<TypingDomain>, nt: &str) -> Option<String> {
    let productions = grammar.productions.get(nt).expect("productions exist");
    let mut used_bindings = HashSet::new();
    let mut child_bindings = HashSet::new();

    for production in productions {
        let child_idx =
            transparent_child_index(production).expect("checked transparent nonterminal wrapper");
        for (index, symbol) in production.rhs.iter().enumerate() {
            if let Some(binding) = symbol.binding() {
                used_bindings.insert(binding.clone());
                if index == child_idx {
                    child_bindings.insert(binding.clone());
                }
            }
        }
    }

    match child_bindings.len() {
        0 => Some(unique_synthetic_child_binding(nt, &mut used_bindings)),
        1 => child_bindings.into_iter().next(),
        _ => None,
    }
}

fn synthetic_rule_name(nt: &str) -> String {
    format!("__br_{}", nt)
}

fn unique_synthetic_child_binding(nt: &str, used: &mut HashSet<String>) -> String {
    let mut counter = 0;
    loop {
        let name = format!("__{}_{}", nt, counter);
        if used.insert(name.clone()) {
            return name;
        }
        counter += 1;
    }
}
