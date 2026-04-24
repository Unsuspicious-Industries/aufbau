use crate::logic::grammar::{Grammar, Production, Symbol};
use crate::logic::typing::Type;
use crate::logic::typing::rule::{
    Conclusion, ConclusionContext, ConclusionKind, Premise, TypingJudgment, TypingRule,
};
use std::collections::HashSet;

// grammar filler
// breidges semantics gaps:
// - if we have a nonterminal with all its productions
// being "transparent", meaning they have only one nonterminal child
// and no bound terminals
// we inject syntathic bridge rule, that propagade the lone NT child type upwards
// this means injectin a syntahtic binding to the NT child
// + injecting a rule into the note tat references this bindings, ascribes it to a meta,
// and concludes by returning the meta type
//
// Example:
// A ::= '-' B '\'
// B(r) ::= C[x]
// C ::= /[0-9]+/
//
// ------------- (r)
// 'Int'
//
// In this example, A has a single NT child, B
// we inject the synthetic stuff and we get
// A(rx) ::= '-' B[bx] '\'
// B(r) ::= C[x]
// C ::= /[0-9]+/
//
// bx : ?R
// ------------- (rx)
// ?R
//
// -------------- (r)
// 'Int'

pub fn fill(mut grammar: Grammar) -> Grammar {
    let nonterminals: Vec<String> = grammar.nonterminals.clone();

    for nt in nonterminals {
        if grammar.nonterminal_rule(&nt).is_some() {
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

        let Some(binding) = bridge_binding(&grammar, &nt) else {
            continue;
        };

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
                    Type::Meta(binding.clone()),
                ))),
            }],
            conclusion: Conclusion {
                context: ConclusionContext::default(),
                kind: ConclusionKind::Type(Type::Meta(binding)),
            },
        };

        grammar.add_typing_rule(rule);
        let _ = grammar.set_nonterminal_rule(nt.clone(), rule_name);
        grammar.mark_bridge_nt(nt);
    }

    grammar
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

fn bridge_binding(grammar: &Grammar, nt: &str) -> Option<String> {
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
