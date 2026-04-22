use crate::logic::grammar::{Grammar, Symbol};
use crate::logic::typing::{Type, TypingRule};
use crate::logic::typing::rule::{Conclusion, ConclusionContext, ConclusionKind, Premise, TypeSetting, TypingJudgment};
use std::collections::HashSet;

// grammar filler
// breidges semantics gaps:
// - if we have a nonterminal with all its producitons
// being "transparent", menaing they have only one nonterminal child
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
        let mut pending_rules = Vec::new();

        {
            let productions = match grammar.productions.get_mut(&nt) {
                Some(prods) => prods,
                None => continue,
            };

            if productions.is_empty() {
                continue;
            }

            let mut used_bindings = HashSet::new();
            for production in productions.iter() {
                for symbol in production.rhs.iter() {
                    if let Some(binding) = symbol.binding() {
                        used_bindings.insert(binding.clone());
                    }
                }
            }

            
        }

        for rule in pending_rules {
            grammar.add_typing_rule(rule);
        }
    }

    grammar
}

fn synthetic_rule_name(nt: &str, alt_idx: usize) -> String {
    format!("__synthetic_{}_{}", nt, alt_idx)
}

fn unique_synthetic_child_binding(nt: &str, used: &mut HashSet<String>) -> String {
    let mut counter = 0;
    loop {
        let name = format!("__{}_child_{}", nt, counter);
        if used.insert(name.clone()) {
            return name;
        }
        counter += 1;
    }
}
