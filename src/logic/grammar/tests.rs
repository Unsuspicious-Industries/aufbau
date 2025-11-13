#![cfg(test)]

use super::*;
use proptest::prelude::*;

// -----------------------------
// Helpers to generate grammars
// -----------------------------

fn sample_literals() -> Vec<&'static str> {
    vec!["'a'", "'b'", "','", "';'", "'x'", "'y'"]
}

fn sample_regexes() -> Vec<&'static str> {
    vec!["/[a-z]+/", "/[0-9]+/", "/[a-z]/"]
}

fn sample_bindings() -> Vec<&'static str> {
    vec!["x", "y", "v", "e"]
}

fn gen_nt_name() -> impl Strategy<Value = String> {
    // Nonterminal names: Capital followed by 0-2 lowercase ASCII letters
    let uppers: Vec<char> = ('A'..='Z').collect();
    let lowers: Vec<char> = ('a'..='z').collect();
    (prop::sample::select(uppers), prop::collection::vec(prop::sample::select(lowers), 0..3))
        .prop_map(|(c, rest)| {
            let mut s = String::new();
            s.push(c);
            for r in rest {
                s.push(r);
            }
            s
        })
}

fn gen_repetition() -> impl Strategy<Value = &'static str> {
    prop_oneof![Just(""), Just("*"), Just("?"), Just("+")]
}

fn gen_symbol(nt_names: Vec<String>) -> impl Strategy<Value = String> {
    let lits = sample_literals();
    let regs = sample_regexes();
    let binds = sample_bindings();

    // base tokens: literal | regex | expression
    let base = prop_oneof![
        // literal
        prop::sample::select(lits.clone()).prop_map(|s| s.to_string()),
        // regex
        prop::sample::select(regs.clone()).prop_map(|s| s.to_string()),
        // expression (nonterminal reference), maybe with binding
        prop::sample::select(nt_names.clone()).prop_map(|nt| nt.to_string()),
    ];

    // optionally add a binding (only on expressions to avoid weird prints)
    let with_binding = (prop::sample::select(nt_names), prop::option::of(prop::sample::select(binds)))
        .prop_map(|(nt, b)| match b {
            Some(b) => format!("{}[{}]", nt, b),
            None => nt,
        });

    let base_or_bound = prop_oneof![base, with_binding];

    // add repetition suffix
    (base_or_bound, gen_repetition()).prop_map(|(b, rep)| format!("{}{}", b, rep))
}

fn gen_group(nt_names: Vec<String>) -> impl Strategy<Value = String> {
    // Group is ( sym sym ... ) with 1..3 symbols, then optional repetition
    prop::collection::vec(gen_symbol(nt_names), 1..4)
        .prop_flat_map(|syms| gen_repetition().prop_map(move |rep| (syms.clone(), rep)))
        .prop_map(|(syms, rep)| format!("({}){}", syms.join(" "), rep))
}

fn gen_rhs_alt(nt_names: Vec<String>) -> impl Strategy<Value = String> {
    // Each alternative is 1..4 items, each item either a group or a symbol
    prop::collection::vec(
        prop_oneof![gen_group(nt_names.clone()), gen_symbol(nt_names.clone())],
        1..5,
    )
    .prop_map(|parts| parts.join(" "))
}

fn gen_production(nt_names: Vec<String>) -> impl Strategy<Value = String> {
    // LHS ::= alt (| alt)*
    (prop::sample::select(nt_names.clone()), prop::collection::vec(gen_rhs_alt(nt_names), 1..4))
        .prop_map(|(lhs, alts)| format!("{} ::= {}", lhs, alts.join(" | ")))
}

fn gen_grammar_spec() -> impl Strategy<Value = String> {
    // 1..4 nonterminals
    prop::collection::vec(gen_nt_name(), 1..5).prop_flat_map(|mut nts| {
        // Deduplicate names
        nts.sort();
        nts.dedup();
        let nt_names = nts.clone();
        prop::collection::vec(gen_production(nt_names.clone()), nts.len()..=nts.len())
            .prop_map(move |prods| prods.join("\n"))
    })
}

// -----------------------------
// Tests
// -----------------------------

// Property: roundtrip load -> save -> load yields same grammar; and desugaring removes +
proptest! {
    #[test]
    fn prop_roundtrip_random_grammar(spec in gen_grammar_spec()) {
        // Parse
        let g1 = match Grammar::load(&spec) {
            Ok(g) => g,
            Err(_) => return Ok(()), // skip invalid randoms
        };

        // After desugar, ensure no + remains (i.e., no repetition (1, None))
        for prods in g1.productions.values() {
            for p in prods {
                for sym in &p.rhs {
                    assert!(!matches!(sym, Symbol::Single { repetition: Some((1, None)), .. }));
                }
            }
        }

        // Roundtrip
        let spec2 = g1.to_spec_string();
        let g2 = Grammar::load(&spec2).expect("re-parse after save");
        prop_assert_eq!(g1, g2);
    }
}

#[test]
fn test_plus_desugaring_simple() {
    let spec = "A ::= 'a'+";
    let g = Grammar::load(spec).unwrap();
    let a = g.productions.get("A").unwrap();
    let rhs = &a[0].rhs;
    assert_eq!(rhs.len(), 2, "'a'+ should become 'a' 'a'*");
    assert!(matches!(rhs[0], Symbol::Litteral(ref s) if s == "a"));
    match &rhs[1] {
        Symbol::Single { value, repetition, .. } => {
            assert!(matches!(value.as_ref(), Symbol::Litteral(l) if l == "a"));
            assert_eq!(repetition.as_ref(), Some(&(0, None)));
        }
        _ => panic!("expected Single for star"),
    }
}

#[test]
fn test_group_plus_desugaring() {
    let spec = "A ::= ('a' 'b')+";
    let g = Grammar::load(spec).unwrap();
    // Expect A.0 synthetic
    assert!(g.productions.contains_key("A.0"));
    let a = g.productions.get("A").unwrap();
    let rhs = &a[0].rhs;
    assert_eq!(rhs.len(), 2, "group+ should expand to S S*");
    // First symbol is Expression("A.0")
    assert!(matches!(rhs[0], Symbol::Expression(ref s) if s == "A.0"));
    // Second is Single(Expression("A.0")) with *
    match &rhs[1] {
        Symbol::Single { value, repetition, .. } => {
            assert!(matches!(value.as_ref(), Symbol::Expression(s) if s == "A.0"));
            assert_eq!(repetition.as_ref(), Some(&(0, None)));
        }
        _ => panic!("expected Single star of synthetic"),
    }
}
