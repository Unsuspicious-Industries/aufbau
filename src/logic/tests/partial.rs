use crate::logic::grammar::Grammar;
use crate::logic::parser::Parser;
use crate::logic::partial::{PartialOutcome};

#[allow(dead_code)]
fn setup_grammar(spec: &str) -> Parser {
    let g = Grammar::load(spec).expect("grammar load");
    Parser::new(g)
}

#[test]
fn partial_empty_input() {
    let spec = r#"
    Start ::= 'a' 'b'
    "#;
    let mut p = setup_grammar(spec);
    let out = p.partial("").expect("init ok");
    match out {
        PartialOutcome::Incomplete { states } => {
            assert!(!states.is_empty());
            // Expect start symbol partial
            let st = &states[0];
            if let Some(nt) = st.ast.as_nonterminal() {
                assert_eq!(nt.value, "Start");
            } else { panic!("expected nonterminal state") }
        }
        _ => panic!("expected incomplete")
    }
}

#[test]
fn partial_single_token_progress() {
    let spec = r#"
    Start ::= 'a' 'b'
    "#;
    let mut p = setup_grammar(spec);
    let out = p.partial("a").expect("init ok");
    match out {
        PartialOutcome::Incomplete { states } => {
            assert!(!states.is_empty());
            // Should show one child consumed
            let st = &states[0];
            let nt = st.ast.as_nonterminal().expect("nt");
            assert_eq!(nt.children.len(), 1);
            assert_eq!(nt.children[0].value(), "a");
        }
        _ => panic!("expected incomplete after one token")
    }
}

#[test]
fn partial_complete() {
    let spec = r#"
    Start ::= 'a' 'b'
    "#;
    let mut p = setup_grammar(spec);
    let out = p.partial("a b").expect("init ok");
    match out {
        PartialOutcome::Complete { node } => {
            let nt = node.as_nonterminal().expect("nt");
            assert_eq!(nt.value, "Start");
            assert_eq!(nt.children.len(), 2);
        }
        other => panic!("unexpected outcome: {:?}", other)
    }
}

#[test]
fn partial_repetition_progress() {
    let spec = r#"
    List ::= Item* 'end'
    Item ::= 'x'
    "#;
    let mut p = setup_grammar(spec);
    let out = p.partial("x x").expect("init ok");
    match out {
        PartialOutcome::Incomplete { states } => {
            assert!(!states.is_empty());
            let st = &states[0];
            let nt = st.ast.as_nonterminal().expect("nt");
            // Last child should be repetition synthetic node or consumed tokens before expecting 'end'
            assert!(nt.children.len() >= 1);
        }
        _ => panic!("expected incomplete (missing 'end')")
    }
}

#[test]
fn partial_nested_nonterminal_incomplete() {
    let spec = r#"
    Start ::= A 'z'
    A ::= 'x' 'y'
    "#;
    let mut p = setup_grammar(spec);
    let out = p.partial("x").expect("init ok");
    match out {
        PartialOutcome::Incomplete { states } => {
            assert!(!states.is_empty());
            // There should be a partial state either for A or Start with A partial
            let mut found = false;
            for st in states {
                let nt = st.ast.as_nonterminal().unwrap();
                if nt.value == "Start" || nt.value == "A" { found = true; break; }
            }
            assert!(found, "Expected partial state for Start or A");
        }
        _ => panic!("expected incomplete for nested partial")
    }
}
