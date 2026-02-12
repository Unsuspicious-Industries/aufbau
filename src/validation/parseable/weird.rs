use super::*;

// Small subset of pathological grammars adapted for parseability checks.

const INFINITE_RIGHT_RECURSIVE: &str = r#"
    A ::= 'a' A | 'b'
    start ::= A
"#;

const EPSILON_HEAVY: &str = r#"
    A ::= 'a' B | ε
    B ::= 'b' C | ε
    C ::= 'c' | ε
    start ::= A B C
"#;

const DEEP_NESTING: &str = r#"
    Atom ::= 'x'
    L1 ::= '(' L2 ')' | Atom
    L2 ::= '(' L3 ')' | L1
    L3 ::= '(' L4 ')' | L2
    start ::= L3
"#;

fn load_inline_grammar(content: &str) -> Grammar {
    Grammar::load(content).expect("failed to load inline grammar")
}

// === Per-grammar case lists ===
fn right_valid_cases() -> Vec<ParseTestCase> {
    vec![
        ParseTestCase::valid("right b", "b"),
        ParseTestCase::valid("right a b", "a b"),
        ParseTestCase::valid("right a a b", "a a b"),
    ]
}

fn right_invalid_cases() -> Vec<ParseTestCase> {
    vec![
        ParseTestCase::invalid("right invalid char", "c"),
        ParseTestCase::invalid("right wrong order", "b a"),
    ]
}

fn epsilon_valid_cases() -> Vec<ParseTestCase> {
    vec![
        ParseTestCase::valid("epsilon empty", ""),
        ParseTestCase::valid("epsilon a", "a"),
        ParseTestCase::valid("epsilon a b c", "a b c"),
    ]
}

fn epsilon_invalid_cases() -> Vec<ParseTestCase> {
    vec![ParseTestCase::invalid("epsilon invalid", "x")]
}

fn deep_valid_cases() -> Vec<ParseTestCase> {
    vec![
        ParseTestCase::valid("deep x", "x"),
        ParseTestCase::valid("deep (x)", "(x)"),
        ParseTestCase::valid("deep ((x))", "((x))"),
    ]
}

fn deep_invalid_cases() -> Vec<ParseTestCase> {
    vec![ParseTestCase::invalid("deep extra close", ")")]
}

/// Expose suites for each inline grammar so the validate runner can exercise
/// each grammar independently.
pub fn suites() -> Vec<(&'static str, Grammar, Vec<ParseTestCase>, Vec<ParseTestCase>)> {
    vec![
        (
            "weird::right",
            load_inline_grammar(INFINITE_RIGHT_RECURSIVE),
            right_valid_cases(),
            right_invalid_cases(),
        ),
        (
            "weird::epsilon",
            load_inline_grammar(EPSILON_HEAVY),
            epsilon_valid_cases(),
            epsilon_invalid_cases(),
        ),
        (
            "weird::deep",
            load_inline_grammar(DEEP_NESTING),
            deep_valid_cases(),
            deep_invalid_cases(),
        ),
    ]
}

#[test]
fn check_weird_parseable() {
    // Run each inline grammar's suite and ensure the parseable runner behaves as
    // expected (no failures in either valids or invalids).
    for (name, grammar, valids, invalids) in suites() {
        println!("\n=== Weird suite: {} ({} valid + {} invalid) ===", name, valids.len(), invalids.len());

        let (res_v, _) = run_parse_batch(&grammar, &valids);
        assert_eq!(res_v.failed, 0, "{} valid failures: {}", name, res_v.format_failures());

        let (res_i, _) = run_parse_batch(&grammar, &invalids);
        assert_eq!(res_i.failed, 0, "{} invalid failures: {}", name, res_i.format_failures());
    }
}
