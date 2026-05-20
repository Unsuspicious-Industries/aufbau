use crate::regex::dfa::DFA;
use crate::regex::nfa::NFA;
use crate::regex::Regex;

#[test]
fn test_dfa_from_nfa() {
    let regex = Regex::from_str("(a|b)bb*|y").unwrap();
    let nfa = NFA::from(regex);
    let dfa = DFA::from(nfa);
    println!("{}", dfa);
}

#[test]
fn test_dfa_derive() {
    let regex = Regex::from_str("(a|b)bb*|y").unwrap();
    let nfa = NFA::from(regex);
    let dfa = DFA::from(nfa);
    let derived_dfa = dfa.derive("ab");
    assert!(derived_dfa.is_some());
    let cleaned_dfa = derived_dfa.unwrap().clean();
    println!("{}", cleaned_dfa);
}

#[test]
fn test_dfa_clean_basic() {
    let regex = Regex::from_str("a*b").unwrap();
    let nfa = NFA::from(regex);
    let dfa = DFA::from(nfa);
    let cleaned_dfa = dfa.clean();

    assert_eq!(dfa.accepts("b"), cleaned_dfa.accepts("b"));
    assert_eq!(dfa.accepts("ab"), cleaned_dfa.accepts("ab"));
    assert_eq!(dfa.accepts("aab"), cleaned_dfa.accepts("aab"));
    assert_eq!(dfa.accepts("aaab"), cleaned_dfa.accepts("aaab"));
    assert_eq!(dfa.accepts("a"), cleaned_dfa.accepts("a"));
    assert_eq!(dfa.accepts(""), cleaned_dfa.accepts(""));
}

#[test]
fn test_dfa_clean_with_unreachable_states() {
    let mut dfa = DFA::new();
    let s0 = dfa.add_state();
    let s1 = dfa.add_state();
    let s2 = dfa.add_state();
    let _s3 = dfa.add_state();

    dfa.start = s0;
    dfa.accept_states.insert(s2);
    dfa.add_transition(s0, 'a', s1);
    dfa.add_transition(s1, 'b', s2);

    let cleaned_dfa = dfa.clean();
    assert!(cleaned_dfa.states.len() < dfa.states.len());
    assert_eq!(dfa.accepts("ab"), cleaned_dfa.accepts("ab"));
    assert_eq!(dfa.accepts("a"), cleaned_dfa.accepts("a"));
    assert_eq!(dfa.accepts("b"), cleaned_dfa.accepts("b"));
    assert_eq!(dfa.accepts(""), cleaned_dfa.accepts(""));
}

#[test]
fn test_dfa_clean_preserves_acceptance() {
    let regex = Regex::from_str("(a|b)*abb").unwrap();
    let nfa = NFA::from(regex);
    let dfa = DFA::from(nfa);
    let cleaned_dfa = dfa.clean();

    let test_cases = vec![
        ("abb", true),
        ("aabb", true),
        ("babb", true),
        ("ababb", true),
        ("ab", false),
        ("abbc", false),
        ("", false),
        ("a", false),
        ("b", false),
    ];
    for (input, expected) in test_cases {
        assert_eq!(dfa.accepts(input), expected, "Failed for input: {}", input);
        assert_eq!(
            cleaned_dfa.accepts(input),
            expected,
            "Failed for input: {}",
            input
        );
        assert_eq!(
            dfa.accepts(input),
            cleaned_dfa.accepts(input),
            "Cleaning changed acceptance for: {}",
            input
        );
    }
}

#[test]
fn test_dfa_clean_idempotent() {
    let regex = Regex::from_str("a(b|c)*d").unwrap();
    let nfa = NFA::from(regex);
    let dfa = DFA::from(nfa);
    let cleaned_once = dfa.clean();
    let cleaned_twice = cleaned_once.clean();

    assert_eq!(cleaned_once.states.len(), cleaned_twice.states.len());
    assert_eq!(cleaned_once.start, cleaned_twice.start);
    assert_eq!(cleaned_once.accept_states, cleaned_twice.accept_states);

    for input in &["ad", "abd", "acd", "abcd", "accbd", "abcbd"] {
        assert_eq!(cleaned_once.accepts(input), cleaned_twice.accepts(input));
    }
}

#[test]
fn test_dfa_operations_intersection() {
    let regex1 = Regex::from_str("a*b*").unwrap();
    let regex2 = Regex::from_str("ab*").unwrap();
    let dfa1 = DFA::from(NFA::from(regex1));
    let dfa2 = DFA::from(NFA::from(regex2));
    let intersection_dfa = &dfa1 & &dfa2;

    assert!(intersection_dfa.accepts("a"));
    assert!(intersection_dfa.accepts("ab"));
    assert!(intersection_dfa.accepts("abb"));
    assert!(!intersection_dfa.accepts("b"));
    assert!(!intersection_dfa.accepts(""));
}

#[test]
fn test_dfa_operations_union() {
    let regex = Regex::from_str("a*").unwrap();
    let dfa1 = DFA::from(NFA::from(regex.clone()));
    let dfa2 = DFA::from(NFA::from(regex));
    let union_dfa = &dfa1 | &dfa2;

    assert!(union_dfa.accepts(""));
    assert!(union_dfa.accepts("a"));
    assert!(union_dfa.accepts("aa"));
}

#[test]
fn test_dfa_operations_difference() {
    let dfa1 = DFA::from(NFA::from(Regex::from_str("a*").unwrap()));
    let dfa2 = DFA::from(NFA::from(Regex::from_str("aa*").unwrap()));
    let difference_dfa = &dfa1 - &dfa2;

    assert!(difference_dfa.accepts(""));
    assert!(!difference_dfa.accepts("a"));
    assert!(!difference_dfa.accepts("aa"));
}

#[test]
fn test_dfa_operations_symmetric_difference() {
    let dfa1 = DFA::from(NFA::from(Regex::from_str("a*").unwrap()));
    let dfa2 = DFA::from(NFA::from(Regex::from_str("aa*").unwrap()));
    let xor_dfa = &dfa1 ^ &dfa2;

    assert!(xor_dfa.accepts(""));
    assert!(!xor_dfa.accepts("a"));
}

#[test]
fn test_dfa_operations_complement() {
    let dfa = DFA::from(NFA::from(Regex::from_str("a*").unwrap()));
    let complement_dfa = !&dfa;

    assert!(!complement_dfa.accepts(""));
    assert!(!complement_dfa.accepts("a"));
}

#[test]
fn test_dfa_operations_complex() {
    let dfa1 = DFA::from(NFA::from(Regex::from_str("a*b*").unwrap()));
    let dfa2 = DFA::from(NFA::from(Regex::from_str("ab*").unwrap()));
    let intersection_dfa = &dfa1 & &dfa2;

    assert!(!intersection_dfa.accepts(""));
    assert!(intersection_dfa.accepts("a"));
    assert!(!intersection_dfa.accepts("b"));
    assert!(intersection_dfa.accepts("ab"));
    assert!(intersection_dfa.accepts("abb"));
}

#[test]
fn test_dfa_is_accepting() {
    let mut dfa = DFA::new();
    let s0 = dfa.add_state();
    let s1 = dfa.add_state();
    dfa.start = s0;
    dfa.accept_states.insert(s1);
    dfa.add_transition(s0, 'a', s1);
    assert!(dfa.is_accepting());

    let mut dfa_no_accept = DFA::new();
    let s0 = dfa_no_accept.add_state();
    dfa_no_accept.start = s0;
    assert!(!dfa_no_accept.is_accepting());
}
