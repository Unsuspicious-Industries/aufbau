//! Comprehensive tests for the regex module.

use crate::set_debug_level;

use super::*;

// ===== Construction =====
#[test]
fn literal_creates_char_sequence() {
    assert!(Regex::literal("abc").matches("abc"));
    assert!(!Regex::literal("abc").matches("ab"));
    assert!(!Regex::literal("abc").matches("abcd"));
}

#[test]
fn literal_empty_is_epsilon() {
    assert_eq!(Regex::literal(""), Regex::Epsilon);
    assert!(Regex::literal("").matches(""));
}

#[test]
fn cat_concatenates() {
    let r = Regex::cat(Regex::Char('a'), Regex::Char('b'));
    assert!(r.matches("ab"));
    assert!(!r.matches("a"));
    assert!(!r.matches("ba"));
}

#[test]
fn or_creates_union() {
    let r = Regex::or(Regex::Char('a'), Regex::Char('b'));
    assert!(r.matches("a"));
    assert!(r.matches("b"));
    assert!(!r.matches("ab"));
}

#[test]
fn star_creates_kleene_star() {
    let r = Regex::star(Regex::Char('a'));
    assert!(r.matches(""));
    assert!(r.matches("a"));
    assert!(r.matches("aaaa"));
    assert!(!r.matches("b"));
}

#[test]
fn plus_requires_at_least_one() {
    let r = Regex::plus(Regex::Char('a'));
    assert!(!r.matches(""));
    assert!(r.matches("a"));
    assert!(r.matches("aaa"));
}

#[test]
fn opt_makes_optional() {
    let r = Regex::opt(Regex::Char('a'));
    assert!(r.matches(""));
    assert!(r.matches("a"));
    assert!(!r.matches("aa"));
}

#[test]
fn concat_many_chains() {
    let r = Regex::concat_many(vec![Regex::Char('a'), Regex::Char('b'), Regex::Char('c')]);
    assert!(r.matches("abc"));
}

#[test]
fn union_many_unions() {
    let r = Regex::union_many(vec![Regex::Char('a'), Regex::Char('b'), Regex::Char('c')]);
    assert!(r.matches("a"));
    assert!(r.matches("c"));
    assert!(!r.matches("d"));
}

#[test]
fn any_of_creates_char_class() {
    let r = Regex::any_of("xyz");
    assert!(r.matches("x"));
    assert!(r.matches("z"));
    assert!(!r.matches("a"));
}

// ===== Predicates =====
#[test]
fn is_empty_checks_empty() {
    assert!(Regex::Empty.is_empty());
    assert!(!Regex::Epsilon.is_empty());
    assert!(!Regex::Char('a').is_empty());
    assert!(Regex::cat(Regex::Empty, Regex::Char('a')).is_empty());
    assert!(!Regex::or(Regex::Empty, Regex::Char('a')).is_empty());
}

#[test]
fn is_nullable_checks_matches_epsilon() {
    assert!(!Regex::Empty.is_nullable());
    assert!(Regex::Epsilon.is_nullable());
    assert!(!Regex::Char('a').is_nullable());
    assert!(Regex::star(Regex::Char('a')).is_nullable());
    assert!(Regex::opt(Regex::Char('a')).is_nullable());
    assert!(!Regex::plus(Regex::Char('a')).is_nullable());
}

#[test]
fn equiv_compares_simplified() {
    let r1 = Regex::cat(Regex::Epsilon, Regex::Char('a'));
    let r2 = Regex::Char('a');
    assert!(r1.equiv(&r2));
}

// ===== Simplification =====
#[test]
fn simplify_removes_epsilon() {
    let r = Regex::cat(Regex::Epsilon, Regex::Char('a'));
    assert_eq!(r.simplify(), Regex::Char('a'));
}

#[test]
fn simplify_propagates_empty() {
    let r = Regex::cat(Regex::Empty, Regex::Char('a'));
    assert_eq!(r.simplify(), Regex::Empty);
}

#[test]
fn simplify_dedupes_union() {
    let r = Regex::or(Regex::Char('a'), Regex::Char('a'));
    assert_eq!(r.simplify(), Regex::Char('a'));
}

// ===== Derivatives =====
#[test]
fn deriv_char_match() {
    let r = Regex::Char('a');
    assert_eq!(r.deriv('a'), Regex::Epsilon);
    assert_eq!(r.deriv('b'), Regex::Empty);
}

#[test]
fn deriv_range() {
    let r = Regex::Range('a', 'z');
    assert_eq!(r.deriv('m'), Regex::Epsilon);
    assert_eq!(r.deriv('0'), Regex::Empty);
}

#[test]
fn deriv_union() {
    let r = Regex::or(Regex::Char('a'), Regex::Char('b'));
    assert!(r.deriv('a').is_nullable());
    assert!(r.deriv('b').is_nullable());
    assert!(r.deriv('c').is_empty());
}

#[test]
fn deriv_concat() {
    let r = Regex::cat(Regex::Char('a'), Regex::Char('b'));
    let d = r.deriv('a').simplify();
    assert!(d.matches("b"));
}

#[test]
fn deriv_star() {
    set_debug_level(crate::DebugLevel::Trace);
    let r = Regex::star(Regex::Char('a'));
    let d = r.derivative("a").simplify();
    println!("{}", d);
    assert!(d.is_nullable()); // can match more 'a's or stop
}

#[test]
fn derivative_full_string() {
    let r = Regex::literal("hello");
    assert!(r.derivative("hello").simplify().is_nullable());
    assert!(!r.derivative("hell").simplify().is_nullable());
}

#[test]
fn matches_basic() {
    let r = Regex::new("a*b").unwrap();
    assert!(r.matches("b"));
    assert!(r.matches("ab"));
    assert!(r.matches("aaab"));
    assert!(!r.matches("a"));
}

// ===== Prefix Matching =====
#[test]
fn prefix_match_no_match() {
    let r = Regex::literal("abc");
    assert!(matches!(r.prefix_match("xyz"), PrefixStatus::NoMatch));
}

#[test]
fn prefix_match_valid_prefix() {
    let r = Regex::literal("abc");
    match r.prefix_match("ab") {
        PrefixStatus::Prefix(_) => {}
        _ => panic!("expected Prefix"),
    }
}

#[test]
fn prefix_match_complete() {
    let r = Regex::Char('a');
    match r.prefix_match("a") {
        PrefixStatus::Complete | PrefixStatus::Extensible(_) => {}
        s => panic!("expected Complete/Extensible, got {:?}", s),
    }
}

#[test]
fn prefix_match_extensible() {
    let r = Regex::star(Regex::Char('a'));
    match r.prefix_match("aa") {
        PrefixStatus::Extensible(_) => {}
        s => panic!("expected Extensible, got {:?}", s),
    }
}

// ===== Cartesian Product =====
#[test]
fn product_basic() {
    let choices = vec![
        vec![Regex::Char('a'), Regex::Char('b')],
        vec![Regex::Char('x'), Regex::Char('y')],
    ];
    let result = Regex::product(choices);
    assert_eq!(result.len(), 4);
    assert!(result.iter().any(|r| r.matches("ax")));
    assert!(result.iter().any(|r| r.matches("by")));
}

#[test]
fn product_traced_tracks_indices() {
    let choices = vec![vec![Regex::Char('a'), Regex::Char('b')]];
    let result = Regex::product_traced(choices);
    assert_eq!(result.len(), 2);
    assert_eq!(result[0].0, vec![0]);
    assert_eq!(result[1].0, vec![1]);
}

#[test]
fn product_empty_is_epsilon() {
    let result = Regex::product(vec![]);
    assert_eq!(result.len(), 1);
    assert!(result[0].matches(""));
}

// ===== String Generation =====
#[test]
fn valids_generates_matching_strings() {
    let r = Regex::or(Regex::Char('a'), Regex::literal("bb"));
    let v = r.valids(2);
    assert!(v.contains("a"));
    assert!(v.contains("bb"));
    for s in &v {
        assert!(r.matches(s));
    }
}

#[test]
fn valids_respects_max_length() {
    let r = Regex::star(Regex::Char('a'));
    let v = r.valids(3);
    assert!(v.contains(""));
    assert!(v.contains("a"));
    assert!(v.contains("aaa"));
    assert!(!v.contains("aaaa"));
}

#[test]
fn example_returns_valid_string() {
    let patterns = ["a", "a*", "ab", "[0-9]+", "(x|y)z"];
    for p in patterns {
        let r = Regex::new(p).unwrap();
        if let Some(ex) = r.example() {
            assert!(r.matches(&ex), "example '{}' doesn't match '{}'", ex, p);
        }
    }
}

#[test]
fn examples_returns_multiple() {
    let r = Regex::Range('a', 'z');
    let ex = r.examples(5);
    assert!(ex.len() <= 5);
    for s in &ex {
        assert!(r.matches(s));
    }
}

// ===== Parsing =====
#[test]
fn new_parses_patterns() {
    let cases = [
        ("a", "a", true),
        ("a|b", "a", true),
        ("a|b", "b", true),
        ("ab", "ab", true),
        ("a*", "", true),
        ("a*", "aaa", true),
        ("a+", "", false),
        ("a+", "a", true),
        ("a?", "", true),
        ("a?", "a", true),
        ("[0-9]", "5", true),
        ("[a-z]+", "hello", true),
    ];
    for (pat, input, expected) in cases {
        let r = Regex::new(pat).expect(pat);
        assert_eq!(r.matches(input), expected, "{} vs {}", pat, input);
    }
}

// ===== to_pattern =====
#[test]
fn to_pattern_roundtrips() {
    let cases = ["a", "ab", "[a-z]", "a*"];
    for pat in cases {
        let r = Regex::new(pat).unwrap();
        let p = r.to_pattern();
        let r2 = Regex::new(&p).unwrap();
        assert!(r.equiv(&r2), "{} -> {} not equivalent", pat, p);
    }
}

// ===== Common Patterns =====
#[test]
fn common_patterns() {
    assert!(Regex::digit().matches("5"));
    assert!(!Regex::digit().matches("a"));
    assert!(Regex::lower().matches("x"));
    assert!(Regex::upper().matches("X"));
    assert!(Regex::alpha().matches("a"));
    assert!(Regex::alpha().matches("Z"));
    assert!(Regex::alnum().matches("a"));
    assert!(Regex::alnum().matches("9"));
    assert!(Regex::word().matches("_"));
}

// ===== Edge Cases =====
#[test]
fn empty_never_matches() {
    assert!(!Regex::Empty.matches(""));
    assert!(!Regex::Empty.matches("a"));
}

#[test]
fn epsilon_only_empty() {
    assert!(Regex::Epsilon.matches(""));
    assert!(!Regex::Epsilon.matches("a"));
}

#[test]
fn nested_star() {
    let r = Regex::star(Regex::star(Regex::Char('a')));
    assert!(r.matches(""));
    assert!(r.matches("a"));
    assert!(r.matches("aaaa"));
}

#[test]
fn complex_pattern() {
    let r = Regex::new("([a-z]+[0-9]*)+").unwrap();
    assert!(r.matches("hello"));
    assert!(r.matches("test123"));
    assert!(r.matches("abc123def456"));
}

#[test]
fn test_match_len() {
    // Test basic character matching
    let r = Regex::Char('a');
    assert_eq!(r.match_len("a"), Some(1));
    assert_eq!(r.match_len("b"), None);
    assert_eq!(r.match_len("aa"), Some(1));

    // Test epsilon (empty match)
    let r = Regex::Epsilon;
    assert_eq!(r.match_len(""), Some(0));
    assert_eq!(r.match_len("a"), Some(0));

    // Test empty regex (never matches)
    let r = Regex::Empty;
    assert_eq!(r.match_len(""), None);
    assert_eq!(r.match_len("a"), None);

    // Test literal strings
    let r = Regex::literal("abc");
    assert_eq!(r.match_len("abc"), Some(3));
    assert_eq!(r.match_len("ab"), None);
    assert_eq!(r.match_len("abcd"), Some(3));
    assert_eq!(r.match_len("xyz"), None);

    // Test concatenation
    let r = Regex::cat(Regex::Char('a'), Regex::Char('b'));
    assert_eq!(r.match_len("ab"), Some(2));
    assert_eq!(r.match_len("a"), None);
    assert_eq!(r.match_len("abc"), Some(2));

    // Test union (OR)
    let r = Regex::or(Regex::Char('a'), Regex::Char('b'));
    assert_eq!(r.match_len("a"), Some(1));
    assert_eq!(r.match_len("b"), Some(1));
    assert_eq!(r.match_len("c"), None);

    // Test Kleene star (zero or more)
    let r = Regex::star(Regex::Char('a'));
    assert_eq!(r.match_len(""), Some(0));
    assert_eq!(r.match_len("a"), Some(1));
    assert_eq!(r.match_len("aaa"), Some(3));
    assert_eq!(r.match_len("aaab"), Some(3));

    // Test plus (one or more)
    let r = Regex::plus(Regex::Char('a'));
    assert_eq!(r.match_len(""), None);
    assert_eq!(r.match_len("a"), Some(1));
    assert_eq!(r.match_len("aaa"), Some(3));
    assert_eq!(r.match_len("aaab"), Some(3));

    // Test optional (zero or one)
    let r = Regex::opt(Regex::Char('a'));
    assert_eq!(r.match_len(""), Some(0));
    assert_eq!(r.match_len("a"), Some(1));
    assert_eq!(r.match_len("aa"), Some(1));

    // Test ranges
    let r = Regex::Range('a', 'z');
    assert_eq!(r.match_len("a"), Some(1));
    assert_eq!(r.match_len("m"), Some(1));
    assert_eq!(r.match_len("z"), Some(1));
    assert_eq!(r.match_len("0"), None);
}
