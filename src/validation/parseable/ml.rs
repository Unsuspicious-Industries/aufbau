//! ML parseability tests — the featured functional core (`examples/ml.auf`):
//! products, lists, conditionals, comparison, and recursive let, all checked by
//! unification.

use super::ParseTestCase;
#[cfg(test)]
use {
    super::{load_example_grammar, run_parse_batch},
    crate::engine::grammar::SPG,
};

#[cfg(test)]
fn ml_grammar() -> SPG {
    load_example_grammar("ml")
}

#[must_use]
pub fn valid_expressions_cases() -> Vec<ParseTestCase> {
    vec![
        // Functions and application.
        ParseTestCase::valid("identity", "(x : Int) => x"),
        ParseTestCase::valid("curried const", "(x : Int) => (y : Bool) => x"),
        ParseTestCase::valid("apply identity", "((x : Int) => x)(5)"),
        // let / arithmetic / comparison.
        ParseTestCase::valid("let int", "let a : Int = 5 in a"),
        ParseTestCase::valid("let arith", "let a : Int = 5 in a + 1"),
        ParseTestCase::valid("compare", "1 < 2"),
        ParseTestCase::valid("let then compare", "let a : Int = 5 in a < 10"),
        // Conditionals.
        ParseTestCase::valid("if literals", "if true then 1 else 2"),
        ParseTestCase::valid("if compare", "if 1 < 2 then 1 else 0"),
        // Products and projections.
        ParseTestCase::valid("pair", "(1, true)"),
        ParseTestCase::valid("fst", "fst (1, true)"),
        ParseTestCase::valid("snd", "snd (1, true)"),
        ParseTestCase::valid("nested pair", "((1, 2), true)"),
        ParseTestCase::valid("fst snd compose", "fst (snd ((1, (2, 3))))"),
        // Lists: nil at any element type, cons fixes it, nesting.
        ParseTestCase::valid("nil", "[]"),
        ParseTestCase::valid("singleton", "1 :: []"),
        ParseTestCase::valid("cons chain", "1 :: 2 :: 3 :: []"),
        ParseTestCase::valid("list of pairs", "(1, true) :: []"),
        ParseTestCase::valid("cons in let", "let xs : Int list = 1 :: [] in xs"),
        // Recursive let.
        ParseTestCase::valid(
            "let rec",
            "let rec f : Int -> Int = (n : Int) => f(n) in f(0)",
        ),
    ]
}

#[must_use]
pub fn invalid_expressions_cases() -> Vec<ParseTestCase> {
    vec![
        ParseTestCase::invalid("unbound var", "(x : Int) => y"),
        ParseTestCase::invalid("add bool", "1 + true"),
        ParseTestCase::invalid("if non-bool cond", "if 1 then 2 else 3"),
        ParseTestCase::invalid("if branch mismatch", "if true then 1 else false"),
        ParseTestCase::invalid("fst of non-pair", "fst 5"),
        ParseTestCase::invalid("let type mismatch", "let a : Bool = 5 in a"),
        ParseTestCase::invalid("compare bool", "true < 2"),
        ParseTestCase::invalid("apply non-function", "5(3)"),
        ParseTestCase::invalid("cons mixed elements", "1 :: true :: []"),
        ParseTestCase::invalid("cons onto non-list", "1 :: 2"),
        ParseTestCase::invalid("list annotation mismatch", "let xs : Bool list = 1 :: [] in xs"),
    ]
}

#[test]
fn valid_expressions_ml() {
    let mut grammar = ml_grammar();
    let cases = valid_expressions_cases();
    let (res, _) = run_parse_batch(&mut grammar, &cases);
    assert_eq!(res.failed, 0, "{}", res.format_failures());
}

#[test]
fn invalid_expressions_ml() {
    let mut grammar = ml_grammar();
    let cases = invalid_expressions_cases();
    let (res, _) = run_parse_batch(&mut grammar, &cases);
    assert_eq!(res.failed, 0, "{}", res.format_failures());
}

/// Real recursive list programs (`match` + `let rec`): the type system run on the
/// kind of code lists exist for.
#[must_use]
pub fn valid_programs_cases() -> Vec<ParseTestCase> {
    vec![
        ParseTestCase::valid(
            "length",
            "let rec length : Int list -> Int = (xs : Int list) => match xs with [] => 0 | h :: t => 1 + length(t) in length(1 :: 2 :: 3 :: [])",
        ),
        ParseTestCase::valid(
            "sum",
            "let rec sum : Int list -> Int = (xs : Int list) => match xs with [] => 0 | h :: t => h + sum(t) in sum(1 :: 2 :: [])",
        ),
        // `inc` deliberately starts with the keyword `in`: maximal-munch tokenizing.
        ParseTestCase::valid(
            "map increment",
            "let rec inc : Int list -> Int list = (xs : Int list) => match xs with [] => [] | h :: t => (h + 1) :: inc(t) in inc(1 :: 2 :: [])",
        ),
        ParseTestCase::valid(
            "member returns Bool",
            "let rec member : Int list -> Bool = (xs : Int list) => match xs with [] => false | h :: t => if h == 0 then true else member(t) in member(0 :: 1 :: [])",
        ),
        ParseTestCase::valid(
            "copy via cons",
            "let rec copy : Int list -> Int list = (xs : Int list) => match xs with [] => [] | h :: t => h :: copy(t) in copy(1 :: 2 :: [])",
        ),
    ]
}

/// List programs that must be rejected: a sound type system has to catch these.
#[must_use]
pub fn invalid_programs_cases() -> Vec<ParseTestCase> {
    vec![
        // The two match arms disagree (Int vs Bool).
        ParseTestCase::invalid("match arms disagree", "match 1 :: [] with [] => 0 | h :: t => true"),
        // Scrutinee is not a list.
        ParseTestCase::invalid("match on non-list", "match 5 with [] => 0 | h :: t => 1"),
        // Declared to return Int, but the nil arm returns a list.
        ParseTestCase::invalid(
            "return type mismatch",
            "let rec bad : Int list -> Int = (xs : Int list) => match xs with [] => [] | h :: t => 0 in bad([])",
        ),
    ]
}

/// Programs the stress test exposes as not yet supported. Each is real ML that
/// `should` type-check; they are recorded (ignored, not deleted) so the gap stays
/// visible and a fix can simply remove `#[ignore]`.
#[cfg(test)]
mod known_limitations {
    use crate::typing::TypingSynth;

    fn type_checks(s: &str) -> bool {
        let mut synth = TypingSynth::new(super::ml_grammar(), s);
        synth.ast().is_ok_and(|a| a.is_complete())
    }

    #[test]
    #[ignore = "higher-order recursion over lists (map/filter/fold) does not type yet"]
    fn higher_order_map() {
        assert!(type_checks(
            "let rec map : (Int -> Int) -> Int list -> Int list = (f : Int -> Int) => (xs : Int list) => match xs with [] => [] | h :: t => f(h) :: map(f)(t) in map((n : Int) => n + 1)(1 :: 2 :: [])"
        ));
    }

    #[test]
    #[ignore = "UNSOUND: match pattern-var element type is not tied to the scrutinee, so `f(h)` (h:Int) wrongly unifies against Int list. Needs constraint propagation (inference)."]
    fn match_head_element_type_is_unconstrained() {
        // `f(h)` applies `f : Int list -> Int` to the head `h : Int`. A sound
        // checker rejects it; today the head's type is a free `?A` that unifies
        // with anything, so this is (wrongly) accepted. The assertion encodes the
        // sound expectation and fails until the gap is closed.
        assert!(!type_checks(
            "let rec f : Int list -> Int = (xs : Int list) => match xs with [] => 0 | h :: t => f(h) in f(1 :: [])"
        ));
    }

    #[test]
    #[ignore = "prefix-completeness gap: the full program types, but a mid-construction prefix does not parse"]
    fn nested_list_prefix() {
        // The full program is well-typed (`ast().is_complete()`), yet the prefix
        // `let xss : Int list list = (1 :: [])` is rejected by the all-prefix
        // check, so it is recorded here rather than in `valid_programs_cases`.
        assert!(type_checks(
            "let xss : Int list list = (1 :: []) :: [] in xss"
        ));
    }
}

#[test]
fn valid_programs_ml() {
    let mut grammar = ml_grammar();
    let cases = valid_programs_cases();
    let (res, _) = run_parse_batch(&mut grammar, &cases);
    assert_eq!(res.failed, 0, "{}", res.format_failures());
}

#[test]
fn invalid_programs_ml() {
    let mut grammar = ml_grammar();
    let cases = invalid_programs_cases();
    let (res, _) = run_parse_batch(&mut grammar, &cases);
    assert_eq!(res.failed, 0, "{}", res.format_failures());
}
