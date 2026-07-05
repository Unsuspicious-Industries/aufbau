//! ML parseability tests — the featured functional core (`examples/ml.auf`):
//! products, lists, conditionals, comparison, and recursive let, all checked by
//! unification.
//!
//! Syntax is strict OCaml subset: `fun (x : int) -> e`, lowercase types, `=`
//! for equality, `->` in match arms.

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
        ParseTestCase::valid("identity", "fun (x : int) -> x"),
        ParseTestCase::valid("curried const", "fun (x : int) -> fun (y : bool) -> x"),
        ParseTestCase::valid("apply identity", "(fun (x : int) -> x)(5)"),
        // let / arithmetic / comparison.
        ParseTestCase::valid("let int", "let a : int = 5 in a"),
        ParseTestCase::valid("let arith", "let a : int = 5 in a + 1"),
        ParseTestCase::valid("compare", "1 < 2"),
        ParseTestCase::valid("let then compare", "let a : int = 5 in a < 10"),
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
        ParseTestCase::valid("cons in let", "let xs : int list = 1 :: [] in xs"),
        // Recursive let.
        ParseTestCase::valid(
            "let rec",
            "let rec f : int -> int = fun (n : int) -> f(n) in f(0)",
        ),
        // Divergence: the universal inhabitant takes any demanded type.
        ParseTestCase::valid("diverge bare", "assert false"),
        ParseTestCase::valid("diverge at int", "let a : int = assert false in a"),
        ParseTestCase::valid("diverge in branch", "if true then 1 else assert false"),
        ParseTestCase::valid(
            "diverge as function",
            "let f : int -> bool = assert false in f(0)",
        ),
    ]
}

#[must_use]
pub fn invalid_expressions_cases() -> Vec<ParseTestCase> {
    vec![
        ParseTestCase::invalid("unbound var", "fun (x : int) -> y"),
        ParseTestCase::invalid("add bool", "1 + true"),
        ParseTestCase::invalid("if non-bool cond", "if 1 then 2 else 3"),
        ParseTestCase::invalid("if branch mismatch", "if true then 1 else false"),
        ParseTestCase::invalid("fst of non-pair", "fst 5"),
        ParseTestCase::invalid("let type mismatch", "let a : bool = 5 in a"),
        ParseTestCase::invalid("compare bool", "true < 2"),
        ParseTestCase::invalid("apply non-function", "5(3)"),
        ParseTestCase::invalid("cons mixed elements", "1 :: true :: []"),
        ParseTestCase::invalid("cons onto non-list", "1 :: 2"),
        ParseTestCase::invalid(
            "list annotation mismatch",
            "let xs : bool list = 1 :: [] in xs",
        ),
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
            "let rec length : int list -> int = fun (xs : int list) -> match xs with [] -> 0 | h :: t -> 1 + length(t) in length(1 :: 2 :: 3 :: [])",
        ),
        ParseTestCase::valid(
            "sum",
            "let rec sum : int list -> int = fun (xs : int list) -> match xs with [] -> 0 | h :: t -> h + sum(t) in sum(1 :: 2 :: [])",
        ),
        // `inc` deliberately starts with the keyword `in`: maximal-munch tokenizing.
        ParseTestCase::valid(
            "map increment",
            "let rec inc : int list -> int list = fun (xs : int list) -> match xs with [] -> [] | h :: t -> (h + 1) :: inc(t) in inc(1 :: 2 :: [])",
        ),
        ParseTestCase::valid(
            "member returns bool",
            "let rec member : int list -> bool = fun (xs : int list) -> match xs with [] -> false | h :: t -> if h = 0 then true else member(t) in member(0 :: 1 :: [])",
        ),
        ParseTestCase::valid(
            "copy via cons",
            "let rec copy : int list -> int list = fun (xs : int list) -> match xs with [] -> [] | h :: t -> h :: copy(t) in copy(1 :: 2 :: [])",
        ),
        // A free element type is fine when the uses agree.
        ParseTestCase::valid(
            "match on nil, consistent head",
            "match [] with [] -> 0 | h :: t -> h + 1",
        ),
    ]
}

/// List programs that must be rejected: a sound type system has to catch these.
#[must_use]
pub fn invalid_programs_cases() -> Vec<ParseTestCase> {
    vec![
        // The two match arms disagree (int vs bool).
        ParseTestCase::invalid(
            "match arms disagree",
            "match 1 :: [] with [] -> 0 | h :: t -> true",
        ),
        // Scrutinee is not a list.
        ParseTestCase::invalid("match on non-list", "match 5 with [] -> 0 | h :: t -> 1"),
        // Declared to return int, but the nil arm returns a list.
        ParseTestCase::invalid(
            "return type mismatch",
            "let rec bad : int list -> int = fun (xs : int list) -> match xs with [] -> [] | h :: t -> 0 in bad([])",
        ),
        // Regression: with a free element type (scrutinee `[]`), the head is one
        // metavariable across its uses, so `h` as bool and as int must clash.
        // Accepted before the global metavariable store landed.
        ParseTestCase::invalid(
            "head used at two types",
            "match [] with [] -> 0 | h :: t -> if h then 1 else h + 1",
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
            "let rec map : (int -> int) -> int list -> int list = fun (f : int -> int) -> fun (xs : int list) -> match xs with [] -> [] | h :: t -> f(h) :: map(f)(t) in map(fun (n : int) -> n + 1)(1 :: 2 :: [])"
        ));
    }

    /// Regression: the head's element type is tied to the scrutinee through the
    /// shared metavariable, so `f(h)` (h:int) against `int list` is rejected.
    #[test]
    fn match_head_element_type_is_tied_to_scrutinee() {
        assert!(!type_checks(
            "let rec f : int list -> int = fun (xs : int list) -> match xs with [] -> 0 | h :: t -> f(h) in f(1 :: [])"
        ));
    }

    #[test]
    #[ignore = "prefix-completeness gap: the full program types, but a mid-construction prefix does not parse"]
    fn nested_list_prefix() {
        // The full program is well-typed (`ast().is_complete()`), yet the prefix
        // `let xss : int list list = (1 :: [])` is rejected by the all-prefix
        // check, so it is recorded here rather than in `valid_programs_cases`.
        assert!(type_checks(
            "let xss : int list list = (1 :: []) :: [] in xss"
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
