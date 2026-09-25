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
    crate::grammar::SPG,
};

#[cfg(test)]
fn ml_grammar() -> SPG {
    load_example_grammar("ml")
}

/// `ml.auf` with `Expression` as the start symbol.
///
/// The grammar's own start is `Program`, which is a list of `Define` structure
/// items (`let name (p : T) : T = body`) — that is what constrained generation
/// targets, because the grader compiles the text and calls `solve` from a driver
/// appended after it, so a program has to bind a name that outlives its own
/// right-hand side.
///
/// Everything in this module below, though, is an *expression*: `1 < 2`, `[]`,
/// `fun (x : int) -> x`, `let a : int = 5 in a`. Those are the units the typing
/// rules are about, and they are worth testing as units. Against the `Program`
/// start every one of them is correctly rejected at its second token, which is
/// exactly what happened: when the top-level section was added these suites
/// began failing wholesale, and the `invalid_*` suites kept passing *vacuously*
/// — their inputs were still rejected, but for the wrong reason, so they proved
/// nothing about the type system.
///
/// See [`valid_structure_items_cases`] for the `Program` start.
#[cfg(test)]
fn ml_expression_grammar() -> SPG {
    let mut g = load_example_grammar("ml");
    g.with_start("Expression");
    g
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
    let mut grammar = ml_expression_grammar();
    let cases = valid_expressions_cases();
    let (res, _) = run_parse_batch(&mut grammar, &cases);
    assert_eq!(res.failed, 0, "{}", res.format_failures());
}

#[test]
fn invalid_expressions_ml() {
    let mut grammar = ml_expression_grammar();
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
        // The expression start, for the reason `ml_expression_grammar` gives:
        // every string below is an expression, not a list of structure items.
        let mut synth = TypingSynth::new(super::ml_expression_grammar(), s);
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
    let mut grammar = ml_expression_grammar();
    let cases = valid_programs_cases();
    let (res, _) = run_parse_batch(&mut grammar, &cases);
    assert_eq!(res.failed, 0, "{}", res.format_failures());
}

#[test]
fn invalid_programs_ml() {
    let mut grammar = ml_expression_grammar();
    let cases = invalid_programs_cases();
    let (res, _) = run_parse_batch(&mut grammar, &cases);
    assert_eq!(res.failed, 0, "{}", res.format_failures());
}

/// Structure items at the grammar's *own* start symbol, `Program`.
///
/// Everything else in this module is an expression, parsed at `Expression` (see
/// [`ml_expression_grammar`]). Nothing covered the `Program` start at all, which
/// is the one constrained generation actually targets -- so the form a generated
/// ML program has to take was the least tested thing in the grammar. These close
/// that gap.
#[must_use]
pub fn valid_structure_items_cases() -> Vec<ParseTestCase> {
    vec![
        ParseTestCase::valid(
            "solve over a list",
            "let solve (xs : int list) : int = match xs with [ ] -> 0 | h :: t -> h",
        ),
        ParseTestCase::valid(
            "returns a list",
            "let solve (xs : int list) : int list = 1 :: xs",
        ),
        ParseTestCase::valid(
            "bool result",
            "let solve (xs : int list) : bool = match xs with [ ] -> true | h :: t -> false",
        ),
        // A later item sees the ones before it, which is what `ProgramList` is for.
        ParseTestCase::valid(
            "two items, the second sees the first",
            "let one (n : int) : int = 1 let solve (xs : int list) : int = one(0)",
        ),
        // `let rec` remains available *inside* a body, which is where recursion lives.
        ParseTestCase::valid(
            "letrec inside a define",
            "let solve (xs : int list) : int = let rec len : int list -> int = fun (ys : int list) -> match ys with [ ] -> 0 | h :: t -> 1 + len(t) in len(xs)",
        ),
    ]
}

/// Structure items that must be rejected at the `Program` start.
#[must_use]
pub fn invalid_structure_items_cases() -> Vec<ParseTestCase> {
    vec![
        // The whole reason the top level exists: a bare term binds no name, so
        // the driver appended after it has no `solve` to call.
        ParseTestCase::invalid("a bare expression is not a program", "1 + 1"),
        ParseTestCase::invalid(
            "the body disagrees with the declared result type",
            "let solve (xs : int list) : int = true",
        ),
        // Self-recursion is deliberately absent from `Define`: a definition sees
        // the ones before it, not itself.
        ParseTestCase::invalid(
            "a define does not see itself",
            "let solve (xs : int list) : int = solve(xs)",
        ),
        ParseTestCase::invalid(
            "a later item is not visible earlier",
            "let solve (xs : int list) : int = two(0) let two (n : int) : int = 2",
        ),
        ParseTestCase::invalid(
            "the parameter is used at the wrong type",
            "let solve (xs : int list) : int = xs + 1",
        ),
    ]
}

#[test]
fn valid_structure_items_ml() {
    let mut grammar = ml_grammar();
    let cases = valid_structure_items_cases();
    let (res, _) = run_parse_batch(&mut grammar, &cases);
    assert_eq!(res.failed, 0, "{}", res.format_failures());
}

#[test]
fn invalid_structure_items_ml() {
    let mut grammar = ml_grammar();
    let cases = invalid_structure_items_cases();
    let (res, _) = run_parse_batch(&mut grammar, &cases);
    assert_eq!(res.failed, 0, "{}", res.format_failures());
}
