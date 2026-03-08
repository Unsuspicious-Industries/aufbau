use super::*;

use TypedCompletionTestCase as T;

fn toy_grammar() -> Grammar {
    crate::validation::parseable::load_example_grammar("toy")
}

pub fn suites() -> Vec<(&'static str, Grammar, Vec<TypedCompletionTestCase>)> {
    let g = toy_grammar();
    vec![("toy::completable", g, completable_cases())]
}

fn completable_cases() -> Vec<TypedCompletionTestCase> {
    vec![
        T::ok("typed value complete", "beep: Fizz", 1),
        T::ok("typed value prefix", "beep", 3),
        T::ok("paren prefix", "(", 4),
        T::ok("concat complete", "beep: Fizz + blorp: Fizz", 2),
        T::ok("concat rhs prefix", "beep: Fizz +", 4),
        T::ok("echo complete", "beep: Fizz x ha", 2),
        T::ok("echo chorus prefix", "beep: Fizz x", 3),
        T::ok("nested echo prefix", "(beep: Fizz + blorp: Fizz) x", 4),
    ]
}

#[test]
fn check_completable() {
    let grammar = toy_grammar();
    let res = run_test_batch(&grammar, &completable_cases());
    res.assert_all_passed();
}
