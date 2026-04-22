use super::*;

#[cfg(test)]
fn toy_grammar() -> Grammar {
    load_example_grammar("toy")
}

pub fn valid_expressions_cases() -> Vec<ParseTestCase> {
    let cases = vec![
        ParseTestCase::valid("typed value fizz", "beep: Fizz"),
        ParseTestCase::valid("typed value buzz", "boop: Buzz"),
        ParseTestCase::valid("paren typed value", "(beep: Fizz)"),
        ParseTestCase::valid("concat same type", "beep: Fizz + blorp: Fizz"),
        ParseTestCase::valid("echo short chorus", "beep: Fizz x ha"),
        ParseTestCase::valid("echo long chorus", "beep: Fizz x ha ho hee"),
        ParseTestCase::valid("paren concat echo", "(beep: Fizz + boop: Fizz) x ho hee"),
    ];

    cases
}

pub fn invalid_expressions_cases() -> Vec<ParseTestCase> {
    vec![
        ParseTestCase::invalid("invalid token", "@"),
        ParseTestCase::invalid("double colon", "beep::Fizz"),
        ParseTestCase::invalid("unknown word", "blip: Fizz"),
        ParseTestCase::invalid("unknown type", "beep: Fuzz"),
        ParseTestCase::invalid("dangling concat", "beep: Fizz +"),
        ParseTestCase::invalid("broken chorus", "beep: Fizz x"),
        ParseTestCase::invalid("mismatched paren suffix", "(beep: Fizz))"),
    ]
}

#[test]
fn valid_expressions_toy() {
    let mut grammar = toy_grammar();
    let cases = valid_expressions_cases();
    let (res, _cases_json) = run_parse_batch(&mut grammar, &cases);
    assert_eq!(res.failed, 0, "{}", res.format_failures());
}

#[test]
fn invalid_expressions_toy() {
    let mut grammar = toy_grammar();
    let cases = invalid_expressions_cases();
    let (res, _cases_json) = run_parse_batch(&mut grammar, &cases);
    assert_eq!(res.failed, 0, "{}", res.format_failures());
}
