use super::*;

pub const ARITHMETIC_GRAMMAR: &str = r#"
    Number ::= /[0-9]+/
    Identifier ::= /[a-z][a-zA-Z0-9]*/
    Literal ::= Number
    Variable ::= Identifier
    Operator ::= '+' | '-' | '*' | '/'
    Primary ::= Literal | Variable | '(' Expression ')'
    Expression ::= Primary | Primary Operator Expression
"#;

pub fn arithmetic_grammar() -> Grammar {
    Grammar::load(ARITHMETIC_GRAMMAR).expect("failed to load arithmetic grammar")
}

pub fn valid_expressions_cases() -> Vec<ParseTestCase> {
    vec![
        ParseTestCase::valid("empty", ""),
        ParseTestCase::valid("single digit", "1"),
        ParseTestCase::valid("multi digit", "42"),
        ParseTestCase::valid("large number", "9999"),
        ParseTestCase::valid("simple var", "x"),
        ParseTestCase::valid("longer var", "abc"),
        ParseTestCase::valid("var with digits", "x1"),
        ParseTestCase::valid("add prefix", "1 +"),
        ParseTestCase::valid("sub prefix", "x -"),
        ParseTestCase::valid("mul prefix", "2 *"),
        ParseTestCase::valid("div prefix", "y /"),
        ParseTestCase::valid("simple add", "1 + 2"),
        ParseTestCase::valid("chain ops", "1 + 2 * 3"),
        ParseTestCase::valid("open paren", "("),
        ParseTestCase::valid("paren number", "(42"),
        ParseTestCase::valid("closed paren", "(42)"),
        ParseTestCase::valid("nested parens", "((1))"),
        ParseTestCase::valid("complex paren", "(x + y) * z"),
    ]
}

pub fn invalid_expressions_cases() -> Vec<ParseTestCase> {
    vec![
        ParseTestCase::invalid("close paren first", ")"),
        ParseTestCase::invalid("operator first", "+ 1"),
        ParseTestCase::invalid("double operator", "1 ++"),
        ParseTestCase::invalid("invalid char", "@"),
        ParseTestCase::invalid("percent op", "1 %"),
        ParseTestCase::invalid("extra close", "1)"),
        ParseTestCase::invalid("misplaced close", "(1))"),
    ]
}

#[test]
fn valid_expressions_arithmetic() {
    let grammar = arithmetic_grammar();
    let cases = valid_expressions_cases();
    let (res, _cases_json) = run_parse_batch(&grammar, &cases);
    assert_eq!(res.failed, 0, "{}", res.format_failures());
}

#[test]
fn invalid_expressions_arithmetic() {
    let grammar = arithmetic_grammar();
    let cases = invalid_expressions_cases();
    let (res, _cases_json) = run_parse_batch(&grammar, &cases);
    assert_eq!(res.failed, 0, "{}", res.format_failures());
}
