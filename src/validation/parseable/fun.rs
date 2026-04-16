use super::*;

#[cfg(test)]
fn fun_grammar() -> Grammar {
    load_example_grammar("fun")
}

pub fn valid_expressions_cases() -> Vec<ParseTestCase> {
    let cases = vec![
        // === Literals ===
        ParseTestCase::valid("integer literal", "42"),
        ParseTestCase::valid("zero", "0"),
        ParseTestCase::valid("float literal", "3.14"),
        ParseTestCase::valid("boolean true", "true"),
        ParseTestCase::valid("boolean false", "false"),
        // === Arithmetic ===
        ParseTestCase::valid("int addition", "1 + 2"),
        ParseTestCase::valid("int multiplication", "3 * 4"),
        ParseTestCase::valid("float addition", "1.0 +. 2.5"),
        ParseTestCase::valid("float division", "10.0 /. 2.0"),
        ParseTestCase::valid("float op with completable int", "1.0 +. 2"),
        // === Lambda ===
        ParseTestCase::valid("simple lambda", "(x: Int) => x + 1"),
        ParseTestCase::valid("float lambda", "(x: Float) => x *. 2.0"),
        // === Application ===
        ParseTestCase::valid("lambda application", "((x: Int) => x + 1)(41)"),
        // === Let binding ===
        ParseTestCase::valid("simple let", "let n: Int = 12; n + 1"),
    ];

    cases
}

pub fn invalid_expressions_cases() -> Vec<ParseTestCase> {
    vec![
        // === Syntax errors ===
        ParseTestCase::invalid("missing ':' in let", "let n Int = 12;"),
        ParseTestCase::invalid("missing semicolon", "let n: Int = 12 n"),
        ParseTestCase::invalid("bad identifier", "let 1x: Int = 3;"),
        ParseTestCase::invalid("close paren first", ")"),
        ParseTestCase::invalid("extra close paren", "(1))"),
        ParseTestCase::invalid("at sign", "@"),
        ParseTestCase::invalid("hash", "#"),
        ParseTestCase::invalid("dollar", "$x"),
        ParseTestCase::invalid("backslash", "\\x"),
        ParseTestCase::invalid("leading plus", "+ 1"),
        ParseTestCase::invalid("leading star", "* 2"),
        ParseTestCase::invalid("double operator", "1 ++ 2"),
        ParseTestCase::invalid("let no name", "let : Int = 1; 1"),
        ParseTestCase::invalid("let double semi", "let x: Int = 1;; x"),
        ParseTestCase::invalid("arrow without lambda", "=> 1"),
        ParseTestCase::invalid("lambda missing arrow", "(x: Int) x"),
        // === Type errors ===
        ParseTestCase::type_error("int expected, float given", "let n: Int = 9.8; n"),
        ParseTestCase::type_error("float expected, int given", "let x: Float = 1; x"),
        ParseTestCase::type_error("let int declared bool value", "let x: Int = true; x"),
        ParseTestCase::type_error("let int declared float value", "let x: Int = 1.0; x"),
        // === Operator type errors ===
        ParseTestCase::type_error("int operator with float", "1 + 2.0"),
        ParseTestCase::type_error("mixed operators", "1 +. 2.0"),
        ParseTestCase::type_error("mixed operators", "3 +. 5"),
        ParseTestCase::type_error("bool plus int", "true + 1"),
        ParseTestCase::type_error("int plus bool", "1 + false"),
        ParseTestCase::type_error("bool float op", "true +. 1.0"),
        // === Application errors ===
        ParseTestCase::type_error("wrong argument type", "((x: Int) => x + 1)(2.0)"),
        ParseTestCase::type_error("apply non-function", "1(2)"),
        ParseTestCase::type_error("apply bool", "true(1)"),
        ParseTestCase::type_error("unbound x", "x"),
        ParseTestCase::type_error("unbound in expr", "x + 1"),
        ParseTestCase::type_error("unbound func", "f(1)"),
        ParseTestCase::type_error("unbound in let body", "let x: Int = 1; y"),
        ParseTestCase::type_error("var outside scope", "let x: Int = y; x"),
        ParseTestCase::type_error("wrong arg type bool for int", "f(true)")
            .with_context(vec![("f", "Int -> Int")]),
        ParseTestCase::type_error("wrong arg type int for bool", "f(1)")
            .with_context(vec![("f", "Bool -> Bool")]),
    ]
}

// FIXME: This test uses the old partial::MetaParser API which no longer exists
// #[test]
// #[ignore = "depth probe - run manually to diagnose fun grammar performance"]
// fn probe_fun_parse_depth() { ... }

#[test]
fn valid_expressions_fun() {
    let mut grammar = fun_grammar();
    let cases = valid_expressions_cases();

    println!("\n=== Fun Valid Expressions ({} cases) ===", cases.len());

    let (res, _cases_json) = run_parse_batch(&mut grammar, &cases);

    assert_eq!(res.failed, 0, "{}", res.format_failures());

    println!(
        "✓ All {} cases passed in {:?} (avg {:?})",
        cases.len(),
        res.total_duration,
        res.avg_duration
    );
}

#[test]
fn invalid_expressions_fun() {
    let mut grammar = fun_grammar();
    let cases = invalid_expressions_cases();

    println!("\n=== Fun Invalid Expressions ({} cases) ===", cases.len());

    let (res, _cases_json) = run_parse_batch(&mut grammar, &cases);

    assert_eq!(res.failed, 0, "{}", res.format_failures());

    println!(
        "✓ All {} cases passed in {:?} (avg {:?})",
        res.passed, res.total_duration, res.avg_duration
    );
}
