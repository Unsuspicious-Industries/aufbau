use super::*;

#[cfg(test)]
fn fun_grammar() -> Grammar {
    load_example_grammar("fun")
}

pub fn valid_expressions_cases() -> Vec<ParseTestCase> {
    vec![
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
        ParseTestCase::valid(
            "let with lambda",
            "let f: Int -> Int = (x: Int) => x * 2; f(21)",
        ),
        ParseTestCase::valid(
            "float let",
            "let f: Float -> Float = (x: Float) => x +. 1.0; f(2.5)",
        ),
    ]
}

pub fn invalid_expressions_cases() -> Vec<ParseTestCase> {
    vec![
        // === Syntax errors ===
        ParseTestCase::invalid("missing ':' in let", "let n Int = 12;"),
        ParseTestCase::invalid("missing semicolon", "let n: Int = 12 n"),
        ParseTestCase::invalid("bad identifier", "let 1x: Int = 3;"),
        // === Type errors ===
        ParseTestCase::type_error("int expected, float given", "let n: Int = 9.8; n"),
        ParseTestCase::type_error("float expected, int given", "let x: Float = 1; x"),
        // === Operator type errors ===
        ParseTestCase::type_error("int operator with float", "1 + 2.0"),
        ParseTestCase::type_error("mixed operators", "1 +. 2.0"),
        ParseTestCase::type_error("mixed operators", "3 +. 5"),
        // === Application errors ===
        ParseTestCase::type_error("wrong argument type", "((x: Int) => x + 1)(2.0)"),
        ParseTestCase::type_error("apply non-function", "1(2)"),
    ]
}

#[test]
fn valid_expressions_fun() {
    let grammar = fun_grammar();
    let cases = valid_expressions_cases();

    println!("\n=== Fun Valid Expressions ({} cases) ===", cases.len());

    let (res, _cases_json) = run_parse_batch(&grammar, &cases);

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
    let grammar = fun_grammar();
    let cases = invalid_expressions_cases();

    println!("\n=== Fun Invalid Expressions ({} cases) ===", cases.len());

    let (res, _cases_json) = run_parse_batch(&grammar, &cases);

    assert_eq!(res.failed, 0, "{}", res.format_failures());

    println!(
        "✓ All {} cases passed in {:?} (avg {:?})",
        res.passed, res.total_duration, res.avg_duration
    );
}
