use super::*;

// Empirical depth bound for parseable prefix checks on the Fun grammar.
// The grammar is highly ambiguous around application/lambda prefixes; using
// a uniform bound avoids short-prefix false negatives from under-budgeting.
const FUN_PARSE_MAX_DEPTH: usize = 27;

#[cfg(test)]
fn fun_grammar() -> Grammar {
    load_example_grammar("fun")
}

pub fn valid_expressions_cases() -> Vec<ParseTestCase> {
    let cases = vec![
        // === Literals ===
        ParseTestCase::structural("integer literal", "42"),
        ParseTestCase::structural("zero", "0"),
        ParseTestCase::structural("float literal", "3.14"),
        ParseTestCase::structural("boolean true", "true"),
        ParseTestCase::structural("boolean false", "false"),
        // === Arithmetic ===
        ParseTestCase::structural("int addition", "1 + 2"),
        ParseTestCase::structural("int multiplication", "3 * 4"),
        ParseTestCase::structural("float addition", "1.0 +. 2.5"),
        ParseTestCase::structural("float division", "10.0 /. 2.0"),
        ParseTestCase::structural("float op with completable int", "1.0 +. 2"),
        // === Lambda ===
        ParseTestCase::structural("simple lambda", "(x: Int) => x + 1").with_parse_max_depth(10),
        ParseTestCase::structural("float lambda", "(x: Float) => x *. 2.0")
            .with_parse_max_depth(10),
        // === Application ===
        ParseTestCase::structural("lambda application", "((x: Int) => x + 1)(41)")
            .with_parse_max_depth(12),
        // === Let binding ===
        ParseTestCase::structural("simple let", "let n: Int = 12; n + 1"),
    ];

    cases
        .into_iter()
        .map(|c| c.with_parse_max_depth(FUN_PARSE_MAX_DEPTH))
        .collect()
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

#[test]
#[ignore = "depth probe - run manually to diagnose fun grammar performance"]
fn probe_fun_parse_depth() {
    use crate::logic::partial::MetaParser;
    use std::time::Instant;
    let grammar = fun_grammar();
    let cases = [
        ("simple", "42"),
        ("lambda", "(x: Int) => x + 1"),
        ("nested app", "(f: Int -> Int) => ((x: Int) => f(x))"),
        (
            "double compose",
            "(f: Int -> Int) => ((g: Int -> Int) => f(g(1)))",
        ),
        (
            "triple compose",
            "(f: Int -> Int) => ((g: Int -> Int) => ((h: Int -> Int) => f(g(h(1)))))",
        ),
        (
            "higher-order app",
            "(f: Int ) => (g: Int -> Int) => (x: Int) => f + g(x)",
        ),
    ];
    for (name, input) in &cases {
        println!("\n--- {} ({} chars) ---", name, input.len());
        for &depth in &[5usize, 8, 10, 12, 15, 18, 20, 25, 30] {
            let start = Instant::now();
            let mut parser = MetaParser::new(grammar.clone())
                .with_max_depth(depth)
                .with_start_depth(depth);
            let res = parser.partial(input);
            let elapsed = start.elapsed();
            println!(
                "  depth={:2}: {} in {:?}",
                depth,
                if res.is_ok() { "OK  " } else { "FAIL" },
                elapsed
            );
            if elapsed.as_secs() > 5 {
                println!("  (too slow, stopping)");
                break;
            }
        }
    }
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
