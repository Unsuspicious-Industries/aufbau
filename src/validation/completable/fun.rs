//! Fun Language Completability Tests
//!
//! Tests typed completion for the `fun` grammar — a functional language with:
//! - Integer, Float, and Boolean literals with distinct types
//! - Integer operators (+, -, *, /) and float operators (+., -., *., /.)
//! - Lambda expressions: (x: Type) => body
//! - Let bindings: let x: Type = value; body
//! - Function application: f(arg)
//! - Type annotations and checking (monomorphic, STLC-style)

use super::*;

/// Load Fun grammar from examples/fun.auf
pub fn fun_grammar() -> Grammar {
    load_example_grammar("fun")
}

// ============================================================================
// Suite Definitions (used by validate binary)
// ============================================================================

pub fn suites() -> Vec<(&'static str, Grammar, Vec<TypedCompletionTestCase>)> {
    let g = fun_grammar();
    vec![
        ("fun::literals", g.clone(), literal_cases()),
        ("fun::int_arithmetic", g.clone(), int_arith_cases()),
        ("fun::float_arithmetic", g.clone(), float_arith_cases()),
        ("fun::lambda", g.clone(), lambda_cases()),
        ("fun::let", g.clone(), let_cases()),
        ("fun::application", g.clone(), application_cases()),
        ("fun::parenthesized", g.clone(), paren_cases()),
        ("fun::variables", g.clone(), variable_cases()),
        ("fun::empty_prefix", g.clone(), empty_prefix_cases()),
        ("fun::prefixes", g, prefix_cases()),
    ]
}

use TypedCompletionTestCase as T;

fn literal_cases() -> Vec<TypedCompletionTestCase> {
    vec![
        T::ok("single digit", "1", 1),
        T::ok("multi digit", "42", 1),
        T::ok("large int", "9999", 1),
        T::ok("zero", "0", 1),
        T::ok("true", "true", 1),
        T::ok("false", "false", 1),
        T::ok("simple float", "1.0", 1),
        T::ok("multi float", "3.14", 1),
        T::ok("large float", "999.999", 1),
    ]
}

fn int_arith_cases() -> Vec<TypedCompletionTestCase> {
    vec![
        T::ok("int add", "1 + 2", 1),
        T::ok("int sub", "5 - 3", 1),
        T::ok("int mul", "2 * 3", 1),
        T::ok("int div", "6 / 2", 1),
        T::ok("add chain", "1 + 2 + 3", 1),
        T::ok("mixed int ops", "1 + 2 * 3", 1),
        T::ok("all int ops", "1 + 2 - 3 * 4 / 5", 1),
        T::ok("int plus partial", "1 +", 2),
        T::ok("int minus partial", "5 -", 2),
        T::ok("int mul partial", "3 *", 2),
        T::ok("int div partial", "6 /", 2),
        T::ok("paren int add", "(1 + 2)", 1),
        T::ok("nested paren int", "((1 + 2))", 1),
        T::ok("paren in expr", "(1 + 2) * 3", 1),
    ]
}

fn float_arith_cases() -> Vec<TypedCompletionTestCase> {
    vec![
        T::ok("float add", "1.0 +. 2.0", 1),
        T::ok("float sub", "5.0 -. 3.0", 1),
        T::ok("float mul", "2.0 *. 3.0", 1),
        T::ok("float div", "6.0 /. 2.0", 1),
        T::ok("float add chain", "1.0 +. 2.0 +. 3.0", 1),
        T::ok("float plus partial", "1.0 +.", 2),
        T::ok("float minus partial", "5.0 -.", 2),
        T::ok("paren float add", "(1.0 +. 2.0)", 1),
    ]
}

fn lambda_cases() -> Vec<TypedCompletionTestCase> {
    vec![
        T::ok("identity int", "(x: Int) => x", 1),
        T::ok("identity bool", "(b: Bool) => b", 1),
        T::ok("lambda body arith", "(x: Int) => x + 1", 1),
        T::ok("lambda body mul", "(x: Int) => x * 2", 1),
        T::ok("lambda float body", "(x: Float) => x +. 1.0", 1),
        T::ok("nested lambda", "(x: Int) => (y: Int) => x + y", 2),
        T::ok("higher order", "(f: Int -> Int) => f(1)", 1),
        T::ok("lambda open paren", "(", 6),
        T::ok("lambda param", "(x", 6),
        T::ok("lambda colon", "(x:", 5),
        T::ok("lambda typed", "(x: Int", 4),
        T::ok("lambda close paren", "(x: Int)", 3),
        T::ok("lambda arrow", "(x: Int) =>", 2),
    ]
}

fn let_cases() -> Vec<TypedCompletionTestCase> {
    vec![
        T::ok("let int", "let x: Int = 1; x", 1),
        T::ok("let bool true", "let b: Bool = true; b", 1),
        T::ok("let bool false", "let b: Bool = false; b", 1),
        T::ok("let float", "let f: Float = 3.14; f", 1),
        T::ok("let with arith body", "let x: Int = 1; x + 2", 1),
        T::ok("let use value in body", "let x: Int = 5; x * x", 1),
        T::ok("nested let", "let x: Int = 1; let y: Int = 2; x + y", 1),
        T::ok("let expr value", "let x: Int = 1 + 2; x", 1),
        T::ok("let float expr", "let x: Float = 1.0 +. 2.0; x", 1),
        T::ok("let keyword", "let", 7),
        T::ok("let name", "let x", 6),
        T::ok("let colon", "let x:", 5),
        T::ok("let typed", "let x: Int", 4),
        T::ok("let equals", "let x: Int =", 3),
        T::ok("let value", "let x: Int = 1", 2),
        T::ok("let semicolon", "let x: Int = 1;", 2),
    ]
}

fn application_cases() -> Vec<TypedCompletionTestCase> {
    vec![
        T::ok("apply id", "f(1)", 1).with_context(vec![("f", "Int -> Int")]),
        T::ok("apply bool fn", "f(true)", 1).with_context(vec![("f", "Bool -> Int")]),
        T::ok(
            "let fn then apply",
            "let f: Int -> Int = (x: Int) => x + 1; f(5)",
            2,
        ),
        T::ok(
            "let fn apply result",
            "let f: Int -> Int = (x: Int) => x; f(42)",
            2,
        ),
        T::ok(
            "higher-order nested application",
            "(f: Int -> Int) => ((x: Int) => f(x))",
            2,
        ),
        T::ok(
            "higher-order compose lambda",
            "(f: Int -> Int) => ((g: Int -> Int) => ((x: Int) => f(g(x))))",
            3,
        ),
        T::ok(
            "higher-order compose concrete arg",
            "(f: Int -> Int) => ((g: Int -> Int) => f(g(1)))",
            3,
        ),
        T::ok(
            "higher-order triple compose concrete arg",
            "(f: Int -> Int) => ((g: Int -> Int) => ((h: Int -> Int) => f(g(h(1)))))",
            4,
        ),
        T::ok("apply var arg", "f(x)", 1).with_context(vec![("f", "Int -> Int"), ("x", "Int")]),
    ]
}

fn paren_cases() -> Vec<TypedCompletionTestCase> {
    vec![
        T::ok("paren int", "(1)", 1),
        T::ok("paren bool", "(true)", 1),
        T::ok("paren float", "(1.0)", 1),
        T::ok("nested parens", "((1))", 1),
        T::ok("deep parens", "(((42)))", 1),
        T::ok("paren var", "(x)", 1).with_context(vec![("x", "Int")]),
    ]
}

fn variable_cases() -> Vec<TypedCompletionTestCase> {
    vec![
        T::ok("simple var", "x", 1).with_context(vec![("x", "Int")]),
        T::ok("multi char var", "foo", 1).with_context(vec![("foo", "Bool")]),
        T::ok("var in expr", "x + y", 1).with_context(vec![("x", "Int"), ("y", "Int")]),
        T::ok("float vars", "a +. b", 1).with_context(vec![("a", "Float"), ("b", "Float")]),
        T::ok("multiple ctx vars", "x", 1).with_context(vec![
            ("x", "Int"),
            ("y", "Bool"),
            ("z", "Float"),
        ]),
    ]
}

fn empty_prefix_cases() -> Vec<TypedCompletionTestCase> {
    vec![T::ok("empty input", "", 3)]
}

fn prefix_cases() -> Vec<TypedCompletionTestCase> {
    vec![
        T::ok("let keyword", "let", 7),
        T::ok("let name", "let x", 6),
        T::ok("let colon", "let x:", 5),
        T::ok("lambda param", "(x", 6),
        T::ok("lambda colon", "(x:", 5),
    ]
}

// ============================================================================
// Tests (delegates to shared case definitions)
// ============================================================================

#[test]
fn check_completable_literals() {
    let grammar = fun_grammar();
    let res = run_test_batch(&grammar, &literal_cases());
    res.assert_all_passed();
}

#[test]
fn check_completable_integer_arithmetic() {
    let grammar = fun_grammar();
    let res = run_test_batch(&grammar, &int_arith_cases());
    res.assert_all_passed();
}

#[test]
fn check_completable_float_arithmetic() {
    let grammar = fun_grammar();
    let res = run_test_batch(&grammar, &float_arith_cases());
    res.assert_all_passed();
}

#[test]
fn float_chain_with_int_operand_is_unsound() {
    let grammar = fun_grammar();
    let input = "let a : ( Float -> ( Float ) ) = 0.0 +. 0.0 +. 0 + ( let a : Float =";
    let result =
        crate::validation::completability::sound_complete(&grammar, input, 6, Some(Context::new()));

    assert!(
        !result.is_sound,
        "prefix soundness should reject float chains that already contain an int operand"
    );
}

#[test]
fn check_completable_lambda() {
    let grammar = fun_grammar();
    let res = run_test_batch(&grammar, &lambda_cases());
    res.assert_all_passed();
}

#[test]
fn check_completable_let() {
    let grammar = fun_grammar();
    let res = run_test_batch(&grammar, &let_cases());
    res.assert_all_passed();
}

#[test]
fn check_completable_application() {
    let grammar = fun_grammar();
    let res = run_test_batch(&grammar, &application_cases());
    res.assert_all_passed();
}

#[test]
fn check_completable_parenthesized() {
    let grammar = fun_grammar();
    let res = run_test_batch(&grammar, &paren_cases());
    res.assert_all_passed();
}

#[test]
fn check_completable_variables() {
    let grammar = fun_grammar();
    let res = run_test_batch(&grammar, &variable_cases());
    res.assert_all_passed();
}

#[test]
fn check_completable_empty_prefix() {
    let grammar = fun_grammar();
    let res = run_test_batch(&grammar, &empty_prefix_cases());
    res.assert_all_passed();
}

#[test]
fn check_completable_prefixes() {
    let grammar = fun_grammar();
    let res = run_test_batch(&grammar, &prefix_cases());
    res.assert_all_passed();
}
