use super::*;

pub fn typescript_grammar() -> Grammar {
    load_example_grammar("typescript")
}

pub fn suites() -> Vec<(&'static str, Grammar, Vec<TypedCompletionTestCase>)> {
    vec![(
        "typescript::completable",
        typescript_grammar(),
        completable_cases(),
    )]
}

fn completable_cases() -> Vec<TypedCompletionTestCase> {
    vec![
        TypedCompletionTestCase::ok("number declaration", "let n: number = 1; n;", 10),
        TypedCompletionTestCase::ok("array declaration", "let xs: number[] = [1]; xs;", 10),
        TypedCompletionTestCase::ok(
            "function expression call",
            "const inc: (number) => number = (x: number) => x + 1; inc(2);",
            10,
        ),
        TypedCompletionTestCase::ok(
            "function declaration call",
            "function inc(x: number): number { return x + 1; } inc(2);",
            10,
        ),
        TypedCompletionTestCase::ok(
            "two arg function declaration call",
            "function add(x: number, y: number): number { return x + y; } add(1, 2);",
            10,
        ),
        TypedCompletionTestCase::ok(
            "two arg function expression call",
            "const pair: (number, string) => string = (n: number, s: string) => s; pair(1, \"ok\");",
            10,
        ),
        TypedCompletionTestCase::ok(
            "four arg function declaration call",
            "function sum4(a: number, b: number, c: number, d: number): number { return a + b + c + d; } sum4(1, 2, 3, 4);",
            10,
        ),
        TypedCompletionTestCase::ok(
            "zero arg function expression",
            "const one: () => number = () => 1; one();",
            10,
        ),
        TypedCompletionTestCase::ok(
            "multi element array",
            "let xs: number[] = [1, 2, 3, 4]; xs;",
            10,
        ),
        TypedCompletionTestCase::ok(
            "object declaration",
            "let user: { id: number, name: string } = { id: 1, name: \"ada\" }; user;",
            10,
        ),
        TypedCompletionTestCase::ok(
            "array of objects",
            "let users: { id: number }[] = [{ id: 1 }, { id: 2 }]; users;",
            10,
        ),
    ]
}

#[test]
fn check_completable_typescript() {
    let grammar = typescript_grammar();
    let res = run_test_batch(&grammar, &completable_cases());
    res.assert_all_passed();
}
