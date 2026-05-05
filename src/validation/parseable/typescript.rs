use super::*;

#[cfg(test)]
fn typescript_grammar() -> Grammar {
    load_example_grammar("typescript")
}

pub fn valid_expressions_cases() -> Vec<ParseTestCase> {
    vec![
        ParseTestCase::valid("number declaration", "let n: number = 1; n;"),
        ParseTestCase::valid("string declaration", "const name: string = \"ada\"; name;"),
        ParseTestCase::valid("boolean declaration", "let ok: boolean = true; ok;"),
        ParseTestCase::valid("array declaration", "let xs: number[] = [1]; xs;"),
        ParseTestCase::valid(
            "dependent declaration",
            "let x: number = 1; let y: number = x + 2; y;",
        ),
        ParseTestCase::valid(
            "function expression call",
            "const inc: (number) => number = (x: number) => x + 1; inc(2);",
        ),
        ParseTestCase::valid(
            "function declaration call",
            "function inc(x: number): number { return x + 1; } inc(2);",
        ),
        ParseTestCase::valid(
            "two arg function declaration call",
            "function add(x: number, y: number): number { return x + y; } add(1, 2);",
        ),
        ParseTestCase::valid(
            "three arg function declaration call",
            "function choose(a: boolean, b: number, c: number): number { return b; } choose(true, 1, 2);",
        ),
        ParseTestCase::valid(
            "four arg function declaration call",
            "function sum4(a: number, b: number, c: number, d: number): number { return a + b + c + d; } sum4(1, 2, 3, 4);",
        ),
        ParseTestCase::valid(
            "two arg function expression call",
            "const pair: (number, string) => string = (n: number, s: string) => s; pair(1, \"ok\");",
        ),
        ParseTestCase::valid(
            "four arg function expression call",
            "const sum4: (number, number, number, number) => number = (a: number, b: number, c: number, d: number) => a + b + c + d; sum4(1, 2, 3, 4);",
        ),
        ParseTestCase::valid(
            "zero arg function declaration",
            "function one(): number { return 1; } one();",
        ),
        ParseTestCase::valid(
            "zero arg function expression",
            "const one: () => number = () => 1; one();",
        ),
        ParseTestCase::valid(
            "multi element array",
            "let xs: number[] = [1, 2, 3, 4]; xs;",
        ),
        ParseTestCase::valid("empty typed array", "let xs: number[] = []; xs;"),
        ParseTestCase::valid(
            "object declaration",
            "let user: { id: number, name: string } = { id: 1, name: \"ada\" }; user;",
        ),
        ParseTestCase::valid(
            "object fields order independent",
            "let user: { id: number, name: string } = { name: \"ada\", id: 1 }; user;",
        ),
        ParseTestCase::valid(
            "array of objects",
            "let users: { id: number }[] = [{ id: 1 }, { id: 2 }]; users;",
        ),
        ParseTestCase::valid("strict equality", "let n: number = 1; n === 1;"),
    ]
}

pub fn invalid_expressions_cases() -> Vec<ParseTestCase> {
    vec![
        ParseTestCase::invalid("unbound variable", "x;"),
        ParseTestCase::invalid("wrong declaration type", "let n: number = \"x\"; n;"),
        ParseTestCase::invalid("numeric op with string", "\"x\" + 1;"),
        ParseTestCase::invalid(
            "wrong call argument",
            "const f: (number) => number = (x: number) => x; f(\"no\");",
        ),
        ParseTestCase::invalid(
            "wrong function return",
            "function bad(x: number): string { return x + 1; }",
        ),
        ParseTestCase::invalid("array element mismatch", "let xs: number[] = [\"x\"]; xs;"),
        ParseTestCase::invalid(
            "wrong function assignment",
            "const f: (number) => string = (x: number) => x + 1;",
        ),
        ParseTestCase::invalid(
            "wrong second call argument",
            "function add(x: number, y: number): number { return x + y; } add(1, \"bad\");",
        ),
        ParseTestCase::invalid(
            "wrong fourth call argument",
            "function sum4(a: number, b: number, c: number, d: number): number { return a + b + c + d; } sum4(1, 2, 3, \"bad\");",
        ),
        ParseTestCase::invalid(
            "wrong multi arg function assignment",
            "const pair: (number, string) => number = (n: number, s: string) => s;",
        ),
        ParseTestCase::invalid(
            "zero arg call with argument",
            "function one(): number { return 1; } one(1);",
        ),
        ParseTestCase::invalid(
            "mixed array elements",
            "let xs: number[] = [1, \"bad\"]; xs;",
        ),
        ParseTestCase::invalid(
            "object field type mismatch",
            "let user: { id: number, name: string } = { id: \"bad\", name: \"ada\" }; user;",
        ),
        ParseTestCase::invalid(
            "object missing field",
            "let user: { id: number, name: string } = { id: 1 }; user;",
        ),
        ParseTestCase::invalid(
            "object wrong field name",
            "let user: { id: number } = { name: 1 }; user;",
        ),
        ParseTestCase::invalid("use before declaration", "x; let x: number = 1;"),
    ]
}

#[test]
fn valid_expressions_typescript() {
    let mut grammar = typescript_grammar();
    let cases = valid_expressions_cases();
    let (res, _cases_json) = run_parse_batch(&mut grammar, &cases);
    assert_eq!(res.failed, 0, "{}", res.format_failures());
}

#[test]
fn invalid_expressions_typescript() {
    let mut grammar = typescript_grammar();
    let cases = invalid_expressions_cases();
    let (res, _cases_json) = run_parse_batch(&mut grammar, &cases);
    assert_eq!(res.failed, 0, "{}", res.format_failures());
}
