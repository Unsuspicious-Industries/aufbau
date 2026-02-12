use super::*;

fn imp_grammar() -> Grammar {
    load_example_grammar("imp")
}

pub fn valid_expressions_cases() -> Vec<ParseTestCase> {
    vec![
        ParseTestCase::valid("assign int", "{ let x:Int=5; }"),
        ParseTestCase::valid("assign int negative", "{ let x:Int=0-5; }"),
        ParseTestCase::valid("assign arithmetic", "{ let x:Int=1+2; }"),
        ParseTestCase::valid("assign bool", "{ let flag:Bool=true; }"),
        ParseTestCase::valid("assign union bool", "{ let u:Int|Bool=true; }"),
        ParseTestCase::valid("operation", "{ let x:Int=5; let y:Int=3; let z:Int=x+y; }"),
        ParseTestCase::valid("sequential var reuse", "{ let x:Int=5; let y:Int=x; }"),
        ParseTestCase::valid("sequential var expr", "{ let x:Int=5; let y:Int=x+1; }"),
        ParseTestCase::valid("parentheses", "{ let x:Int=5; let y:Int=(x+1); }"),
        ParseTestCase::valid(
            "if expression",
            "{ if (1==1) { let x:Int=1; } else { let x:Int=2; } }",
        ),
        ParseTestCase::valid("while expression", "{ while (1==1) { let x:Int=1; } }"),
    ]
}

pub fn invalid_expressions_cases() -> Vec<ParseTestCase> {
    vec![
        ParseTestCase::type_error("unbound var", "{ let y:Int=x; }"),
        ParseTestCase::type_error("unbound var", "{ let y:Int=1; let z:Int=y-x; }"),
        ParseTestCase::type_error("union mismatch", "{ let u:Int|Bool=1+2; let z:Int=u+1; }"),
        ParseTestCase::type_error(
            "if branch context isolation",
            "{ if (1==1) { let x:Int=1; } else { let y:Int=x; } }",
        ),
        ParseTestCase::type_error(
            "while body does not leak bindings",
            "{ while (1==1) { let x:Int=1; } let y:Int=x; }",
        ),
        ParseTestCase::invalid(
            "if condition missing comparator",
            "{ if 1 { let x:Int=1; } else { let x:Int=2; } }",
        ),
        ParseTestCase::type_error("while condition type mismatch", "{ while (1==true) { let x:Int=1; } }"),
    ]
}

#[test]
fn valid_expressions_imp() {
    let grammar = imp_grammar();
    let cases = valid_expressions_cases();

    println!("\n=== IMP Valid Expressions ({} cases) ===", cases.len());
    let (res, _cases_json) = run_parse_batch(&grammar, &cases);
    assert_eq!(res.failed, 0, "{}", res.format_failures());
    println!(
        "All {} tests passed in {:?} (avg: {:?})",
        cases.len(),
        res.total_duration,
        res.avg_duration
    );
}

#[test]
fn invalid_expressions_imp() {
    let grammar = imp_grammar();
    let cases = invalid_expressions_cases();
    println!("\n=== IMP Invalid Expressions ({} cases) ===", cases.len());
    let (res, _cases_json) = run_parse_batch(&grammar, &cases);
    assert_eq!(res.failed, 0, "{}", res.format_failures());
    println!(
        "All {} tests passed in {:?} (avg: {:?})",
        cases.len(),
        res.total_duration,
        res.avg_duration
    );
}
