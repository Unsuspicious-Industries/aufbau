use super::*;

fn imp_grammar() -> Grammar {
    load_example_grammar("imp")
}

#[test]
fn valid_expressions_imp() {
    let grammar = imp_grammar();
    let cases = vec![
        ParseTestCase::valid("assign int", "x:Int=5;"),
        ParseTestCase::valid("assign int negative", "x:Int=-5;"),
        ParseTestCase::valid("assign arithmetic", "x:Int=1+2;"),
        ParseTestCase::valid("assign bool", "flag:Bool=true;"),
        ParseTestCase::valid("assign union bool", "u:Int|Bool=true;"),
        ParseTestCase::valid("operation", "x:Int=5; y:Int=3; x+y;"),
        ParseTestCase::valid("sequential var reuse", "x:Int=5; y:Int=x;"),
        ParseTestCase::valid("sequential var expr", "x:Int=5; y:Int=x+1;"),
        ParseTestCase::valid("parentheses", "x:Int=5; y:Int=3; x+y;"),
        ParseTestCase::valid("if expression", "if 1==1 { x:Int=1; } else { x:Int=2; }"),
        ParseTestCase::valid("while expression", "while 1==1 { x:Int=1; }"),
    ];

    println!("\n=== IMP Valid Expressions ({} cases) ===", cases.len());
    let res = run_parse_batch(&grammar, &cases);
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
    let cases = vec![
        ParseTestCase::type_error("unbound var", "y:Int=x;"),
        ParseTestCase::type_error("unbound var", "y:Int=1;y-x;"),
        ParseTestCase::type_error("union mismatch", "u:Int|Bool=1+2; u+1;"),
        ParseTestCase::type_error(
            "if branch context isolation",
            "if 1==1 { x:Int=1; } else { y:Int=x; }",
        ),
        ParseTestCase::type_error(
            "while body does not leak bindings",
            "while 1==1 { x:Int=1; } y:Int=x;",
        ),
        ParseTestCase::invalid("if condition missing comparator", "if 1 { x:Int=1; } else { x:Int=2; }"),
        ParseTestCase::type_error("while condition type mismatch", "while 1==true { x:Int=1; }"),
    ];
    println!("\n=== IMP Invalid Expressions ({} cases) ===", cases.len());
    let res = run_parse_batch(&grammar, &cases);
    assert_eq!(res.failed, 0, "{}", res.format_failures());
    println!(
        "All {} tests passed in {:?} (avg: {:?})",
        cases.len(),
        res.total_duration,
        res.avg_duration
    );
}
