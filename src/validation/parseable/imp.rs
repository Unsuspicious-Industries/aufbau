use super::ParseTestCase;
#[cfg(test)]
use {
    super::{load_example_grammar, run_parse_batch},
    crate::domains::typing::TypingDomain,
    crate::engine::grammar::SPG,
};

#[cfg(test)]
fn imp_grammar() -> SPG<TypingDomain> {
    load_example_grammar("imp")
}

#[must_use]
pub fn valid_expressions_cases() -> Vec<ParseTestCase> {
    let cases = vec![
        ParseTestCase::valid("assign int", "{ let x:Int=5; }"),
        ParseTestCase::valid("assign int negative", "{ let x:Int=0-5; }"),
        ParseTestCase::valid("assign arithmetic", "{ let x:Int=1+2; }"),
        ParseTestCase::valid("assign bool", "{ let flag:Bool=true; }"),
        ParseTestCase::valid("assign union bool", "{ let u:Int|Bool=true; }"),
        ParseTestCase::valid("operation", "{ let x:Int=5; let y:Int=3; let z:Int=x+y; }"),
        ParseTestCase::valid(
            "long decl chain",
            "{ let a:Int=1; let b:Int=2; let c:Int=3; let d:Int=4; let e:Int=5; let f:Int=6; let g:Int=7; let h:Int=8; }",
        ),
        ParseTestCase::valid("sequential var reuse", "{ let x:Int=5; let y:Int=x; }"),
        ParseTestCase::valid("sequential var expr", "{ let x:Int=5; let y:Int=x+1; }"),
        ParseTestCase::valid("parentheses", "{ let x:Int=5; let y:Int=(x+1); }"),
        ParseTestCase::valid(
            "if expression",
            "{ if (1==1) { let x:Int=1; } else { let x:Int=2; } }",
        ),
        ParseTestCase::valid("while expression", "{ while (1==1) { let x:Int=1; } }"),
    ];

    cases
}

#[must_use]
pub fn invalid_expressions_cases() -> Vec<ParseTestCase> {
    vec![
        ParseTestCase::invalid("assign before decl", "{x=5;"),
        ParseTestCase::invalid("missing type for declaration", "{let x=5;"),
        ParseTestCase::invalid("missing value", "{let x:Int;"),
        ParseTestCase::invalid("wrong type token", "{let x:String=5;"),
        ParseTestCase::invalid("lowercase type", "{let x:int=5;"),
        ParseTestCase::invalid("invalid operator", "{let x:Int=5%2;"),
        ParseTestCase::invalid("operator first", "{let x:Int=+5;"),
        ParseTestCase::invalid("double operator", "{let x:Int=1++2;"),
        ParseTestCase::invalid("extra close paren", "{let x:Int=(1+2));"),
        ParseTestCase::invalid("missing close paren", "{let x:Int=(1+2;"),
        ParseTestCase::invalid("missing open brace", "let"),
        ParseTestCase::invalid("close brace first", "}"),
        ParseTestCase::invalid("random chars", "@#$;"),
        ParseTestCase::invalid("missing block close", "{ let x:Int=1; "),
        ParseTestCase::invalid(
            "if missing else block brace",
            "{ if (1==1) { let x:Int=1; } else let y:Int=2; }",
        ),
        ParseTestCase::invalid(
            "while missing condition close",
            "{ while (1==1 { let x:Int=1; } }",
        ),
        ParseTestCase::invalid("unbound var", "{ let y:Int=x; }"),
        ParseTestCase::invalid("unbound var", "{ let y:Int=1; let z:Int=y-x; }"),
        ParseTestCase::invalid("use before decl", "{ let y:Int=x+1; let x:Int=5; }"),
        ParseTestCase::invalid("union mismatch", "{ let u:Int|Bool=1+2; let z:Int=u+1; }"),
        ParseTestCase::invalid(
            "union used as int",
            "{ let u:Int|Bool=true; let z:Int=u+1; }",
        ),
        ParseTestCase::invalid(
            "if branch context isolation",
            "{ if (1==1) { let x:Int=1; } else { let y:Int=x; } }",
        ),
        ParseTestCase::invalid(
            "while body does not leak bindings",
            "{ while (1==1) { let x:Int=1; } let y:Int=x; }",
        ),
        ParseTestCase::invalid(
            "if condition missing comparator",
            "{ if 1 { let x:Int=1; } else { let x:Int=2; } }",
        ),
        ParseTestCase::invalid(
            "while condition type mismatch",
            "{ while (1==true) { let x:Int=1; } }",
        ),
        ParseTestCase::invalid("bool declaration from int", "{ let flag:Bool=1; }"),
        ParseTestCase::invalid("assignment from bool to int", "{ let x:Int=1; x=true; }"),
    ]
}

#[test]
fn valid_expressions_imp() {
    let mut grammar = imp_grammar();
    let cases = valid_expressions_cases();

    println!("\n=== IMP Valid Expressions ({} cases) ===", cases.len());
    let (res, _cases_json) = run_parse_batch(&mut grammar, &cases);
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
    let mut grammar = imp_grammar();
    let cases = invalid_expressions_cases();
    println!("\n=== IMP Invalid Expressions ({} cases) ===", cases.len());
    let (res, _cases_json) = run_parse_batch(&mut grammar, &cases);
    assert_eq!(res.failed, 0, "{}", res.format_failures());
    println!(
        "All {} tests passed in {:?} (avg: {:?})",
        cases.len(),
        res.total_duration,
        res.avg_duration
    );
}
