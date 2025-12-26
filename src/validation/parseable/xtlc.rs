//! XTLC Parseability Tests
//!
//! Fast tests for Extended Typed Lambda Calculus that verify
//! all prefixes are parseable without doing full completion search.

use super::*;

fn xtlc_grammar() -> Grammar {
    load_example_grammar("xtlc")
}

#[test]
fn valid_expressions_xtlc() {
    let grammar = xtlc_grammar();
    let cases = vec![
        // === simple partial cases
        ParseTestCase::valid("empty lambda", "λ"),
        ParseTestCase::valid("lambda var", "λx"),
        ParseTestCase::valid("lambda colon", "λx:"),
        ParseTestCase::valid("lambda type dec", "{y:B}λx:A"),
        ParseTestCase::valid("extra open paren", "((λx:A.x)"),
        ParseTestCase::valid("mismatched parens", "(λx:A.x)("),
        ParseTestCase::valid("partial no body after dot", "λx:A."),
        // === Identity functions ===
        ParseTestCase::valid("identity A", "λx:A.x"),
        ParseTestCase::valid("identity B", "λx:B.x"),
        ParseTestCase::valid("identity C", "λx:C.x"),
        ParseTestCase::valid("identity with parens", "(λx:A.x)"),
        ParseTestCase::valid("identity double parens", "((λx:A.x))"),
        // === Nested lambdas (2 deep) ===
        ParseTestCase::valid("nested return outer", "λx:A.λy:B.x"),
        ParseTestCase::valid("nested return inner", "λx:A.λy:B.y"),
        ParseTestCase::valid("nested same type", "λx:A.λy:A.x"),
        ParseTestCase::valid("nested same type inner", "λx:A.λy:A.y"),
        // === Nested lambdas (3 deep) ===
        ParseTestCase::valid("triple return first", "λx:A.λy:B.λz:C.x"),
        ParseTestCase::valid("triple return second", "λx:A.λy:B.λz:C.y"),
        ParseTestCase::valid("triple return third", "λx:A.λy:B.λz:C.z"),
        // === Nested lambdas (4+ deep) ===
        ParseTestCase::valid("quad nested", "λa:A.λb:B.λc:C.λd:D.a"),
        ParseTestCase::valid("quad return last", "λa:A.λb:B.λc:C.λd:D.d"),
        ParseTestCase::valid("penta nested", "λa:A.λb:B.λc:C.λd:D.λe:E.e"),
        ParseTestCase::valid("hexa nested", "λa:A.λb:B.λc:C.λd:D.λe:E.λf:F.a"),
        // === Simple arrow types ===
        ParseTestCase::valid("arrow A->B", "λf:A->B.f"),
        ParseTestCase::valid("arrow B->C", "λf:B->C.f"),
        ParseTestCase::valid("arrow A->A", "λf:A->A.f"),
        // === Nested arrow types (right associative) ===
        ParseTestCase::valid("arrow A->B->C", "λf:A->B->C.f"),
        ParseTestCase::valid("arrow A->B->C->D", "λf:A->B->C->D.f"),
        ParseTestCase::valid("arrow A->B->C->D->E", "λf:A->B->C->D->E.f"),
        // === Parenthesized arrow types ===
        ParseTestCase::valid("paren arrow (A->B)->C", "λf:(A->B)->C.f"),
        ParseTestCase::valid("paren arrow A->(B->C)", "λf:A->(B->C).f"),
        ParseTestCase::valid("paren arrow ((A->B)->C)->D", "λf:((A->B)->C)->D.f"),
        ParseTestCase::valid("paren arrow (A->(B->C))->D", "λf:(A->(B->C))->D.f"),
        ParseTestCase::valid("paren arrow A->((B->C)->D)", "λf:A->((B->C)->D).f"),
        ParseTestCase::valid("complex nested arrows", "λf:(A->B)->(C->D).f"),
        // === Function application ===
        ParseTestCase::valid("apply f to x", "λf:A->B.λx:A.f x"),
        ParseTestCase::valid("apply in parens", "λf:A->B.λx:A.(f x)"),
        ParseTestCase::valid("double apply", "λf:A->B->C.λx:A.λy:B.f x y"),
        ParseTestCase::valid("double apply parens left", "λf:A->B->C.λx:A.λy:B.(f x) y"),
        ParseTestCase::valid("double apply parens full", "λf:A->B->C.λx:A.λy:B.((f x) y)"),
        ParseTestCase::valid("triple apply", "λf:A->B->C->D.λx:A.λy:B.λz:C.f x y z"),
        // === Lambda as argument ===
        ParseTestCase::valid("lambda as arg simple", "λf:(A->B)->C.f (λx:A.x)"),
        ParseTestCase::valid("lambda as arg nested", "λf:((A->B)->C)->D.f (λg:A->B.g)"),
        // === Let bindings ===
        ParseTestCase::valid("let simple x", "{x:A}x"),
        ParseTestCase::valid("let simple y", "{y:B}y"),
        ParseTestCase::valid("let simple z", "{z:C}z"),
        ParseTestCase::valid("let seq 2", "{x:A}{y:B}x"),
        ParseTestCase::valid("let seq 2 inner", "{x:A}{y:B}y"),
        ParseTestCase::valid("let seq 3", "{x:A}{y:B}{z:C}x"),
        ParseTestCase::valid("let seq 3 middle", "{x:A}{y:B}{z:C}y"),
        ParseTestCase::valid("let seq 3 last", "{x:A}{y:B}{z:C}z"),
        ParseTestCase::valid("let seq 4", "{a:A}{b:B}{c:C}{d:D}a"),
        // === Let with lambda body ===
        ParseTestCase::valid("let with lambda", "{x:A}λy:B.x"),
        ParseTestCase::valid("let with lambda inner", "{x:A}λy:B.y"),
        ParseTestCase::valid("let with nested lambda", "{x:A}λy:B.λz:C.x"),
        ParseTestCase::valid("let with nested lambda mid", "{x:A}λy:B.λz:C.y"),
        ParseTestCase::valid("let with nested lambda inner", "{x:A}λy:B.λz:C.z"),
        // === Let with application ===
        ParseTestCase::valid("let apply", "{f:A->B}{x:A}f x"),
        ParseTestCase::valid("let apply parens", "{f:A->B}{x:A}(f x)"),
        ParseTestCase::valid("let double apply", "{f:A->B->C}{x:A}{y:B}f x y"),
        // === Variable shadowing ===
        ParseTestCase::valid("shadow x with x", "λx:A.λx:B.x"),
        ParseTestCase::valid("shadow triple", "λx:A.λx:B.λx:C.x"),
        ParseTestCase::valid("shadow with arrow type", "λx:A.λx:A->B.x"),
        ParseTestCase::valid("shadow in let", "{x:A}{x:B}x"),
        ParseTestCase::valid("shadow let then lambda", "{x:A}λx:B.x"),
        // === Church encodings structure ===
        ParseTestCase::valid("church true", "λt:A.λf:A.t"),
        ParseTestCase::valid("church false", "λt:A.λf:A.f"),
        ParseTestCase::valid("church pair first", "λx:A.λy:B.λp:A->B->C.p x y"),
        // === Combinator structures ===
        ParseTestCase::valid("I combinator", "λx:A.x"),
        ParseTestCase::valid("K combinator", "λx:A.λy:B.x"),
        ParseTestCase::valid("K* combinator", "λx:A.λy:B.y"),
        ParseTestCase::valid("B combinator shape", "λf:B->C.λg:A->B.λx:A.f (g x)"),
        ParseTestCase::valid("C combinator shape", "λf:A->B->C.λy:B.λx:A.f x y"),
        // === Complex parenthesization ===
        ParseTestCase::valid("paren identity", "(λx:A.x)"),
        ParseTestCase::valid("paren nested", "(λx:A.(λy:B.x))"),
        ParseTestCase::valid("paren deep", "((λx:A.((λy:B.x))))"),
        ParseTestCase::valid("paren in body", "λx:A.(x)"),
        ParseTestCase::valid("paren in type", "λx:(A).x"),
        ParseTestCase::valid("paren everywhere", "(λx:(A).(x))"),
        // === Mixed constructs ===
        ParseTestCase::valid("let then apply lambda", "{f:A->B}(λx:A.f x)"),
        ParseTestCase::valid("apply let result", "λf:(A->B)->C.f {x:A->B}x"),
        ParseTestCase::valid("complex mix 1", "{f:A->B->C}{x:A}λy:B.f x y"),
        // === Edge cases ===
        ParseTestCase::valid("single char vars", "λa:A.a"),
        ParseTestCase::valid("longer var names", "λfoo:A.foo"),
        ParseTestCase::valid("var with digits", "λx1:A.x1"),
        ParseTestCase::valid("many parens type", "λf:(((A->B))).f"),
        // f (f (f x)) requires f : A -> A (endomorphism), not A -> A -> A -> A
        ParseTestCase::valid("deeply nested app", "λf:A->A.λx:A.f (f (f x))"),
    ];

    println!("\n=== XTLC Valid Expressions ({} cases) ===", cases.len());
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
fn invalid_expressions_xtlc() {
    let grammar = xtlc_grammar();
    let cases = vec![
        // === Syntax errors: missing parts ===
        ParseTestCase::invalid("double colon", "λx::A.x"),
        ParseTestCase::invalid("missing var after lambda", "λ:A.x"),
        ParseTestCase::invalid("missing type after colon", "λx:.x"),
        ParseTestCase::invalid("missing colon", "λx A.x"),
        ParseTestCase::invalid("missing dot", "λx:A x"),
        ParseTestCase::invalid("just colon", ":"),
        ParseTestCase::invalid("just dot", "."),
        ParseTestCase::invalid("just arrow", "->"),
        // === Syntax errors: wrong order ===
        ParseTestCase::invalid("close paren first", ")"),
        ParseTestCase::invalid("arrow first", "->A"),
        ParseTestCase::invalid("dot first", ".x"),
        ParseTestCase::invalid("colon first", ":A"),
        // === Syntax errors: mismatched parens ===
        ParseTestCase::invalid("extra close paren", "(λx:A.x))"),
        ParseTestCase::invalid("wrong paren order", ")λx:A.x("),
        ParseTestCase::invalid("unmatched in type", "λx:(A.x"),
        ParseTestCase::invalid("unmatched in type 2", "λx:A).x"),
        // === Syntax errors: double operators ===
        ParseTestCase::invalid("double arrow", "λf:A-->B.f"),
        ParseTestCase::invalid("double dot", "λx:A..x"),
        ParseTestCase::invalid("double lambda", "λλx:A.x"),
        ParseTestCase::invalid("double colon in type", "λf:A::B.f"),
        // Weird invalid mixes
        ParseTestCase::invalid("nested let in lambda", "λx:A.{y:B}x"),
        // === Syntax errors: let bindings ===
        ParseTestCase::invalid("let missing brace", "{x:A x"),
        ParseTestCase::invalid("let missing colon", "{x A}x"),
        ParseTestCase::invalid("let missing type", "{x:}x"),
        ParseTestCase::invalid("let missing var", "{:A}x"),
        ParseTestCase::invalid("let empty", "{}x"),
        // === Type errors: unbound variables ===
        ParseTestCase::type_error("unbound x alone", "x"),
        ParseTestCase::type_error("unbound y alone", "y"),
        ParseTestCase::type_error("unbound z alone", "z"),
        ParseTestCase::type_error("unbound foo", "foo"),
        ParseTestCase::type_error("unbound in lambda body", "λx:A.y"),
        ParseTestCase::type_error("unbound in nested body", "λx:A.λy:B.z"),
        ParseTestCase::type_error("unbound in triple nested", "λx:A.λy:B.λz:C.w"),
        ParseTestCase::type_error("unbound after scope", "λx:A.x y"),
        ParseTestCase::type_error("unbound in application", "λf:A->B.f y"),
        ParseTestCase::type_error("unbound second arg", "λf:A->B->C.λx:A.f x y"),
        ParseTestCase::type_error("type before var", "λA:x.x"),
        // === Type errors: unbound in let ===
        ParseTestCase::type_error("unbound before let", "x {x:A}"),
        ParseTestCase::type_error("unbound in let body wrong var", "{x:A}y"),
        ParseTestCase::type_error("unbound after let scope", "{x:A}x y"),
        // === Type errors: scope issues ===
        ParseTestCase::type_error("var escapes lambda", "(λx:A.x) x"),
        ParseTestCase::type_error("inner var escapes", "λx:A.(λy:B.y) y"),
        // === Invalid token sequences ===
        ParseTestCase::invalid("arrow arrow", "λf:A->->B.f"),
        ParseTestCase::invalid("colon arrow", "λx::->A.x"),
        ParseTestCase::invalid("dot dot", "λx:A.λy:B..x"),
        ParseTestCase::invalid("lambda dot", "λ.x"),
        ParseTestCase::invalid("lambda arrow", "λ->x"),
        ParseTestCase::invalid("empty parens type", "λx:().x"),
        ParseTestCase::invalid("arrow no right", "λx:A->.x"),
        ParseTestCase::invalid("arrow no left", "λx:->B.x"),
    ];

    println!("\n=== XTLC Invalid Expressions ({} cases) ===", cases.len());
    let res = run_parse_batch(&grammar, &cases);
    assert_eq!(res.failed, 0, "{}", res.format_failures());
    println!(
        "All {} tests passed in {:?} (avg: {:?})",
        cases.len(),
        res.total_duration,
        res.avg_duration
    );
}
