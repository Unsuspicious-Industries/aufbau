use crate::logic::partial::parse::Parser;
use crate::validation::completable::get_completions;
use crate::{set_debug_level, testing::*};

fn imp_grammar() -> &'static crate::logic::grammar::Grammar {
    grammars::imp()
}

/// Quick, opt-in debug harness for IMP completion.
///
/// This is meant for fast iteration while debugging completability failures.
/// It's skipped unless you set `P7_DEBUG_IMP_COMPLETION=1`.
#[test]
fn imp_completion_debug_smoke() {
    set_debug_level(crate::DebugLevel::Trace);
    let grammar = imp_grammar();

    // Start with the smallest possible input that currently fails in the
    // `validation::completable::imp` batch: empty string.
    let input = "x";
    let max_depth = 3;

    // 1) Token-level completion suggestions from the partial parser.
    let tokens = get_completions(grammar, input);
    eprintln!("\n=== IMP completion debug ===");
    eprintln!("input={:?} max_depth={}", input, max_depth);
    eprintln!("token completions ({}):", tokens.len());
    for (i, t) in tokens.iter().take(50).enumerate() {
        eprintln!("  {:>2}. {}", i + 1, t);
    }
}

#[test]
fn imp_typing_fails_on_unbound_var_in_assignment_rhs() {
    set_debug_level(crate::DebugLevel::Trace);
    // RHS uses `x` before any `x:Int=...;` appears, so typing must fail.
    let grammar = imp_grammar();
    let mut parser = Parser::new(grammar.clone());

    let res = parser.partial_typed("y:Int=x;");
    match &res {
        Ok(_t) => {
            panic!("expected type error for unbound var `x`, but typing succeeded with type");
        }
        Err(e) => {
            println!("Received expected error: {}", e);
        }
    }
    assert!(
        res.is_err(),
        "expected type error for unbound var `x`, but typing succeeded"
    );
}

#[test]
fn imp_typing_fails_on_unbound_var_in_operation_rhs() {
    // `x` is unbound; `y` becomes bound after assignment.
    let grammar = imp_grammar();
    let mut parser = Parser::new(grammar.clone());

    let res = parser.partial_typed("y:Int=1;y-x;");
    assert!(
        res.is_err(),
        "expected type error for unbound var `x`, but typing succeeded"
    );
}

#[test]
fn imp_typing_accepts_union_assignment() {
    // Union annotation should accept either member type through inclusion.
    let grammar = imp_grammar();
    let mut parser = Parser::new(grammar.clone());

    let res = parser.partial_typed("u:Int|Bool=true;");
    assert!(
        res.is_ok(),
        "expected union assignment to typecheck, got {:?}",
        res.err()
    );
}

#[test]
fn imp_typing_rejects_union_in_int_operation() {
    // A union-typed variable cannot be used where Int is required for arithmetic.
    let grammar = imp_grammar();
    let mut parser = Parser::new(grammar.clone());

    let res = parser.partial_typed("u:Int|Bool=true; u+1;");
    assert!(
        res.is_err(),
        "expected union-int mismatch to fail, got {:?}",
        res.ok()
    );
}

#[test]
fn imp_typing_accepts_sequential_var_reuse() {
    let grammar = imp_grammar();
    let mut parser = Parser::new(grammar.clone());

    let res = parser.partial_typed("x:Int=5; y:Int=x;");
    assert!(
        res.is_ok(),
        "expected sequential var reuse to typecheck, got {:?}",
        res.err()
    );
}

#[test]
fn imp_typing_accepts_sequential_var_in_expr() {
    let grammar = imp_grammar();
    let mut parser = Parser::new(grammar.clone());

    let res = parser.partial_typed("x:Int=5; y:Int=x+1;");
    assert!(
        res.is_ok(),
        "expected sequential var use in expression to typecheck, got {:?}",
        res.err()
    );
}

#[test]
fn imp_if_branches_do_not_share_context() {
    let grammar = imp_grammar();
    let mut parser = Parser::new(grammar.clone());

    let res = parser.partial_typed("if 1==1 { x:Int=1; } else { y:Int=x; }");
    assert!(
        res.is_err(),
        "expected branch-local context isolation, got {:?}",
        res.ok()
    );
}

#[test]
fn imp_while_body_does_not_leak_bindings() {
    let grammar = imp_grammar();
    let mut parser = Parser::new(grammar.clone());

    let res = parser.partial_typed("while 1==1 { x:Int=1; } y:Int=x;");
    assert!(
        res.is_err(),
        "expected while-body bindings to stay local, got {:?}",
        res.ok()
    );
}
