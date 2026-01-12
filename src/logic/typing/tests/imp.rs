use crate::logic::partial::parse::Parser;
use crate::validation::completability::sound_complete;
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
    assert!(res.is_err(), "expected type error for unbound var `x`, but typing succeeded");
}

#[test]
fn imp_typing_fails_on_unbound_var_in_operation_rhs() {
    // `x` is unbound; `y` becomes bound after assignment.
    let grammar = imp_grammar();
    let mut parser = Parser::new(grammar.clone());

    let res = parser.partial_typed("y:Int=1;y-x;");
    assert!(res.is_err(), "expected type error for unbound var `x`, but typing succeeded");
}
