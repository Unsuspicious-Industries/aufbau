use core::panic;

use crate::logic::partial::parse::Parser;
use crate::logic::partial::Synthesizer;
use crate::logic::typing::core::Context;
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
    // Start with the smallest possible input that currently fails in the
    // `validation::completable::imp` batch: empty string.
    let input = "{";
    let grammar = imp_grammar();
    let mut synth = Synthesizer::new(grammar.clone(), input);

    // 1) Token-level completion suggestions from the partial parser.
    let ctx = Context::new();
    let tokens = synth.typed_completions(&ctx);
    eprintln!("\n=== IMP completion debug ===");
    eprintln!("token completions ({}):", tokens.len());
    for (i, t) in tokens.iter().take(50).enumerate() {
        eprintln!("  {:>2}. {}", i + 1, t);
    }

    let mut current_input = input.to_string();
    // pseudo-random rotation seed for selecting completions deterministically
    let mut prng: u64 = 0x9e3779b977c8715u64;
    // have a completion loop
    for i in 0..100 {
        synth.set_input(current_input.clone());
        let tokens = synth.typed_completions(&ctx);
        println!("Tokens at iteration {}: ", i);
        for (j, t) in tokens.iter().take(10).enumerate() {
            println!("  {:>2}. {}", j + 1, t);
        }
        if tokens.is_empty() {
            // if the tree is complete yay else nay
            if let Some(croot) = synth.complete() {
                println!("Complete tree found at iteration {}: {}", i, croot);
                return;
            } else {
                panic!(
                    "No completions found at iteration {}, input={:?}",
                    i, current_input
                );
            }
        }

        // rotate PRNG (LCG) and pick an index into tokens
        prng = prng.wrapping_mul(636413923846793005).wrapping_add(1);
        let idx = (prng as usize) % tokens.len();
        let t = tokens.iter().nth(idx).expect("token index out of range");

        let example = match t.example() {
            Some(value) => value,
            None => {
                println!("Skipped token without example at iteration {}", i);
                continue;
            }
        };

        match synth.try_extend(&example, &ctx) {
            Ok((_partial, extended)) => current_input = extended,
            Err(_) => {
                println!("Rejected token at iteration {}: {}", i, example);
                continue;
            }
        }
        println!("Current input after iteration {}: {:?}", i, current_input);
    }
    eprintln!("\n=== Cache report ===\n{}", "(no cache report)");
    eprintln!("Final completed input: {:?}", current_input);
}

#[test]
fn imp_typing_fails_on_unbound_var_in_assignment_rhs() {
    set_debug_level(crate::DebugLevel::Trace);
    // RHS uses `x` before any `let x:Int=...;` appears, so typing must fail.
    let grammar = imp_grammar();
    let mut parser = Parser::new(grammar.clone());

    let res = parser.partial_typed("{ let y:Int=x; }");
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

    let res = parser.partial_typed("{ let y:Int=1; y-x; }");
    assert!(
        res.is_err(),
        "expected type error for unbound var `x`, but typing succeeded"
    );
}

/*

// Not yet implemented
// WIP

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
*/

#[test]
fn imp_typing_accepts_sequential_var_reuse() {
    let grammar = imp_grammar();
    let mut parser = Parser::new(grammar.clone());

    let res = parser.partial_typed("{ let x:Int=5; let y:Int=x; }");
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

    let res = parser.partial_typed("{ let x:Int=5; let y:Int=x+1; }");
    assert!(
        res.is_ok(),
        "expected sequential var use in expression to typecheck, got {:?}",
        res.err()
    );
}

#[test]
fn imp_typing_rejects_non_bool_if_condition() {
    let grammar = imp_grammar();
    let mut parser = Parser::new(grammar.clone());

    let res = parser.partial_typed("{ if ( 0 ) { if ( 0 ) { let a : Int = 0 ; } }}");
    assert!(
        res.is_err(),
        "expected int condition to fail typing, got {:?}",
        res
    );
}

#[test]
fn imp_if_branches_do_not_share_context() {
    let grammar = imp_grammar();
    let mut parser = Parser::new(grammar.clone());

    let res = parser.partial_typed("if 1==1 { let x:Int=1; } else { let y:Int=x; }");
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

    let res = parser.partial_typed("{ while 1==1 { let x:Int=1; } let y:Int=x; }");
    assert!(
        res.is_err(),
        "expected while-body bindings to stay local, got {:?}",
        res.ok()
    );
}
