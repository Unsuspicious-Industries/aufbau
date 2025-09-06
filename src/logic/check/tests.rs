use super::TypeChecker;
use crate::logic::grammar::tests::STLC_SPEC;
use crate::logic::grammar::Grammar;
use crate::logic::parser::Parser;
use crate::logic::debug::{set_debug_level, set_debug_input, DebugLevel};
use crate::debug_info;

#[test] fn stlc_simple_ok() {
    let expr = "(λy:a->a.y)((λx:a->a.x)z)";

    // Enable debug output for this test
    set_debug_level(DebugLevel::Info);
    set_debug_input(Some(expr.to_string()));

    let mut parser = Parser::new(
        Grammar::load(STLC_SPEC)
                        .unwrap()
    );

    let ast = parser.parse(expr).unwrap();
    debug_info!("test", "AST: {}", ast.pretty());

    let mut tc = TypeChecker::with_input(Some(expr.to_string()));
    // Add the free variable z to the context
    tc.bind("z".to_string(), crate::logic::typing::Type::Atom("a".to_string()));
    let res = tc.check(&ast);
    match res {
        Ok(ty) => {
            if let Some(ty) = ty {
                debug_info!("test", "Type: {}", ty);
            } else {
                debug_info!("test", "No type inferred");
            }
        }
        Err(e) => {
            debug_info!("test", "Error: {}", e);
            panic!("Type checking failed: {}", e);
        }
    }
}

#[test] fn stlc_type_mismatch_fails() {
    // This should fail: applying a function expecting Int to a Bool
    let expr = "(λx:Int.x)True";

    let mut parser = Parser::new(
        Grammar::load(STLC_SPEC).unwrap()
    );

    let ast = parser.parse(expr).unwrap();
    let mut tc = TypeChecker::with_input(Some(expr.to_string()));
    // Add True : Bool to context
    tc.bind("True".to_string(), crate::logic::typing::Type::Atom("Bool".to_string()));
    
    let res = tc.check(&ast);
    assert!(res.is_err(), "Expected type mismatch error but type checking succeeded");
    
    if let Err(e) = res {
        debug_info!("test", "Expected error: {}", e);
        assert!(e.contains("Type mismatch") || e.contains("expected Int, found Bool"));
    }
}

#[test] fn stlc_unbound_variable_fails() {
    // This should fail: unbound variable
    let expr = "(λx:a.x)y";

    let mut parser = Parser::new(
        Grammar::load(STLC_SPEC).unwrap()
    );

    let ast = parser.parse(expr).unwrap();
    let mut tc = TypeChecker::with_input(Some(expr.to_string()));
    // Don't add y to context - it should fail
    
    let res = tc.check(&ast);
    assert!(res.is_err(), "Expected unbound variable error but type checking succeeded");
    
    if let Err(e) = res {
        debug_info!("test", "Expected error: {}", e);
        assert!(e.contains("not found in context") || e.contains("unbound"));
    }
}

#[test] fn stlc_simple_lambda_ok() {
    // This should pass: simple lambda application
    let expr = "(λx:a.x)y";

    let mut parser = Parser::new(
        Grammar::load(STLC_SPEC).unwrap()
    );

    let ast = parser.parse(expr).unwrap();
    let mut tc = TypeChecker::with_input(Some(expr.to_string()));
    // Add variable to context
    tc.bind("y".to_string(), crate::logic::typing::Type::Atom("a".to_string()));
    
    let res = tc.check(&ast);
    assert!(res.is_ok(), "Expected type checking to succeed");
    
    if let Ok(Some(ty)) = res {
        assert_eq!(ty, crate::logic::typing::Type::Atom("a".to_string()));
        debug_info!("test", "Successfully inferred type: {}", ty);
    }
}