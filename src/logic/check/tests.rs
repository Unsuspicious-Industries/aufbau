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

    let mut tc = TypeChecker::new();
    // Add the free variable z to the context
    tc.add("z".to_string(), crate::logic::bind::typing::BoundType::Atom("a".to_string()));
    let res = tc.check(&ast);
    match res {
        Ok(ty) => {
            if let Some(ty) = ty {
                debug_info!("test", "Type: {:?}", ty);
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
    // This should fail: applying a function expecting a->b to something of type c->d
    let expr = "(λf:a->b.f)((λx:c->d.x)z)";

    let mut parser = Parser::new(
        Grammar::load(STLC_SPEC).unwrap()
    );

    let ast = parser.parse(expr).unwrap();
    let mut tc = TypeChecker::new();
    // Add z with a type that doesn't match what's expected
    tc.add("z".to_string(), crate::logic::bind::typing::BoundType::Atom("wrongtype".to_string()));

    let res = tc.check(&ast);
    assert!(res.is_err(), "Expected type mismatch error but type checking succeeded");
    
    if let Err(e) = res {
        debug_info!("test", "Expected error: {}", e);
        assert!(e.contains("Type mismatch") || e.contains("expected") || e.contains("found") || e.contains("Could not resolve"));
    }
}

#[test] fn stlc_unbound_variable_fails() {
    // This should fail: unbound variable z
    let expr = "(λx:a->a.x)((λy:a->a.y)z)";

    let mut parser = Parser::new(
        Grammar::load(STLC_SPEC).unwrap()
    );

    let ast = parser.parse(expr).unwrap();
    let mut tc = TypeChecker::new();    
    // Don't add z to context - it should fail as unbound variable
    
    let res = tc.check(&ast);
    assert!(res.is_err(), "Expected unbound variable error but type checking succeeded");
    
    if let Err(e) = res {
        debug_info!("test", "Expected error: {}", e);
        assert!(e.contains("not found in context") || e.contains("unbound") || e.contains("Could not resolve"));
    }
}

#[test] fn stlc_simple_lambda_ok() {
    // This should pass: simple lambda application
    let expr = "(λx:a->a.x)((λy:a->a.y)z)";

    let mut parser = Parser::new(
        Grammar::load(STLC_SPEC).unwrap()
    );

    let ast = parser.parse(expr).unwrap();
    let mut tc = TypeChecker::new();
    // Add z with matching type
    tc.add("z".to_string(), crate::logic::bind::typing::BoundType::Atom("a".to_string()));

    let res = tc.check(&ast);
    assert!(res.is_ok(), "Expected type checking to succeed");
    
    if let Ok(Some(ty)) = res {
        assert_eq!(ty, crate::logic::bind::typing::BoundType::Atom("a".to_string()));
        debug_info!("test", "Successfully inferred type: {:?}", ty);
    }
}