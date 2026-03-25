use super::*;
use crate::logic::grammar::Grammar;
use crate::logic::partial::parse::Parser;
use crate::logic::partial::SppfForest;
use crate::set_debug_level;

fn parse(spec: &str, input: &str) -> (SppfForest, Grammar) {
    let g = Grammar::load(spec).unwrap();
    let mut p = Parser::new(g.clone());
    (p.partial(input).unwrap(), g)
}

// ========================================================================
// Basic API tests
// ========================================================================

#[test]
fn test_typed_basic() {
    let spec = "
Num(n) ::= /[0-9]+/
start ::= Num

-------------- (n)
'int'
";
    let (ast, g) = parse(spec, "42");
    let typed = ast.typed(&g).unwrap();
    assert!(!typed.is_empty());
    assert!(typed.first().is_some());
    println!("typed: {}", typed);
}

#[test]
fn test_typed_complete_composition() {
    set_debug_level(crate::DebugLevel::Trace);
    let spec = "
        Var(v) ::= /[a-z]+/[x]
        start ::= Var

        x ∈ Γ
        -------------- (v)
        Γ(x)
        ";
    let (ast, g) = parse(spec, "x");
    let ctx = Context::new()
        .extend("x".into(), Type::Raw("Int".into()))
        .unwrap();
    assert!(ast.typed_complete_ctx(&g, &ctx).is_ok());
    let typed = ast.typed_complete_ctx(&g, &ctx).unwrap();
    println!("typed: {}", typed);
}

#[test]
fn test_has_well_typed() {
    let spec = "start ::= 'a'";
    let (ast, g) = parse(spec, "a");
    assert!(ast.has_well_typed(&g));
}

// ========================================================================
// Error cases - context-dependent typing
// ========================================================================

#[test]
fn test_variable_requires_context() {
    set_debug_level(crate::DebugLevel::Trace);
    // Variable rule requires x ∈ Γ - should fail without context
    let spec = "
        Var(v) ::= /[a-z]+/[e]
        start ::= Var

        e ∈ Γ
        -------------- (v)
        Γ(e)";
    let (ast, g) = parse(spec, "x");
    // typed_complete with context should work
    let ctx = Context::new()
        .extend("x".into(), Type::Atom("Int".into()))
        .unwrap();
    assert!(ast.typed_complete_ctx(&g, &ctx).is_ok());
    let typed = ast.typed_complete_ctx(&g, &ctx).unwrap();
    println!("typed: {}", typed);
}

#[test]
fn test_partial_with_complete_filter() {
    // Tests that typed_complete uses SppfForest::complete() check
    let spec = "start ::= 'a' 'b' 'c'";
    let (ast, g) = parse(spec, "a b");
    // The partial AST itself is not complete
    assert!(!ast.is_complete(), "partial input should not be complete");
    // typed_complete should fail for partial
    assert!(ast.typed_complete(&g).is_err());
}

// ========================================================================
// TypedNode tests
// ========================================================================

#[test]
fn test_typed_node_is_complete() {
    let spec = "start ::= 'a'";
    let (ast, g) = parse(spec, "a");
    let typed = ast.typed(&g).unwrap();
    let root = typed.first().unwrap();
    assert!(root.is_complete());
}

#[test]
fn test_typed_node_type_access() {
    let spec = "Num(n) ::= /[0-9]+/\nstart ::= Num\n-------------- (n)\n'int'";
    let (ast, g) = parse(spec, "42");
    let typed = ast.typed(&g).unwrap();
    let root = typed.first().unwrap();
    // Root is 'start' which drills through to Num
    let _ty = root.ty(); // Should not panic
}

// ========================================================================
// Context propagation tests
// ========================================================================

#[test]
fn test_lambda_binds_variable() {
    // Lambda should bind x in its body
    let spec = r#"
        Identifier ::= /[a-z]+/
        Variable(var) ::= Identifier[x]
        Lambda(lam) ::= 'λ' Identifier[x] '.' Variable[e]
        start ::= Lambda

        x ∈ Γ
        -------------- (var)
        Γ(x)

        Γ[x:'int'] ⊢ e : ?B
        -------------- (lam)
        'int' → ?B
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = Parser::new(g.clone());

    // λx.x should typecheck - x is bound by lambda
    let ast = p.partial("λ x . x").unwrap();
    assert!(
        ast.typed_complete(&g).is_ok(),
        "lambda should bind its variable"
    );
}

#[test]
fn test_variable_with_context_succeeds() {
    let spec = r#"
        Identifier ::= /[a-z]+/
        Variable(var) ::= Identifier[x]
        start ::= Variable

        x ∈ Γ
        -------------- (var)
        Γ(x)
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = Parser::new(g.clone());

    set_debug_level(crate::DebugLevel::Trace);
    // Variable with context should work
    let ast = p.partial("y").unwrap();
    let ctx = Context::new()
        .extend("y".into(), Type::Raw("Int".into()))
        .unwrap();
    assert!(ast.typed_complete_ctx(&g, &ctx).is_ok());
    println!(
        "Typed AST with context: {}",
        ast.typed_ctx(&g, &ctx).unwrap()
    );
}

#[test]
fn test_complete_filter() {
    let spec = "start ::= 'a' 'b'";
    let (ast, g) = parse(spec, "a b");
    let typed = ast.typed(&g).unwrap();
    let filtered = typed.completes().unwrap();
    assert!(!filtered.is_empty());
}

// ========================================================================
// Display tests
// ========================================================================

#[test]
fn test_typed_ast_display() {
    let spec = "start ::= 'hello'";
    let (ast, g) = parse(spec, "hello");
    let typed = ast.typed(&g).unwrap();
    let display = format!("{}", typed);
    assert!(display.contains("hello"));
    assert!(display.contains("start"));
    println!("TypedAST display:\n{}", display);
}
