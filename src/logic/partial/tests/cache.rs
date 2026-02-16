use crate::logic::grammar::Grammar;
use crate::logic::partial::Parser;

#[test]
fn test_cache_reuse_incremental_prefix() {
    let spec = r#"
    Identifier ::= /[a-z]+/
    Number ::= /[0-9]+/
    Atom ::= Number | Identifier | '(' Expr ')'
    Term ::= Atom | Term '*' Atom
    Expr ::= Term | Expr '+' Term
    start ::= Expr
    "#;

    let g = Grammar::load(spec).unwrap();
    let mut p = Parser::new(g);
    p.enable_cache_monitoring(true);

    let inputs = ["x", "x + 1", "x + 1 * 2", "x + 1 * 2 + 3"];
    for input in inputs {
        let result = p.partial(input);
        assert!(result.is_ok(), "partial parse should succeed for {input}");
    }

    let stats = p.cache_stats();
    let report = p.cache_report(5, 3);
    assert_eq!(
        stats.cache_invalidations, 0,
        "prefix parsing should not invalidate cache"
    );
    assert!(stats.stores > 0, "should store cached spans");
    assert!(stats.lookups > 0, "should perform cache lookups");
    assert!(report.contains("span_buckets="));
}

#[test]
fn test_cache_reuse_exact_same_input() {
    let spec = r#"
    Identifier ::= /[A-Za-z_][A-Za-z0-9_]*/
    Number ::= /[0-9]+/
    Atom ::= Number | Identifier | '(' Expr ')'
    Term ::= Atom | Term '*' Atom
    Expr ::= Term | Expr '+' Term
    start ::= Expr
    "#;

    let g = Grammar::load(spec).unwrap();
    let mut p = Parser::new(g);
    p.enable_cache_monitoring(true);

    let input = "foo + 12 * bar";
    let result = p.partial(input);
    assert!(result.is_ok());

    p.reset_cache_monitoring();

    let result = p.partial(input);
    assert!(result.is_ok());

    let stats = p.cache_stats();
    assert!(
        stats.lookup_hits_exact > 0,
        "expected cache hits on same input"
    );
}

#[test]
fn test_cache_with_complex_grammar() {
    let spec = r#"
    Identifier ::= /[a-z]+/
    Number ::= /[0-9]+/
    Expr ::= Number | Identifier | '(' Expr ')' | Expr '+' Expr
    Stmt ::= Identifier '=' Expr ';' | 'if' '(' Expr ')' Stmt 'else' Stmt | '{' StmtList '}'
    StmtList ::= Stmt StmtList | ε
    start ::= StmtList
    "#;

    let g = Grammar::load(spec).unwrap();
    let mut p = Parser::new(g);
    p.enable_cache_monitoring(true);

    let inputs = ["x=1;", "x=1; y=2;", "if(x){x=1;}else{x=2;}"];
    for input in inputs {
        let result = p.partial(input);
        assert!(result.is_ok(), "partial parse should succeed for {input}");
    }

    let stats = p.cache_stats();
    assert!(stats.stores > 0, "should store cached spans");
    assert!(stats.lookups > 0, "should perform cache lookups");
}
