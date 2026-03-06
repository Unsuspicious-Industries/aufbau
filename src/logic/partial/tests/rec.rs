use crate::logic::grammar::Grammar;
use crate::logic::partial::MetaParser;
use crate::logic::Parser;
use crate::set_debug_level;

#[test]
fn test_left_rec_single_operator() {
    // E ::= E '+' 'n' | 'n'
    let spec = r#"
    Expr ::= Expr '+' 'n' | 'n'
    start ::= Expr
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = MetaParser::new(g);

    // Base case
    let ast = p.parse("n").unwrap();
    assert!(ast.is_complete());

    // One recursion
    let ast = p.parse("n + n").unwrap();
    assert!(ast.is_complete());

    // Multiple recursions
    let ast = p.parse("n + n + n + n").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_left_rec_multiple_operators() {
    set_debug_level(crate::DebugLevel::Trace);

    // E ::= E '+' E | E '*' E | 'n'
    let spec = r#"
    Expr ::= Expr '+' Expr | Expr '*' Expr | 'n'
    start ::= Expr
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = MetaParser::new(g);

    let ast = p.parse("n + n * n").unwrap();
    assert!(ast.is_complete());

    let ast = p.parse("n * n + n * n").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_left_rec_with_parentheses() {
    set_debug_level(crate::DebugLevel::Debug);
    // E ::= E '+' E | '(' E ')' | 'n'
    let spec = r#"
    Expr ::= Expr '+' Expr | '(' Expr ')' | 'n'
    start ::= Expr
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = Parser::new(g);

    let ast = p.parse("( n + n ) + n").unwrap();
    assert!(ast.is_complete());

    let ast = p.parse("( ( n ) )").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_left_rec_list_construction() {
    // L ::= L ',' 'x' | 'x'
    let spec = r#"
    List ::= List ',' 'x' | 'x'
    start ::= List
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = MetaParser::new(g);

    let ast = p.parse("x").unwrap();
    assert!(ast.is_complete());

    let ast = p.parse("x , x , x , x , x").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_left_rec_deep_chain() {
    // Test parser doesn't stack overflow on long left-recursive chains
    let spec = r#"
    Chain ::= Chain 'a' | 'a'
    start ::= Chain
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = MetaParser::new(g);

    // Build a long chain
    let input = (0..20).map(|_| "a").collect::<Vec<_>>().join(" ");
    let ast = p.parse(&input).unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_right_rec_single_operator() {
    // E ::= 'n' '+' E | 'n'
    let spec = r#"
    Expr ::= 'n' '+' Expr | 'n'
    start ::= Expr
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = MetaParser::new(g);

    let ast = p.parse("n").unwrap();
    assert!(ast.is_complete());

    let ast = p.parse("n + n + n + n").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_right_rec_exponentiation() {
    // E ::= 'n' '^' E | 'n'  (right-associative)
    let spec = r#"
    Expr ::= 'n' '^' Expr | 'n'
    start ::= Expr
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = MetaParser::new(g);

    let ast = p.parse("n ^ n ^ n").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_right_rec_nested_structures() {
    // N ::= '(' N ')' | 'x'
    let spec = r#"
    Nested ::= '(' Nested ')' | 'x'
    start ::= Nested
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = MetaParser::new(g);

    let ast = p.parse("x").unwrap();
    assert!(ast.is_complete());

    let ast = p.parse("( x )").unwrap();
    assert!(ast.is_complete());

    let ast = p.parse("( ( ( ( x ) ) ) )").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_right_rec_cons_list() {
    // L ::= 'x' '::' L | 'nil'
    let spec = r#"
    List ::= 'x' '::' List | 'nil'
    start ::= List
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = MetaParser::new(g);

    let ast = p.parse("nil").unwrap();
    assert!(ast.is_complete());

    let ast = p.parse("x :: x :: x :: nil").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_indirect_left_rec_two_nonterminals() {
    // A ::= B 'a' | 'a'
    // B ::= A 'b' | 'b'
    let spec = r#"
    A ::= B 'a' | 'a'
    B ::= A 'b' | 'b'
    start ::= A
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = MetaParser::new(g);

    let ast = p.parse("a").unwrap();
    assert!(ast.is_complete());

    let ast = p.parse("b a").unwrap();
    assert!(ast.is_complete());

    // a -> B a -> (b) a
    // a -> B a -> (A b) a -> (a b) a
    let ast = p.parse("a b a").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_indirect_left_rec_three_nonterminals() {
    // A ::= B 'a' | 'a'
    // B ::= C 'b' | 'b'
    // C ::= A 'c' | 'c'
    let spec = r#"
    A ::= B 'a' | 'a'
    B ::= C 'b' | 'b'
    C ::= A 'c' | 'c'
    start ::= A
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = MetaParser::new(g);

    let ast = p.parse("a").unwrap();
    assert!(ast.is_complete());

    let ast = p.parse("b a").unwrap();
    assert!(ast.is_complete());

    let ast = p.parse("c b a").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_indirect_left_rec_expression_grammar() {
    // E ::= T | T '+' E
    // T ::= F | F '*' T
    // F ::= E | 'n'
    let spec = r#"
    Expr ::= Term | Term '+' Expr
    Term ::= Factor | Factor '*' Term
    Factor ::= Expr | 'n'
    start ::= Expr
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = MetaParser::new(g);

    let ast = p.parse("n").unwrap();
    assert!(ast.is_complete());

    let ast = p.parse("n + n").unwrap();
    assert!(ast.is_complete());

    let ast = p.parse("n * n + n").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_mutual_rec_simple() {
    // Even ::= 'z' | 'o' Odd
    // Odd ::= 'o' Even
    let spec = r#"
    Even ::= 'z' | 'o' Odd
    Odd ::= 'o' Even
    start ::= Even
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = MetaParser::new(g);

    let ast = p.parse("z").unwrap();
    assert!(ast.is_complete());

    let ast = p.parse("o o z").unwrap();
    assert!(ast.is_complete());

    let ast = p.parse("o o o o z").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_mutual_rec_expression_statement() {
    // Stmt ::= Expr ';' | 'if' Expr 'then' Stmt 'else' Stmt
    // Expr ::= 'n' | '(' Stmt ')'
    let spec = r#"
    Stmt ::= Expr ';' | 'if' Expr 'then' Stmt 'else' Stmt
    Expr ::= 'n' | '(' Stmt ')'
    start ::= Stmt
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = MetaParser::new(g);

    let ast = p.parse("n ;").unwrap();
    assert!(ast.is_complete());

    let ast = p.parse("if n then n ; else n ;").unwrap();
    assert!(ast.is_complete());

    let ast = p.parse("( n ; ) ;").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_mutual_rec_nested_structures() {
    // A ::= '[' B ']' | 'x'
    // B ::= A | A ',' B
    let spec = r#"
    A ::= '[' B ']' | 'x'
    B ::= A | A ',' B
    start ::= A
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = MetaParser::new(g);

    let ast = p.parse("x").unwrap();
    assert!(ast.is_complete());

    let ast = p.parse("[ x ]").unwrap();
    assert!(ast.is_complete());

    let ast = p.parse("[ x , x ]").unwrap();
    assert!(ast.is_complete());

    let ast = p.parse("[ x , [ x ] , x ]").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_mixed_left_right_recursion() {
    // E ::= E '+' T | T
    // T ::= 'n' '^' T | 'n'
    let spec = r#"
    Expr ::= Expr '+' Term | Term
    Term ::= 'n' '^' Term | 'n'
    start ::= Expr
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = MetaParser::new(g);

    let ast = p.parse("n + n ^ n + n").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_mixed_complex_grammar() {
    // E ::= E '+' E | E '*' E | 'n' '^' E | '(' E ')' | 'n'
    let spec = r#"
    Expr ::= Expr '+' Expr 
            | Expr '*' Expr 
            | 'n' '^' Expr 
            | '(' Expr ')' 
            | 'n'
    start ::= Expr
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = MetaParser::new(g);

    let ast = p.parse("n + n * ( n ^ n )").unwrap();
    assert!(ast.is_complete());

    let ast = p.parse("( n + n ) * n ^ n").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_mixed_layered_operators() {
    // E ::= E '||' C | C
    // C ::= C '&&' A | A
    // A ::= A '+' M | M
    // M ::= M '*' U | U
    // U ::= 'n' '^' U | P
    // P ::= '(' E ')' | 'n'
    let spec = r#"
    Expr ::= Expr '||' Conj | Conj
    Conj ::= Conj '&&' Add | Add
    Add ::= Add '+' Mult | Mult
    Mult ::= Mult '*' Unary | Unary
    Unary ::= 'n' '^' Unary | Primary
    Primary ::= '(' Expr ')' | 'n'
    start ::= Expr
    "#;
    set_debug_level(crate::DebugLevel::Debug);
    let g = Grammar::load(spec).unwrap();
    let mut p = MetaParser::new(g);

    let ast = p.parse("n + n * n").unwrap();
    assert!(ast.is_complete());

    let ast = p.parse("n || n && n + n").unwrap();
    assert!(ast.is_complete());

    let ast = p.parse("( n + n ) * n ^ n || n").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_partial_in_left_recursive() {
    let spec = r#"
    Expr ::= Expr '+' 'n' | 'n'
    start ::= Expr
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = MetaParser::new(g);

    // Partial in middle of operator
    let ast = p.partial("n + n +").unwrap();
    assert!(!ast.is_complete());
}

#[test]
fn test_partial_in_right_recursive() {
    let spec = r#"
    Expr ::= 'n' '^' Expr | 'n'
    start ::= Expr
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = MetaParser::new(g);

    let ast = p.partial("n ^ n ^").unwrap();
    assert!(!ast.is_complete());
}

#[test]
fn test_partial_in_nested_structure() {
    let spec = r#"
    Expr ::= '(' Expr ')' | 'x'
    start ::= Expr
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = MetaParser::new(g);

    let ast = p.partial("( ( x").unwrap();
    assert!(!ast.is_complete());

    let ast = p.partial("( ( x )").unwrap();
    assert!(!ast.is_complete());
}

#[test]
fn test_partial_in_mixed_recursion() {
    let spec = r#"
    Expr ::= Expr '+' Term | Term
    Term ::= 'n' '^' Term | 'n'
    start ::= Expr
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = MetaParser::new(g);

    let ast = p.partial("n + n ^").unwrap();
    assert!(!ast.is_complete());

    let ast = p.partial("n ^ n +").unwrap();
    assert!(!ast.is_complete());
}
