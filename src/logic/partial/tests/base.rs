use super::MetaParser;
use super::Parser;
use crate::logic::grammar::Grammar;
use crate::set_debug_level;

// This file mainly test left recursive shemas
// Made because it can be hard in the case of partial stuff

// ============================================================================
// CATEGORY 1: DIRECT LEFT RECURSION, Core left-recursive patterns
// ============================================================================

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
fn test_metaparser_multiplicative_growth() {
    // Verify MetaParser grows depth multiplicatively (ceil(d * factor)), and
    // that it returns the first depth in the growth sequence that can parse.
    let spec = r#"
    Chain ::= Chain 'a' | 'a'
    start ::= Chain
    "#;
    let g = Grammar::load(spec).unwrap();

    // choose an input that requires a non-trivial recursion depth
    let chain_len = 12usize;
    let input = (0..chain_len).map(|_| "a").collect::<Vec<_>>().join(" ");

    // find the minimal depth that makes a raw Parser succeed
    let mut baseline = Parser::new(g.clone());
    let mut min_depth = None;
    for d in 1..=100usize {
        baseline.set_max_recursion(d);
        match baseline.partial(&input) {
            crate::logic::partial::PartialParseOutcome::Success { .. } => {
                min_depth = Some(d);
                break;
            }
            _ => {}
        }
    }
    let min_depth = min_depth.expect("should find a depth that succeeds");

    // Configure MetaParser with multiplicative growth starting low
    let start = 2usize;
    let factor = 1.5f64;
    let mut mp = MetaParser::new(g.clone())
        .with_start_depth(start)
        .with_max_depth(200)
        .with_depth_factor(factor);

    // compute expected depth produced by multiplicative growth
    let mut expected = start;
    while expected < min_depth {
        let mut next = ((expected as f64) * factor).ceil() as usize;
        if next <= expected {
            next = expected + 1;
        }
        expected = next;
    }

    // run partial and confirm it returns our expected growth value
    let (_ast, used_depth) = mp.partial_with_depth(&input).expect("meta parse should succeed");
    assert_eq!(used_depth, expected);
    assert!(used_depth >= min_depth);
}

// ============================================================================
// CATEGORY 2: DIRECT RIGHT RECURSION, Core right-recursive patterns
// ============================================================================

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

// ============================================================================
// CATEGORY 3: INDIRECT LEFT RECURSION, Multi-nonterminal cycles
// ============================================================================

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

// ============================================================================
// CATEGORY 4: MUTUAL RECURSION,  Non-cyclic mutual references
// ============================================================================

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

// ============================================================================
// CATEGORY 5: MIXED RECURSION - Combining left, right, and mutual
// ============================================================================

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

// ============================================================================
// CATEGORY 6: AMBIGUOUS GRAMMARS - Multiple parse trees
// ============================================================================

#[test]
fn test_ambiguous_expression() {
    // E ::= E '+' E | E '*' E | 'n' (fully ambiguous)
    let spec = r#"
    Expr ::= Expr '+' Expr | Expr '*' Expr | 'n'
    start ::= Expr
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = MetaParser::new(g);

    // This has multiple valid parses: (n+n)+n vs n+(n+n)
    let ast = p.parse("n + n + n").unwrap();
    assert!(ast.is_complete());
    // Parser will return multiple roots or one valid parse
}

#[test]
fn test_ambiguous_dangeling_else() {
    // S ::= 'if' 'c' 'then' S 'else' S | 'if' 'c' 'then' S | 'a'
    let spec = r#"
    Stmt ::=  'if' 'c' 'then' Stmt 'else' Stmt 
            | 'if' 'c' 'then' Stmt 
            | 'a'
    start ::= Stmt
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = MetaParser::new(g);

    // Classic dangling else: two ways to parse
    // "if c then if c then a else a" can be parsed as:
    // 1. if c then (if c then a else a)  -- else binds to inner if
    // 2. if c then (if c then a) else a  -- else binds to outer if (but grammar doesn't allow this without full structure)
    let ast = p.parse("if c then if c then a else a").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_ambiguous_prefix_overlap() {
    // A ::= 'x' | 'x' 'y' | 'x' 'y' 'z'
    let spec = r#"
    A ::= 'x' | 'x' 'y' | 'x' 'y' 'z'
    start ::= A
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = MetaParser::new(g);

    // All three alternatives have "x" as a prefix
    let ast = p.partial("x").unwrap();
    // Should have multiple roots (complete for first, partial for others)
    assert!(ast.roots.len() >= 1);

    let ast = p.parse("x y z").unwrap();
    assert!(ast.is_complete());
}

// ============================================================================
// CATEGORY 7: DEEP NESTING - Stress testing recursion depth
// ============================================================================

#[test]
fn test_deep_parentheses() {
    // E ::= '(' E ')' | 'x'
    let spec = r#"
    Expr ::= '(' Expr ')' | 'x'
    start ::= Expr
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = MetaParser::new(g);

    // Deep nesting
    let mut input = String::from("x");
    for _ in 0..15 {
        input = format!("( {} )", input);
    }

    let ast = p.parse(&input).unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_deep_nested_lists() {
    // L ::= '[' L ']' | '[' L ',' L ']' | 'x'
    let spec = r#"
    List ::= '[' List ']' | '[' List ',' List ']' | 'x'
    start ::= List
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = MetaParser::new(g);

    let ast = p.parse("[ [ x ] , [ x , x ] ]").unwrap();
    assert!(ast.is_complete());

    let ast = p.parse("[ [ [ x ] ] ]").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_deep_function_application() {
    set_debug_level(crate::DebugLevel::Debug);
    // E ::= E E | '(' E ')' | 'f' | 'x'
    // NOTE: This grammar is highly ambiguous (Expr Expr)
    // Requires higher recursion depth due to ambiguity + nesting
    let spec = r#"
    Expr ::= Expr Expr | '(' Expr ')' | 'f' | 'x'
    start ::= Expr
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = MetaParser::new(g);

    // passes
    let ast = p.parse("f x").unwrap();
    assert!(ast.is_complete());

    // passes
    let ast = p.parse("f f f x").unwrap();
    assert!(ast.is_complete());

    // needs higher depth due to ambiguity
    let ast = p.parse("f ( f x ) ( f x )").unwrap();
    assert!(ast.is_complete());
}

// ============================================================================
// CATEGORY 8: PARTIAL PARSING WITH COMPLEX RECURSION
// ============================================================================

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

// ============================================================================
// CATEGORY 9: EPSILON WITH RECURSION - Complex optional patterns
// ============================================================================

#[test]
fn test_epsilon_with_left_recursion() {
    // L ::= L ',' 'x' | 'x' | ε
    let spec = r#"
    List ::= List ',' 'x' | 'x' | ε
    start ::= List
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = MetaParser::new(g);

    let ast = p.parse("").unwrap();
    assert!(ast.is_complete());

    let ast = p.parse("x").unwrap();
    assert!(ast.is_complete());

    let ast = p.parse("x , x , x").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_epsilon_with_right_recursion() {
    // L ::= 'x' ',' L | ε
    let spec = r#"
    List ::= 'x' ',' List | ε
    start ::= List
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = MetaParser::new(g);

    let ast = p.parse("").unwrap();
    assert!(ast.is_complete());

    let ast = p.parse("x , x , x ,").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_optional_prefix_with_recursion() {
    // E ::= O E '+' O 'n' | O 'n'
    // O ::= '!' | ε
    let spec = r#"
    Expr ::= Opt Expr '+' Opt 'n' | Opt 'n'
    Opt ::= '!' | ε
    start ::= Expr
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = MetaParser::new(g);

    let ast = p.parse("n + n").unwrap();
    assert!(ast.is_complete());

    let ast = p.parse("! n + n").unwrap();
    assert!(ast.is_complete());

    let ast = p.parse("! n + ! n").unwrap();
    assert!(ast.is_complete());
}

// ============================================================================
// CATEGORY 10: PATHOLOGICAL CASES - Stress testing memoization
// ============================================================================

#[test]
fn test_highly_ambiguous_grammar() {
    set_debug_level(crate::DebugLevel::Debug);
    // Every input has exponentially many parses
    // E ::= E E | E '+' E | '(' E ')' | 'x'
    let spec = r#"
    Expr ::= Expr Expr | Expr '+' Expr | '(' Expr ')' | 'x'
    start ::= Expr
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = MetaParser::new(g);

    // Should complete without timeout due to memoization
    let ast = p.parse("x + x + x").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_repeated_nonterminal_references() {
    // A ::= B B B B B
    // B ::= 'x' | ε
    let spec = r#"
    A ::= B B B B B
    B ::= 'x' | ε
    start ::= A
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = MetaParser::new(g);

    let ast = p.parse("").unwrap();
    assert!(ast.is_complete());

    let ast = p.parse("x x x").unwrap();
    assert!(ast.is_complete());

    let ast = p.partial("x x x x x").unwrap();
    assert!(ast.is_complete());

    println!("ast: {}", ast);
}

#[test]
fn test_alternating_recursion() {
    // A ::= B C | 'a'
    // B ::= A D | 'b'
    // C ::= A B | 'c'
    // D ::= B C | 'd'
    let spec = r#"
    A ::= B C | 'a'
    B ::= A D | 'b'
    C ::= A B | 'c'
    D ::= B C | 'd'
    start ::= A
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = MetaParser::new(g);

    let ast = p.parse("a").unwrap();
    assert!(ast.is_complete());

    let ast = p.parse("b c").unwrap();
    assert!(ast.is_complete());

    let ast = p.parse("b a b").unwrap();
    assert!(ast.is_complete());
}
