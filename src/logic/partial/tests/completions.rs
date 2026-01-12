use super::super::completion::*;
use crate::{logic::partial::parse::Parser, set_debug_level, testing::load_example_grammar};
use crate::logic::grammar::Grammar;
use crate::regex::Regex as DerivativeRegex;

fn complete(spec: &str, input: &str) -> CompletionSet {
    let g = crate::logic::grammar::Grammar::load(spec).unwrap();
    let mut p = Parser::new(g.clone());
    let past = p.partial(input).unwrap();
    past.completions(&g)
}

#[test]
fn test_completions() {
    let spec = r#"
U ::= 'b' 'a' 'r' 'c' 'b' 'a' 'r' 'c' 'u'
A ::= 'a'
B ::= 'b' A 'r'
Loop ::= B 'c' Loop | B 'c'
start ::= U | Loop | 't'
    "#;
    let g = crate::logic::grammar::Grammar::load(spec).unwrap();
    let mut p = Parser::new(g.clone());
    let input = "b a r c b a r c";
    let past = p.partial(input).unwrap();

    println!("Partial AST:  {}", past);
    set_debug_level(crate::DebugLevel::Trace);
    let completions = past.completions(&g);

    println!(
        "DEBUG: Roots = {:?}",
        past.roots
            .iter()
            .map(|r| (r.name.clone(), r.is_complete()))
            .collect::<Vec<_>>()
    );
    println!("DEBUG: Completions = {:?}", completions);
    println!("DEBUG: Does 'b' match? {}", completions.matches("b"));
    println!("DEBUG: Does 'u' match? {}", completions.matches("u"));

    assert!(
        completions.matches("u"),
        "expected literal 'u' in completions"
    );
    assert!(
        completions.matches("b"),
        "expected literal 'b' in completions"
    );
    assert!(
        completions.matches("b"),
        "expected literal 'b' in completions"
    );
}

#[test]
fn completion_first_sets_with_alternatives() {
    let spec = r#"
    A(ruleA) ::= 'a' 'x' | 'a'
    B(ruleB) ::= 'b'
    start ::= A | B
    "#;

    let completions = complete(spec, "");
    assert!(completions.matches("a"), "expected 'a' from FIRST(start)");
    assert!(completions.matches("b"), "expected 'b' from FIRST(start)");
}

#[test]
fn completion_next_symbol_prediction() {
    let spec = r#"
    start ::= 'a' 'b'
    "#;
    let completions = complete(spec, "a");
    assert!(completions.matches("b"), "expected next literal 'b'");
}

#[test]
fn completion_binary_op_requires_operand() {
    let spec = r#"
    Number ::= /[0-9]+/
    Identifier ::= /[a-z][a-zA-Z0-9]*/
    Literal ::= Number[n]
    Variable ::= Identifier[x]
    AtomicExpr ::= Literal | Variable | '(' Expression ')'
Operator ::= '+' | '-' | '*' | '/'
BinaryOp ::= AtomicExpr[left] Operator[op] AtomicExpr[right]
    Expression ::= AtomicExpr | BinaryOp
    "#;

    let completions = complete(spec, "");
    assert!(
        completions.matches("[0-9]+"),
        "expected numeric literal to be suggested before operators"
    );
    assert!(
        !completions.matches("+"),
        "operator '+' should not be suggested before first operand"
    );
}

#[test]
fn completion_tail_repetition_plus() {
    let spec = r#"
start ::= 'a' | 'a' start
    "#;
    let completions = complete(spec, "a");
    assert!(
        completions.matches("a"),
        "expected tail repetition to suggest another 'a'"
    );
}

#[test]
fn completion_nullable_group_lookahead() {
    let spec = r#"
start ::= 'a' 'b' | 'b'
    "#;
    let completions = complete(spec, "");
    assert!(completions.matches("a"), "nullable group allows 'a'");
    assert!(
        completions.matches("b"),
        "nullable group allows lookahead 'b'"
    );
}

#[test]
fn completion_group_repetition_tail() {
    let spec = r#"
start ::= 'c' | 'a' 'b' start
    "#;
    let completions = complete(spec, "ab");
    // For group repetition, FIRST set for the group starts with 'a'
    assert!(
        completions.matches("a"),
        "expected to suggest restarting the group with 'a'"
    );
}

#[test]
fn completion_regex_identifier() {
    let spec = r#"
    Identifier ::= /[a-z][a-z0-9]*/
    start ::= Identifier
    "#;
    let completions = complete(spec, "");
    assert!(
        completions.matches("[a-z][a-z0-9]*"),
        "expected identifier regex completion"
    );
}

#[test]
fn completion_single_wrapped_regex() {
    let spec = r#"
    Identifier ::= /[A-Z][a-z]+/
    // Single wraps the inner regex (with a binding)
    Name(name) ::= Identifier[x]
    start ::= Name
    "#;
    let completions = complete(spec, "");
    assert!(
        completions.matches("[A-Z][a-z]+"),
        "expected FIRST(Name) to expose inner Identifier regex"
    );
}

#[test]
fn completion_deduplicates_identical_tokens() {
    let spec = r#"
    S1(r1) ::= 'x'
    S2(r2) ::= 'x' 'y'
    start ::= S1 | S2
    "#;
    let completions = complete(spec, "");
    // Only a single 'x' token should appear after dedup
    let count_x = completions
        .iter()
        .filter(|t| t.equiv(&DerivativeRegex::literal("x")))
        .count();
    assert_eq!(
        count_x, 1,
        "expected deduplication of identical 'x' suggestions"
    );
}

// ============================================================================
// Some tests on edge cases, not full suit
// ============================================================================

#[test]
fn completion_single_literal() {
    let spec = r#"
    start ::= 'hello'
    "#;
    let completions = complete(spec, "");
    // Single literal production
    assert!(completions.matches("hello"), "should suggest 'hello'");
    assert_eq!(
        completions.tokens.len(),
        1,
        "should have exactly one completion"
    );
}

#[test]
fn completion_nested_nullable_groups() {
    let spec = r#"
start ::= 'a' 'b' 'c' | 'a' 'c' | 'b' 'c' | 'c'
    "#;
    let completions = complete(spec, "");
    // Should suggest 'a', 'b', and 'c' (all three are valid starts)
    assert!(completions.matches("a"), "should suggest 'a'");
    assert!(completions.matches("b"), "should suggest 'b'");
    assert!(completions.matches("c"), "should suggest 'c'");
}

#[test]
fn completion_group_with_optional_prefix() {
    let spec = r#"
start ::= 'a' 'b' 'c' 'd' | 'c' 'd'
    "#;
    let completions = complete(spec, "");
    // Group is optional, so should suggest both 'a' (from group) and 'c' (skipping group)
    assert!(
        completions.matches("a"),
        "should suggest 'a' from optional group"
    );
    assert!(
        completions.matches("c"),
        "should suggest 'c' (skipping optional group)"
    );
}

#[test]
fn completion_multiple_alternatives_all_contribute() {
    let spec = r#"
    A ::= 'x'
    B ::= 'y'
    C ::= 'z'
    start ::= A | B | C
    "#;
    let completions = complete(spec, "");
    // All three alternatives should contribute their FIRST sets
    assert!(completions.matches("x"), "should include 'x' from A");
    assert!(completions.matches("y"), "should include 'y' from B");
    assert!(completions.matches("z"), "should include 'z' from C");
    assert_eq!(
        completions.tokens.len(),
        3,
        "should have exactly 3 completions"
    );
}

#[test]
fn completion_deeply_nested_nonterminals() {
    let spec = r#"
    D ::= 'd'
    C ::= D
    B ::= C
    A ::= B
    start ::= A
    "#;
    let completions = complete(spec, "");
    // Should drill down through all nonterminals to find 'd'
    assert!(
        completions.matches("d"),
        "should find 'd' through nested nonterminals"
    );
    assert_eq!(
        completions.tokens.len(),
        1,
        "should have exactly one completion"
    );
}

#[test]
fn completion_partial_literal_midway() {
    let spec = r#"
    start ::= 'hello' 'world'
    "#;
    let completions = complete(spec, "hello");
    // After matching first literal, should suggest second
    assert!(
        completions.matches("world"),
        "should suggest 'world' after 'hello'"
    );
}

#[test]
fn completion_star_repetition_can_skip() {
    let spec = r#"
A ::= 'a'
start ::= 'b' | A start
    "#;
    let completions = complete(spec, "");
    // * allows zero matches, so both 'a' and 'b' are valid
    assert!(
        completions.matches("a"),
        "should suggest 'a' from repetition"
    );
    assert!(
        completions.matches("b"),
        "should suggest 'b' since * is nullable"
    );
}

#[test]
fn completion_plus_repetition_after_one_match() {
    let spec = r#"
A ::= 'a'
start ::= A 'b' | A start
    "#;
    let completions = complete(spec, "a");
    // After one match of A, can repeat or continue to 'b'
    assert!(completions.matches("a"), "can repeat A");
    assert!(completions.matches("b"), "can continue to 'b'");
}

#[test]
fn completion_optional_single_symbol() {
    let spec = r#"
Foo ::= 'foo'
start ::= Foo 'bar' | 'bar'
    "#;
    let completions = complete(spec, "");
    // Optional nonterminal's FIRST and following symbol
    assert!(
        completions.matches("foo"),
        "should suggest 'foo' from optional Foo"
    );
    assert!(
        completions.matches("bar"),
        "should suggest 'bar' since Foo is optional"
    );
}

#[test]
fn completion_regex_alternatives() {
    let spec = r#"
    Number ::= /[0-9]+/
    Identifier ::= /[a-z]+/
    start ::= Number | Identifier
    "#;
    let completions = complete(spec, "");
    assert!(completions.matches("[0-9]+"), "should suggest number regex");
    assert!(
        completions.matches("[a-z]+"),
        "should suggest identifier regex"
    );
}

#[test]
fn completion_mixed_literals_and_regex() {
    let spec = r#"
    Num ::= /[0-9]+/
    start ::= 'let' Num
    "#;
    let completions = complete(spec, "");
    assert!(completions.matches("let"), "should suggest 'let' first");
    // Note: may also suggest other tokens from alternative parses

    let completions = complete(spec, "let");
    assert!(
        completions.matches("[0-9]+"),
        "should suggest regex after 'let'"
    );
}

#[test]
fn completion_multiple_completed_repetitions() {
    let spec = r#"
ASeq ::= ε | 'a' ASeq
BSeq ::= ε | 'b' BSeq
start ::= ASeq BSeq 'c'
    "#;
    let completions = complete(spec, "aabb");
    // After matching 'aa' and 'bb', can continue second repetition or move to 'c'
    // Cannot continue first repetition because we have already started BSeq (seen 'b')
    assert!(
        !completions.matches("a"),
        "cannot continue first repetition after starting second"
    );
    assert!(completions.matches("b"), "can continue second repetition");
    assert!(completions.matches("c"), "can move to 'c'");
}

#[test]
fn completion_group_with_multiple_symbols() {
    let spec = r#"
start ::= 'a' 'b' 'c' Tail
Tail ::= 'd' | 'a' 'b' 'c' Tail
    "#;
    let completions = complete(spec, "abc");
    // After one complete group iteration, can repeat or continue
    assert!(completions.matches("a"), "can repeat the group");
    assert!(completions.matches("d"), "can move to 'd'");
}

#[test]
fn completion_no_ambiguity_in_sequence() {
    let spec = r#"
    start ::= 'a' 'b' 'c'
    "#;
    let completions = complete(spec, "ab");
    // Unambiguous: only 'c' is valid next
    eprintln!("completions: {:?}", completions.tokens);
    assert!(completions.matches("c"), "should suggest 'c'");
    assert_eq!(
        completions.tokens.len(),
        1,
        "should have exactly one completion"
    );
}

#[test]
fn completion_alternatives_with_common_prefix() {
    let spec = r#"
    A ::= 'a' 'b'
    B ::= 'a' 'c'
    start ::= A | B
    "#;
    let completions = complete(spec, "a");
    // After common prefix 'a', both alternatives are possible
    assert!(completions.matches("b"), "should suggest 'b' from A");
    assert!(completions.matches("c"), "should suggest 'c' from B");
}

// ============================================================================
// Typed Completion Tests
// ============================================================================

#[test]
fn typed_completion_basic() {
    let spec = r#"
    Num(num) ::= /[0-9]+/
    start ::= Num

    -------------- (num)
    'int'
    "#;

    let g = Grammar::load(spec).unwrap();
    let mut p = Parser::new(g.clone());
    let ast = p.partial("").unwrap();
    let untyped = ast.completions(&g);
    assert!(untyped.matches("[0-9]+"));
}

#[test]
fn typed_completion_with_context() {
    let spec = r#"
    Var(var) ::= /[a-z]+/
    Num(num) ::= /[0-9]+/
    Assign(assign) ::= Var '=' Num
    start ::= Assign

    -------------- (var)
    'string'

    -------------- (num)
    'int'

    -------------- (assign)
    'unit'
    "#;

    let g = Grammar::load(spec).unwrap();
    let mut p = Parser::new(g.clone());

    // At start, should suggest variable
    let ast1 = p.partial("").unwrap();
    let completions1 = ast1.completions(&g);
    assert!(
        completions1.matches("[a-z]+"),
        "should suggest var at start"
    );

    // After var and =, should suggest number
    let ast2 = p.partial("x =").unwrap();
    let completions2 = ast2.completions(&g);
    assert!(completions2.matches("[0-9]+"), "should suggest num after =");
}

#[test]
fn typed_completion_preserves_all_valid() {
    let spec = r#"
    A(ruleA) ::= 'a'
    B(ruleB) ::= 'b'
    start ::= A | B

    -------------- (ruleA)
    'typeA'

    -------------- (ruleB)
    'typeB'
    "#;

    let g = Grammar::load(spec).unwrap();
    let mut p = Parser::new(g.clone());
    let ast = p.partial("").unwrap();

    let typed = ast.completions(&g);

    // Both should be suggested (both have valid typing rules)
    assert!(typed.matches("a"), "should suggest 'a'");
    assert!(typed.matches("b"), "should suggest 'b'");
    assert_eq!(typed.tokens.len(), 2);
}

#[test]
fn typed_completion_complex_expression() {
    let spec = r#"
    Num(num) ::= /[0-9]+/
    Add(add) ::= Num '+' Num
    start ::= Add

    -------------- (num)
    'int'

    -------------- (add)
    'int'
    "#;

    let g = Grammar::load(spec).unwrap();
    let mut p = Parser::new(g.clone());

    let ast = p.partial("42 +").unwrap();
    let completions = ast.completions(&g);

    // After '+', should suggest another number
    assert!(
        completions.matches("[0-9]+"),
        "should suggest number after +"
    );
}

#[test]
fn test_paren_expr_is_complete() {
    let spec = r#"
    Number ::= /[0-9]+/
    Identifier ::= /[a-z][a-zA-Z0-9]*/

    Literal(lit) ::= Number[n]
    Variable(var) ::= Identifier[x]

    AtomicExpr ::= Literal | Variable | '(' Expression ')'
Operator ::= '+' | '-' | '*' | '/'
BinaryOp(binop) ::= AtomicExpr[left] Operator[op] AtomicExpr[right]

    Expression ::= AtomicExpr | BinaryOp
    "#;

    let grammar = crate::logic::grammar::Grammar::load(spec).unwrap();

    crate::set_debug_level(crate::DebugLevel::Trace);
    crate::set_debug_input(Some("(42)".to_string()));

    let mut parser1 = crate::logic::Parser::new(grammar.clone());
    let partial1 = parser1.partial("42").unwrap();
    println!("=== Parsing '42' ===");
    println!("Complete: {}", partial1.is_complete());
    println!("Roots: {}", partial1.roots.len());

    // Second test: parse "(42" - this is where the issue is
    let mut parser2 = crate::logic::Parser::new(grammar.clone());
    let partial2 = parser2.partial("(42").unwrap();
    println!("\n=== Parsing '(42' ===");
    println!("Complete: {}", partial2.is_complete());
    println!("Roots: {}", partial2.roots.len());

    // Completions
    let completions = partial2.completions(&grammar);
    println!("Completions for '(42': {:?}", completions);
}

// ============================================================================
// Typed Completion - Root Filtering Tests
// ============================================================================

#[test]
fn typed_completions_filters_malformed_roots() {
    let g = load_example_grammar("stlc");
    let mut p = Parser::new(g.clone());

    // This should be well-typed (partial)
    let ast = p.partial("").unwrap();

    // Without context, the lambda body 'x' is well-typed because
    // the lambda binds x:A
    set_debug_level(crate::DebugLevel::Trace);
    let completions = ast.completions(&g);
    println!("Completions: {:?}", completions);

    // assert lambda unicode symbol is suggested
    assert!(
        completions.tokens.contains(&DerivativeRegex::literal("λ")),
        "Should suggest lambda unicode symbol"
    );

    assert!(
        !completions.tokens.is_empty(),
        "Should have completions for well-typed partial parse"
    );
}
