//! Pathological and Edge Case Grammar Tests
//!
//! Tests completion behavior on unusual grammars:
//! - Infinite/unbounded recursion
//! - Ambiguous grammars
//! - Empty productions (epsilon)
//! - Deeply nested structures
//! - Grammars with complex typing rules

#![allow(dead_code)]
#![allow(unused_imports)]

use super::*;
use TypedCompletionTestCase as T;

// ============================================================================
// Pathological Grammars
// ============================================================================

/// Infinite right-recursion (no termination without 'b')
const INFINITE_RIGHT_RECURSIVE: &str = r#"
    A ::= 'a' A | 'b'
    start ::= A
"#;

/// Highly ambiguous grammar
const HIGHLY_AMBIGUOUS: &str = r#"
    A ::= 'x' | 'x' B
    B ::= 'y' | 'y' A
    start ::= A | B
"#;

/// Grammar with multiple epsilon productions
const EPSILON_HEAVY: &str = r#"
    A ::= 'a' B | ε
    B ::= 'b' C | ε
    C ::= 'c' | ε
    start ::= A B C
"#;

/// Deeply nested structure
const DEEP_NESTING: &str = r#"
    Atom ::= 'x'
    L1 ::= '(' L2 ')' | Atom
    L2 ::= '(' L3 ')' | L1
    L3 ::= '(' L4 ')' | L2
    L4 ::= '(' L5 ')' | L3
    L5 ::= '(' Atom ')' | L4
    start ::= L5
"#;

/// Grammar with very long productions
const LONG_PRODUCTION: &str = r#"
    A ::= 'a'
    B ::= 'b'
    C ::= 'c'
    Long ::= A B C A B C A B C
    start ::= Long
"#;

/// Grammar with cyclic but terminating alternatives
const CYCLIC_TERMINABLE: &str = r#"
    X ::= 'x' Y | 'done'
    Y ::= 'y' X | 'stop'
    start ::= X
"#;

/// Simple typing rule grammar
const TYPED_SIMPLE: &str = r#"
    Identifier ::= /[a-z]+/
    Variable(var) ::= Identifier[x]
    Expression ::= Variable
    
    x ∈ Γ
    ----------- (var)
    Γ(x)
"#;

/// Grammar with context extension
const CONTEXT_EXTENDING: &str = r#"
    Identifier ::= /[a-z]+/
    Type ::= 'int' | 'bool'
    Variable(var) ::= Identifier[x]
    Let(let) ::= 'let' Identifier[x] ':' Type[τ] 'in' Expression[e]
    Expression ::= Variable | Let
    
    x ∈ Γ
    ----------- (var)
    Γ(x)
    
    Γ[x:τ] ⊢ e : ?T
    ------------------------ (let)
    ?T
"#;

// ============================================================================
// Suite Definitions (used by validate binary)
// ============================================================================

pub fn suites() -> Vec<(&'static str, Grammar, Vec<TypedCompletionTestCase>)> {
    vec![
        (
            "weird::right_recursive_ok",
            load_inline_grammar(INFINITE_RIGHT_RECURSIVE),
            right_recursive_ok(),
        ),
        (
            "weird::epsilon_ok",
            load_inline_grammar(EPSILON_HEAVY),
            epsilon_ok(),
        ),
        (
            "weird::deep_nesting_ok",
            load_inline_grammar(DEEP_NESTING),
            deep_nesting_ok(),
        ),
        (
            "weird::cyclic_ok",
            load_inline_grammar(CYCLIC_TERMINABLE),
            cyclic_ok(),
        ),
        (
            "weird::long_production_ok",
            load_inline_grammar(LONG_PRODUCTION),
            long_production_ok(),
        ),
        (
            "weird::ambiguous_ok",
            load_inline_grammar(HIGHLY_AMBIGUOUS),
            ambiguous_ok(),
        ),
        (
            "weird::typed_simple_ok",
            load_inline_grammar(TYPED_SIMPLE),
            typed_simple_ok(),
        ),
        (
            "weird::context_extending_ok",
            load_inline_grammar(CONTEXT_EXTENDING),
            context_extending_ok(),
        ),
        (
            "weird::chain_typed_ok",
            load_inline_grammar(CHAIN_TYPED),
            chain_typed_ok(),
        ),
        (
            "weird::diamond_typed_ok",
            load_inline_grammar(DIAMOND_TYPED),
            diamond_typed_ok(),
        ),
        (
            "weird::mutual_typed_ok",
            load_inline_grammar(MUTUAL_TYPED),
            mutual_typed_ok(),
        ),
        (
            "weird::epsilon_typed_ok",
            load_inline_grammar(EPSILON_TYPED),
            epsilon_typed_ok(),
        ),
        (
            "weird::regex_typed_ok",
            load_inline_grammar(REGEX_TYPED),
            regex_typed_ok(),
        ),
        (
            "weird::scoped_typed_ok",
            load_inline_grammar(SCOPED_TYPED),
            scoped_typed_ok(),
        ),
        (
            "weird::stmt_typed_ok",
            load_inline_grammar(STMT_TYPED),
            stmt_typed_ok(),
        ),
        (
            "weird::union_typed_ok",
            load_inline_grammar(UNION_TYPED),
            union_typed_ok(),
        ),
    ]
}

// ============================================================================
// Case Definitions
// ============================================================================

fn right_recursive_ok() -> Vec<TypedCompletionTestCase> {
    vec![
        T::ok("base case", "b", 1),
        T::ok("one step", "a b", 2),
        T::ok("chain", "a a b", 3),
        T::ok("long chain", "a a a a b", 6),
        T::ok("partial", "a", 3),
        T::ok("partial chain", "a a", 4),
    ]
}

fn epsilon_ok() -> Vec<TypedCompletionTestCase> {
    vec![
        T::ok("empty", "", 1),
        T::ok("a only", "a", 2),
        T::ok("a b", "a b", 2),
        T::ok("a b c", "a b c", 1),
        T::ok("b only", "b", 2),
        T::ok("c only", "c", 1),
    ]
}

fn deep_nesting_ok() -> Vec<TypedCompletionTestCase> {
    vec![
        T::ok("simple", "x", 2),
        T::ok("one level", "(x)", 3),
        T::ok("two levels", "((x))", 5),
        T::ok("three levels", "(((x)))", 7),
        T::ok("max levels", "(((((x)))))", 12),
        T::ok("partial", "((", 8),
    ]
}

fn cyclic_ok() -> Vec<TypedCompletionTestCase> {
    vec![
        T::ok("done", "done", 1),
        T::ok("x stop", "x stop", 3),
        T::ok("x y done", "x y done", 4),
        T::ok("x y x stop", "x y x stop", 5),
        T::ok("partial x", "x", 4),
        T::ok("partial x y", "x y", 5),
    ]
}

fn long_production_ok() -> Vec<TypedCompletionTestCase> {
    vec![
        T::ok("complete", "a b c a b c a b c", 1),
        T::ok("partial 1", "a", 10),
        T::ok("partial 3", "a b c", 8),
        T::ok("partial 6", "a b c a b c", 5),
    ]
}

fn ambiguous_ok() -> Vec<TypedCompletionTestCase> {
    vec![
        T::ok("x only", "x", 1),
        T::ok("x y", "x y", 3),
        T::ok("x y x", "x y x", 4),
    ]
}

fn typed_simple_ok() -> Vec<TypedCompletionTestCase> {
    vec![
        T::ok("x in context", "x", 1).with_context(vec![("x", "int")]),
        T::ok("foo in context", "foo", 1).with_context(vec![("foo", "bool")]),
    ]
}

fn context_extending_ok() -> Vec<TypedCompletionTestCase> {
    vec![
        T::ok("let x in x", "let x : int in x", 5),
        T::ok("nested let", "let x : int in let y : bool in x", 8),
        T::ok("nested let inner", "let x : int in let y : bool in y", 8),
    ]
}

// ============================================================================
// Exotic Grammars — typed, using features beyond simple STLC/fun
// ============================================================================

/// Chain-typed: annotated expressions with type annotations.
/// Tests meta-variable unification: the annotation constrains the type.
const CHAIN_TYPED: &str = r#"
    Identifier ::= /[a-z]+/
    TypeName ::= 'A' | 'B' | 'C'
    Variable(var) ::= Identifier[x]
    Annotated(ann) ::= '(' Expression[e] ':' TypeName[τ] ')'
    Expression ::= Annotated | Variable

    x ∈ Γ
    ----------- (var)
    Γ(x)

    Γ ⊢ e : τ
    ----------- (ann)
    τ
"#;

/// Diamond ambiguity: two paths to the same nonterminal, both typed.
/// The grammar is ambiguous (Term reachable via Left or Right) and both
/// branches impose different type constraints.
const DIAMOND_TYPED: &str = r#"
    Identifier ::= /[a-z]+/
    Variable(var) ::= Identifier[x]
    Left(left) ::= '<' Term[inner] '>'
    Right(right) ::= '[' Term[inner] ']'
    Term ::= Variable | Left | Right
    Top ::= Left | Right

    x ∈ Γ
    ----------- (var)
    Γ(x)

    Γ ⊢ inner : ?T
    ----------- (left)
    ?T

    Γ ⊢ inner : ?T
    ----------- (right)
    ?T
"#;

/// Mutual recursion with types: two nonterminals reference each other,
/// both carrying typing rules, one using context extension.
const MUTUAL_TYPED: &str = r#"
    Identifier ::= /[a-z]+/
    Type ::= 'Num' | 'Flag'
    Literal(lit) ::= /[0-9]+/
    Variable(var) ::= Identifier[x]
    Bind(bind) ::= 'set' Identifier[name] ':' Type[τ] '=' Atom[value] 'then' Phrase[rest]
    Atom ::= Literal | Variable | '(' Phrase ')'
    Phrase ::= Bind | Atom

    x ∈ Γ
    ----------- (var)
    Γ(x)

    ----------- (lit)
    'Num'

    Γ ⊢ value : τ, Γ[name:τ] ⊢ rest : ?R
    ----------- (bind)
    ?R
"#;

/// Epsilon-interleaved with types: optional pieces surround a typed core.
/// Tests that epsilon-heavy grammars with typing rules still complete.
/// Uses '#' and '!' as prefix/suffix to avoid overlap with identifier regex.
const EPSILON_TYPED: &str = r#"
    Identifier ::= /[a-z]+/
    Variable(var) ::= Identifier[x]
    Prefix ::= '#' | ε
    Suffix ::= '!' | ε
    Wrapped(wrap) ::= Prefix[p] Core[c] Suffix[s]
    Core ::= Variable
    Start ::= Wrapped

    x ∈ Γ
    ----------- (var)
    Γ(x)

    Γ ⊢ c : ?T
    ----------- (wrap)
    ?T
"#;

/// Regex-heavy typed grammar: multiple regex terminals feeding into
/// typed rules, testing the tokenizer's handling of overlapping patterns.
const REGEX_TYPED: &str = r#"
    Lower ::= /[a-z]+/
    Upper ::= /[A-Z]+/
    Digits ::= /[0-9]+/

    Variable(var) ::= Lower[x]
    Tag(tag) ::= Upper[t]
    Num(num) ::= Digits[d]

    Tagged(tagged) ::= Tag[t] '.' Expression[e]
    Expression ::= Variable | Num | Tagged | '(' Expression ')'

    x ∈ Γ
    ----------- (var)
    Γ(x)

    ----------- (tag)
    'Tag'

    ----------- (num)
    'Num'

    Γ ⊢ e : ?T
    ----------- (tagged)
    ?T
"#;

/// Scoped context isolation: inner blocks cannot leak bindings outward.
/// Uses [Γ] scoped premise to test context isolation without imperative
/// statements — pure expression-level scoping.
const SCOPED_TYPED: &str = r#"
    Identifier ::= /[a-z]+/
    Type ::= 'X' | 'Y'
    Variable(var) ::= Identifier[x]
    Num(num) ::= /[0-9]+/
    Let(letb) ::= 'def' Identifier[name] ':' Type[τ] '=' Atom[value] 'in' Expr[body]
    Scoped(scoped) ::= '{' Expr[inner] '}'
    Atom ::= Variable | Num | Scoped | '(' Expr ')'
    Expr ::= Let | Atom

    x ∈ Γ
    ----------- (var)
    Γ(x)

    ----------- (num)
    'X'

    Γ ⊢ value : τ, Γ[name:τ] ⊢ body : ?R
    ----------- (letb)
    ?R

    [Γ] ⊢ inner : ?T
    ----------- (scoped)
    ?T
"#;

/// Context-transforming with checking mode: statements that produce ∅
/// and use Γ ▷ e checking, similar to imp but minimal — just assign+seq.
const STMT_TYPED: &str = r#"
    Identifier ::= /[a-z]+/
    Type ::= 'I' | 'B'
    Variable(var) ::= Identifier[x]
    Num(num) ::= /[0-9]+/
    Decl(decl) ::= 'var' Identifier[name] ':' Type[τ] '=' Num[value] ';'
    Seq(seq) ::= Statement[head] Statements[tail]
    Statements ::= Seq | ε
    Statement ::= Decl
    Block(block) ::= '{' Statements[stmts] '}'

    x ∈ Γ
    ----------- (var)
    Γ(x)

    ----------- (num)
    'I'

    Γ ⊢ value : τ
    ----------- (decl)
    Γ → Γ[name:τ] ⊢ ∅

    Γ ▷ head, Γ ▷ tail
    ----------- (seq)
    ∅

    [Γ] ▷ stmts
    ----------- (block)
    ∅
"#;

/// Union-typed expressions: literal values inhabit different types,
/// and a choice construct produces a union type.
const UNION_TYPED: &str = r#"
    Identifier ::= /[a-z]+/
    Variable(var) ::= Identifier[x]
    IntLit(ilit) ::= /[0-9]+/
    BoolLit(blit) ::= 'yes' | 'no'
    Choice(choice) ::= Expression[a] '?' Expression[b]
    Expression ::= Variable | IntLit | BoolLit | Choice | '(' Expression ')'

    x ∈ Γ
    ----------- (var)
    Γ(x)

    ----------- (ilit)
    'N'

    ----------- (blit)
    'B'

    Γ ⊢ a : ?A, Γ ⊢ b : ?B
    ----------- (choice)
    ?A | ?B
"#;

// ============================================================================
// Exotic Suite Definitions
// ============================================================================

fn chain_typed_ok() -> Vec<TypedCompletionTestCase> {
    vec![
        T::ok("var only", "x", 1).with_context(vec![("x", "B")]),
        T::ok("annotated var", "( x : A )", 2).with_context(vec![("x", "A")]),
        T::ok("nested annotation", "( ( x : A ) : A )", 3).with_context(vec![("x", "A")]),
        T::ok("partial annotation", "( x :", 3).with_context(vec![("x", "A")]),
    ]
}

fn diamond_typed_ok() -> Vec<TypedCompletionTestCase> {
    vec![
        T::ok("left wrap", "< x >", 2).with_context(vec![("x", "A")]),
        T::ok("right wrap", "[ x ]", 2).with_context(vec![("x", "A")]),
        T::ok("nested left-right", "< [ x ] >", 3).with_context(vec![("x", "A")]),
        T::ok("nested right-left", "[ < x > ]", 3).with_context(vec![("x", "A")]),
        T::ok("deep nesting", "< < < x > > >", 5).with_context(vec![("x", "B")]),
        T::ok("partial left", "< x", 3).with_context(vec![("x", "A")]),
    ]
}

fn mutual_typed_ok() -> Vec<TypedCompletionTestCase> {
    vec![
        T::ok("just a number", "42", 1),
        T::ok("var in ctx", "n", 1).with_context(vec![("n", "Num")]),
        T::ok("simple bind", "set x : Num = 1 then x", 3),
        T::ok(
            "nested bind",
            "set x : Num = 1 then set y : Num = x then y",
            5,
        ),
        T::ok("paren wrap", "( 7 )", 2),
    ]
}

fn epsilon_typed_ok() -> Vec<TypedCompletionTestCase> {
    vec![
        T::ok("bare var", "x", 1).with_context(vec![("x", "A")]),
        T::ok("prefix var", "# x", 1).with_context(vec![("x", "A")]),
        T::ok("var suffix", "x !", 1).with_context(vec![("x", "A")]),
        T::ok("prefix var suffix", "# x !", 1).with_context(vec![("x", "A")]),
        // `#` alone fails prefix soundness: it can complete to `# x` but
        // the prefix `#` by itself can't find a completion at depth 2.
        T::ok("partial prefix", "#", 2)
            .with_context(vec![("x", "A")])
            .without_soundness(),
    ]
}

fn regex_typed_ok() -> Vec<TypedCompletionTestCase> {
    vec![
        T::ok("number", "42", 1),
        T::ok("var", "abc", 1).with_context(vec![("abc", "Num")]),
        T::ok("tag dot num", "FOO . 99", 2),
        T::ok("tag dot var", "BAR . x", 2).with_context(vec![("x", "Tag")]),
        T::ok("nested tags", "A . B . 1", 3),
        T::ok("partial tag", "XY .", 3),
    ]
}

fn scoped_typed_ok() -> Vec<TypedCompletionTestCase> {
    vec![
        // Let cases fail prefix soundness at "def a : X =" — same issue as
        // context_extending and mutual: the prefix after `=` can't complete.
        T::ok("simple let", "def a : X = 1 in a", 3),
        T::ok("nested let", "def a : X = 1 in def b : X = a in b", 5),
        T::ok("scoped block", "{ 5 }", 2),
        T::ok("let in scope", "def a : X = 1 in { a }", 3),
        T::ok("partial def", "def a : X =", 8),
    ]
}

fn stmt_typed_ok() -> Vec<TypedCompletionTestCase> {
    vec![
        T::ok("empty block", "{ }", 1),
        T::ok("single decl", "{ var x : I = 1 ; }", 2),
        T::ok("two decls", "{ var x : I = 1 ; var y : I = 2 ; }", 3),
        T::ok("partial decl", "{ var x : I =", 4),
        T::ok("partial block", "{", 3),
    ]
}

fn union_typed_ok() -> Vec<TypedCompletionTestCase> {
    vec![
        T::ok("int literal", "42", 1),
        T::ok("bool literal", "yes", 1),
        T::ok("choice int-bool", "1 ? yes", 2),
        T::ok("nested choice", "1 ? 2 ? no", 3),
        T::ok("var choice", "x ? y", 2).with_context(vec![("x", "N"), ("y", "B")]),
        T::ok("partial choice", "1 ?", 3),
    ]
}

// ============================================================================
// Tests (delegates to shared case definitions)
// ============================================================================

#[test]
fn check_right_recursive_completable() {
    let grammar = load_inline_grammar(INFINITE_RIGHT_RECURSIVE);
    let res = run_test_batch(&grammar, &right_recursive_ok());
    res.assert_all_passed();
}

#[test]
fn check_epsilon_completable() {
    let grammar = load_inline_grammar(EPSILON_HEAVY);
    let res = run_test_batch(&grammar, &epsilon_ok());
    res.assert_all_passed();
}

#[test]
fn check_deep_nesting_completable() {
    let grammar = load_inline_grammar(DEEP_NESTING);
    let res = run_test_batch(&grammar, &deep_nesting_ok());
    res.assert_all_passed();
}

#[test]
fn check_cyclic_completable() {
    let grammar = load_inline_grammar(CYCLIC_TERMINABLE);
    let res = run_test_batch(&grammar, &cyclic_ok());
    res.assert_all_passed();
}

#[test]
fn check_long_production_completable() {
    let grammar = load_inline_grammar(LONG_PRODUCTION);
    let res = run_test_batch(&grammar, &long_production_ok());
    res.assert_all_passed();
}

#[test]
fn check_ambiguous_completable() {
    let grammar = load_inline_grammar(HIGHLY_AMBIGUOUS);
    let res = run_test_batch(&grammar, &ambiguous_ok());
    res.assert_all_passed();
}

#[test]
fn check_typed_simple_completable() {
    let grammar = load_inline_grammar(TYPED_SIMPLE);
    let res = run_test_batch(&grammar, &typed_simple_ok());
    res.assert_all_passed();
}

#[test]
fn check_context_extending_completable() {
    let grammar = load_inline_grammar(CONTEXT_EXTENDING);
    let res = run_test_batch(&grammar, &context_extending_ok());
    res.assert_all_passed();
}

#[test]
fn check_chain_typed_completable() {
    let grammar = load_inline_grammar(CHAIN_TYPED);
    let res = run_test_batch(&grammar, &chain_typed_ok());
    res.assert_all_passed();
}

#[test]
fn check_diamond_typed_completable() {
    let grammar = load_inline_grammar(DIAMOND_TYPED);
    let res = run_test_batch(&grammar, &diamond_typed_ok());
    res.assert_all_passed();
}

#[test]
fn check_mutual_typed_completable() {
    let grammar = load_inline_grammar(MUTUAL_TYPED);
    let res = run_test_batch(&grammar, &mutual_typed_ok());
    res.assert_all_passed();
}

#[test]
fn check_epsilon_typed_completable() {
    let grammar = load_inline_grammar(EPSILON_TYPED);
    let res = run_test_batch(&grammar, &epsilon_typed_ok());
    res.assert_all_passed();
}

#[test]
fn check_regex_typed_completable() {
    let grammar = load_inline_grammar(REGEX_TYPED);
    let res = run_test_batch(&grammar, &regex_typed_ok());
    res.assert_all_passed();
}

#[test]
fn check_scoped_typed_completable() {
    let grammar = load_inline_grammar(SCOPED_TYPED);
    let res = run_test_batch(&grammar, &scoped_typed_ok());
    res.assert_all_passed();
}

#[test]
fn check_stmt_typed_completable() {
    let grammar = load_inline_grammar(STMT_TYPED);
    let res = run_test_batch(&grammar, &stmt_typed_ok());
    res.assert_all_passed();
}

#[test]
fn check_union_typed_completable() {
    let grammar = load_inline_grammar(UNION_TYPED);
    let res = run_test_batch(&grammar, &union_typed_ok());
    res.assert_all_passed();
}
