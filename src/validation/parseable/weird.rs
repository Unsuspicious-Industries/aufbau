use super::*;

// Small subset of pathological grammars adapted for parseability checks.

const INFINITE_RIGHT_RECURSIVE: &str = r#"
    A ::= 'a' A | 'b'
    start ::= A
"#;

const EPSILON_HEAVY: &str = r#"
    A ::= 'a' B | ε
    B ::= 'b' C | ε
    C ::= 'c' | ε
    start ::= A B C
"#;

const DEEP_NESTING: &str = r#"
    Atom ::= 'x'
    L1 ::= '(' L2 ')' | Atom
    L2 ::= '(' L3 ')' | L1
    L3 ::= '(' L3 ')' | L2
    start ::= L3
"#;

// ============================================================================
// Exotic grammars — exercise more advanced grammar features
// ============================================================================

/// Diamond ambiguity: two paths to the same nonterminal via Left/Right.
const DIAMOND: &str = r#"
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

/// Mutual recursion with typed bindings.
const MUTUAL: &str = r#"
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

/// Epsilon-interleaved around a typed core.
const EPSILON_WRAPPED: &str = r#"
    Identifier ::= /[a-z]+/
    Variable(var) ::= Identifier[x]
    Prefix ::= 'pre' | ε
    Suffix ::= 'post' | ε
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

/// Regex-heavy: multiple regex patterns feeding into typed rules.
const REGEX_HEAVY: &str = r#"
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

/// Scoped context isolation with [Γ].
const SCOPED: &str = r#"
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

/// Statement-like: context-transforming conclusions with Γ → Γ[x:τ] ⊢ ∅.
const STMT: &str = r#"
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

/// Union-typed choice operator producing union types.
const UNION_CHOICE: &str = r#"
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

fn load_inline_grammar(content: &str) -> Grammar {
    Grammar::load(content).expect("failed to load inline grammar")
}

// === Per-grammar case lists ===
fn right_valid_cases() -> Vec<ParseTestCase> {
    vec![
        ParseTestCase::valid("right b", "b"),
        ParseTestCase::valid("right a b", "a b"),
        ParseTestCase::valid("right a a b", "a a b"),
    ]
}

fn right_invalid_cases() -> Vec<ParseTestCase> {
    vec![
        ParseTestCase::invalid("right invalid char", "c"),
        ParseTestCase::invalid("right wrong order", "b a"),
        ParseTestCase::invalid("right invalid symbol", "@"),
    ]
}

fn epsilon_valid_cases() -> Vec<ParseTestCase> {
    vec![
        ParseTestCase::valid("epsilon empty", ""),
        ParseTestCase::valid("epsilon a", "a"),
        ParseTestCase::valid("epsilon a b c", "a b c"),
    ]
}

fn epsilon_invalid_cases() -> Vec<ParseTestCase> {
    vec![
        ParseTestCase::invalid("epsilon invalid", "x"),
        ParseTestCase::invalid("epsilon wrong order", "c b a"),
    ]
}

fn deep_valid_cases() -> Vec<ParseTestCase> {
    vec![
        ParseTestCase::valid("deep x", "x"),
        ParseTestCase::valid("deep (x)", "(x)"),
        ParseTestCase::valid("deep ((x))", "((x))"),
    ]
}

fn deep_invalid_cases() -> Vec<ParseTestCase> {
    vec![
        ParseTestCase::invalid("deep extra close", ")"),
        ParseTestCase::invalid("deep invalid atom", "y"),
        ParseTestCase::invalid("deep trailing close", "x)"),
    ]
}

// --- Diamond ---
fn diamond_valid_cases() -> Vec<ParseTestCase> {
    vec![
        ParseTestCase::valid("diamond left", "< x >"),
        ParseTestCase::valid("diamond right", "[ x ]"),
        ParseTestCase::valid("diamond nested lr", "< [ x ] >"),
        ParseTestCase::valid("diamond nested rl", "[ < x > ]"),
        ParseTestCase::valid("diamond deep", "< < < x > > >"),
        ParseTestCase::valid("diamond partial left", "< x"),
        ParseTestCase::valid("diamond partial right", "[ x"),
    ]
    .into_iter()
    .map(|c| c.with_context(vec![("x", "X")]))
    .collect()
}

fn diamond_invalid_cases() -> Vec<ParseTestCase> {
    vec![
        ParseTestCase::invalid("diamond mismatched", "< x ]"),
        ParseTestCase::invalid("diamond lone close", ">"),
        ParseTestCase::invalid("diamond lone bracket", "]"),
        ParseTestCase::invalid("diamond empty angle", "< >"),
    ]
}

// --- Mutual ---
fn mutual_valid_cases() -> Vec<ParseTestCase> {
    vec![
        ParseTestCase::valid("mutual number", "42"),
        ParseTestCase::valid("mutual bind", "set x : Num = 1 then x"),
        ParseTestCase::valid(
            "mutual nested",
            "set x : Num = 1 then set y : Num = 2 then x",
        ),
        ParseTestCase::valid("mutual paren", "( 7 )"),
        ParseTestCase::valid("mutual partial bind", "set x : Num ="),
        ParseTestCase::valid("mutual partial then", "set x : Num = 1 then"),
    ]
}

fn mutual_invalid_cases() -> Vec<ParseTestCase> {
    vec![
        ParseTestCase::invalid("mutual missing type", "set x = 1 then x"),
        ParseTestCase::invalid("mutual missing then", "set x : Num = 1 x"),
        ParseTestCase::invalid("mutual bad type", "set x : Bad = 1 then x"),
        ParseTestCase::invalid("mutual empty parens", "( )"),
    ]
}

// --- Epsilon wrapped ---
fn epsilon_wrapped_valid_cases() -> Vec<ParseTestCase> {
    vec![
        ParseTestCase::valid("ewrap bare", "x"),
        ParseTestCase::valid("ewrap pre", "pre x"),
        ParseTestCase::valid("ewrap post", "x post"),
        ParseTestCase::valid("ewrap both", "pre x post"),
        ParseTestCase::valid("ewrap partial pre", "pre"),
    ]
    .into_iter()
    .map(|c| c.with_context(vec![("x", "X")]))
    .collect()
}

fn epsilon_wrapped_invalid_cases() -> Vec<ParseTestCase> {
    vec![
        // "pre post" is actually valid: pre=Prefix, post=Variable(Identifier), Suffix=ε
        // So we only test genuinely invalid inputs here.
        ParseTestCase::invalid("ewrap number", "123"),
        ParseTestCase::invalid("ewrap special", "@x"),
    ]
}

// --- Regex heavy ---
fn regex_valid_cases() -> Vec<ParseTestCase> {
    vec![
        ParseTestCase::valid("regex number", "42"),
        ParseTestCase::valid("regex var", "abc"),
        ParseTestCase::valid("regex tag", "FOO . 99"),
        ParseTestCase::valid("regex nested tag", "A . B . 1"),
        ParseTestCase::valid("regex paren", "( 42 )"),
        ParseTestCase::valid("regex partial tag", "XY ."),
    ]
    .into_iter()
    .map(|c| c.with_context(vec![("abc", "Num")]))
    .collect()
}

fn regex_invalid_cases() -> Vec<ParseTestCase> {
    vec![
        ParseTestCase::invalid("regex dot alone", "."),
        ParseTestCase::invalid("regex tag no dot", "FOO 99"),
        ParseTestCase::invalid("regex special char", "@"),
    ]
}

// --- Scoped ---
fn scoped_valid_cases() -> Vec<ParseTestCase> {
    vec![
        ParseTestCase::valid("scoped let", "def a : X = 1 in a"),
        ParseTestCase::valid("scoped nested let", "def a : X = 1 in def b : X = 2 in a"),
        ParseTestCase::valid("scoped block", "{ 5 }"),
        ParseTestCase::valid("scoped let in block", "def a : X = 1 in { a }"),
        ParseTestCase::valid("scoped partial def", "def a : X ="),
    ]
}

fn scoped_invalid_cases() -> Vec<ParseTestCase> {
    vec![
        ParseTestCase::invalid("scoped missing in", "def a : X = 1 a"),
        ParseTestCase::invalid("scoped bad type", "def a : Z = 1 in a"),
        ParseTestCase::invalid("scoped empty braces", "{ }"),
    ]
}

// --- Stmt ---
fn stmt_valid_cases() -> Vec<ParseTestCase> {
    vec![
        ParseTestCase::valid("stmt empty block", "{ }"),
        ParseTestCase::valid("stmt single decl", "{ var x : I = 1 ; }"),
        ParseTestCase::valid("stmt two decls", "{ var x : I = 1 ; var y : I = 2 ; }"),
        ParseTestCase::valid("stmt partial", "{ var x : I ="),
    ]
}

fn stmt_invalid_cases() -> Vec<ParseTestCase> {
    vec![
        ParseTestCase::invalid("stmt no brace", "var x : I = 1 ;"),
        ParseTestCase::invalid("stmt missing semi", "{ var x : I = 1 }"),
        ParseTestCase::invalid("stmt bad type", "{ var x : Z = 1 ; }"),
        ParseTestCase::invalid("stmt missing eq", "{ var x : I 1 ; }"),
    ]
}

// --- Union choice ---
fn union_valid_cases() -> Vec<ParseTestCase> {
    vec![
        ParseTestCase::valid("union int", "42"),
        ParseTestCase::valid("union bool yes", "yes"),
        ParseTestCase::valid("union bool no", "no"),
        ParseTestCase::valid("union choice", "1 ? yes"),
        ParseTestCase::valid("union nested", "1 ? 2 ? no"),
        ParseTestCase::valid("union paren", "( yes )"),
        ParseTestCase::valid("union partial", "1 ?"),
    ]
}

fn union_invalid_cases() -> Vec<ParseTestCase> {
    vec![
        ParseTestCase::invalid("union question alone", "?"),
        ParseTestCase::invalid("union double question", "1 ? ? yes"),
        ParseTestCase::invalid("union special", "@"),
    ]
}

/// Expose suites for each inline grammar so the validate runner can exercise
/// each grammar independently.
pub fn suites() -> Vec<(
    &'static str,
    Grammar,
    Vec<ParseTestCase>,
    Vec<ParseTestCase>,
)> {
    vec![
        (
            "weird::right",
            load_inline_grammar(INFINITE_RIGHT_RECURSIVE),
            right_valid_cases(),
            right_invalid_cases(),
        ),
        (
            "weird::epsilon",
            load_inline_grammar(EPSILON_HEAVY),
            epsilon_valid_cases(),
            epsilon_invalid_cases(),
        ),
        (
            "weird::deep",
            load_inline_grammar(DEEP_NESTING),
            deep_valid_cases(),
            deep_invalid_cases(),
        ),
        (
            "weird::diamond",
            load_inline_grammar(DIAMOND),
            diamond_valid_cases(),
            diamond_invalid_cases(),
        ),
        (
            "weird::mutual",
            load_inline_grammar(MUTUAL),
            mutual_valid_cases(),
            mutual_invalid_cases(),
        ),
        (
            "weird::epsilon_wrapped",
            load_inline_grammar(EPSILON_WRAPPED),
            epsilon_wrapped_valid_cases(),
            epsilon_wrapped_invalid_cases(),
        ),
        (
            "weird::regex_heavy",
            load_inline_grammar(REGEX_HEAVY),
            regex_valid_cases(),
            regex_invalid_cases(),
        ),
        (
            "weird::scoped",
            load_inline_grammar(SCOPED),
            scoped_valid_cases(),
            scoped_invalid_cases(),
        ),
        (
            "weird::stmt",
            load_inline_grammar(STMT),
            stmt_valid_cases(),
            stmt_invalid_cases(),
        ),
        (
            "weird::union_choice",
            load_inline_grammar(UNION_CHOICE),
            union_valid_cases(),
            union_invalid_cases(),
        ),
    ]
}

#[test]
fn check_weird_parseable() {
    // Run each inline grammar's suite and ensure the parseable runner behaves as
    // expected (no failures in either valids or invalids).
    for (name, mut grammar, valids, invalids) in suites() {
        println!(
            "\n=== Weird suite: {} ({} valid + {} invalid) ===",
            name,
            valids.len(),
            invalids.len()
        );

        let (res_v, _) = run_parse_batch(&mut grammar, &valids);
        assert_eq!(
            res_v.failed,
            0,
            "{} valid failures: {}",
            name,
            res_v.format_failures()
        );

        let (res_i, _) = run_parse_batch(&mut grammar, &invalids);
        assert_eq!(
            res_i.failed,
            0,
            "{} invalid failures: {}",
            name,
            res_i.format_failures()
        );
    }
}
