use super::*;
use crate::logic::synth::Synthesizer;
use crate::logic::typing::Context;
use crate::logic::typing::Type;
use crate::validation::parseable::load_example_grammar;

// ── helpers ──────────────────────────────────────────────────────────────────

fn ctx_of(pairs: &[(&str, &str)]) -> Context {
    let mut ctx = Context::new();
    for (name, ty) in pairs {
        ctx.add(name.to_string(), Type::parse_raw(ty).unwrap());
    }
    ctx
}

fn parse(grammar: &str, input: &str, ctx: &Context) -> Result<String, String> {
    let g = crate::logic::grammar::Grammar::load(grammar).expect("bad grammar");
    let mut synth = Synthesizer::new(g, input);
    match synth.parse_with(ctx) {
        Ok(ast) => Ok(format!(
            "ok: complete={} roots={}",
            ast.is_complete(),
            ast.len()
        )),
        Err(e) => Err(e),
    }
}

// ── inline grammars (copied from weird.rs) ───────────────────────────────────

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

// ── repro tests ──────────────────────────────────────────────────────────────

/// Minimal typed grammar: a number literal should parse as 'Num'.
#[test]
fn repro_mutual_number_42() {
    crate::set_debug_level(crate::DebugLevel::Trace);
    let result = parse(MUTUAL, "42", &Context::new());
    assert!(
        result.is_ok(),
        "parse '42' in MUTUAL grammar failed: {:?}",
        result
    );
}

/// "set x : Num =" — partial bind prefix in MUTUAL grammar.
#[test]
fn repro_mutual_partial_bind() {
    let result = parse(MUTUAL, "set x : Num =", &Context::new());
    assert!(
        result.is_ok(),
        "parse 'set x : Num =' in MUTUAL failed: {:?}",
        result
    );
}

/// Variable lookup: 'x' in a grammar that requires x ∈ Γ, with x provided.
#[test]
fn repro_ewrap_bare_x() {
    crate::set_debug_level(crate::DebugLevel::Trace);
    crate::add_module_filter("fusion_parser");
    crate::add_module_filter("fusion_typing");
    let ctx = ctx_of(&[("x", "X")]);
    let result = parse(EPSILON_WRAPPED, "x", &ctx);
    assert!(
        result.is_ok(),
        "parse 'x' in EPSILON_WRAPPED with context {{x:X}} failed: {:?}",
        result
    );
}

/// 'pre' prefix before a variable — Prefix=pre, Core=x, Suffix=ε.
#[test]
fn repro_ewrap_pre_x() {
    crate::set_debug_level(crate::DebugLevel::Trace);
    let ctx = ctx_of(&[("x", "X")]);
    let result = parse(EPSILON_WRAPPED, "pre x", &ctx);
    assert!(
        result.is_ok(),
        "parse 'pre x' in EPSILON_WRAPPED with context {{x:X}} failed: {:?}",
        result
    );
}

/// STLC: identity function λx:A.x should parse completely.
#[test]
fn repro_stlc_identity_a() {
    set_debug_level(DebugLevel::Trace);
    crate::add_module_filter("fusion_typing");
    let g = load_example_grammar("stlc");
    let mut synth = Synthesizer::new(g, "λx:A.x");
    let result = synth.parse_with(&Context::new());
    assert!(
        result.is_ok(),
        "parse 'λx:A.x' in STLC failed: {:?}",
        result
    );
}

/// STLC: prefix 'λ' alone should produce a partial parse.
#[test]
fn repro_stlc_partial_lambda() {
    let g = load_example_grammar("stlc");
    let mut synth = Synthesizer::new(g, "λ");
    let result = synth.parse_with(&Context::new());
    assert!(result.is_ok(), "parse 'λ' in STLC failed: {:?}", result);
}

/// Fun: integer literal.
#[test]
fn repro_fun_integer() {
    let g = load_example_grammar("fun");
    let mut synth = Synthesizer::new(g, "42");
    let result = synth.parse_with(&Context::new());
    assert!(result.is_ok(), "parse '42' in Fun failed: {:?}", result);
}

/// Fun: partial prefix '1 +' should yield a partial parse.
#[test]
fn repro_fun_partial_add() {
    let g = load_example_grammar("fun");
    let mut synth = Synthesizer::new(g, "1 +");
    let result = synth.parse_with(&Context::new());
    assert!(result.is_ok(), "parse '1 +' in Fun failed: {:?}", result);
}

/// Imp: basic block.
#[test]
fn repro_imp_assign_int() {
    let g = load_example_grammar("imp");
    let mut synth = Synthesizer::new(g, "{ let x:Int=5; }");
    let result = synth.parse_with(&Context::new());
    assert!(
        result.is_ok(),
        "parse '{{ let x:Int=5; }}' in Imp failed: {:?}",
        result
    );
}

/// Imp: two decl block — context must propagate.
#[test]
fn repro_imp_two_decls() {
    let g = load_example_grammar("imp");
    let mut synth = Synthesizer::new(g, "{ let x:Int=5; let y:Int=3; }");
    let result = synth.parse_with(&Context::new());
    assert!(
        result.is_ok(),
        "parse '{{ let x:Int=5; let y:Int=3; }}' in Imp failed: {:?}",
        result
    );
}

/// Toy: typed concat with same types.
#[test]
fn repro_toy_concat() {
    let g = load_example_grammar("toy");
    let mut synth = Synthesizer::new(g, "beep: Fizz + blorp: Fizz");
    let result = synth.parse_with(&Context::new());
    assert!(result.is_ok(), "toy concat failed: {:?}", result);
}

/// Fun: "1.0 +. 2" — the trailing "2" is a prefix of a valid Float
/// (e.g. "2.0"), so the parser should produce a partial parse.
#[test]
fn repro_fun_float_partial() {
    let g = load_example_grammar("fun");
    let mut synth = Synthesizer::new(g, "1.0 +. 2");
    let result = synth.parse_with(&Context::new());
    assert!(result.is_ok(), "fun float partial failed: {:?}", result);
    let ast = result.unwrap();
    assert!(!ast.is_complete(), "should be partial, not complete");
}

// ── Prefix soundness repros ──────────────────────────────────────────────────

#[test]
fn repro_partial_identifier_prefix() {
    crate::set_debug_level(crate::DebugLevel::Trace);
    crate::clear_module_filters();
    crate::add_module_filter("fusion_parser");
    crate::add_module_filter("fusion_typing");

    let spec = r#"
        Identifier ::= /[a-z]+/
        Variable(var) ::= Identifier[x]
        Expression ::= Variable

        x ∈ Γ
        ----------- (var)
        Γ(x)
    "#;
    let ctx = ctx_of(&[("foo", "bool")]);
    let result = parse(spec, "f", &ctx);
    assert!(result.is_ok(), "prefix 'f' should parse: {:?}", result);
}

#[test]
fn repro_let_x_prefix() {
    let spec = r#"
        Identifier ::= /[a-z]+/
        Let ::= 'let' Identifier ':' 'int' 'in' Identifier
        Expression ::= Let | Identifier
    "#;
    let result = parse(spec, "let x", &Context::new());
    assert!(result.is_ok(), "prefix 'let x' should parse: {:?}", result);
}

#[test]
fn repro_let_int_in_x_complete() {
    let spec = r#"
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
    let result = parse(spec, "let x : int in x", &Context::new());
    eprintln!("result: {:?}", result);
    assert!(result.is_ok(), "should parse: {:?}", result);
}

#[test]
fn repro_let_prefix_before_body() {
    let spec = r#"
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
    let result = parse(spec, "let x : int in", &Context::new());
    assert!(result.is_ok(), "prefix 'let x : int in' should parse: {:?}", result);
}

// ── Isolation tests for weird grammars (frontier lifting termination) ────────

#[test]
fn repro_weird_epsilon_empty() {
    let g = Grammar::load("A ::= 'a' B | ε\nB ::= 'b' C | ε\nC ::= 'c' | ε\nstart ::= A B C").unwrap();
    let mut synth = Synthesizer::new(g, "");
    let result = synth.parse_with(&Context::new());
    assert!(result.is_ok(), "epsilon empty: {:?}", result);
}

#[test]
fn repro_weird_epsilon_a() {
    let g = Grammar::load("A ::= 'a' B | ε\nB ::= 'b' C | ε\nC ::= 'c' | ε\nstart ::= A B C").unwrap();
    let mut synth = Synthesizer::new(g, "a");
    let result = synth.parse_with(&Context::new());
    assert!(result.is_ok(), "epsilon a: {:?}", result);
}

#[test]
fn repro_weird_deep_x() {
    let g = Grammar::load("Atom ::= 'x'\nL1 ::= '(' L2 ')' | Atom\nL2 ::= '(' L3 ')' | L1\nL3 ::= '(' L3 ')' | L2\nstart ::= L3").unwrap();
    let mut synth = Synthesizer::new(g, "x");
    let result = synth.parse_with(&Context::new());
    assert!(result.is_ok(), "deep x: {:?}", result);
}

#[test]
fn repro_weird_stmt_empty_block() {
    let g = Grammar::load(r#"
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
    "#).unwrap();
    let mut synth = Synthesizer::new(g, "{ }");
    let result = synth.parse_with(&Context::new());
    assert!(result.is_ok(), "stmt empty block: {:?}", result);
}

#[test]
fn repro_weird_diamond_deep() {
    let g = Grammar::load(r#"
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
    "#).unwrap();
    let ctx = ctx_of(&[("x", "X")]);
    let mut synth = Synthesizer::new(g, "< < < x > > >");
    let result = synth.parse_with(&ctx);
    assert!(result.is_ok(), "diamond deep: {:?}", result);
}

#[test]
fn repro_weird_scoped_let() {
    let g = Grammar::load(r#"
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
    "#).unwrap();
    let mut synth = Synthesizer::new(g, "def a : X = 1 in a");
    let result = synth.parse_with(&Context::new());
    assert!(result.is_ok(), "scoped let: {:?}", result);
}

#[test]
fn repro_weird_mutual_nested() {
    let mut synth = Synthesizer::new(
        Grammar::load(MUTUAL).unwrap(),
        "set x : Num = 1 then set y : Num = 2 then x",
    );
    let result = synth.parse_with(&Context::new());
    assert!(result.is_ok(), "mutual nested: {:?}", result);
}

#[test]
fn repro_weird_union_partial() {
    let g = Grammar::load(r#"
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
    "#).unwrap();
    let mut synth = Synthesizer::new(g, "1 ?");
    let result = synth.parse_with(&Context::new());
    assert!(result.is_ok(), "union partial: {:?}", result);
}
