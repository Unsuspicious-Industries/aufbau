use super::*;
use crate::typing::Context;
use crate::typing::Type;
use crate::typing::TypingSynth;
use crate::validation::parseable::check_all_prefixes_parseable;
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
    let g = crate::engine::grammar::SPG::load(grammar).expect("bad grammar");
    let mut synth = TypingSynth::new(g, input);
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
    let mut synth = TypingSynth::new(g, "λx:A.x");
    let result = synth.parse_with(&Context::new());
    assert!(
        result.is_ok(),
        "parse 'λx:A.x' in STLC failed: {:?}",
        result
    );
}

/// STLC: lambda-bound variable must not escape its scope when used as an argument.
/// Specifically `λx:A.x (x)` parses as `(λx:A.x)(x)`, but the outer `(x)` references
/// `x` which is only in scope inside the lambda — cross-context node reuse must not
/// allow the inner `{x:A}` context to bleed into the outer `{}` context.
#[test]
fn repro_stlc_escape() {
    set_debug_level(DebugLevel::Trace);
    crate::add_module_filter("fusion_typing");
    let g = load_example_grammar("stlc");
    let mut synth = TypingSynth::new(g, "λx:A.x (x)");
    let result = synth.parse_with(&Context::new());
    for r in result.iter() {
        println!("{}", r);
    }
    assert!(
        result.is_err(),
        "scope-escape: λx:A.x (x) should be rejected but parsed as: {:?}",
        result
    );
}

/// STLC: `(λx:A.x) y` — applying lambda to unbound variable must be rejected.
#[test]
fn repro_stlc_escape_applied_unbound() {
    let g = load_example_grammar("stlc");
    let mut synth = TypingSynth::new(g, "(λx:A.x) y");
    let result = synth.parse_with(&Context::new());
    assert!(
        result.is_err(),
        "scope-escape: (λx:A.x) y should be rejected (y unbound) but got: {:?}",
        result
    );
}

/// STLC: nested lambda where inner variable escapes outer application must be rejected.
#[test]
fn repro_stlc_escape_nested() {
    let g = load_example_grammar("stlc");
    // λx:A.x applied to (λy:B.y) applied to outer x — outer x is unbound
    let mut synth = TypingSynth::new(g, "λx:A.x (λy:B.y) (x)");
    let result = synth.parse_with(&Context::new());
    assert!(
        result.is_err(),
        "scope-escape nested: should be rejected but got: {:?}",
        result
    );
}

/// STLC: prefix 'λ' alone should produce a partial parse.
#[test]
fn repro_stlc_partial_lambda() {
    set_debug_level(DebugLevel::Trace);
    let g = load_example_grammar("stlc");
    let mut synth = TypingSynth::new(g, "λ");
    let result = synth.parse_with(&Context::new());
    assert!(result.is_ok(), "parse 'λ' in STLC failed: {:?}", result);
}

/// Fun: integer literal.
#[test]
fn repro_fun_integer() {
    let g = load_example_grammar("fun");
    let mut synth = TypingSynth::new(g, "42");
    let result = synth.parse_with(&Context::new());
    assert!(result.is_ok(), "parse '42' in Fun failed: {:?}", result);
}

/// Fun: partial prefix '1 +' should yield a partial parse.
#[test]
fn repro_fun_partial_add() {
    set_debug_level(DebugLevel::Trace);
    let g = load_example_grammar("fun");
    let mut synth = TypingSynth::new(g, "1 +");
    let result = synth.parse_with(&Context::new());
    assert!(result.is_ok(), "parse '1 +' in Fun failed: {:?}", result);
}

#[test]
fn repro_fun_prefix_lambda_app_open_paren() {
    set_debug_level(DebugLevel::Trace);
    crate::add_module_filter("fusion_parser");
    crate::add_module_filter("fusion_typing");
    let g = load_example_grammar("fun");
    let prefix = "((x: Int) => x + 1)(";
    let mut synth = TypingSynth::new(g, prefix);
    let result = synth.parse_with(&Context::new());
    println!("repro prefix='{}' result={:?}", prefix, result);
    assert!(
        result.is_ok(),
        "parse '{}' in Fun failed: {:?}",
        prefix,
        result
    );
}

/// Imp: basic block.
#[test]
fn repro_imp_assign_int() {
    let g = load_example_grammar("imp");
    let mut synth = TypingSynth::new(g, "{ let x:Int=5; }");
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
    let mut synth = TypingSynth::new(g, "{ let x:Int=5; let y:Int=3; }");
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
    let mut synth = TypingSynth::new(g, "beep: Fizz + blorp: Fizz");
    let result = synth.parse_with(&Context::new());
    assert!(result.is_ok(), "toy concat failed: {:?}", result);
}

#[test]
#[ignore = "debug trace for toy typed value"]
fn trace_toy_typed_value() {
    set_debug_level(DebugLevel::Trace);
    crate::clear_module_filters();
    crate::add_module_filter("fusion_parser");
    crate::add_module_filter("fusion_typing");
    let g = load_example_grammar("toy");
    let input = "beep: Fizz";
    let mut synth = TypingSynth::new(g, input);
    let result = synth.parse_with(&Context::new());
    println!("toy typed value input='{}' result={:?}", input, result);
    assert!(result.is_ok(), "toy typed value failed: {:?}", result);
}

#[test]
#[ignore = "debug trace for fun float operator prefix"]
fn trace_fun_float_op_prefix() {
    set_debug_level(DebugLevel::Trace);
    crate::clear_module_filters();
    crate::add_module_filter("fusion_parser");
    crate::add_module_filter("fusion_typing");
    let g = load_example_grammar("fun");
    let input = "10 /.";
    let mut synth = TypingSynth::new(g, input);
    let result = synth.parse_with(&Context::new());
    println!("fun float-op prefix input='{}' result={:?}", input, result);
    assert!(result.is_ok(), "fun float-op prefix failed: {:?}", result);
}

/// Fun: "1.0 +. 2" — the trailing "2" is a prefix of a valid Float
/// (e.g. "2.0"), so the parser should produce a partial parse.
#[test]
fn repro_fun_float_partial() {
    set_debug_level(DebugLevel::Trace);
    let g = load_example_grammar("fun");
    let mut synth = TypingSynth::new(g, "1.0 +. 2");
    let result = synth.parse_with(&Context::new());
    assert!(result.is_ok(), "fun float partial failed: {:?}", result);
    let ast = result.unwrap();
    assert!(!ast.is_complete(), "should be partial, not complete");
}

#[test]
#[ignore = "debug trace for context-flow investigation"]
fn trace_imp_context_flow_seq_decl_use() {
    set_debug_level(DebugLevel::Trace);
    crate::clear_module_filters();
    crate::add_module_filter("fusion_parser");
    crate::add_module_filter("fusion_typing");

    let g = load_example_grammar("imp");
    let input = "{ let x:Int=5; let y:Int=x; }";
    let mut synth = TypingSynth::new(g, input);
    let result = synth.parse_with(&Context::new());
    println!("imp trace input='{}' result={:?}", input, result);
    assert!(result.is_ok(), "imp context flow failed: {:?}", result);
}

#[test]
#[ignore = "debug trace for application-path investigation"]
fn trace_fun_apply_bool_context() {
    set_debug_level(DebugLevel::Trace);
    crate::clear_module_filters();
    crate::add_module_filter("fusion_parser");
    crate::add_module_filter("fusion_typing");

    let g = load_example_grammar("fun");
    let mut synth = TypingSynth::new(g, "f(true)");
    let result = synth.parse_with(&ctx_of(&[("f", "Bool -> Int")]));
    println!("fun trace input='f(true)' result={:?}", result);
    assert!(result.is_ok(), "fun bool application failed: {:?}", result);
}

#[test]
#[ignore = "debug trace for application-prefix investigation"]
fn trace_fun_apply_open_paren_prefix() {
    set_debug_level(DebugLevel::Trace);
    crate::clear_module_filters();
    crate::add_module_filter("fusion_parser");
    crate::add_module_filter("fusion_typing");

    let g = load_example_grammar("fun");
    let mut synth = TypingSynth::new(g, "f(");
    let result = synth.parse_with(&ctx_of(&[("f", "Bool -> Int")]));
    println!("fun trace input='f(' result={:?}", result);
    assert!(result.is_ok(), "fun open-paren prefix failed: {:?}", result);
}

#[test]
#[ignore = "debug trace for stlc left-application investigation"]
fn trace_stlc_left_app_ctx() {
    set_debug_level(DebugLevel::Trace);
    crate::clear_module_filters();
    crate::add_module_filter("fusion_parser");
    crate::add_module_filter("fusion_typing");

    let g = load_example_grammar("stlc");
    let mut synth = TypingSynth::new(g, "f x");
    let result = synth.parse_with(&ctx_of(&[("f", "A->B"), ("x", "A")]));
    println!("stlc trace input='f x' result={:?}", result);
    assert!(result.is_ok(), "stlc left app failed: {:?}", result);
}

#[test]
#[ignore = "debug trace for lambda-scoped STLC chain"]
fn trace_stlc_lambda_chain_ctx() {
    set_debug_level(DebugLevel::Trace);
    crate::clear_module_filters();
    crate::add_module_filter("fusion_parser");
    crate::add_module_filter("fusion_typing");

    let g = load_example_grammar("stlc");
    let input = "λf:A->B->C.λx:A.λy:B.f x";
    let mut synth = TypingSynth::new(g, input);
    let result = synth.parse_with(&Context::new());
    println!(
        "stlc lambda-chain trace input='{}' result={:?}",
        input, result
    );
    assert!(result.is_ok(), "stlc lambda chain failed: {:?}", result);
}

#[test]
#[ignore = "debug trace for parenthesized STLC app chain prefix"]
fn trace_stlc_paren_double_app_prefix() {
    set_debug_level(DebugLevel::Trace);
    crate::clear_module_filters();
    crate::add_module_filter("fusion_parser");
    crate::add_module_filter("fusion_typing");

    let g = load_example_grammar("stlc");
    let input = "((f x) y";
    let mut synth = TypingSynth::new(g, input);
    let result = synth.parse_with(&ctx_of(&[("f", "A->B->C"), ("x", "A"), ("y", "B")]));
    println!(
        "stlc paren double app prefix input='{}' result={:?}",
        input, result
    );
    assert!(
        result.is_ok(),
        "stlc paren double app prefix failed: {:?}",
        result
    );
}

#[test]
#[ignore = "debug trace for long left-application investigation"]
fn trace_stlc_long_left_app_ctx() {
    let g = load_example_grammar("stlc");
    let mut synth = TypingSynth::new(g, "f x y z w v u");
    let result = synth.parse_with(&ctx_of(&[
        ("f", "A->B->C->D->E->F->G"),
        ("x", "A"),
        ("y", "B"),
        ("z", "C"),
        ("w", "D"),
        ("v", "E"),
        ("u", "F"),
    ]));
    println!("stlc long trace result={:?}", result);
    assert!(result.is_ok(), "stlc long left app failed: {:?}", result);
}

#[test]
#[ignore = "debug trace for parseable STLC prefix runner"]
fn trace_stlc_parseable_prefix_runner() {
    let mut grammar = load_example_grammar("stlc");
    let result =
        check_all_prefixes_parseable(&mut grammar, "f x", &ctx_of(&[("f", "A->B"), ("x", "A")]));
    println!("stlc parseable prefix runner result={:?}", result);
    assert!(matches!(
        result,
        crate::validation::parseable::ParseResult::Pass { .. }
    ));
}

#[test]
#[ignore = "debug trace for sequential STLC prefixes"]
fn trace_stlc_prefixes_sequential() {
    let grammar = load_example_grammar("stlc");
    let ctx = ctx_of(&[("f", "A->B"), ("x", "A")]);
    for prefix in ["f", "f x"] {
        let mut synth = TypingSynth::new(grammar.clone(), prefix);
        let result = synth.parse_with(&ctx);
        println!("stlc sequential prefix='{}' result={:?}", prefix, result);
        assert!(result.is_ok(), "prefix {} failed: {:?}", prefix, result);
    }
}

#[test]
#[ignore = "debug trace for transparent unary propagation"]
fn trace_transparent_unary_propagation() {
    set_debug_level(DebugLevel::Trace);
    crate::clear_module_filters();
    crate::add_module_filter("fusion_parser");
    crate::add_module_filter("fusion_typing");

    let grammar = crate::engine::grammar::SPG::load(
        r#"
        Identifier ::= /[a-z]+/
        Variable(var) ::= Identifier[x]
        Wrap ::= Variable
        Start ::= Wrap

        x ∈ Γ
        ----------- (var)
        Γ(x)
        "#,
    )
    .unwrap();
    let mut synth = TypingSynth::new(grammar, "x");
    let ast = synth.parse_with(&ctx_of(&[("x", "'X'")])).unwrap();
    let arena = ast.arena();
    println!(
        "type0={:?} type1={:?} type2={:?}",
        synth.runtime().evidence_of(0),
        synth.runtime().evidence_of(1),
        synth.runtime().evidence_of(2)
    );
    for id in 0..arena.node_count() {
        let node = arena.node(id).unwrap();
        println!(
            "node id={} nt={} status={:?} ty={:?}",
            id,
            synth.grammar().nt(node.nt).unwrap_or("<?>"),
            node.status,
            synth.runtime().evidence_of(node.evidence)
        );
    }
    for &id in ast.root_ids() {
        let node = arena.node(id).unwrap();
        println!(
            "root id={} nt={} status={:?} ty={:?}",
            id,
            synth.grammar().nt(node.nt).unwrap_or("<?>"),
            node.status,
            synth.runtime().evidence_of(node.evidence)
        );
    }
}

#[test]
#[ignore = "debug trace for epsilon-wrapped prefix investigation"]
fn trace_weird_pre_prefix() {
    set_debug_level(DebugLevel::Trace);
    crate::clear_module_filters();
    crate::add_module_filter("fusion_parser");
    crate::add_module_filter("fusion_typing");

    let grammar = SPG::load(
        r#"
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
    "#,
    )
    .unwrap();
    let mut synth = TypingSynth::new(grammar, "pre");
    let result = synth.parse_with(&ctx_of(&[("x", "T")]));
    println!("weird trace input='pre' result={:?}", result);
    assert!(
        result.is_ok(),
        "epsilon-wrapped prefix failed: {:?}",
        result
    );
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
    assert!(
        result.is_ok(),
        "prefix 'let x : int in' should parse: {:?}",
        result
    );
}

// ── Isolation tests for weird grammars (frontier lifting termination) ────────

#[test]
fn repro_weird_epsilon_empty() {
    let g = SPG::load("A ::= 'a' B | ε\nB ::= 'b' C | ε\nC ::= 'c' | ε\nstart ::= A B C").unwrap();
    let mut synth = TypingSynth::new(g, "");
    let result = synth.parse_with(&Context::new());
    assert!(result.is_ok(), "epsilon empty: {:?}", result);
}

#[test]
fn repro_weird_epsilon_a() {
    let g = SPG::load("A ::= 'a' B | ε\nB ::= 'b' C | ε\nC ::= 'c' | ε\nstart ::= A B C").unwrap();
    let mut synth = TypingSynth::new(g, "a");
    let result = synth.parse_with(&Context::new());
    assert!(result.is_ok(), "epsilon a: {:?}", result);
}

#[test]
fn repro_weird_deep_x() {
    let g = SPG::load("Atom ::= 'x'\nL1 ::= '(' L2 ')' | Atom\nL2 ::= '(' L3 ')' | L1\nL3 ::= '(' L3 ')' | L2\nstart ::= L3").unwrap();
    let mut synth = TypingSynth::new(g, "x");
    let result = synth.parse_with(&Context::new());
    assert!(result.is_ok(), "deep x: {:?}", result);
}

#[test]
fn repro_weird_stmt_empty_block() {
    let g = SPG::load(
        r#"
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

    "#,
    )
    .unwrap();
    let mut synth = TypingSynth::new(g, "{ }");
    let result = synth.parse_with(&Context::new());
    assert!(result.is_ok(), "stmt empty block: {:?}", result);
}

#[test]
fn repro_weird_diamond_deep() {
    let g = SPG::load(
        r#"
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
    "#,
    )
    .unwrap();
    let ctx = ctx_of(&[("x", "X")]);
    let mut synth = TypingSynth::new(g, "< < < x > > >");
    let result = synth.parse_with(&ctx);
    assert!(result.is_ok(), "diamond deep: {:?}", result);
}

#[test]
fn repro_weird_scoped_let() {
    let g = SPG::load(
        r#"
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
    "#,
    )
    .unwrap();
    let mut synth = TypingSynth::new(g, "def a : X = 1 in a");
    let result = synth.parse_with(&Context::new());
    assert!(result.is_ok(), "scoped let: {:?}", result);
}

#[test]
fn repro_weird_mutual_nested() {
    let mut synth = TypingSynth::new(
        SPG::load(MUTUAL).unwrap(),
        "set x : Num = 1 then set y : Num = 2 then x",
    );
    let result = synth.parse_with(&Context::new());
    assert!(result.is_ok(), "mutual nested: {:?}", result);
}

#[test]
fn repro_weird_union_partial() {
    let g = SPG::load(
        r#"
    Identifier ::= /[a-z]+/
    TyName ::= 'N' | 'B'
    Union ::= TyName '|' Ty
    Ty(*) ::= TyName | Union
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
    "#,
    )
    .unwrap();
    let mut synth = TypingSynth::new(g, "1 ?");
    let result = synth.parse_with(&Context::new());
    assert!(result.is_ok(), "union partial: {:?}", result);
}
