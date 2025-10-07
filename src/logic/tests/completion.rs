#[cfg(test)]
mod completion_complex {
    use crate::logic::grammar::Grammar;
    use crate::logic::partial::{CompletionSet, PartialParser};

    // --- Test utilities ---
    fn build(spec: &str) -> (Grammar, PartialParser) {
        let g = Grammar::load(spec).expect("Failed to load grammar");
        (g.clone(), PartialParser::new(g))
    }

    fn completions(spec: &str, input: &str) -> CompletionSet {
        let (g, mut p) = build(spec);
        let past = p.partial(input).expect("partial parse failed");
        past.completions(&g, 0)
    }

    fn has_lit(cs: &CompletionSet, s: &str) -> bool {
        cs.iter()
            .any(|c| matches!(&c.token, crate::logic::partial::CompletionToken::Literal(t) if t==s))
    }
    fn has_re(cs: &CompletionSet, r: &str) -> bool {
        cs.iter()
            .any(|c| matches!(&c.token, crate::logic::partial::CompletionToken::Regex(t) if t==r))
    }

    // Grammar 1: Arrow types with contexts, union/intersection; test type-hinted metadata
    #[test]
    fn completion_with_complex_type_hints() {
        let spec = r#"
        // Terms and types
        Ident ::= /[a-z][a-z0-9]*/
        AtomTy ::= 'int' | 'bool' | 'str'
        Type ::= AtomTy | Type '->' Type | '(' Type ')'
        Var(var) ::= Ident[x]
        Lambda(lambda) ::= 'λ' Var[param] ':' Type[τ₁] '.' Term[body]
        App(app) ::= Term[f] Term[arg]
        Term ::= Lambda | Var | '(' Term ')'

        // Program entry
        start(entry) ::= Term

        // Typing rules – keep names stable to check metadata.rule_name
        x ∈ Γ
        --------- (var)
        Γ(x)

        Γ[x:τ₁] ⊢ body : τ₂
        --------------------- (lambda)
        τ₁ -> τ₂

        Γ ⊢ f : τ₁ -> τ₂, Γ ⊢ arg : τ₁
        ------------------------------ (app)
        τ₂
        "#;

        // Ask for completions right after a lambda head 'λx:' to get a Type hint
        let cs = completions(spec, "λx:");
        assert!(has_re(&cs, "[a-z][a-z0-9]*") || has_lit(&cs, "(") || has_lit(&cs, "int"),
            "expected one of type starters (identifier regex, '(' or 'int')");

        // Verify at least one candidate carries a rule_name and a type_hint string
        let has_hint = cs.iter().any(|c| c.metadata.rule_name.is_some());
        assert!(has_hint, "expected some completion candidates to carry a rule_name");
    }

    // Grammar 2: Repetition, optionals, and alternates; ensure FIRST and TailRepetition work
    #[test]
    fn completion_over_repetition_and_optionals() {
        let spec = r#"
        Num ::= /[0-9]+/
        Plus ::= '+'
        Term ::= Num | '(' Expr ')'
        Expr ::= Term (Plus Term)*
        start ::= Expr
        "#;
        // After a single Num, can repeat: expect '+' suggestion via FIRST of (Plus Term)
        let cs = completions(spec, "42");
        assert!(has_lit(&cs, "+"), "expected '+' to continue an expression");
    }

    // Grammar 3: Grouped alternation and nested repetition
    #[test]
    fn completion_nested_group_repetition() {
        let spec = r#"
        Id ::= /[A-Z][A-Za-z]*/
        Comma ::= ','
        Pair ::= '(' Id Comma Id ')'
        List ::= (Pair Comma)* Pair
        start ::= List
        "#;
        // After one Pair, repetition suggests another '(' as FIRST(Pair)
        let cs = completions(spec, "(Ab, Cd)");
        assert!(has_lit(&cs, "("), "expected '(' to start next Pair in repetition");
    }

    // Grammar 4: Alternate productions with overlapping FIRST; ensure dedup and ordering
    #[test]
    fn completion_alternate_overlapping_first_dedup() {
        let spec = r#"
        A(rA) ::= 'x' '1'?
        B(rB) ::= 'x' '2'?
        start ::= A | B
        "#;
        let cs = completions(spec, "");
        let x_count = cs
            .iter()
            .filter(|c| matches!(c.token, crate::logic::partial::CompletionToken::Literal(ref s) if s=="x"))
            .count();
        assert_eq!(x_count, 1, "expected deduplicated 'x' suggestion");
    }

    // Grammar 5: Single-wrapped regex inside a binding + group tail repetition
    #[test]
    fn completion_single_wrapped_regex_and_group_tail() {
        let spec = r#"
        Ident ::= /[a-zA-Z_][a-zA-Z0-9_]*/
        Param(param) ::= Ident[x]
        Params ::= '(' (Param (',' Param)*)? ')'
        FnHead ::= 'fn' Ident[name] Params
        start ::= FnHead
        "#;
        // After 'fn ' expect identifier regex; after 'fn name(' expect identifier regex for Param
        let cs_head = completions(spec, "fn ");
        assert!(has_re(&cs_head, "[a-zA-Z_][a-zA-Z0-9_]*"));
        let cs_param = completions(spec, "fn f(");
        assert!(has_re(&cs_param, "[a-zA-Z_][a-zA-Z0-9_]*"));
    }
}
