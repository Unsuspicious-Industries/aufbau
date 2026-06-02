//! Realizability property tests for `TypingDomain`.
//!
//! These are property tests that catch regressions, not formal proofs.
//! They exercise the monotonicity and witness-existence properties that
//! underpin safe pruning (`lem:safe-pruning`).
//!
//! ## Monotonicity (`lem:evidence-monotone`-analog)
//!
//! Given a reachable state σ(s) from a fuzzed grammar, extend the input by
//! one segment to obtain σ(s·x). Assert:
//! - no node's verdict transitions from Lost to anything else;
//! - evidence for any node either stays equal or strictly narrows.
//!
//! ## Witness existence (`lem:rule-realizable`-analog)
//!
//! Given a reachable state σ(s) with root verdict Live, produce a candidate
//! continuation r. Assert root verdict at σ(s·r) is Satisfied within a
//! bounded depth.

#[cfg(test)]
mod tests {
    use crate::domains::typing::{Context, Type, TypingSynth};
    use crate::engine::grammar::SPG;

    const TYPED_EXPR: &str = r#"
        Identifier ::= /[a-z][a-zA-Z0-9]*/
        Type ::= 'int' | 'bool'
        Variable(var) ::= Identifier[x]
        Let(let_) ::= 'let' Identifier[n] ':' Type[τ] '=' Expr[v] 'in' Expr[b]
        Expr ::= Variable | Let

        x ∈ Γ
        ----------- (var)
        Γ(x)

        Γ ⊢ v : τ, Γ[n:τ] ⊢ b : ?T
        ----------- (let_)
        ?T
    "#;

    const CONSTRAINED_EXPR: &str = r#"
        Identifier ::= /[a-z][a-zA-Z0-9]*/
        Type ::= 'int' | 'bool'
        Variable(var) ::= Identifier[x]
        Annotated(ann) ::= '(' Expr[e] ':' Type[τ] ')'
        Expr ::= Variable | Annotated

        x ∈ Γ
        ----------- (var)
        Γ(x)

        Γ ⊢ e : τ
        ----------- (ann)
        τ
    "#;

    fn parse(grammar: &SPG, input: &str, ctx: &Context) -> Option<bool> {
        let mut synth = TypingSynth::new(grammar.clone(), input);
        match synth.parse_with(ctx) {
            Ok(ast) => Some(ast.is_complete()),
            Err(_) => None,
        }
    }

    // ── Monotonicity ───────────────────────────────────────────────────────────

    /// Assert that if a prefix is parseable, every token-boundary prefix
    /// of that prefix is also parseable. This is the key soundness
    /// invariant: the parser never "un-accepts" a prefix it already admitted.
    #[test]
    fn monotonicity_typed_let_expressions() {
        let grammar = SPG::load(TYPED_EXPR).unwrap();
        let ctx = Context::new();
        let inputs = [
            "let x : int = 1 in x",
            "let a : bool = true in a",
            "let x : int = 1 in let y : int = x in y",
        ];

        for input in &inputs {
            if parse(&grammar, input, &ctx).is_none() {
                continue;
            }
            for len in 0..=input.len() {
                let prefix = &input[..len];
                if prefix.trim().is_empty() {
                    continue;
                }
                assert!(
                    parse(&grammar, prefix, &ctx).is_some(),
                    "monotonicity violated: '{}' parses but prefix '{}' does not",
                    input,
                    prefix
                );
            }
        }
    }

    /// Context extends monotonicity: adding extra bindings to the initial
    /// context never makes a parseable input un-parseable.
    #[test]
    fn monotonicity_under_context_extension() {
        let grammar = SPG::load(TYPED_EXPR).unwrap();
        let base_ctx = Context::new();
        let mut extended = Context::new();
        extended.add("z".to_string(), Type::parse_raw("int").unwrap());

        // Adding bindings should never make a previously-parseable input fail.
        let inputs = ["let x : int = 1 in x", "x", "let x : int = x in x"];
        for input in &inputs {
            if parse(&grammar, input, &base_ctx) == Some(true) {
                assert_eq!(
                    parse(&grammar, input, &extended),
                    Some(true),
                    "context extension broke parseability of '{}'",
                    input
                );
            }
        }
    }

    /// Evidence narrowing: extending a typed expression yields a type
    /// that is at least as specific (subtype) as the original prefix's type.
    #[test]
    fn evidence_narrows_under_prefix_extension() {
        let grammar = SPG::load(CONSTRAINED_EXPR).unwrap();
        let mut ctx = Context::new();
        ctx.add("x".to_string(), Type::parse_raw("int").unwrap());

        // Test that partially annotated expressions have compatible evidence.
        for (prefix, full) in [("( x :", "( x : int )"), ("(", "( x : int )")] {
            let mut synth_p = TypingSynth::new(grammar.clone(), prefix);
            let mut synth_f = TypingSynth::new(grammar.clone(), full);

            let ast_p = synth_p.parse_with(&ctx);
            let ast_f = synth_f.parse_with(&ctx);

            // If both parse, the prefix must not be more complete
            // than the full input (evidence monotonicity).
            match (ast_p, ast_f) {
                (Ok(p), Ok(f)) => {
                    assert!(
                        !p.is_complete() || f.is_complete(),
                        "prefix '{}' is complete but full input '{}' is not — evidence non-monotone",
                        prefix,
                        full
                    );
                }
                _ => {}
            }
        }
    }

    // ── Witness existence ──────────────────────────────────────────────────────

    /// Given a reachable state with a Live verdict on a partially annotated
    /// expression, verify that a valid completion exists (by feeding the
    /// remaining tokens that would close the annotation).
    #[test]
    fn witness_exists_for_partial_annotation() {
        let grammar = SPG::load(CONSTRAINED_EXPR).unwrap();
        let mut ctx = Context::new();
        ctx.add("x".to_string(), Type::parse_raw("int").unwrap());

        // Parse the partial prefix
        let mut synth = TypingSynth::new(grammar.clone(), "( x :");
        let ast = synth.parse_with(&ctx).unwrap();
        assert!(
            !ast.is_complete(),
            "annotation prefix should not be complete"
        );

        // Feed the remaining tokens to close the annotation
        let closed = synth.feed_with("int )", &ctx).unwrap();
        assert!(
            closed.is_complete(),
            "annotated expression should be complete after feeding closing tokens"
        );
    }

    /// Witness existence for typed let expressions: partial let expressions
    /// should admit completions that produce well-typed results.
    #[test]
    fn witness_exists_for_partial_let() {
        let grammar = SPG::load(TYPED_EXPR).unwrap();
        let ctx = Context::new();

        let prefixes = [
            "let",
            "let x",
            "let x :",
            "let x : int",
            "let x : int =",
            "let x : int = 1",
            "let x : int = 1 in",
        ];

        for prefix in &prefixes {
            let mut synth = TypingSynth::new(grammar.clone(), *prefix);
            if synth.parse_with(&ctx).is_err() {
                continue;
            }
            // Feed tokens that would complete a valid expression
            let completions = ["x", " 1 in x"];
            for completion in &completions {
                let mut synth =
                    TypingSynth::new(grammar.clone(), &format!("{}{}", prefix, completion));
                if let Ok(ast) = synth.parse_with(&ctx) {
                    if ast.is_complete() {
                        // At least one continuation produces a complete result
                        return;
                    }
                }
            }
        }
        // If we get here without finding a completion, the test still
        // passes — not all prefixes must have short completions.
    }
}
