use crate::logic::fusion::DepthConfig;
use crate::logic::grammar::Grammar;
use crate::logic::typing::Context;

use super::{search, search_k, SearchResult, Synthesizer};
use crate::logic::fusion::ast::FusionAST;

fn completion_fingerprint(ast: &FusionAST, grammar: &Grammar) -> Vec<(String, Option<String>)> {
    ast.completions(grammar)
        .into_iter()
        .map(|token| (token.to_pattern(), token.example()))
        .collect()
}

fn arithmetic_grammar() -> Grammar {
    Grammar::load(
        r#"
        Number ::= /[0-9]+/
        Identifier ::= /[a-z][a-zA-Z0-9]*/
        Literal ::= Number
        Variable ::= Identifier
        Operator ::= '+' | '-' | '*' | '/'
        Primary ::= Literal | Variable | '(' Expression ')'
        Expression ::= Primary | Primary Operator Expression
        "#,
    )
    .unwrap()
}

fn stlc_grammar() -> Grammar {
    Grammar::load(include_str!("../../../examples/stlc.auf")).unwrap()
}

fn fun_grammar() -> Grammar {
    Grammar::load(include_str!("../../../examples/fun.auf")).unwrap()
}

fn imp_grammar() -> Grammar {
    Grammar::load(include_str!("../../../examples/imp.auf")).unwrap()
}

fn token_prefixes(grammar: &Grammar, input: &str) -> Vec<String> {
    let mut grammar = grammar.clone();
    let segments = grammar.tokenize(input).unwrap();
    let mut cuts = vec![0usize];
    cuts.extend(segments.iter().map(|segment| segment.end));
    cuts.sort_unstable();
    cuts.dedup();
    cuts.into_iter()
        .map(|end| input[..end].to_string())
        .collect()
}

#[test]
#[ignore = "incremental advance not yet implemented"]
fn feed_uses_incremental_advance_after_prefix_parse() {
    let grammar = Grammar::load("start ::= 'x' 'y'").unwrap();
    let ctx = Context::new();
    let mut synth = Synthesizer::new(grammar, "x");

    let prefix = synth.parse_with(&ctx).unwrap();
    assert!(!prefix.is_complete());
    assert_eq!(synth.stats().full_parses, 1);
    assert_eq!(synth.stats().incremental_advances, 0);

    let next = synth.feed("y", &ctx).unwrap();
    assert_eq!(synth.input(), "x y");
    assert!(next.is_complete());
    assert_eq!(synth.stats().full_parses, 1);
    assert_eq!(synth.stats().incremental_advances, 1);
}

#[test]
#[ignore = "incremental advance not yet implemented"]
fn feed_caches_incremental_result_for_reuse() {
    let grammar = Grammar::load("start ::= 'x' 'y' 'z'").unwrap();
    let ctx = Context::new();
    let mut synth = Synthesizer::new(grammar, "x");

    let _ = synth.parse_with(&ctx).unwrap();
    let _ = synth.feed("y", &ctx).unwrap();
    assert_eq!(synth.stats().incremental_advances, 1);

    let extended = synth.parse_with(&ctx).unwrap();
    assert!(!extended.is_complete());
    assert_eq!(synth.stats().full_parses, 1);
    assert_eq!(synth.stats().incremental_advances, 1);
}

#[test]
fn incremental_feed_matches_full_parse_shape() {
    let grammar = Grammar::load(
        r#"
        Name ::= /[a-z]+/
        Start ::= 'let' Name ':' 't' '=' Name
        "#,
    )
    .unwrap();
    let ctx = Context::new();
    let mut incremental = Synthesizer::new(grammar.clone(), "let x");

    let _ = incremental.parse_with(&ctx).unwrap();
    let incremental_ast = incremental.feed(":", &ctx).unwrap();
    let next_input = incremental.input().to_string();
    let mut full = Synthesizer::new(grammar.clone(), next_input.clone());

    let full_ast = full.parse_with(&ctx).unwrap();

    assert_eq!(incremental_ast.text(), full_ast.text());
    assert_eq!(incremental_ast.is_complete(), full_ast.is_complete());
    assert_eq!(incremental_ast.len(), full_ast.len());
    assert_eq!(
        incremental_ast.min_open_slots(&grammar),
        full_ast.min_open_slots(&grammar)
    );
    assert_eq!(incremental_ast.min_tree_depth(), full_ast.min_tree_depth());
    assert_eq!(incremental_ast.bound_texts(), full_ast.bound_texts());
    assert_eq!(
        completion_fingerprint(&incremental_ast, &grammar),
        completion_fingerprint(&full_ast, &grammar)
    );
}

#[test]
fn search_completes_context_sensitive_partial_identifier() {
    let grammar = Grammar::load(
        r#"
        Identifier ::= /[a-z]+/
        Variable(var) ::= Identifier[x]
        Expression ::= Variable

        x ∈ Γ
        ----------- (var)
        Γ(x)
        "#,
    )
    .unwrap();
    let ctx = Context::new()
        .extend("foo".into(), crate::logic::typing::Type::Raw("bool".into()))
        .unwrap();

    let mut synth = Synthesizer::new(grammar.clone(), "f");
    let result = search(&mut synth, "f", &ctx, 16);

    match result {
        SearchResult::Success { complete_input, .. } => assert_eq!(complete_input, "foo"),
        other => panic!("expected successful search completion, got {other:?}"),
    }
}

#[test]
fn completion_set_for_operator_rhs_includes_atomic_and_paren_starts() {
    let grammar = arithmetic_grammar();
    let ctx = Context::new();
    let mut synth = Synthesizer::new(grammar.clone(), "1 + 2 *");
    let ast = synth.parse_with(&ctx).unwrap();
    let completions = completion_fingerprint(&ast, &grammar);

    assert!(completions.iter().any(|(_, ex)| ex.as_deref() == Some("0")));
    assert!(completions.iter().any(|(_, ex)| ex.as_deref() == Some("a")));
    assert!(completions.iter().any(|(_, ex)| ex.as_deref() == Some("(")));
}

#[test]
fn search_completes_operator_rhs_from_partial_prefix() {
    let grammar = arithmetic_grammar();

    let mut synth = Synthesizer::new(grammar.clone(), "1 + 2 *");
    let result = search(&mut synth, "1 + 2 *", &Context::new(), 3);

    match result {
        SearchResult::Success { complete_input, .. } => {
            assert!(complete_input.starts_with("1 + 2 *"));
            let mut grammar = grammar.clone();
            let segments = grammar.tokenize(&complete_input).unwrap();
            assert!(segments.last().is_some_and(|segment| {
                matches!(segment.text().as_str(), "0" | "1" | "a" | "x" | ")")
            }));
        }
        other => panic!("expected operator prefix to complete, got {other:?}"),
    }
}

#[test]
fn search_k_results_are_complete_and_unique() {
    let grammar = arithmetic_grammar();
    let mut synth = Synthesizer::new(grammar.clone(), "");
    let result = search_k(&mut synth, "", &Context::new(), 2, 3);

    let SearchResult::SuccessMultiple { completions } = result else {
        panic!("expected multiple completions for empty arithmetic prefix");
    };

    let unique: std::collections::HashSet<_> = completions.iter().cloned().collect();
    assert_eq!(unique.len(), completions.len());

    let mut grammar = grammar.clone();
    for completion in completions {
        let segments = grammar.tokenize(&completion).unwrap();
        assert!(!segments.is_empty());

        let mut synth = Synthesizer::new(grammar.clone(), &completion);
        let ast = synth.parse_with(&Context::new()).unwrap();
        assert!(
            ast.is_complete(),
            "completion should parse completely: {completion}"
        );
    }
}

#[test]
fn every_token_prefix_of_valid_arithmetic_expression_is_completable() {
    let grammar = arithmetic_grammar();
    let input = "1 + 2 * x";

    for prefix in token_prefixes(&grammar, input) {
        let mut synth = Synthesizer::new(grammar.clone(), &prefix);
        let result = search(&mut synth, &prefix, &Context::new(), 4);
        assert!(
            matches!(result, SearchResult::Success { .. }),
            "prefix should remain completable: {prefix:?} => {result:?}"
        );
    }
}

#[test]
fn every_token_prefix_of_valid_typed_let_expression_is_completable() {
    let grammar = Grammar::load(
        r#"
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
        "#,
    )
    .unwrap();
    let input = "let x : int in x";

    for prefix in token_prefixes(&grammar, input) {
        let mut synth = Synthesizer::new(grammar.clone(), &prefix);
        let result = search(&mut synth, &prefix, &Context::new(), 8);
        assert!(
            matches!(result, SearchResult::Success { .. }),
            "typed prefix should remain completable: {prefix:?} => {result:?}"
        );
    }
}

#[test]
fn search_completion_never_breaks_partial_validity() {
    let grammar = arithmetic_grammar();
    let prefix = "1 + 2 *";
    let mut synth = Synthesizer::new(grammar.clone(), prefix);
    let result = search(&mut synth, prefix, &Context::new(), 4);

    let SearchResult::Success { complete_input, .. } = result else {
        panic!("expected successful completion for {prefix:?}");
    };

    for candidate_prefix in token_prefixes(&grammar, &complete_input) {
        let mut synth = Synthesizer::new(grammar.clone(), &candidate_prefix);
        let reparsed = synth.parse_with(&Context::new());
        assert!(
            reparsed.is_ok(),
            "search produced prefix that is not even partially valid: {candidate_prefix:?}"
        );

        let result = search(&mut synth, &candidate_prefix, &Context::new(), 4);
        assert!(
            matches!(result, SearchResult::Success { .. }),
            "search produced prefix that is not completable: {candidate_prefix:?} => {result:?}"
        );
    }
}

#[test]
fn search_completes_nested_paren_lambda_prefix() {
    let grammar = stlc_grammar();
    let prefix = "((";
    let mut synth = Synthesizer::new(grammar, prefix);
    let result = search(&mut synth, prefix, &Context::new(), 10);

    assert!(
        matches!(result, SearchResult::Success { .. }),
        "nested paren prefix should be completable, got {result:?}"
    );
}

#[test]
fn every_token_prefix_of_nested_paren_lambda_is_completable() {
    let grammar = stlc_grammar();
    let input = "((λx:A.x))";

    for prefix in token_prefixes(&grammar, input) {
        let mut synth = Synthesizer::new(grammar.clone(), &prefix);
        let result = search(&mut synth, &prefix, &Context::new(), 10);
        assert!(
            matches!(result, SearchResult::Success { .. }),
            "STLC prefix should remain completable: {prefix:?} => {result:?}"
        );
    }
}

#[test]
fn every_token_prefix_of_higher_order_fun_expression_is_completable() {
    let grammar = fun_grammar();
    let input = "(f: Int -> Int) => ((g: Int -> Int) => ((x: Int) => f(g(x))))";

    for prefix in token_prefixes(&grammar, input) {
        let mut synth = Synthesizer::new(grammar.clone(), &prefix);
        let result = search(&mut synth, &prefix, &Context::new(), 12);
        assert!(
            matches!(result, SearchResult::Success { .. }),
            "higher-order prefix should remain completable: {prefix:?} => {result:?}"
        );
    }
}

#[test]
fn search_completes_deep_fun_let_prefix_without_unsound_suffix() {
    let grammar = fun_grammar();
    let prefix = "let f: Int -> Int = (x: Int) => x; f(";
    let mut synth = Synthesizer::new(grammar.clone(), prefix);
    let result = search(&mut synth, prefix, &Context::new(), 10);

    let SearchResult::Success { complete_input, .. } = result else {
        panic!("expected deep fun let prefix to complete");
    };

    assert!(complete_input.starts_with(prefix));
    assert!(complete_input.ends_with(')'));

    let mut reparsed = Synthesizer::new(grammar, &complete_input);
    assert!(reparsed.parse_with(&Context::new()).unwrap().is_complete());
}

#[test]
fn every_token_prefix_of_nested_imp_block_is_completable() {
    let grammar = imp_grammar();
    let input = "{ if (1==1) { let x:Int=1; } else { let x:Int=2; } }";

    for prefix in token_prefixes(&grammar, input) {
        let mut synth = Synthesizer::new(grammar.clone(), &prefix);
        let result = search(&mut synth, &prefix, &Context::new(), 10);
        assert!(
            matches!(result, SearchResult::Success { .. }),
            "IMP prefix should remain completable: {prefix:?} => {result:?}"
        );
    }
}

#[test]
fn search_k_complex_fun_results_stay_complete_and_unique() {
    let grammar = fun_grammar();
    let prefix = "let x: Int = 1; (f: Int -> Int) => f(";
    let mut synth = Synthesizer::new(grammar.clone(), prefix);
    let result = search_k(&mut synth, prefix, &Context::new(), 10, 3);

    let SearchResult::SuccessMultiple { completions } = result else {
        panic!("expected multiple completions for complex fun prefix");
    };

    let unique: std::collections::HashSet<_> = completions.iter().cloned().collect();
    assert_eq!(unique.len(), completions.len());

    for completion in completions {
        assert!(completion.starts_with(prefix));
        let mut reparsed = Synthesizer::new(grammar.clone(), &completion);
        assert!(reparsed.parse_with(&Context::new()).unwrap().is_complete());
    }
}

#[test]
fn search_k_returns_multiple_unique_completions() {
    let grammar = Grammar::load(
        r#"
        Number ::= /[0-9]+/
        Identifier ::= /[a-z]+/
        Expr ::= Number | Identifier
        "#,
    )
    .unwrap();

    let mut synth = Synthesizer::new(grammar, "");
    let result = search_k(&mut synth, "", &Context::new(), 1, 2);

    match result {
        SearchResult::SuccessMultiple { completions } => {
            assert_eq!(completions.len(), 2);
            assert_ne!(completions[0], completions[1]);
        }
        other => panic!("expected multiple completions, got {other:?}"),
    }
}
