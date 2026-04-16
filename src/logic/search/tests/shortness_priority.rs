use super::super::*;
use crate::logic::grammar::Grammar;
use crate::logic::synth::Synthesizer;
use crate::logic::typing::Context;

fn load(spec: &str) -> Grammar {
    Grammar::load(spec).unwrap()
}

// ============================================================================
// Syntactic Completeness
// ============================================================================

#[test]
fn bfs_returns_syntactically_complete() {
    let mut grammar = load(
        r#"
        Name ::= /[a-z]+/
        Expr ::= Name | Name Name
        Start ::= Expr
        "#,
    );

    let result = complete(&grammar, "x", 4, None);
    if let CompletionResult::Success { complete_input, .. } = result {
        let mut synth = Synthesizer::new_with_max_depth(grammar.clone(), &complete_input, 8);
        let ast = synth.parse_with(&Context::new()).expect("must parse");
        println!("complete_input='{}'", complete_input);
        println!("ast={:#?}", ast);
        assert!(
            ast.is_complete(),
            "completion must be syntactically complete"
        );
    }
}

#[test]
fn bfs_complete_k_all_syntactically_complete() {
    let mut grammar = load(
        r#"
        Name ::= /[a-z]+/
        Expr ::= Name | Name Name
        Start ::= Expr
        "#,
    );

    let results = complete_k(&grammar, "x", 4, 5);
    for completed in results {
        let mut synth = Synthesizer::new_with_max_depth(grammar.clone(), &completed, 8);
        let ast = synth.parse_with(&Context::new()).expect("must parse");
        assert!(
            ast.is_complete(),
            "completion '{}' must be syntactically complete",
            completed
        );
    }
}

// ============================================================================
// Prefix Preservation
// ============================================================================

#[test]
fn bfs_completion_preserves_prefix() {
    let mut grammar = load(
        r#"
        Name ::= /[a-z]+/
        Expr ::= Name Name
        Start ::= Expr
        "#,
    );

    let result = complete(&grammar, "foo", 4, None);
    if let CompletionResult::Success { complete_input, .. } = result {
        assert!(
            complete_input.starts_with("foo"),
            "completion '{}' must preserve prefix 'foo'",
            complete_input
        );
    }
}

#[test]
fn bfs_complete_k_all_preserve_prefix() {
    let mut grammar = load(
        r#"
        Name ::= /[a-z]+/
        Expr ::= Name Name
        Start ::= Expr
        "#,
    );

    let results = complete_k(&grammar, "foo", 4, 5);
    for completed in &results {
        assert!(
            completed.starts_with("foo"),
            "completion '{}' must preserve prefix 'foo'",
            completed
        );
    }
}

// ============================================================================
// Shortest-First Guarantee (BFS invariant)
// ============================================================================

#[test]
fn bfs_complete_k_sorted_by_length() {
    let mut grammar = load(
        r#"
        Name ::= /[a-z]+/
        Expr ::= Name | Name Name | Name Name Name
        Start ::= Expr
        "#,
    );

    let results = complete_k(&grammar, "", 4, 10);

    for i in 1..results.len() {
        assert!(
            results[i - 1].len() <= results[i].len(),
            "result[{}]='{}' (len={}) should not be longer than result[{}]='{}' (len={})",
            i - 1,
            results[i - 1],
            results[i - 1].len(),
            i,
            results[i],
            results[i].len()
        );
    }
}

#[test]
fn bfs_finds_shortest_completion() {
    let grammar = load(
        r#"
        Name ::= /[a-z]+/
        Expr ::= Name | Name Name | Name Name Name
        Start ::= Expr
        "#,
    );

    let result = complete(&grammar, "", 4, None);

    match result {
        CompletionResult::Success { complete_input, .. } => {
            let tokens: Vec<_> = complete_input.split_whitespace().collect();
            assert_eq!(
                tokens.len(),
                1,
                "BFS should find single-token completion first"
            );
        }
        other => panic!("expected success, got {:?}", other),
    }
}

// ============================================================================
// Result Uniqueness
// ============================================================================

#[test]
fn bfs_complete_k_returns_unique_results() {
    let mut grammar = load(
        r#"
        Name ::= /[a-z]+/
        Expr ::= Name | Name Name
        Start ::= Expr
        "#,
    );

    let results = complete_k(&grammar, "", 4, 10);
    let mut seen = std::collections::HashSet::new();
    for r in &results {
        assert!(seen.insert(r.clone()), "duplicate completion '{}'", r);
    }
}

// ============================================================================
// Max Depth Respect
// ============================================================================

#[test]
fn bfs_respects_max_depth() {
    let mut grammar = load(
        r#"
        A ::= 'a' A | 'b'
        Start ::= A
        "#,
    );

    let result = complete(&grammar, "", 2, None);
    if let CompletionResult::Failure {
        max_depth_reached, ..
    } = result
    {
        assert_eq!(max_depth_reached, 2, "max depth must be respected");
    }
}

// ============================================================================
// No Special Tokens in Output
// ============================================================================

#[test]
fn bfs_no_special_tokens_in_completion() {
    let mut grammar = load(
        r#"
        Name ::= /[a-z]+/
        Expr ::= Name
        Start ::= Expr
        "#,
    );

    let result = complete(&grammar, "", 4, None);
    if let CompletionResult::Success { complete_input, .. } = result {
        for special in grammar.specials() {
            assert!(
                !complete_input.contains(special),
                "completion '{}' must not contain special token '{}'",
                complete_input,
                special
            );
        }
    }
}

// ============================================================================
// Idempotency
// ============================================================================

#[test]
fn bfs_idempotent() {
    let mut grammar = load(
        r#"
        Name ::= /[a-z]+/
        Expr ::= Name Name
        Start ::= Expr
        "#,
    );

    let r1 = complete(&grammar, "foo", 4, None);
    let r2 = complete(&grammar, "foo", 4, None);

    match (&r1, &r2) {
        (
            CompletionResult::Success {
                complete_input: i1, ..
            },
            CompletionResult::Success {
                complete_input: i2, ..
            },
        ) => {
            assert_eq!(i1, i2, "BFS should be idempotent");
        }
        _ => panic!("both results should be Success"),
    }
}

// ============================================================================
// Typedness (Well-Typed Root)
// ============================================================================

#[test]
fn bfs_returns_well_typed_completion() {
    let mut grammar = load(
        r#"
        Name ::= /[a-z]+/
        Ty ::= 'Int' | 'Bool'
        Expr ::= Name | Name ':' Ty
        Start ::= Expr
        "#,
    );

    let result = complete(&grammar, "x : ", 4, None);
    if let CompletionResult::Success { complete_input, .. } = result {
        let mut synth = Synthesizer::new_with_max_depth(grammar.clone(), &complete_input, 8);
        let ast = synth.parse_with(&Context::new()).expect("must parse");
        assert!(
            ast.is_complete(),
            "completion '{}' must parse to a complete AST",
            complete_input
        );
    }
}

// ============================================================================
// Right-Recursive Grammar
// ============================================================================

#[test]
fn bfs_right_recursive() {
    let grammar = load(
        r#"
        A ::= 'a' A | 'b'
        Start ::= A
        "#,
    );

    let result = complete(&grammar, "a a a a", 8, None);
    match result {
        CompletionResult::Success { complete_input, .. } => {
            assert_eq!(complete_input, "a a a a b");
        }
        other => panic!("expected success, got {:?}", other),
    }
}

#[test]
fn bfs_prefers_shorter() {
    let grammar = load(
        r#"
        A ::= 'a' A | 'b'
        Start ::= A
        "#,
    );

    let result = complete(&grammar, "", 8, None);
    match result {
        CompletionResult::Success { complete_input, .. } => {
            assert_eq!(
                complete_input, "b",
                "BFS should find 'b' as shortest completion"
            );
        }
        other => panic!("expected success, got {:?}", other),
    }
}

// ============================================================================
// Already-Complete Input
// ============================================================================

#[test]
fn bfs_returns_already_complete_input() {
    let mut grammar = load(
        r#"
        Name ::= /[a-z]+/
        Expr ::= Name
        Start ::= Expr
        "#,
    );

    let result = complete(&grammar, "foo", 4, None);
    match result {
        CompletionResult::Success { complete_input, .. } => {
            assert_eq!(complete_input, "foo");
        }
        other => panic!(
            "expected success for already-complete input, got {:?}",
            other
        ),
    }
}

// ============================================================================
// Empty Prefix
// ============================================================================

#[test]
fn bfs_completes_from_empty() {
    let mut grammar = load(
        r#"
        Name ::= /[a-z]+/
        Expr ::= Name
        Start ::= Expr
        "#,
    );

    let result = complete(&grammar, "", 4, None);
    match result {
        CompletionResult::Success { complete_input, .. } => {
            assert!(
                !complete_input.is_empty(),
                "must produce non-empty completion"
            );
        }
        other => panic!("expected success from empty prefix, got {:?}", other),
    }
}

// ============================================================================
// Multiple Alternatives
// ============================================================================

#[test]
fn bfs_complete_k_respects_k_bound() {
    let mut grammar = load(
        r#"
        Name ::= /[a-z]+/
        Expr ::= Name | Name Name
        Start ::= Expr
        "#,
    );

    let k = 3;
    let results = complete_k(&grammar, "", 4, k);
    assert!(
        results.len() <= k,
        "complete_k returned {} results but k={}",
        results.len(),
        k
    );
}
