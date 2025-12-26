//! Extended Typed Lambda Calculus (XTLC) Tests
//!
//! Tests typed completion for lambda calculus with:
//! - Variable scoping via (var) rule
//! - Lambda abstractions with type annotations
//! - Function application with type checking
//! - Let bindings that extend context

use super::*;
use crate::regex::Regex;
use std::time::Instant;

/// Load the XTLC grammar from examples/xtlc.spec
pub fn xtlc_grammar() -> Grammar {
    load_example_grammar("xtlc")
}

// ============================================================================
// Batch Test Cases
// ============================================================================

#[test]
fn check_completable() {
    let cases = vec![
        // Already complete - should be fast (depth 0 or 1)
        TypedCompletionTestCase::new("identity", "λx:A.x", false).with_depth(1),
        TypedCompletionTestCase::new("nested complete", "λx:A.λy:B.x", false).with_depth(1),
        TypedCompletionTestCase::new("nested inner var", "λx:A.λy:B.y", false).with_depth(1),
        TypedCompletionTestCase::new("shadowing", "λx:A.λx:B.x", false).with_depth(1),
        // Nearly complete - need 1-2 tokens
        TypedCompletionTestCase::new("lambda dot", "λx:A.", false).with_depth(2),
        TypedCompletionTestCase::new("func type", "λf:A->B.", false).with_depth(4),
        TypedCompletionTestCase::new("nested func type", "λf:(A->B)->C.", false).with_depth(4),
        TypedCompletionTestCase::new("paren lambda", "(λx:A.x", false).with_depth(2),
        // Partial - need more tokens
        TypedCompletionTestCase::new("lambda type", "λx:A", false).with_depth(3),
        TypedCompletionTestCase::new("lambda colon", "λx:", false).with_depth(4),
        TypedCompletionTestCase::new("nested start", "λx:A.λ", false).with_depth(6),
        // Very partial
        TypedCompletionTestCase::new("empty", "", false).with_depth(3),
        TypedCompletionTestCase::new("lambda start", "λ", false).with_depth(6),
        TypedCompletionTestCase::new("lambda var", "λx", false).with_depth(4),
        // since we are taking context into account,
        // (x) is invalide
        // paren open needs more states
        TypedCompletionTestCase::new("paren open with lambda", "(λ", false).with_depth(6),
        TypedCompletionTestCase::new("paren open", "(", false).with_depth(7),
        // Let bindings
        TypedCompletionTestCase::new("let binding", "{x:A}x", false).with_depth(5),
        TypedCompletionTestCase::new("let sequence", "{x:A}{y:B}x", false).with_depth(6),
    ];

    let grammar = xtlc_grammar();
    let res = run_test_batch(&grammar, &cases);
    assert!(
        res.passed == cases.len(),
        "{} out of {} tests passed",
        res.passed,
        cases.len()
    );
    println!("Average duration: {:?}", res.avg_duration);
}

#[test]
fn check_fail() {
    let cases = vec![
        // Syntax errors
        TypedCompletionTestCase::new("double colon", "λx::A", true),
        TypedCompletionTestCase::new("missing var", "λ:A", true),
        TypedCompletionTestCase::new("close paren first", ")", true),
        TypedCompletionTestCase::new("arrow first", "->", true),
        TypedCompletionTestCase::new("dot first", ".", true),
        // Type errors (unbound variables without context)
        TypedCompletionTestCase::new("unbound x", "x", true),
        TypedCompletionTestCase::new("unbound in lambda body", "λx:A.y", true).with_depth(10),
        TypedCompletionTestCase::new("unbound nested", "λx:A.λy:B.z", true),
    ];

    let grammar = xtlc_grammar();
    let res = run_test_batch(&grammar, &cases);
    assert!(
        res.passed == cases.len(),
        "{} out of {} tests passed",
        res.passed,
        cases.len()
    );
    println!("Average duration: {:?}", res.avg_duration);
}

/// Profile the actual slow test case
#[test]
fn perf_profile_lambda_dot_case() {
    let grammar = xtlc_grammar();
    let input = "λx:A.";

    println!("\n=== Lambda Dot Case Profile ===");
    println!("Input: '{}'", input);

    // Clear cache to see cold performance
    crate::regex::clear_valids_cache();

    // Cold run
    let start = Instant::now();
    let (result, _) = timed_sound_complete(&grammar, input, 2, None, None);
    let cold_time = start.elapsed();
    println!("Cold cache time: {:?}", cold_time);

    // Warm run (same input)
    let start = Instant::now();
    let (_, _) = timed_sound_complete(&grammar, input, 2, None, None);
    let warm_time = start.elapsed();
    println!("Warm cache time: {:?}", warm_time);

    if cold_time > warm_time {
        println!(
            "Cache speedup: {:.1}x",
            cold_time.as_secs_f64() / warm_time.as_secs_f64()
        );
    }
}

/// Test that caching helps across multiple completions
#[test]
fn perf_profile_cache_benefit() {
    let grammar = xtlc_grammar();

    println!("\n=== Cache Benefit Profile ===");

    // Clear cache
    crate::regex::clear_valids_cache();

    // First completion - cold cache
    let start = Instant::now();
    let _ = complete(&grammar, "λx:A.", 2, None, None);
    let cold_time = start.elapsed();
    println!("Cold cache: {:?}", cold_time);

    // Second completion with same grammar - warm cache
    let start = Instant::now();
    let _ = complete(&grammar, "λy:B.", 2, None, None);
    let warm_time = start.elapsed();
    println!("Warm cache: {:?}", warm_time);

    // Third - should be even faster
    let start = Instant::now();
    let _ = complete(&grammar, "λz:C.", 2, None, None);
    let hot_time = start.elapsed();
    println!("Hot cache:  {:?}", hot_time);

    let (entries, _) = crate::regex::valids_cache_stats();
    println!("Cache entries: {}", entries);

    if cold_time > warm_time {
        println!(
            "✓ Cache provides {:.1}x speedup",
            cold_time.as_secs_f64() / warm_time.as_secs_f64()
        );
    }
}
