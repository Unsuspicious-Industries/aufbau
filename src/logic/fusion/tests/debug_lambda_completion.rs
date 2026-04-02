//! Debug test for lambda completion failure
//!
//! Reproduces the unsound completion failure for "(x: Int) => x"
//! where the prefix "(x" fails to complete.

use crate::logic::fusion::Synthesizer;
use crate::logic::search::complete;
use crate::logic::typing::Context;
use crate::validation::completability::sound_complete;

#[test]
fn debug_lambda_prefix_parens_x() {
    crate::set_debug_level(crate::logic::debug::DebugLevel::Trace);
    crate::add_module_filter("fusion_parser");
    crate::add_module_filter("fusion_typing");

    let grammar = crate::testing::load_example_grammar("fun");
    let input = "(x: Int) => x";

    // Get token boundaries for this input
    let segments = grammar.tokenize(input).expect("tokenize failed");
    eprintln!("\n=== Token Segments for '{}' ===", input);
    for (i, seg) in segments.iter().enumerate() {
        eprintln!(
            "  seg[{}]: '{}' @ {}..{}",
            i,
            seg.as_str(),
            seg.index,
            seg.end
        );
    }

    // Build all prefixes
    let mut cuts = vec![0usize];
    cuts.extend(segments.iter().map(|s| s.end));
    cuts.sort_unstable();
    cuts.dedup();

    eprintln!("\n=== Testing Prefixes ===");
    for (i, &byte_end) in cuts.iter().enumerate() {
        let prefix = &input[..byte_end];
        if prefix.trim().is_empty() {
            continue;
        }

        eprintln!(
            "\n--- Prefix[{}]: '{}' (len={}, bytes={}) ---",
            i,
            prefix,
            prefix.len(),
            byte_end
        );

        // Try parsing with Synthesizer
        let mut synth = Synthesizer::new_with_max_depth(grammar.clone(), prefix, 10);
        match synth.parse_with(&Context::new()) {
            Ok(ast) => {
                eprintln!(
                    "  Parse: OK, is_complete={}, roots={}",
                    ast.is_complete(),
                    ast.roots().count()
                );
            }
            Err(e) => {
                eprintln!("  Parse: FAIL - {}", e);
            }
        }

        // Try completion
        let result = complete(&grammar, prefix, 10, Some(Context::new()));
        match result {
            crate::logic::search::CompletionResult::Success { complete_input, .. } => {
                eprintln!("  Complete: OK -> '{}'", complete_input);
            }
            crate::logic::search::CompletionResult::Failure { visited_states, .. } => {
                eprintln!("  Complete: FAIL (visited {} states)", visited_states.len());
            }
            _ => {
                eprintln!("  Complete: ERROR/INVALID");
            }
        }
    }
}

#[test]
fn debug_lambda_sound_complete_breakdown() {
    let grammar = crate::testing::load_example_grammar("fun");
    let input = "(x: Int) => x";
    let ctx = Context::new();

    let result = sound_complete(&grammar, input, 10, Some(ctx));

    eprintln!("\n=== Sound Completion Analysis ===");
    eprintln!("Input: '{}'", input);
    eprintln!("Is Sound: {}", result.is_sound);
    eprintln!("Prefixes Checked: {}", result.prefixes_checked);

    if let Some(failing) = &result.failing_prefix {
        eprintln!("\nFailing Prefix: '{}'", failing);
        eprintln!(
            "Visited States: {}",
            result
                .failing_prefix_visited_states
                .as_ref()
                .map(|v| v.len())
                .unwrap_or(0)
        );
    }

    eprintln!("\n=== All Prefix Details ===");
    for (i, detail) in result.prefix_meta.iter().enumerate() {
        eprintln!("\nPrefix[{}]: '{}'", i, detail.prefix);
        eprintln!("  OK: {}", detail.ok);
        eprintln!("  Time: {} μs", detail.time_us);
        eprintln!("  States Explored: {:?}", detail.states_explored);
        eprintln!("  Visited Count: {:?}", detail.visited_count);
        if !detail.visited_sample.is_empty() {
            eprintln!("  Visited Sample: {:?}", detail.visited_sample);
        }
    }

    if !result.is_sound {
        panic!("Unsound completion detected for '{}'", input);
    }
}

#[test]
fn debug_minimal_lambda_paren_x() {
    crate::set_debug_level(crate::logic::debug::DebugLevel::Trace);
    crate::add_module_filter("fusion_parser");

    let grammar = crate::testing::load_example_grammar("fun");

    // Test just the failing prefix directly
    let prefix = "(x";

    eprintln!("\n=== Minimal Test: '{}' ===", prefix);

    let mut synth = Synthesizer::new_with_max_depth(grammar.clone(), prefix, 10);
    let parse_result = synth.parse_with(&Context::new());

    match parse_result {
        Ok(ast) => {
            eprintln!(
                "Parse succeeded: complete={}, roots={}",
                ast.is_complete(),
                ast.roots().count()
            );
            eprintln!("AST:\n{}", ast);

            let tokens = synth.tokens_with(&Context::new());
            eprintln!("\nCompletion tokens: {}", tokens.len());
            for (i, tok) in tokens.iter().take(10).enumerate() {
                eprintln!(
                    "  tok[{}]: pattern='{}' example={:?}",
                    i,
                    tok.to_pattern(),
                    tok.example()
                );
            }
        }
        Err(e) => {
            eprintln!("Parse failed: {}", e);
        }
    }

    // Now try sound_complete
    let result = sound_complete(&grammar, prefix, 10, Some(Context::new()));
    eprintln!("\nSound Complete Result:");
    eprintln!("  is_sound: {}", result.is_sound);
    eprintln!("  complete_string: {:?}", result.complete_string);

    assert!(result.is_sound, "Prefix '(x' should be sound/completable");
}
