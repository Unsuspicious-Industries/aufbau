//! Test lambda completion with depth=1 (the actual failing configuration)

use crate::logic::fusion::Synthesizer;
use crate::logic::search::complete;
use crate::logic::typing::Context;
use crate::validation::completability::sound_complete;

#[test]
fn debug_lambda_paren_x_depth_1() {
    crate::set_debug_level(crate::logic::debug::DebugLevel::Trace);
    crate::add_module_filter("fusion_parser");

    let grammar = crate::testing::load_example_grammar("fun");
    let prefix = "(x";

    eprintln!("\n=== Testing '{}' with depth=1 ===", prefix);

    // Try with depth=1 (the failing config)
    let mut synth = Synthesizer::new_with_max_depth(grammar.clone(), prefix, 1);
    match synth.parse_with(&Context::new()) {
        Ok(ast) => {
            eprintln!(
                "Parse OK: complete={}, roots={}",
                ast.is_complete(),
                ast.roots().count()
            );
        }
        Err(e) => {
            eprintln!("Parse FAIL: {}", e);
        }
    }

    let result = complete(&grammar, prefix, 1, Some(Context::new()));
    eprintln!("\nCompletion result: {:?}", result);

    match result {
        crate::logic::search::CompletionResult::Success { complete_input, .. } => {
            eprintln!("SUCCESS: completed to '{}'", complete_input);
        }
        crate::logic::search::CompletionResult::Failure { visited_states, .. } => {
            eprintln!("FAILURE: visited {} states", visited_states.len());
            if visited_states.is_empty() {
                eprintln!("  ^^^ ZERO STATES VISITED - THIS IS THE BUG");
            }
        }
        _ => {
            eprintln!("ERROR/INVALID result");
        }
    }
}

#[test]
fn debug_lambda_full_depth_1() {
    let grammar = crate::testing::load_example_grammar("fun");
    let input = "(x: Int) => x";

    eprintln!("\n=== Testing full input '{}' with depth=1 ===", input);

    let result = sound_complete(&grammar, input, 1, Some(Context::new()));

    eprintln!("Is Sound: {}", result.is_sound);
    eprintln!("Prefixes Checked: {}", result.prefixes_checked);

    if let Some(failing) = &result.failing_prefix {
        eprintln!("\n!!! FAILING PREFIX: '{}' !!!", failing);
        eprintln!(
            "Visited States: {}",
            result
                .failing_prefix_visited_states
                .as_ref()
                .map(|v| v.len())
                .unwrap_or(0)
        );
    }

    eprintln!("\n=== Prefix Details ===");
    for (i, detail) in result.prefix_meta.iter().enumerate() {
        eprintln!(
            "Prefix[{}]: '{}' ok={} visited={:?}",
            i, detail.prefix, detail.ok, detail.visited_count
        );
    }

    if !result.is_sound {
        panic!(
            "Unsound with depth=1! Failing prefix: {:?}",
            result.failing_prefix
        );
    }
}
