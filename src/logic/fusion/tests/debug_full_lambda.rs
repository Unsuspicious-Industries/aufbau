//! Debug test to understand why full lambda fails with depth=1

use crate::logic::fusion::Synthesizer;
use crate::logic::search::complete;
use crate::logic::typing::Context;

#[test]
fn debug_full_lambda_depth_1() {
    let grammar = crate::testing::load_example_grammar("fun");
    let input = "(x: Int) => x";

    eprintln!("\n=== Testing FULL input '{}' with depth=1 ===", input);

    // Try parsing
    let mut synth = Synthesizer::new_with_max_depth(grammar.clone(), input, 1);
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

    // Try completion
    let result = complete(&grammar, input, 1, Some(Context::new()));
    eprintln!("\nCompletion result:");
    match result {
        crate::logic::search::CompletionResult::Success { complete_input, .. } => {
            eprintln!("  SUCCESS: completed to '{}'", complete_input);
        }
        crate::logic::search::CompletionResult::Failure {
            visited_states,
            max_depth_reached,
            ..
        } => {
            eprintln!(
                "  FAILURE: visited {} states, max_depth_reached={}",
                visited_states.len(),
                max_depth_reached
            );
        }
        crate::logic::search::CompletionResult::Invalid(msg) => {
            eprintln!("  INVALID: {}", msg);
        }
        _ => {
            eprintln!("  OTHER");
        }
    }

    // Try with higher depths to find minimum
    for depth in 1..=5 {
        let result = complete(&grammar, input, depth, Some(Context::new()));
        let status = match result {
            crate::logic::search::CompletionResult::Success { .. } => "SUCCESS",
            crate::logic::search::CompletionResult::Failure { .. } => "FAILURE",
            crate::logic::search::CompletionResult::Invalid(_) => "INVALID",
            _ => "OTHER",
        };
        eprintln!("  depth={}: {}", depth, status);
    }
}
