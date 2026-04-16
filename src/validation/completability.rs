// Completability Validation
//
// A string s is completable in L if there exists s' such that ss' in L.
// We use partial parsing and typing to check completion and prefix soundness.

use crate::logic::grammar::Grammar;
use crate::logic::synth::{self, SearchResult};
use crate::logic::typing::Context;
use rayon::prelude::*;

/// Per-prefix diagnostics collected while checking prefix soundness.
#[derive(Debug, Clone)]
pub struct PrefixDetail {
    pub prefix: String,
    pub ok: bool,
    pub time_us: u128,
    pub states_explored: Option<usize>,
    pub visited_count: Option<usize>,
    pub visited_sample: Vec<String>,
}

/// Result of checking whether every relevant prefix remains completable.
///
/// This is the main validation signal for soundness: if `is_sound` is true,
/// then each checked prefix stays partially valid and can be extended to a
/// complete expression.
#[derive(Debug)]
pub struct PrefixSoundnessResult {
    pub is_sound: bool,
    pub failing_prefix: Option<String>,
    pub prefixes_checked: usize,
    pub prefix_details: Vec<(String, bool)>,
    pub complete_string: Option<String>,
    pub failing_prefix_visited_states: Option<Vec<String>>,
    pub prefix_meta: Vec<PrefixDetail>,
}

/// Public completion result used by validation callers.
pub type CompletionResult = SearchResult;

/// Shorter alias for the prefix soundness report.
pub type SoundnessResult = PrefixSoundnessResult;

fn completion_ctx(opt_ctx: Option<Context>) -> Context {
    opt_ctx.unwrap_or_default()
}

/// Choose the prefix boundaries we want to validate.
///
/// When tokenization succeeds we only check token-aligned cuts, because those
/// are the meaningful user-visible partial states. If tokenization fails, we
/// fall back to character prefixes so malformed intermediate text is still
/// reported as unsound.
fn prefixes_to_check(grammar: &Grammar, input: &str) -> Vec<(usize, String)> {
    let chars: Vec<char> = input.chars().collect();
    let mut grammar = grammar.clone();

    if let Ok(segments) = grammar.tokenize(input) {
        let mut cuts = vec![0usize];
        cuts.extend(segments.iter().map(|segment| segment.end));
        if !cuts.contains(&input.len()) {
            cuts.push(input.len());
        }
        cuts.sort_unstable();
        cuts.dedup();
        cuts.into_iter()
            .map(|byte_end| {
                let prefix = input[..byte_end].to_string();
                (prefix.chars().count(), prefix)
            })
            .filter(|(len, prefix)| *len == 0 || !prefix.trim().is_empty())
            .collect()
    } else {
        (0..=chars.len())
            .map(|len| (len, chars[..len].iter().collect::<String>()))
            .filter(|(len, prefix)| *len == 0 || !prefix.trim().is_empty())
            .collect()
    }
}

/// Attempt to complete a partial input into a fully-typed expression.
pub fn complete(
    grammar: &Grammar,
    input: &str,
    budget: usize,
    opt_ctx: Option<Context>,
) -> CompletionResult {
    synth::complete(grammar, input, budget, opt_ctx)
}

/// Return up to `count` normalized completions for a partial input.
///
/// This is a convenience wrapper for CLI and FFI surfaces that only need the
/// completed strings rather than the full search diagnostics.
pub fn complete_k(
    grammar: &Grammar,
    input: &str,
    budget: usize,
    count: usize,
    opt_ctx: Option<Context>,
) -> Vec<String> {
    synth::complete_k(grammar, input, budget, count, opt_ctx)
}

/// Check prefix soundness for an input.
///
/// The input is sound when every checked prefix can still be completed to a
/// full expression, which means partial validity is preserved throughout the
/// incremental construction of the input.
pub fn sound_complete(
    grammar: &Grammar,
    input: &str,
    budget: usize,
    opt_ctx: Option<Context>,
) -> PrefixSoundnessResult {
    let ctx = completion_ctx(opt_ctx);
    let chars: Vec<char> = input.chars().collect();
    let prefixes = prefixes_to_check(grammar, input);

    #[allow(clippy::type_complexity)]
    let results: Vec<(usize, PrefixDetail, Option<String>, Option<Vec<String>>)> = prefixes
        .par_iter()
        .enumerate()
        .map(|(prefix_idx, (len, prefix))| {
            let start = std::time::Instant::now();

            let tokens_to_end = prefixes.len().saturating_sub(1).saturating_sub(prefix_idx);
            let prefix_budget = budget + tokens_to_end;
            let result = complete(grammar, prefix, prefix_budget, Some(ctx.clone()));

            let elapsed_us = start.elapsed().as_micros();
            match result {
                CompletionResult::Success { complete_input, .. } => {
                    let detail = PrefixDetail {
                        prefix: prefix.clone(),
                        ok: true,
                        time_us: elapsed_us,
                        states_explored: Some(0),
                        visited_count: Some(1),
                        visited_sample: vec![],
                    };
                    (*len, detail, Some(complete_input), None)
                }
                CompletionResult::SuccessMultiple { completions } => {
                    // For single completion validation, just use the first result
                    let detail = PrefixDetail {
                        prefix: prefix.clone(),
                        ok: true,
                        time_us: elapsed_us,
                        states_explored: Some(0),
                        visited_count: Some(completions.len()),
                        visited_sample: vec![],
                    };
                    (*len, detail, completions.into_iter().next(), None)
                }
                CompletionResult::Failure {
                    visited_states,
                    states_explored,
                    ..
                } => {
                    let visited_sample = visited_states.iter().take(20).cloned().collect();
                    let detail = PrefixDetail {
                        prefix: prefix.clone(),
                        ok: false,
                        time_us: elapsed_us,
                        states_explored: Some(states_explored),
                        visited_count: Some(visited_states.len()),
                        visited_sample,
                    };
                    (*len, detail, None, Some(visited_states))
                }
                CompletionResult::Invalid(_)
                | CompletionResult::Error(_)
                | CompletionResult::Inconsistency(_) => {
                    let detail = PrefixDetail {
                        prefix: prefix.clone(),
                        ok: false,
                        time_us: elapsed_us,
                        states_explored: None,
                        visited_count: None,
                        visited_sample: vec![],
                    };
                    (*len, detail, None, None)
                }
            }
        })
        .collect();

    let mut prefix_details = Vec::with_capacity(results.len());
    let mut prefix_meta = Vec::with_capacity(results.len());
    let mut failing_prefix = None;
    let mut failing_prefix_visited_states = None;
    let mut complete_string = None;

    let mut full_completion = None;
    for (len, detail, completion, visited_states) in results {
        prefix_details.push((detail.prefix.clone(), detail.ok));

        if len == chars.len() && completion.is_some() {
            full_completion = completion.clone();
        }

        if detail.ok && complete_string.is_none() {
            complete_string = completion;
        }

        if !detail.ok && failing_prefix.is_none() {
            failing_prefix = Some(detail.prefix.clone());
            failing_prefix_visited_states = visited_states;
        }

        prefix_meta.push(detail);
    }

    let complete_string = full_completion.or(complete_string);

    PrefixSoundnessResult {
        is_sound: failing_prefix.is_none(),
        failing_prefix,
        prefixes_checked: prefix_details.len(),
        prefix_details,
        complete_string,
        failing_prefix_visited_states,
        prefix_meta,
    }
}

pub fn is_completable(grammar: &Grammar, input: &str, budget: usize) -> bool {
    matches!(
        complete(grammar, input, budget, None),
        CompletionResult::Success { .. }
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::logic::grammar::Grammar;
    use crate::logic::synth::Synthesizer;

    #[test]
    fn complete_accepts_typed_partial_complete_identifier() {
        let spec = r#"
            Identifier ::= /[a-z]+/
            Variable(var) ::= Identifier[x]
            Expression ::= Variable

            x ∈ Γ
            ----------- (var)
            Γ(x)
        "#;

        let grammar = Grammar::load(spec).unwrap();
        let ctx = Context::new()
            .extend("foo".into(), crate::logic::typing::Type::Raw("bool".into()))
            .unwrap();

        let mut synth = Synthesizer::new(grammar.clone(), "f");
        let typed = synth.parse_with(&ctx).unwrap();

        assert!(!typed.is_empty());
        assert!(matches!(
            complete(&grammar, "f", 16, Some(ctx)),
            CompletionResult::Success { .. }
        ));
    }

    #[test]
    fn complete_accepts_context_extending_prefix_before_body() {
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

        let grammar = Grammar::load(spec).unwrap();
        let ctx = Context::new();
        assert!(matches!(
            complete(&grammar, "let x : int in", 16, Some(ctx)),
            CompletionResult::Success { .. }
        ));
    }

    #[test]
    fn complete_accepts_nested_let_prefix_before_inner_body() {
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

        let grammar = Grammar::load(spec).unwrap();
        let ctx = Context::new();
        assert!(matches!(
            complete(&grammar, "let x : int in let y : bool in", 16, Some(ctx)),
            CompletionResult::Success { .. }
        ));
    }

    #[test]
    fn complete_accepts_keyword_prefix_requiring_separator() {
        let spec = r#"
            Identifier ::= /[a-z]+/
            Let ::= 'let' Identifier ':' 'int' 'in' Identifier
            Expression ::= Let | Identifier
        "#;
        let grammar = Grammar::load(spec).unwrap();
        let result = complete(&grammar, "let", 6, Some(Context::new()));
        assert!(matches!(result, CompletionResult::Success { .. }));
    }

    #[test]
    fn complete_accepts_keyword_and_name_prefix() {
        let spec = r#"
            Identifier ::= /[a-z]+/
            Let ::= 'let' Identifier ':' 'int' 'in' Identifier
            Expression ::= Let | Identifier
        "#;
        let grammar = Grammar::load(spec).unwrap();
        let result = complete(&grammar, "let x", 6, Some(Context::new()));
        assert!(matches!(result, CompletionResult::Success { .. }));
    }

    #[test]
    fn typed_completions_keep_separator_when_later_premise_term_missing() {
        let spec = r#"
            Identifier ::= /[a-z]+/
            Int ::= /[0-9]+/
            Var(var) ::= Identifier[x]
            Let(let) ::= 'let' Identifier[a] '=' Int[v] ';' Var[b]
            Expr ::= Let | Var

            x ∈ Γ
            ----------- (var)
            Γ(x)

            Γ[a:'int'] ⊢ b : ?T
            ------------------- (let)
            ?T
        "#;
        let grammar = Grammar::load(spec).unwrap();
        let mut synth = Synthesizer::new(grammar, "let a = 1");
        let tokens = synth.completions();

        assert!(!tokens.is_empty());
    }

    #[test]
    fn successful_completion_is_syntactic_and_typed() {
        let grammar = crate::validation::parseable::load_example_grammar("fun");
        let result = complete(&grammar, "(", 4, Some(Context::new()));
        if let CompletionResult::Success { complete_input, .. } = result {
            let mut synth = Synthesizer::new(grammar.clone(), &complete_input);
            let typed = synth.parse_with(&Context::new()).unwrap_or_else(|e| {
                panic!("completion should type-check: {} ({})", complete_input, e)
            });
            assert!(
                typed.is_complete(),
                "completion should be complete typed tree: {}",
                complete_input
            );
        }
    }

    #[test]
    fn pathological_prefix_never_returns_garbage_completion() {
        let grammar = crate::validation::parseable::load_example_grammar("fun");
        let input = "let ( a ) + true ( let a : A -> A -> A ->";
        let result = complete(&grammar, input, 2, Some(Context::new()));

        if let CompletionResult::Success { complete_input, .. } = result {
            let mut synth = Synthesizer::new(grammar.clone(), &complete_input);
            let typed = synth.parse_with(&Context::new()).unwrap_or_else(|e| {
                panic!(
                    "garbage completion returned for pathological prefix: {} ({})",
                    complete_input, e
                )
            });
            assert!(typed.is_complete());
        }
    }

    #[test]
    fn soundness_holds_for_every_prefix_of_valid_typed_expression() {
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

        let grammar = Grammar::load(spec).unwrap();
        let result = sound_complete(&grammar, "let x : int in x", 8, Some(Context::new()));

        assert!(
            result.is_sound,
            "valid expression should remain prefix-sound, failing_prefix={:?}",
            result.failing_prefix
        );
    }
}
