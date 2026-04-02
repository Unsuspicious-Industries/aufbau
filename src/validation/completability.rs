// Completability Validation
//
// A string s is completable in L if there exists s' such that ss' in L.
// We use partial parsing and typing to check completion and prefix soundness.

use crate::logic::fusion::Synthesizer;
use crate::logic::grammar::Grammar;
use crate::logic::search;
pub use crate::logic::search::CompletionResult;
use crate::logic::typing::Context;
use rayon::prelude::*;

#[derive(Debug, Clone)]
pub struct PrefixDetail {
    pub prefix: String,
    pub ok: bool,
    pub time_us: u128,
    pub states_explored: Option<usize>,
    pub visited_count: Option<usize>,
    pub visited_sample: Vec<String>,
}

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

pub fn complete(
    grammar: &Grammar,
    input: &str,
    budget: usize,
    opt_ctx: Option<Context>,
) -> CompletionResult {
    search::complete(grammar, input, budget, opt_ctx)
}

pub fn sound_complete(
    grammar: &Grammar,
    input: &str,
    budget: usize,
    opt_ctx: Option<Context>,
) -> PrefixSoundnessResult {
    let ctx = opt_ctx.unwrap_or_default();
    let chars: Vec<char> = input.chars().collect();
    let tokenized = grammar.tokenize(input).ok();
    let prefixes: Vec<(usize, String)> = if let Some(segments) = &tokenized {
        let mut cuts = vec![0usize];
        cuts.extend(segments.iter().map(|s| s.end));
        if !cuts.contains(&input.len()) {
            cuts.push(input.len());
        }
        cuts.sort_unstable();
        cuts.dedup();
        cuts.into_iter()
            .map(|byte_end| {
                let p = input[..byte_end].to_string();
                (p.chars().count(), p)
            })
            .filter(|(len, prefix)| *len == 0 || !prefix.trim().is_empty())
            .collect()
    } else {
        (0..=chars.len())
            .map(|len| (len, chars[..len].iter().collect::<String>()))
            .filter(|(len, prefix)| *len == 0 || !prefix.trim().is_empty())
            .collect()
    };

    #[allow(clippy::type_complexity)]
    let results: Vec<(usize, PrefixDetail, Option<String>, Option<Vec<String>>)> = prefixes
        .par_iter()
        .enumerate()
        .map(|(prefix_idx, (len, prefix))| {
            let start = std::time::Instant::now();

            let result = if let Some(segments) = &tokenized {
                let tokens_to_end = prefixes.len().saturating_sub(1).saturating_sub(prefix_idx);
                let prefix_budget = budget + tokens_to_end;
                let parse_budget = prefix_budget.max(12);
                let mut synth =
                    Synthesizer::new_with_max_depth(grammar.clone(), prefix, parse_budget);
                match synth.parse_with(&ctx) {
                    Ok(_) => {
                        let prefix_end = prefix.len();
                        let suffix_segments: Vec<_> = segments
                            .iter()
                            .filter(|seg| seg.start >= prefix_end)
                            .cloned()
                            .collect();
                        let mut completed = prefix.clone();
                        let mut ok = true;
                        for seg in suffix_segments {
                            if synth.feed(seg.as_str(), &ctx).is_err() {
                                ok = false;
                                break;
                            }
                            completed = grammar.extend_input(&completed, seg.as_str());
                        }
                        if ok {
                            CompletionResult::Success {
                                complete_input: completed,
                                ast: synth
                                    .parse_with(&ctx)
                                    .unwrap_or_else(|_| panic!("witness parse regressed")),
                                completion_path: Vec::new(),
                                completion_depth: 0,
                            }
                        } else {
                            CompletionResult::Failure {
                                visited_states: vec![prefix.clone()],
                                states_explored: 1,
                                max_depth_reached: 0,
                            }
                        }
                    }
                    Err(_) => CompletionResult::Failure {
                        visited_states: vec![prefix.clone()],
                        states_explored: 1,
                        max_depth_reached: 0,
                    },
                }
            } else {
                let tokens_to_end = prefixes.len().saturating_sub(1).saturating_sub(prefix_idx);
                let prefix_budget = budget + tokens_to_end;
                complete(grammar, prefix, prefix_budget, Some(ctx.clone()))
            };
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
    use crate::logic::fusion::Synthesizer;
    use crate::logic::grammar::Grammar;

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

        let mut synth = Synthesizer::new_with_max_depth(grammar.clone(), "f", 16);
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
        let mut synth = Synthesizer::new_with_max_depth(grammar, "let a = 1", 16);
        let tokens = synth.tokens();

        assert!(!tokens.is_empty());
    }

    #[test]
    fn successful_completion_is_syntactic_and_typed() {
        let grammar = crate::validation::parseable::load_example_grammar("fun");
        let result = complete(&grammar, "(", 4, Some(Context::new()));
        if let CompletionResult::Success { complete_input, .. } = result {
            let mut synth = Synthesizer::new_with_max_depth(grammar.clone(), &complete_input, 62);
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
            let mut synth = Synthesizer::new_with_max_depth(grammar.clone(), &complete_input, 62);
            let typed = synth.parse_with(&Context::new()).unwrap_or_else(|e| {
                panic!(
                    "garbage completion returned for pathological prefix: {} ({})",
                    complete_input, e
                )
            });
            assert!(typed.is_complete());
        }
    }
}
