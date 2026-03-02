// Completability Validation
//
// A string s is completable in L if there exists s' such that ss' in L.
// We use partial parsing and typing to check completion and prefix soundness.

use crate::logic::grammar::Grammar;
use crate::logic::partial::{NonTerminal, Synthesizer};
use crate::logic::typing::core::TreeStatus;
use crate::logic::typing::eval::check_tree_with_context;
use crate::logic::typing::Context;
use crate::logic::{search_complete, SearchConfig, SearchResult};
use crate::regex::Regex as DerivativeRegex;

#[derive(Debug)]
pub enum CompletionResult {
    Success {
        complete_input: String,
        ast: NonTerminal,
        completion_path: Vec<DerivativeRegex>,
        completion_depth: usize,
    },
    Failure {
        max_depth_reached: usize,
        states_explored: usize,
        visited_states: Vec<String>,
    },
    Invalid(String),
    Inconsistency(String),
    Error(String),
}

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
    max_depth: usize,
    opt_ctx: Option<Context>,
) -> CompletionResult {
    let ctx = opt_ctx.unwrap_or_else(Context::new);

    let search_config = SearchConfig {
        max_depth,
        ..Default::default()
    };

    match search_complete(grammar, input, &search_config, &ctx) {
        SearchResult::Success {
            complete_input,
            ast,
            completion_path,
            depth,
        } => CompletionResult::Success {
            complete_input,
            ast,
            completion_path,
            completion_depth: depth,
        },
        SearchResult::Invalid { message } => CompletionResult::Invalid(message),
        SearchResult::Exhausted {
            max_depth: depth,
            states_explored,
            visited_states,
        } => CompletionResult::Failure {
            max_depth_reached: depth,
            states_explored,
            visited_states,
        },
    }
}

pub fn sound_complete(
    grammar: &Grammar,
    input: &str,
    max_depth: usize,
    opt_ctx: Option<Context>,
) -> PrefixSoundnessResult {
    let chars: Vec<char> = input.chars().collect();
    let ctx = opt_ctx.unwrap_or_else(Context::new);

    let prefixes: Vec<(usize, String)> = (0..=chars.len())
        .map(|len| (len, chars[..len].iter().collect::<String>()))
        .filter(|(len, prefix)| *len == 0 || !prefix.trim().is_empty())
        .collect();

    let results: Vec<(usize, PrefixDetail, Option<String>, Option<Vec<String>>)> = prefixes
        .iter()
        .map(|(len, prefix)| {
            // Keep one extra expansion step available for the exact input prefix.
            // This prevents off-by-one failures on prefixes that require an
            // additional structural token before meaningful term expansion.
            let depth_budget = max_depth + (chars.len() - len) + 2;
            let start = std::time::Instant::now();

            if *len == chars.len() {
                // Fast path: if the full input already has a complete well-typed

                let result = complete(grammar, prefix, depth_budget, Some(ctx.clone()));
                let elapsed_us = start.elapsed().as_micros();
                return match result {
                    CompletionResult::Success { complete_input, .. } => {
                        let detail = PrefixDetail {
                            prefix: prefix.clone(),
                            ok: true,
                            time_us: elapsed_us,
                            states_explored: None,
                            visited_count: None,
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
                };
            }

            // Use a fresh Synthesizer for each prefix so that depth hints
            // learned from earlier (shorter) prefixes do not bleed into the
            // parse of later prefixes.  A short prefix like "set" may parse at
            // depth 1 (Variable), and reusing that hint for "set " then fails
            // to find the deeper Bind partial tree that is the only valid
            // continuation, producing a false soundness failure.
            let mut prefix_synth = Synthesizer::new(grammar.clone(), prefix.clone());
            let ok = match prefix_synth.partial() {
                Ok(partial) => {
                    let mut tokens = prefix_synth.typed_completions(&ctx);
                    if tokens.is_empty() {
                        // For untyped grammars (no typing rules) typed_completions
                        // always returns empty. Fall back to syntactic completions,
                        // mirroring the fallback in search::build_children.
                        tokens = prefix_synth.completions();
                    }
                    prefix_ok(grammar, &ctx, &partial, &tokens)
                }
                Err(_) => false,
            };

            let elapsed_us = start.elapsed().as_micros();
            let detail = PrefixDetail {
                prefix: prefix.clone(),
                ok,
                time_us: elapsed_us,
                states_explored: None,
                visited_count: None,
                visited_sample: vec![],
            };
            (*len, detail, None, None)
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

pub fn is_completable(grammar: &Grammar, input: &str, max_depth: usize) -> bool {
    matches!(
        complete(grammar, input, max_depth, None),
        CompletionResult::Success { .. }
    )
}

fn prefix_ok(
    grammar: &Grammar,
    ctx: &Context,
    partial: &crate::logic::PartialAST,
    tokens: &crate::logic::partial::CompletionSet,
) -> bool {
    if !tokens.is_empty() {
        return true;
    }

    let mut saw_typed_root = false;

    for root in &partial.roots {
        match check_tree_with_context(root, grammar, ctx) {
            // Prefix soundness accepts any root that is currently typable or
            // typing-indeterminate (partial), even if the root is syntactically
            // incomplete. This is required for states like `... in` where the
            // body has not started yet.
            TreeStatus::Valid(_) | TreeStatus::Partial(_) => {
                saw_typed_root = true;
                break;
            }
            _ => {}
        }
    }

    saw_typed_root
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::logic::grammar::Grammar;
    use crate::logic::partial::Synthesizer;

    #[test]
    fn prefix_ok_accepts_typed_partial_complete_identifier() {
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
        let partial = synth.partial().unwrap();
        let tokens = synth.typed_completions(&ctx);

        assert!(prefix_ok(&grammar, &ctx, &partial, &tokens));
    }

    #[test]
    fn prefix_ok_accepts_context_extending_prefix_before_body() {
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
        let mut synth = Synthesizer::new(grammar.clone(), "let x : int in");
        let partial = synth.partial().unwrap();
        let tokens = synth.typed_completions(&ctx);

        assert!(prefix_ok(&grammar, &ctx, &partial, &tokens));
    }

    #[test]
    fn prefix_ok_accepts_nested_let_prefix_before_inner_body() {
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
        let mut synth = Synthesizer::new(grammar.clone(), "let x : int in let y : bool in");
        let partial = synth.partial().unwrap();
        let tokens = synth.typed_completions(&ctx);

        assert!(prefix_ok(&grammar, &ctx, &partial, &tokens));
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
        let tokens = synth.typed_completions(&Context::new());

        assert!(!tokens.is_empty());
    }
}
