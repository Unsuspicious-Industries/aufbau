//! Feed-replay checks used as empirical witnesses for prefix soundness.
//!
//! These helpers do not prove the replay theorem. They instantiate it on bounded
//! token sequences: if a prefix is accepted, replaying the observed token stream
//! through `feed` should preserve acceptance and tokenization.

use crate::logic::grammar::Grammar;
use crate::logic::synth::Synthesizer;
use crate::logic::typing::Context;

#[derive(Debug)]
pub struct PrefixSoundnessResult {
    pub is_sound: bool,
    pub failing_prefix: Option<String>,
    pub prefixes_checked: usize,
    pub accepted_input: String,
    pub failure: Option<String>,
}

pub type SoundnessResult = PrefixSoundnessResult;

fn validation_ctx(opt_ctx: Option<Context>) -> Context {
    opt_ctx.unwrap_or_default()
}

fn replay_tokens(grammar: &Grammar, input: &str) -> Vec<String> {
    let mut grammar = grammar.clone();
    match grammar.tokenize(input) {
        Ok(segments) => segments
            .into_iter()
            .map(|segment| segment.text().to_string())
            .collect(),
        Err(_) => input.chars().map(|ch| ch.to_string()).collect(),
    }
}

// Returns feed deltas: each entry is the original-input slice from the
// previous token's end to the current token's end, preserving separators.
// The caller is responsible for spacing; feed just concatenates.
fn replay_feed_deltas(grammar: &Grammar, input: &str) -> Vec<String> {
    let mut grammar = grammar.clone();
    match grammar.tokenize(input) {
        Ok(segments) => {
            let mut deltas = Vec::new();
            let mut prev_end = 0;
            for segment in segments {
                deltas.push(input[prev_end..segment.end].to_string());
                prev_end = segment.end;
            }
            deltas
        }
        Err(_) => input.chars().map(|ch| ch.to_string()).collect(),
    }
}

/// Extract the token sequence by running the full tokenizer on the input.
///
/// This is NOT about feed acceptance. feed is already called before this.
/// After feed accepts a prefix, we ask:
/// - what does `synth.input()` (the reconstructed input) tokenize to?
/// - does it match the original token stream from the beginning?
///
/// If this diverges, it means feed updated the internal state in a way that
/// would cause future tokenization to differ from the original replay stream.
fn token_fingerprint(grammar: &Grammar, input: &str) -> Result<Vec<String>, String> {
    let mut grammar = grammar.clone();
    grammar
        .tokenize(input)
        .map(|segments| {
            segments
                .into_iter()
                .map(|segment| segment.text().to_string())
                .collect()
        })
        .map_err(|err| err.to_string())
}

// Property: probe tokens come from a finite grammar/input token universe rather
// than ad hoc hand-written continuations.
fn rejected_token_probes(grammar: &Grammar, tokens: &[String], expected_next: &str) -> Vec<String> {
    let mut candidates = grammar.specials.clone();
    candidates.extend(tokens.iter().cloned());
    candidates.sort();
    candidates.dedup();
    candidates
        .into_iter()
        .filter(|token| token != expected_next)
        .collect()
}

/// Empirical replay check.
///
/// Property: replaying the tokenization of `input` through incremental `feed`
/// reproduces the same token sequence and rejects at least one invalid next step
/// after each accepted prefix.
pub fn check_feed_soundness(
    grammar: &Grammar,
    input: &str,
    opt_ctx: Option<Context>,
) -> Result<(), String> {
    let ctx = validation_ctx(opt_ctx);
    let tokens = replay_tokens(grammar, input);
    let deltas = replay_feed_deltas(grammar, input);
    let mut synth = Synthesizer::new(grammar.clone(), "");

    for (idx, (token, delta)) in tokens.iter().zip(deltas.iter()).enumerate() {
        let prefix = synth.input().to_string();
        synth.feed_with(delta, &ctx).map_err(|err| {
            format!("feed rejected expected token {token:?} after {prefix:?}: {err}")
        })?;

        let actual = token_fingerprint(grammar, synth.input())
            .map_err(|err| format!("tokenization failed after feeding {token:?}: {err}"))?;
        if actual != tokens[..=idx] {
            return Err(format!(
                "token fingerprint drift after feeding {token:?}: expected {:?}, got {:?}",
                &tokens[..=idx],
                actual
            ));
        }

        let mut saw_rejected_alternative = false;
        for alternative in rejected_token_probes(grammar, &tokens, token) {
            let mut probe = Synthesizer::new(grammar.clone(), synth.input());
            probe.set_ctx(ctx.clone());
            if probe.feed(&alternative).is_err() {
                saw_rejected_alternative = true;
                break;
            }
        }

        if !saw_rejected_alternative {
            return Err(format!(
                "no invalid continuation rejected after prefix {:?}",
                synth.input()
            ));
        }
    }

    Ok(())
}

/// Empirical check: can we incrementally replay an accepted input through feed
/// while maintaining consistent tokenization?
///
/// This feeds tokens one-by-one and checks two things:
/// 1. feed accepts each expected token (the real replay test)
/// 2. after each feed, tokenizing the reconstructed input matches the original
///
/// Property: incremental feed does not cause tokenization to drift from the
/// original token stream. This is a consistency check on internal state after
/// feed, not a proof that feed is correct.
pub fn check_incremental_feed_replay(
    grammar: &Grammar,
    input: &str,
    opt_ctx: Option<Context>,
) -> PrefixSoundnessResult {
    let ctx = validation_ctx(opt_ctx);
    let tokens = replay_tokens(grammar, input);
    let deltas = replay_feed_deltas(grammar, input);
    let mut synth = Synthesizer::new(grammar.clone(), "");

    for (idx, (_token, delta)) in tokens.iter().zip(deltas.iter()).enumerate() {
        let prefix = synth.input().to_string();
        let result = synth.feed_with(delta, &ctx);

        match result {
            Ok(_) => {
                let expected = tokens[..=idx].to_vec();
                match token_fingerprint(grammar, synth.input()) {
                    Ok(actual) if actual == expected => {}
                    Ok(actual) => {
                        return PrefixSoundnessResult {
                            is_sound: false,
                            failing_prefix: Some(prefix),
                            prefixes_checked: idx + 1,
                            accepted_input: synth.input().to_string(),
                            failure: Some(format!(
                                "token fingerprint drift: expected {:?}, got {:?}",
                                expected, actual
                            )),
                        };
                    }
                    Err(err) => {
                        return PrefixSoundnessResult {
                            is_sound: false,
                            failing_prefix: Some(prefix),
                            prefixes_checked: idx + 1,
                            accepted_input: synth.input().to_string(),
                            failure: Some(err),
                        };
                    }
                }
            }
            Err(err) => {
                return PrefixSoundnessResult {
                    is_sound: false,
                    failing_prefix: Some(prefix),
                    prefixes_checked: idx + 1,
                    accepted_input: synth.input().to_string(),
                    failure: Some(err),
                };
            }
        }
    }

    PrefixSoundnessResult {
        is_sound: true,
        failing_prefix: None,
        prefixes_checked: tokens.len(),
        accepted_input: synth.input().to_string(),
        failure: None,
    }
}

pub fn is_completable(grammar: &Grammar, input: &str, budget: usize) -> bool {
    let _ = budget;
    check_incremental_feed_replay(grammar, input, None).is_sound
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sound_complete_accepts_typed_identifier_replay() {
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

        let result = check_incremental_feed_replay(&grammar, "foo", Some(ctx));

        assert!(result.is_sound);
        assert_eq!(result.accepted_input, "foo");
    }

    #[test]
    fn sound_complete_accepts_context_extending_let_prefix() {
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

        let result =
            check_incremental_feed_replay(&grammar, "let x : int in x", Some(Context::new()));

        assert!(
            result.is_sound,
            "failing_prefix={:?}",
            result.failing_prefix
        );
    }

    #[test]
    fn sound_complete_reports_failing_prefix_when_feed_rejects_next_token() {
        let grammar = Grammar::load("Start ::= 'x'").unwrap();

        let result = check_incremental_feed_replay(&grammar, "x y", Some(Context::new()));

        assert!(!result.is_sound);
        assert_eq!(result.failing_prefix.as_deref(), Some("x"));
    }

    #[test]
    fn check_feed_soundness_replays_tokens_and_rejects_bad_alternatives() {
        let grammar = Grammar::load(
            r#"
                Number ::= /[0-9]+/
                Expr ::= Number | '(' Expr ')'
                Start ::= Expr '+' Expr
            "#,
        )
        .unwrap();

        let result = check_feed_soundness(&grammar, "(1) + 2", Some(Context::new()));

        assert!(result.is_ok(), "{result:?}");
    }
}
