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

fn completion_ctx(opt_ctx: Option<Context>) -> Context {
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

fn invalid_continuation_candidates(tokens: &[String], expected_next: &str) -> Vec<String> {
    let mut candidates = tokens
        .iter()
        .filter(|token| token.as_str() != expected_next)
        .cloned()
        .collect::<Vec<_>>();
    candidates.extend(
        [")", "(", "let", "in", "else", ";", ":", "=>", "@"]
            .into_iter()
            .filter(|token| *token != expected_next)
            .map(str::to_string),
    );
    candidates.sort();
    candidates.dedup();
    candidates
}

pub fn check_feed_soundness(
    grammar: &Grammar,
    input: &str,
    opt_ctx: Option<Context>,
) -> Result<(), String> {
    let ctx = completion_ctx(opt_ctx);
    let tokens = replay_tokens(grammar, input);
    let mut synth = Synthesizer::new(grammar.clone(), "");

    for (idx, token) in tokens.iter().enumerate() {
        let prefix = synth.input().to_string();
        synth.feed_with(token, &ctx).map_err(|err| {
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
        for alternative in invalid_continuation_candidates(&tokens, token) {
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

pub fn sound_complete(
    grammar: &Grammar,
    input: &str,
    opt_ctx: Option<Context>,
) -> PrefixSoundnessResult {
    let ctx = completion_ctx(opt_ctx);
    let tokens = replay_tokens(grammar, input);
    let mut synth = Synthesizer::new(grammar.clone(), "");

    for (idx, token) in tokens.iter().enumerate() {
        let prefix = synth.input().to_string();
        let result = synth.feed_with(token, &ctx);

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
    sound_complete(grammar, input, None).is_sound
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

        let result = sound_complete(&grammar, "foo", Some(ctx));

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

        let result = sound_complete(&grammar, "let x : int in x", Some(Context::new()));

        assert!(
            result.is_sound,
            "failing_prefix={:?}",
            result.failing_prefix
        );
    }

    #[test]
    fn sound_complete_reports_failing_prefix_when_feed_rejects_next_token() {
        let grammar = Grammar::load("Start ::= 'x'").unwrap();

        let result = sound_complete(&grammar, "x y", Some(Context::new()));

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
