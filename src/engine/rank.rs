// generic framework for somehting that ranks bits of strings

use std::cmp::Ordering;
use std::collections::HashSet;
use std::sync::Arc;

use anyhow::{Context, Result, anyhow};

use crate::ml::{self, LogitsProvider};

pub trait Ranker {
    fn vocab(&self) -> Vec<String>;
    fn rank(&self, input: &str) -> Result<Vec<(String, f32)>>;
}

// default ranker thats random with a vocab of all ascii chars + some stuff
pub struct DefaultRanker;
impl Ranker for DefaultRanker {
    fn vocab(&self) -> Vec<String> {
        let mut set: HashSet<String> = HashSet::new();
        let mut vocab = Vec::new();

        for c in 32..127 {
            let s = (c as u8 as char).to_string();
            if set.insert(s.clone()) {
                vocab.push(s);
            }
        }

        for s in ["\n", "\t", " "].iter() {
            let ss = s.to_string();
            if set.insert(ss.clone()) {
                vocab.push(ss);
            }
        }

        for s in ["(", ")", "[", "]", "{", "}", "<", ">"].iter() {
            let ss = s.to_string();
            if set.insert(ss.clone()) {
                vocab.push(ss);
            }
        }

        for s in ["+", "-", "*", "/"].iter() {
            let ss = s.to_string();
            if set.insert(ss.clone()) {
                vocab.push(ss);
            }
        }

        for s in [".", ":", "?"].iter() {
            let ss = s.to_string();
            if set.insert(ss.clone()) {
                vocab.push(ss);
            }
        }

        vocab
    }

    fn rank(&self, input: &str) -> Result<Vec<(String, f32)>> {
        let vocab = self.vocab();
        // Baseline random scores
        let mut scores: Vec<f32> = vocab
            .iter()
            .map(|_| {
                let x: f32 = rand::random::<f32>();
                x.max(1e-8)
            })
            .collect();

        let sum: f32 = scores.iter().copied().sum::<f32>().max(1e-8);
        for s in &mut scores {
            *s /= sum;
        }
        let ranked: Vec<(String, f32)> = vocab.into_iter().zip(scores.into_iter()).collect();
        crate::debug_debug!(
            "rank",
            "Ranked {} tokens for prefix len {}: {:?}",
            ranked.len(),
            input.len(),
            ranked[0..5].to_vec()
        );
        Ok(ranked)
    }
}

// "stlc" ranker: kinda random but with vocab drawn from the XTLC spec
// and a bias toward tokens: () λ {} : ->
pub struct StlcRanker;
impl Ranker for StlcRanker {
    fn vocab(&self) -> Vec<String> {
        use std::collections::HashSet;
        let mut set: HashSet<String> = HashSet::new();
        let mut vocab = Vec::new();

        // Identifiers: allow letters, digits, and underscore as single-char tokens
        for c in 'a'..='z' {
            let s = c.to_string();
            if set.insert(s.clone()) {
                vocab.push(s);
            }
        }
        for c in 'A'..='Z' {
            let s = c.to_string();
            if set.insert(s.clone()) {
                vocab.push(s);
            }
        }
        for c in '0'..='9' {
            let s = c.to_string();
            if set.insert(s.clone()) {
                vocab.push(s);
            }
        }

        // Unicode commonly used in the spec
        let unicode_terms = [
            "λ", // lambda
            "τ", // tau type variable
            "₀", "₁", "₂", "₃", "₄", "₅", "₆", "₇", "₈", "₉", // subscript digits
        ];
        for s in unicode_terms.iter() {
            let ss = s.to_string();
            if set.insert(ss.clone()) {
                vocab.push(ss);
            }
        }

        // Structural and punctuation tokens present in the grammar
        for s in ["(", ")", "{", "}", ":", "."].iter() {
            let ss = s.to_string();
            if set.insert(ss.clone()) {
                vocab.push(ss);
            }
        }
        vocab.push("->".to_string());
        for s in ["+", "-", "*", "/", "=", ";", ",", "!", "?", "<", ">"].iter() {
            let ss = s.to_string();
            if set.insert(ss.clone()) {
                vocab.push(ss);
            }
        }

        // Whitespace tokens used during synthesis
        for s in [" ", "\n", "\t"].iter() {
            let ss = s.to_string();
            if set.insert(ss.clone()) {
                vocab.push(ss);
            }
        }

        vocab
    }

    fn rank(&self, input: &str) -> Result<Vec<(String, f32)>> {
        let vocab = self.vocab();
        // Start with random base scores
        let mut scores: Vec<f32> = vocab
            .iter()
            .map(|_| {
                let x: f32 = rand::random::<f32>();
                x.max(1e-8)
            })
            .collect();

        // Bias toward frequently useful STLC tokens
        // Target tokens: "(", ")", "λ", "{", "}", ":", "->"
        for (tok, score) in vocab.iter().zip(scores.iter_mut()) {
            let boost = match tok.as_str() {
                "(" | ")" | "λ" | "{" | "}" | ":" => 10.0,
                "->" | "-" | ">" => 5.0,
                "." => 4.0,
                _ => 1.0,
            };
            *score *= boost;
        }

        // Normalize to probabilities
        let sum: f32 = scores.iter().copied().sum::<f32>().max(1e-8);
        for s in &mut scores {
            *s /= sum;
        }

        let ranked: Vec<(String, f32)> = vocab.into_iter().zip(scores.into_iter()).collect();
        crate::debug_debug!(
            "rank",
            "[stlc] Ranked {} tokens for prefix len {}",
            ranked.len(),
            input.len()
        );
        Ok(ranked)
    }
}

pub struct LLMRanker {
    provider: LogitsProvider,
    id_to_token: Arc<Vec<Option<String>>>,
    special_token_ids: Arc<HashSet<u32>>,
    top_k: usize,
    model_name: String,
}

impl LLMRanker {
    pub fn new(model_name: &str) -> Result<Self> {
        let provider = ml::get_logits_provider(model_name)
            .with_context(|| format!("failed to load logits provider '{}'", model_name))?;

        let vocab_map = provider.tokenizer.get_vocab(true);
        let max_id = vocab_map.values().copied().max().unwrap_or(0) as usize;
        let mut id_to_token: Vec<Option<String>> = vec![None; max_id + 1];
        for (token, id) in vocab_map {
            let id = id as usize;
            if id >= id_to_token.len() {
                id_to_token.resize(id + 1, None);
            }
            id_to_token[id] = Some(token);
        }

        let special_token_ids = id_to_token
            .iter()
            .enumerate()
            .filter_map(|(idx, token)| {
                token.as_ref().and_then(|tok| {
                    if tok.starts_with('<') && tok.ends_with('>') {
                        Some(idx as u32)
                    } else {
                        None
                    }
                })
            })
            .collect();

        Ok(Self {
            provider,
            id_to_token: Arc::new(id_to_token),
            special_token_ids: Arc::new(special_token_ids),
            top_k: 64,
            model_name: model_name.to_string(),
        })
    }

    fn decode_token(&self, token_id: u32) -> Result<Option<String>> {
        if self.special_token_ids.contains(&token_id) {
            return Ok(None);
        }

        let decoded = self
            .provider
            .tokenizer
            .decode(&[token_id], false)
            .map_err(|e| anyhow!("failed to decode token {}: {}", token_id, e))?;

        if decoded.is_empty() {
            return Ok(None);
        }

        Ok(Some(decoded))
    }
}

impl Ranker for LLMRanker {
    fn vocab(&self) -> Vec<String> {
        self.id_to_token
            .iter()
            .filter_map(|maybe| maybe.clone())
            .collect()
    }

    fn rank(&self, input: &str) -> Result<Vec<(String, f32)>> {
        let token_ids = self
            .provider
            .tokenize(input)
            .context("failed to tokenize input for Mixtral ranker")?;
        if token_ids.is_empty() {
            return Err(anyhow!("tokenizer produced no tokens"));
        }

        let logits = self
            .provider
            .get_logits(&token_ids)
            .context("failed to compute logits for Mixtral ranker")?;

        let mut indexed: Vec<(usize, f32)> = logits.into_iter().enumerate().collect();
        indexed.sort_unstable_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(Ordering::Equal));

        let mut results = Vec::new();
        for (idx, score) in indexed.into_iter().take(self.top_k * 4) {
            let token_id = idx as u32;
            if let Some(text) = self.decode_token(token_id)? {
                results.push((text, score));
                if results.len() >= self.top_k {
                    break;
                }
            }
        }

        if results.is_empty() {
            return Err(anyhow!("no valid tokens produced by Mixtral ranker"));
        }

        Ok(results)
    }
}


