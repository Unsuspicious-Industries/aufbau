// generic framework for somehting that ranks bits of strings

pub trait Ranker {
    fn vocab(&self) -> Vec<String>;
    fn rank(&self, input: &str) -> Vec<(String, f32)>;
}

// default ranker thats random with a vocab of all ascii chars + some stuff
pub struct DefaultRanker;
impl Ranker for DefaultRanker {
    fn vocab(&self) -> Vec<String> {
        use std::collections::HashSet;
        let mut set: HashSet<String> = HashSet::new();
        let mut vocab = Vec::new();
        for c in 32..127 {
            let s = (c as u8 as char).to_string();
            if set.insert(s.clone()) { vocab.push(s); }
        }
        for s in ["\n", "\t"].iter() {
            let ss = s.to_string();
            if set.insert(ss.clone()) { vocab.push(ss); }
        }
        for s in ["α","β","γ","δ","ε"].iter() {
            let ss = s.to_string();
            if set.insert(ss.clone()) { vocab.push(ss); }
        }
        if set.insert("λ".to_string()) { vocab.push("λ".to_string()); }
        for s in ["(", ")", "[", "]", "{", "}", "<", ">"].iter() {
            let ss = s.to_string();
            if set.insert(ss.clone()) { vocab.push(ss); }
        }
        for s in ["+","-","*","/"].iter() {
            let ss = s.to_string();
            if set.insert(ss.clone()) { vocab.push(ss); }
        }
        for s in [".", ":", "?"].iter() {
            let ss = s.to_string();
            if set.insert(ss.clone()) { vocab.push(ss); }
        }
        vocab
    }

    fn rank(&self, input: &str) -> Vec<(String, f32)> {
        let vocab = self.vocab();
        // Baseline random scores
        let mut scores: Vec<f32> = vocab.iter().map(|_| {
            let x: f32 = rand::random::<f32>();
            x.max(1e-8)
        }).collect();

        let sum: f32 = scores.iter().copied().sum::<f32>().max(1e-8);
        for s in &mut scores { *s /= sum; }
        let ranked: Vec<(String, f32)> = vocab.into_iter().zip(scores.into_iter()).collect();
        crate::debug_debug!("rank", "Ranked {} tokens for prefix len {}: {:?}", ranked.len(), input.len(), ranked[0..5].to_vec());
        ranked
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
        for c in 'a'..='z' { let s = c.to_string(); if set.insert(s.clone()) { vocab.push(s); } }
        for c in 'A'..='Z' { let s = c.to_string(); if set.insert(s.clone()) { vocab.push(s); } }
        for c in '0'..='9' { let s = c.to_string(); if set.insert(s.clone()) { vocab.push(s); } }

        // Unicode commonly used in the spec
        let unicode_terms = [
            "λ", // lambda
            "τ", // tau type variable
            "₀","₁","₂","₃","₄","₅","₆","₇","₈","₉", // subscript digits
        ];
        for s in unicode_terms.iter() {
            let ss = s.to_string();
            if set.insert(ss.clone()) { vocab.push(ss); }
        }

        // Structural and punctuation tokens present in the grammar
        for s in ["(", ")", "{", "}", ":", "."].iter() {
            let ss = s.to_string();
            if set.insert(ss.clone()) { vocab.push(ss); }
        }
        vocab.push("->".to_string());
        for s in ["+", "-", "*", "/", "=", ";", ",", "!", "?", "<", ">"].iter() {
            let ss = s.to_string();
            if set.insert(ss.clone()) { vocab.push(ss); }
        }

        // Whitespace tokens used during synthesis
        for s in [" ", "\n", "\t"].iter() {
            let ss = s.to_string();
            if set.insert(ss.clone()) { vocab.push(ss); }
        }

        vocab
    }

    fn rank(&self, input: &str) -> Vec<(String, f32)> {
        let vocab = self.vocab();
        // Start with random base scores
        let mut scores: Vec<f32> = vocab.iter().map(|_| {
            let x: f32 = rand::random::<f32>();
            x.max(1e-8)
        }).collect();

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
        for s in &mut scores { *s /= sum; }

        let ranked: Vec<(String, f32)> = vocab.into_iter().zip(scores.into_iter()).collect();
        crate::debug_debug!("rank", "[stlc] Ranked {} tokens for prefix len {}", ranked.len(), input.len());
        ranked
    }
}