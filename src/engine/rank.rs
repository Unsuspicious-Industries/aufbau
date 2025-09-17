// generic framework for somehting that ranks bits of strings

pub trait Ranker {
    fn vocab(&self) -> Vec<String>;
    fn rank(&self, input: &str) -> Vec<(String, f32)>;
}

// default ranker thats random with a vocab of all ascii chars + some stuff
pub struct DefaultRanker;
impl Ranker for DefaultRanker {
    fn vocab(&self) -> Vec<String> {
        let mut vocab = Vec::new();
        for c in 32..127 {
            vocab.push((c as u8 as char).to_string());
        }
        vocab.push("\n".to_string());
        vocab.push("\t".to_string());
        vocab.push(" ".to_string());
        // greek letters
        vocab.push("α".to_string());
        vocab.push("β".to_string());
        vocab.push("γ".to_string());
        vocab.push("δ".to_string());
        vocab.push("ε".to_string());
        //lambda
        vocab.push("λ".to_string());
        // paren and all similar chars
        vocab.push("(".to_string());
        vocab.push(")".to_string());
        vocab.push("[".to_string());
        vocab.push("]".to_string());
        vocab.push("{".to_string());
        vocab.push("}".to_string());
        vocab.push("<".to_string());
        vocab.push(">".to_string());
        // common operators
        vocab.push("+".to_string());
        vocab.push("-".to_string());
        vocab.push("*".to_string());
        vocab.push("/".to_string());
        // punctuation
        vocab.push(".".to_string());
        vocab.push(":".to_string());
        vocab.push("?".to_string());
        vocab
    }

    fn rank(&self, input: &str) -> Vec<(String, f32)> {
        use rand::seq::SliceRandom;
        use rand::thread_rng;

        let mut rng = thread_rng();
        let mut vocab = self.vocab();
        vocab.shuffle(&mut rng);
        vocab.into_iter().map(|s| (s, rand::random::<f32>())).collect()
    }
}