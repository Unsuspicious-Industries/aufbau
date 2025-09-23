use std::{fmt::Error, hash::Hash};

use bimap::BiMap;

pub type TokenId = usize;

pub struct Tokenizer {
    tokens: BiMap<String, TokenId>,
    special_tokens: Vec<String>,
    delimiters: Vec<char>,
}

impl Tokenizer {
    pub fn new(special_tokens: Vec<String>, delimiters: Vec<char>) -> Self {

        let tokens = BiMap::new();
        let mut tokenizer = Self {
            tokens,
            special_tokens: special_tokens.clone(),
            delimiters: delimiters.clone(),
        };
        for token in special_tokens {
            tokenizer.token(token);
        }

        tokenizer
    }

    pub fn token(&mut self, token: String) -> TokenId {
        if let Some(id) = self.tokens.get_by_left(&token) {
            *id
        } else {
            let mut id = self.tokens.len();
            while self.tokens.get_by_right(&id).is_some() {
                // Ensure unique IDs
                id += 1;
            }
            self.tokens.insert(token, id);
            id
        }
    }

    pub fn str(&self, id:TokenId) -> Option<String> {
        self.tokens.get_by_right(&id).cloned()
    }

    /// Tokenize the input string into a vector of tokens, handling special tokens
    pub fn tokenize(&mut self, input: String) -> Result<Vec<TokenId>,Error> {
        let mut split = Vec::<String>::new();
        let mut i = 0;
        let chars: Vec<char> = input.chars().collect();
        let input_len = chars.len();

        while i < input_len {
            // Try to match a special token at the current position
            let mut matched = None;
            for special in &self.special_tokens {
                let special_chars: Vec<char> = special.chars().collect();
                if i + special_chars.len() <= input_len &&
                    &chars[i..i + special_chars.len()] == special_chars.as_slice()
                {
                    matched = Some(special.clone());
                    break;
                }
            }
            if let Some(token) = matched {
                split.push(token.clone());
                i += token.chars().count();  // Use character count, not byte length
                continue;
            }

            // If not a special token, check for delimiter
            if self.delimiters.contains(&chars[i]) {
                // skip delimiter, do not push
                i += 1;
                continue;
            }
            // Otherwise, accumulate a normal token
            let mut current = String::new();
            while i < input_len
                && !self.delimiters.contains(&chars[i])
                && !self.special_tokens.iter().any(|s| {
                    let s_chars: Vec<char> = s.chars().collect();
                    i + s_chars.len() <= input_len &&
                        &chars[i..i + s_chars.len()] == s_chars.as_slice()
                })
            {
                current.push(chars[i]);
                i += 1;
            }
            if !current.is_empty() {
                split.push(current);
            }
        }

        // Now map split tokens to TokenIds
        let result = split.iter().map(|s| self.token(s.clone())).collect::<Vec<_>>();

        Ok(result)
    }

    /// Tokenize the input string and return token ids with character spans (start,end)
    pub fn tokenize_with_spans(&mut self, input: &str) -> Result<Vec<(TokenId, usize, usize)>, Error> {
        let mut out: Vec<(TokenId, usize, usize)> = Vec::new();
        let mut i = 0;
        let chars: Vec<char> = input.chars().collect();
        let input_len = chars.len();

        while i < input_len {
            // Try to match a special token at the current position
            let mut matched: Option<(String, usize)> = None; // (token, len)
            for special in &self.special_tokens {
                let special_chars: Vec<char> = special.chars().collect();
                if i + special_chars.len() <= input_len &&
                    &chars[i..i + special_chars.len()] == special_chars.as_slice()
                {
                    matched = Some((special.clone(), special_chars.len()));
                    break;
                }
            }
            if let Some((tok, len)) = matched {
                let start = i;
                let end = i + len;
                let id = self.token(tok);
                out.push((id, start, end));
                i = end;
                continue;
            }

            // If not a special token, check for delimiter (whitespace)
            if self.delimiters.contains(&chars[i]) {
                i += 1;
                continue;
            }

            // Otherwise, accumulate a normal token
            let start = i;
            let mut current = String::new();
            while i < input_len
                && !self.delimiters.contains(&chars[i])
                && !self.special_tokens.iter().any(|s| {
                    let s_chars: Vec<char> = s.chars().collect();
                    i + s_chars.len() <= input_len &&
                        &chars[i..i + s_chars.len()] == s_chars.as_slice()
                })
            {
                current.push(chars[i]);
                i += 1;
            }
            if !current.is_empty() {
                let end = i;
                let id = self.token(current);
                out.push((id, start, end));
            }
        }

        Ok(out)
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;

    #[test]
    fn test_tokenize_with_special_tokens() {
        let input = "x=r+4;print(x)".to_string();
        let special_tokens = vec!["+".to_string(),"=".to_string()];
        let delimiters = vec![ ';', '(', ')'];
        let mut tokenizer = Tokenizer::new(special_tokens.clone(), delimiters);

        let tokens = tokenizer.tokenize(input).unwrap();
        let token_strs: Vec<_> = tokens
            .iter()
            .map(|id| tokenizer.tokens.get_by_right(id).unwrap().clone())
            .collect();

        // Removed debug print - use unified debug system if needed
        assert_eq!(token_strs, vec!["x", "=", "r", "+", "4", "print", "x"]);
    }

    #[test]
    fn test_tokenize_with_spans_positions() {
        let input = "int x = 5;";
        let special_tokens = vec!["int".to_string(), "=".to_string(), ";".to_string()];
        let delimiters = vec![' ', '\t', '\n'];
        let mut tokenizer = Tokenizer::new(special_tokens.clone(), delimiters);
        let occ = tokenizer.tokenize_with_spans(input).unwrap();
        let strs: Vec<_> = occ.iter().map(|(id, _, _)| tokenizer.str(*id).unwrap()).collect();
        assert_eq!(strs, vec!["int", "x", "=", "5", ";"]);
        // Check spans map to substrings
        let pieces: Vec<_> = occ.iter().map(|(_, s, e)| &input[*s..*e]).collect();
        assert_eq!(pieces, vec!["int", "x", "=", "5", ";"]);
    }
}