//! Tokenizer for grammar-based parsing.
//!
//! This module provides the `Tokenizer` struct which holds precomputed regexes
//! for special tokens and delimiters, and the `Segment` type representing
//! tokenized pieces of input.

use crate::regex::{PrefixStatus, Regex as DerivativeRegex};

/// Default delimiters for tokenization (whitespace).
/// !JANKY! This causes completions problems,
/// and adds harcoded constraints that should ideally
/// be dealt with at the grammar level instead.
pub const DEFAULT_DELIMITERS: &[char] = &[' ', '\n', '\t'];

/// A tokenized segment of input with text and position information.
/// Uses byte-based positions and storage to avoid Unicode issues.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Segment {
    /// Raw bytes of the segment (UTF-8 encoded)
    bytes: Vec<u8>,
    /// Byte-based start position in the original input
    pub start: usize,
    /// Byte-based end position in the original input
    pub end: usize,
    /// The segment's index in the token stream (set during tokenization)
    pub index: usize,
    /// If true, this segment is a prefix of one or more special tokens
    /// (occurs at end of input when we have incomplete special tokens like "-" for "->")
    /// Also not very elegant
    pub is_partial_special: bool,
}

impl Segment {
    /// Create a new segment from bytes and byte positions
    pub fn new(bytes: Vec<u8>, start: usize, end: usize) -> Self {
        Self {
            bytes,
            start,
            end,
            index: 0,
            is_partial_special: false,
        }
    }

    /// Create a new segment with an index
    pub fn with_index(bytes: Vec<u8>, start: usize, end: usize, index: usize) -> Self {
        Self {
            bytes,
            start,
            end,
            index,
            is_partial_special: false,
        }
    }

    /// Create a segment from a string slice and byte positions
    pub fn from_str(text: &str, start: usize, end: usize) -> Self {
        Self {
            bytes: text.as_bytes().to_vec(),
            start,
            end,
            index: 0,
            is_partial_special: false,
        }
    }

    /// Create a segment marked as a partial special token
    pub fn partial_special(bytes: Vec<u8>, start: usize, end: usize, index: usize) -> Self {
        Self {
            bytes,
            start,
            end,
            index,
            is_partial_special: true,
        }
    }

    /// Get the text as a UTF-8 string
    pub fn text(&self) -> String {
        String::from_utf8_lossy(&self.bytes).into_owned()
    }

    /// Get the raw bytes
    pub fn bytes(&self) -> &[u8] {
        &self.bytes
    }

    /// Get the length in bytes
    pub fn len(&self) -> usize {
        self.bytes.len()
    }

    /// Check if the segment is empty
    pub fn is_empty(&self) -> bool {
        self.bytes.is_empty()
    }
}

impl std::fmt::Display for Segment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.text())
    }
}

/// Tokenizer with precomputed regexes for efficient tokenization.
///
/// Created from special tokens and delimiters, holds the compiled regex forms
/// for fast matching during tokenization.
#[derive(Debug, Clone)]
pub struct Tokenizer {
    special_tokens: Vec<DerivativeRegex>,
    delimiters: Vec<DerivativeRegex>,
}

impl Tokenizer {
    /// Create a tokenizer from special tokens and delimiters.
    /// Special tokens are sorted by length (longest first) for proper matching.
    pub fn new(mut special_tokens: Vec<String>, delimiters: Vec<char>) -> Self {
        // Sort by length descending for longest-match priority
        special_tokens.sort_by(|a, b| b.len().cmp(&a.len()).then_with(|| a.cmp(b)));

        Self {
            special_tokens: special_tokens
                .into_iter()
                .map(|s| DerivativeRegex::literal(&s))
                .collect(),
            delimiters: delimiters.into_iter().map(DerivativeRegex::Char).collect(),
        }
    }

    /// Check if a character is a delimiter
    fn is_delimiter(&self, c: char) -> bool {
        let s = c.to_string();
        self.delimiters.iter().any(|d| d.matches(&s))
    }

    /// Find the length of the longest prefix of `text` that `regex` accepts.
    fn match_len(regex: &DerivativeRegex, text: &str) -> Option<usize> {
        let mut r = regex.clone();
        let mut last_match = if r.is_nullable() { Some(0) } else { None };

        for (i, c) in text.char_indices() {
            r = r.deriv(c).simplify();
            if r.is_empty() {
                break;
            }
            if r.is_nullable() {
                last_match = Some(i + c.len_utf8());
            }
        }

        last_match
    }

    /// Try to match a special token at the start of `text`.
    /// Returns the length of the longest complete match, or None.
    fn match_special(&self, text: &str) -> Option<usize> {
        let mut best_len: Option<usize> = None;

        for regex in &self.special_tokens {
            if let Some(len) = Self::match_len(regex, text) {
                if len > 0 && best_len.map_or(true, |b| len > b) {
                    best_len = Some(len);
                }
            }
        }

        best_len
    }

    /// Check if `text` is a prefix of any special token (but not a complete match).
    fn prefix_special(&self, text: &str) -> bool {
        for regex in &self.special_tokens {
            if let PrefixStatus::Prefix(_) = regex.prefix_match(text) {
                return true;
            }
        }
        false
    }

    /// Tokenize the input string into segments with byte-based spans.
    pub fn tokenize(&self, input: &str) -> Result<Vec<Segment>, String> {
        let mut segments = Vec::new();
        let mut byte_pos = 0;
        let bytes = input.as_bytes();

        while byte_pos < bytes.len() {
            let remaining = &input[byte_pos..];

            // Try to match a special token at the current position
            if let Some(match_len) = self.match_special(remaining) {
                let index = segments.len();
                let matched_bytes = remaining[..match_len].as_bytes().to_vec();
                segments.push(Segment::with_index(
                    matched_bytes,
                    byte_pos,
                    byte_pos + match_len,
                    index,
                ));
                byte_pos += match_len;
                continue;
            }

            // Check if remaining is a prefix of a special token (partial at end of input)
            if self.prefix_special(remaining) {
                let index = segments.len();
                segments.push(Segment::partial_special(
                    remaining.as_bytes().to_vec(),
                    byte_pos,
                    bytes.len(),
                    index,
                ));
                break;
            }

            // Check if current char is a delimiter
            let c = remaining.chars().next().unwrap();
            if self.is_delimiter(c) {
                byte_pos += c.len_utf8();
                continue;
            }

            // Otherwise, accumulate a normal token
            let start_pos = byte_pos;
            let mut token_end = byte_pos;

            for (i, c) in remaining.char_indices() {
                let abs_pos = byte_pos + i;

                if self.is_delimiter(c) {
                    break;
                }

                let substr = &input[abs_pos..];
                if self.match_special(substr).is_some() {
                    break;
                }

                if i > 0 && self.prefix_special(substr) {
                    break;
                }

                token_end = abs_pos + c.len_utf8();
            }

            if token_end > start_pos {
                let index = segments.len();
                let token_bytes = input[start_pos..token_end].as_bytes().to_vec();
                segments.push(Segment::with_index(
                    token_bytes,
                    start_pos,
                    token_end,
                    index,
                ));
                byte_pos = token_end;
            } else {
                // Edge case: single char that's not a delimiter and not special
                return Err("Found weird unexpected char !".to_string());
            }
        }

        Ok(segments)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize_with_special_tokens() {
        let input = "x=r+4;print(x)";
        let special_tokens = vec!["+".to_string(), "=".to_string()];
        let delimiters = vec![';', '(', ')'];
        let tokenizer = Tokenizer::new(special_tokens, delimiters);

        let segments = tokenizer.tokenize(input).unwrap();
        let token_strs: Vec<_> = segments.iter().map(|seg| seg.text()).collect();

        assert_eq!(token_strs, vec!["x", "=", "r", "+", "4", "print", "x"]);
    }

    #[test]
    fn test_tokenize_with_spans_positions() {
        let input = "int x = 5;";
        let special_tokens = vec!["int".to_string(), "=".to_string(), ";".to_string()];
        let delimiters = vec![' ', '\t', '\n'];
        let tokenizer = Tokenizer::new(special_tokens, delimiters);
        let segments = tokenizer.tokenize(input).unwrap();

        let strs: Vec<_> = segments.iter().map(|seg| seg.text()).collect();
        assert_eq!(strs, vec!["int", "x", "=", "5", ";"]);

        // Verify byte positions
        assert_eq!(segments[0].start, 0);
        assert_eq!(segments[0].end, 3);
        assert_eq!(segments[1].start, 4);
        assert_eq!(segments[1].end, 5);
    }

    #[test]
    fn test_partial_special_token_at_end() {
        let input = "foo-";
        let special_tokens = vec!["->".to_string()];
        let delimiters = vec![' '];
        let tokenizer = Tokenizer::new(special_tokens, delimiters);

        let segments = tokenizer.tokenize(input).unwrap();
        assert_eq!(segments.len(), 2);
        assert_eq!(segments[0].text(), "foo");
        assert!(!segments[0].is_partial_special);
        assert_eq!(segments[1].text(), "-");
        assert!(segments[1].is_partial_special);
    }

    #[test]
    fn test_complete_special_token_not_partial() {
        let input = "foo->";
        let special_tokens = vec!["->".to_string()];
        let delimiters = vec![' '];
        let tokenizer = Tokenizer::new(special_tokens, delimiters);

        let segments = tokenizer.tokenize(input).unwrap();
        assert_eq!(segments.len(), 2);
        assert_eq!(segments[0].text(), "foo");
        assert_eq!(segments[1].text(), "->");
        assert!(!segments[1].is_partial_special);
    }

    #[test]
    fn test_partial_special_token_in_lambda_type() {
        let input = "λf:(A-";
        let special_tokens = vec!["->".to_string(), "λ".to_string(), ":".to_string()];
        let delimiters = vec![' ', '(', ')'];
        let tokenizer = Tokenizer::new(special_tokens, delimiters);

        let segments = tokenizer.tokenize(input).unwrap();
        let tokens: Vec<_> = segments.iter().map(|s| s.text()).collect();
        assert_eq!(tokens, vec!["λ", "f", ":", "A", "-"]);
        assert!(segments[4].is_partial_special);
    }
}
