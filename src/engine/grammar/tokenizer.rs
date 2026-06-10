//! Tokenizer for grammar-based parsing.
//!
//! This module provides the `Tokenizer` struct which holds precomputed tables
//! for special tokens and delimiters, and the `Segment` type representing
//! tokenized pieces of input.

use std::collections::HashSet;

/// Default delimiters for tokenization (whitespace).
pub const DEFAULT_DELIMITERS: &[char] = &[' ', '\n', '\t'];

/// A word-constituent character: the ASCII identifier alphabet (`\w`). A keyword
/// spelled from these (`in`, `let`) is delimited by transitions in and out of the
/// class, so `in` is the keyword only when not flanked by them; a symbolic
/// keyword like `λ` lies outside it and tokenizes on its own (`λx` → `λ`, `x`).
fn is_word_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

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
    /// If true, this segment is a prefix of one or more special tokens.
    /// (Occurs at end of input when we have incomplete special tokens like "-" for "->")
    pub is_partial_special: bool,
}

impl Segment {
    /// Create a new segment from bytes and byte positions
    #[must_use]
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
    #[must_use]
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
    #[must_use]
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
    #[must_use]
    pub fn partial_special(bytes: Vec<u8>, start: usize, end: usize, index: usize) -> Self {
        Self {
            bytes,
            start,
            end,
            index,
            is_partial_special: true,
        }
    }

    /// Get the text as a borrowed UTF-8 string.
    #[must_use]
    pub fn as_str(&self) -> &str {
        // SAFETY: segment bytes are always sliced from the original
        // UTF-8 input string by Tokenizer::tokenize, which operates
        // on &str boundaries.
        unsafe { std::str::from_utf8_unchecked(&self.bytes) }
    }

    /// Get the text as an owned UTF-8 string
    #[must_use]
    pub fn text(&self) -> String {
        self.as_str().to_owned()
    }

    /// Get the raw bytes
    #[must_use]
    pub fn bytes(&self) -> &[u8] {
        &self.bytes
    }

    /// Get the length in bytes
    #[must_use]
    pub fn len(&self) -> usize {
        self.bytes.len()
    }

    /// Check if the segment is empty
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.bytes.is_empty()
    }
}

impl std::fmt::Display for Segment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.text())
    }
}

/// Tokenizer with precomputed tables for efficient tokenization.
///
/// Created from special tokens and delimiters, holds sorted literal specials
/// and delimiter lookup tables for fast matching during tokenization.
#[derive(Debug, Clone)]
pub struct Tokenizer {
    special_tokens: Vec<String>,
    delimiters: HashSet<char>,
    sorted_specials: Vec<String>,
    needs_build: bool,
}

impl Default for Tokenizer {
    fn default() -> Self {
        Self {
            special_tokens: Vec::new(),
            delimiters: DEFAULT_DELIMITERS.iter().copied().collect(),
            sorted_specials: Vec::new(),
            needs_build: false,
        }
    }
}

impl Tokenizer {
    /// Create a new empty tokenizer with default delimiters.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a tokenizer from special tokens and delimiters.
    /// Special tokens are sorted by length (longest first) for proper matching.
    #[must_use]
    pub fn with_specials_and_delimiters(
        special_tokens: Vec<String>,
        delimiters: Vec<char>,
    ) -> Self {
        let mut tokenizer = Self {
            special_tokens: Vec::new(),
            delimiters: delimiters.into_iter().collect(),
            sorted_specials: Vec::new(),
            needs_build: false,
        };
        for token in special_tokens {
            tokenizer.add_special(token);
        }
        tokenizer.build();
        tokenizer
    }

    /// Add a special token that won't be split during tokenization.
    pub fn add_special(&mut self, token: String) {
        if !self.special_tokens.contains(&token) {
            self.special_tokens.push(token);
            self.needs_build = true;
        }
    }

    /// Return the list of configured special tokens.
    #[must_use]
    pub fn specials(&self) -> &Vec<String> {
        &self.special_tokens
    }

    /// Rebuild the sorted/deduplicated lookup table.
    /// Called automatically by `tokenize` if needed.
    pub fn build(&mut self) {
        if !self.needs_build {
            return;
        }
        self.sorted_specials = self.special_tokens.clone();
        self.sorted_specials
            .sort_by(|a, b| b.len().cmp(&a.len()).then_with(|| a.cmp(b)));
        self.sorted_specials.dedup();
        self.needs_build = false;
    }

    /// Check if a character is a delimiter
    fn is_delimiter(&self, c: char) -> bool {
        self.delimiters.contains(&c)
    }

    /// Try to match a special token at the start of `text`.
    /// Returns the length of the longest complete match, or None.
    fn match_special(&self, text: &str) -> Option<usize> {
        // Tokens are sorted by descending length, so the first match is the longest.
        for token in &self.sorted_specials {
            if token.is_empty() || !text.starts_with(token) {
                continue;
            }
            // A word-like keyword (`in`, `match`) is a token only at a right word
            // boundary: `in` inside `inc` starts a longer identifier. Symbolic
            // specials (`->`, `::`) end on punctuation, so they always match.
            let ends_word = token.chars().next_back().is_some_and(is_word_char);
            let next_is_word = text[token.len()..].chars().next().is_some_and(is_word_char);
            if ends_word && next_is_word {
                continue;
            }
            return Some(token.len());
        }
        None
    }

    /// Check if `text` is a prefix of any special token (but not a complete match).
    fn prefix_special(&self, text: &str) -> bool {
        self.sorted_specials
            .iter()
            .any(|token| token.len() > text.len() && token.starts_with(text))
    }

    /// Tokenize the input string into segments with byte-based spans.
    pub fn tokenize(&mut self, input: &str) -> Result<Vec<Segment>, String> {
        self.build();

        let mut segments = Vec::new();
        let mut byte_pos = 0;
        let bytes = input.as_bytes();

        while byte_pos < bytes.len() {
            let remaining = &input[byte_pos..];

            // Try to match a special token at the current position
            if let Some(match_len) = self.match_special(remaining) {
                let index = segments.len();
                let matched_bytes = remaining.as_bytes()[..match_len].to_vec();
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
            let Some(c) = remaining.chars().next() else {
                break;
            };
            if self.is_delimiter(c) {
                byte_pos += c.len_utf8();
                continue;
            }

            // Otherwise, accumulate a normal token
            let start_pos = byte_pos;
            let mut token_end = byte_pos;
            let mut prev_char: Option<char> = None;

            for (i, c) in remaining.char_indices() {
                let abs_pos = byte_pos + i;

                if self.is_delimiter(c) {
                    break;
                }

                let substr = &input[abs_pos..];
                if self.match_special(substr).is_some() {
                    // A word-like keyword in the interior of a word (previous char
                    // is a word char too) belongs to the identifier (`within`), so
                    // it is no boundary; a symbolic special still breaks the word.
                    let prev_is_word = prev_char.is_some_and(is_word_char);
                    if !(is_word_char(c) && prev_is_word) {
                        break;
                    }
                }

                // Only break for a prefix-special if there is a character class
                // transition from the previous character to the current one.
                // This prevents "Int" from being split as ["In", "t"(partial)]
                // when "t" is a prefix of keyword "then"/"true", while still
                // allowing "foo-" to split as ["foo", "-"(partial)] when "-"
                // is a prefix of "->".
                if i > 0 && self.prefix_special(substr) {
                    let prev_is_word = prev_char.is_some_and(|p| p.is_alphanumeric() || p == '_');
                    let curr_is_word = c.is_alphanumeric() || c == '_';
                    if !(prev_is_word && curr_is_word) {
                        break;
                    }
                }

                token_end = abs_pos + c.len_utf8();
                prev_char = Some(c);
            }

            if token_end > start_pos {
                let index = segments.len();
                let token_bytes = input.as_bytes()[start_pos..token_end].to_vec();
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
