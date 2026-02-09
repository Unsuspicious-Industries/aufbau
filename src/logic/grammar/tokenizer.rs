//! Tokenizer for grammar-based parsing.
//!
//! This module provides the `Tokenizer` struct which holds precomputed tables
//! for special tokens and delimiters, and the `Segment` type representing
//! tokenized pieces of input.

use std::collections::HashSet;

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

    /// Get the text as a borrowed UTF-8 string.
    pub fn as_str(&self) -> &str {
        // Segments are always built from slices of the original UTF-8 input.
        std::str::from_utf8(&self.bytes).expect("segment bytes must be valid UTF-8")
    }

    /// Get the text as an owned UTF-8 string
    pub fn text(&self) -> String {
        self.as_str().to_owned()
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

/// Tokenizer with precomputed tables for efficient tokenization.
///
/// Created from special tokens and delimiters, holds sorted literal specials
/// and delimiter lookup tables for fast matching during tokenization.
#[derive(Debug, Clone)]
pub struct Tokenizer {
    special_tokens: Vec<String>,
    delimiters: HashSet<char>,
}

impl Tokenizer {
    /// Create a tokenizer from special tokens and delimiters.
    /// Special tokens are sorted by length (longest first) for proper matching.
    pub fn new(mut special_tokens: Vec<String>, delimiters: Vec<char>) -> Self {
        // Sort by length descending for longest-match priority
        special_tokens.sort_by(|a, b| b.len().cmp(&a.len()).then_with(|| a.cmp(b)));
        special_tokens.dedup();

        Self {
            special_tokens,
            delimiters: delimiters.into_iter().collect(),
        }
    }

    /// Check if a character is a delimiter
    fn is_delimiter(&self, c: char) -> bool {
        self.delimiters.contains(&c)
    }

    /// Try to match a special token at the start of `text`.
    /// Returns the length of the longest complete match, or None.
    fn match_special(&self, text: &str) -> Option<usize> {
        // Tokens are sorted by descending length, so the first match is the longest.
        for token in &self.special_tokens {
            if !token.is_empty() && text.starts_with(token) {
                return Some(token.len());
                }
        }
        None
    }

    /// Check if `text` is a prefix of any special token (but not a complete match).
    fn prefix_special(&self, text: &str) -> bool {
        self.special_tokens
            .iter()
            .any(|token| token.len() > text.len() && token.starts_with(text))
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
            let mut prev_char: Option<char> = None;

            for (i, c) in remaining.char_indices() {
                let abs_pos = byte_pos + i;

                if self.is_delimiter(c) {
                    break;
                }

                let substr = &input[abs_pos..];
                if self.match_special(substr).is_some() {
                    break;
                }

                // Only break for a prefix-special if there is a character class
                // transition from the previous character to the current one.
                // This prevents "Int" from being split as ["In", "t"(partial)]
                // when "t" is a prefix of keyword "then"/"true", while still
                // allowing "foo-" to split as ["foo", "-"(partial)] when "-"
                // is a prefix of "->".
                if i > 0 && self.prefix_special(substr) {
                    let prev_is_word = prev_char.map_or(false, |p| p.is_alphanumeric() || p == '_');
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

    // ================================================================
    // Regression tests for the word-boundary fix in the accumulation
    // loop. The old code broke tokens like "Int" when a trailing
    // alphanumeric character happened to prefix a keyword (e.g. "t"
    // prefixes "true"/"then").
    // ================================================================

    /// Helper: build a tokenizer with the same specials as the fun grammar.
    fn fun_tokenizer() -> Tokenizer {
        let special_tokens = vec![
            "->".into(),
            "λ".into(),
            ":".into(),
            ".".into(),
            "=".into(),
            "let".into(),
            "in".into(),
            "if".into(),
            "then".into(),
            "else".into(),
            "true".into(),
            "false".into(),
            "(".into(),
            ")".into(),
        ];
        Tokenizer::new(special_tokens, vec![' ', '\n', '\t'])
    }

    /// Collect (text, is_partial) pairs for compact assertions.
    fn tok(tokenizer: &Tokenizer, input: &str) -> Vec<(String, bool)> {
        tokenizer
            .tokenize(input)
            .unwrap()
            .iter()
            .map(|s| (s.text(), s.is_partial_special))
            .collect()
    }

    // --- TypeName tokens that previously broke ---

    #[test]
    fn fun_typename_int_standalone() {
        let t = fun_tokenizer();
        // "Int" at end of input — 't' prefixes "true"/"then" but must NOT split
        assert_eq!(tok(&t, "Int"), vec![("Int".into(), false)]);
    }

    #[test]
    fn fun_typename_bool_standalone() {
        let t = fun_tokenizer();
        // "Bool" — no trailing char prefixes a keyword, but verify anyway
        assert_eq!(tok(&t, "Bool"), vec![("Bool".into(), false)]);
    }

    #[test]
    fn fun_typename_int_in_lambda() {
        let t = fun_tokenizer();
        // The original failing case: λx:Int
        let result = tok(&t, "λx:Int");
        assert_eq!(
            result,
            vec![
                ("λ".into(), false),
                ("x".into(), false),
                (":".into(), false),
                ("Int".into(), false),
            ]
        );
    }

    #[test]
    fn fun_typename_int_before_dot() {
        let t = fun_tokenizer();
        // "Int." — '.' is a special token, so accumulation should stop before it
        let result = tok(&t, "λx:Int.x");
        assert_eq!(
            result,
            vec![
                ("λ".into(), false),
                ("x".into(), false),
                (":".into(), false),
                ("Int".into(), false),
                (".".into(), false),
                ("x".into(), false),
            ]
        );
    }

    #[test]
    fn fun_typename_int_before_arrow() {
        let t = fun_tokenizer();
        let result = tok(&t, "Int->Bool");
        assert_eq!(
            result,
            vec![
                ("Int".into(), false),
                ("->".into(), false),
                ("Bool".into(), false),
            ]
        );
    }

    #[test]
    fn fun_typename_int_before_space() {
        let t = fun_tokenizer();
        let result = tok(&t, "Int Bool");
        assert_eq!(result, vec![("Int".into(), false), ("Bool".into(), false),]);
    }

    // --- Names whose suffixes overlap with keyword prefixes ---

    #[test]
    fn fun_typename_ending_in_t() {
        let t = fun_tokenizer();
        // Various names ending in 't' (prefix of "then"/"true")
        // These should NOT be split by the prefix_special check.
        // Note: "Elet" WILL be split as ["E", "let"] because `let` is a
        // complete keyword match inside the accumulation — that's the
        // match_special check, not the prefix_special bug.
        for name in &["Int", "Nat", "Set", "Abst"] {
            let result = tok(&t, name);
            assert_eq!(
                result,
                vec![(name.to_string(), false)],
                "TypeName '{}' should tokenize as a single token",
                name
            );
        }
    }

    #[test]
    fn fun_typename_ending_in_e() {
        let t = fun_tokenizer();
        // 'e' prefixes "else"
        for name in &["Type", "Name", "Base"] {
            let result = tok(&t, name);
            assert_eq!(
                result,
                vec![(name.to_string(), false)],
                "TypeName '{}' should tokenize as a single token",
                name
            );
        }
    }

    #[test]
    fn fun_typename_ending_in_f() {
        let t = fun_tokenizer();
        // 'f' prefixes "false"
        for name in &["Ref", "Def"] {
            let result = tok(&t, name);
            assert_eq!(
                result,
                vec![(name.to_string(), false)],
                "TypeName '{}' should tokenize as a single token",
                name
            );
        }
    }

    #[test]
    fn fun_typename_ending_in_i() {
        let t = fun_tokenizer();
        // 'i' prefixes "if"/"in"
        for name in &["Fi", "Pi"] {
            let result = tok(&t, name);
            assert_eq!(
                result,
                vec![(name.to_string(), false)],
                "TypeName '{}' should tokenize as a single token",
                name
            );
        }
    }

    #[test]
    fn fun_identifier_ending_in_keyword_prefix() {
        let t = fun_tokenizer();
        // Lowercase identifiers — note that some contain complete keywords:
        // "int" → ["in", "t"(partial)] because "in" is a keyword matched greedily
        // "iff" → ["if", "f"(partial)] because "if" is a keyword
        // These are NOT the prefix_special bug — they're match_special doing its job.
        // Only identifiers where no keyword is an exact prefix should stay whole.
        for name in &["nat", "set", "ref", "pi"] {
            let result = tok(&t, name);
            assert_eq!(
                result,
                vec![(name.to_string(), false)],
                "identifier '{}' should tokenize as a single token",
                name
            );
        }
    }

    #[test]
    fn fun_identifier_containing_keyword() {
        let t = fun_tokenizer();
        // "int" starts with keyword "in" → splits as ["in", "t"(partial)]
        let result = tok(&t, "int");
        assert_eq!(
            result,
            vec![
                ("in".into(), false),
                ("t".into(), true), // 't' prefixes "then"/"true"
            ]
        );
        // "iff" starts with keyword "if" → splits as ["if", "f"(partial)]
        let result = tok(&t, "iff");
        assert_eq!(
            result,
            vec![
                ("if".into(), false),
                ("f".into(), true), // 'f' prefixes "false"
            ]
        );
    }

    // --- Keyword specials still work correctly ---

    #[test]
    fn fun_keywords_match_exactly() {
        let t = fun_tokenizer();
        for kw in &["let", "in", "if", "then", "else", "true", "false"] {
            let result = tok(&t, kw);
            assert_eq!(
                result,
                vec![(kw.to_string(), false)],
                "keyword '{}' should tokenize as a single special token",
                kw
            );
        }
    }

    #[test]
    fn fun_keyword_followed_by_space_and_identifier() {
        let t = fun_tokenizer();
        assert_eq!(
            tok(&t, "let x"),
            vec![("let".into(), false), ("x".into(), false),]
        );
        assert_eq!(
            tok(&t, "if true then 1 else 2"),
            vec![
                ("if".into(), false),
                ("true".into(), false),
                ("then".into(), false),
                ("1".into(), false),
                ("else".into(), false),
                ("2".into(), false),
            ]
        );
    }

    // --- Partial specials still detected correctly ---

    #[test]
    fn fun_partial_arrow_after_typename() {
        let t = fun_tokenizer();
        // "Int-" — '-' is a prefix of '->', must split off as partial
        let result = tok(&t, "Int-");
        assert_eq!(
            result,
            vec![
                ("Int".into(), false),
                ("-".into(), true), // partial special
            ]
        );
    }

    #[test]
    fn fun_partial_arrow_after_identifier() {
        let t = fun_tokenizer();
        let result = tok(&t, "foo-");
        assert_eq!(result, vec![("foo".into(), false), ("-".into(), true),]);
    }

    #[test]
    fn fun_partial_keyword_at_end() {
        let t = fun_tokenizer();
        // "le" at end of input is a prefix of "let"
        let result = tok(&t, "le");
        assert_eq!(result, vec![("le".into(), true)]);
        // "tru" is a prefix of "true"
        let result = tok(&t, "tru");
        assert_eq!(result, vec![("tru".into(), true)]);
        // "th" is a prefix of "then"
        let result = tok(&t, "th");
        assert_eq!(result, vec![("th".into(), true)]);
    }

    // --- Full lambda/let/if expressions ---

    #[test]
    fn fun_full_lambda_expression() {
        let t = fun_tokenizer();
        let result = tok(&t, "λf:Int->Bool.f");
        // Note: trailing "f" at end of input is partial_special because
        // "f" is a prefix of "false". This is correct tokenizer behavior.
        assert_eq!(
            result,
            vec![
                ("λ".into(), false),
                ("f".into(), false), // not partial here: followed by ":"
                (":".into(), false),
                ("Int".into(), false),
                ("->".into(), false),
                ("Bool".into(), false),
                (".".into(), false),
                ("f".into(), true), // partial: "f" prefixes "false" at end of input
            ]
        );
    }

    #[test]
    fn fun_full_let_expression() {
        let t = fun_tokenizer();
        let result = tok(&t, "let x:Int=42 in x");
        assert_eq!(
            result,
            vec![
                ("let".into(), false),
                ("x".into(), false),
                (":".into(), false),
                ("Int".into(), false),
                ("=".into(), false),
                ("42".into(), false),
                ("in".into(), false),
                ("x".into(), false),
            ]
        );
    }

    #[test]
    fn fun_full_if_expression() {
        let t = fun_tokenizer();
        let result = tok(&t, "if true then 1 else 2");
        assert_eq!(
            result,
            vec![
                ("if".into(), false),
                ("true".into(), false),
                ("then".into(), false),
                ("1".into(), false),
                ("else".into(), false),
                ("2".into(), false),
            ]
        );
    }

    #[test]
    fn fun_nested_lambda_with_arrow_types() {
        let t = fun_tokenizer();
        let result = tok(&t, "λf:(Int->Int).λx:Int.f x");
        assert_eq!(
            result,
            vec![
                ("λ".into(), false),
                ("f".into(), false),
                (":".into(), false),
                ("(".into(), false),
                ("Int".into(), false),
                ("->".into(), false),
                ("Int".into(), false),
                (")".into(), false),
                (".".into(), false),
                ("λ".into(), false),
                ("x".into(), false),
                (":".into(), false),
                ("Int".into(), false),
                (".".into(), false),
                ("f".into(), false),
                ("x".into(), false),
            ]
        );
    }

    #[test]
    fn fun_partial_lambda_prefix_int() {
        let t = fun_tokenizer();
        // "λx:Int" at end of input — Int must stay whole, no partial
        let result = tok(&t, "λx:Int");
        assert_eq!(
            result,
            vec![
                ("λ".into(), false),
                ("x".into(), false),
                (":".into(), false),
                ("Int".into(), false),
            ]
        );
        // None of them should be partial specials
        let tokenizer_result = t.tokenize("λx:Int").unwrap();
        assert!(tokenizer_result.iter().all(|s| !s.is_partial_special));
    }

    // --- Edge: identifier that looks like a keyword prefix ---

    #[test]
    fn fun_identifier_is_keyword_prefix() {
        let t = fun_tokenizer();
        // "le" is a prefix of "let" — at end of input it's partial_special
        // But "le x" — "le" followed by space is still partial_special
        // because the outer loop catches it before accumulation
        let result = tok(&t, "le");
        assert_eq!(result[0].0, "le");
        assert!(result[0].1); // partial special
    }

    // --- Edge: numbers that end in keyword-prefix chars ---

    #[test]
    fn fun_number_token_not_split() {
        let t = fun_tokenizer();
        // Digits shouldn't be split even if they happen to end in keyword-prefix chars
        // (unlikely with digits, but verify the general mechanism)
        assert_eq!(tok(&t, "42"), vec![("42".into(), false)]);
        assert_eq!(tok(&t, "100"), vec![("100".into(), false)]);
    }
}
