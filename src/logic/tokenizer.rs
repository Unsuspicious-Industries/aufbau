use crate::{debug_trace, regex::{PrefixStatus, Regex as DerivativeRegex}};

/// A tokenized segment of input with text and position information
/// Uses byte-based positions and storage to avoid Unicode issues
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
}

impl Segment {
    /// Create a new segment from bytes and byte positions
    pub fn new(bytes: Vec<u8>, start: usize, end: usize) -> Self {
        Self {
            bytes,
            start,
            end,
            index: 0,
        }
    }

    /// Create a new segment with an index
    pub fn with_index(bytes: Vec<u8>, start: usize, end: usize, index: usize) -> Self {
        Self {
            bytes,
            start,
            end,
            index,
        }
    }

    /// Create a segment from a string slice and byte positions
    pub fn from_str(text: &str, start: usize, end: usize) -> Self {
        Self {
            bytes: text.as_bytes().to_vec(),
            start,
            end,
            index: 0,
        }
    }

    /// Get the text as a UTF-8 string
    /// Returns empty string if bytes are not valid UTF-8
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

pub struct Tokenizer {
    special_tokens: Vec<Vec<u8>>,
    delimiters: Vec<u8>,
    /// Optional regex to validate that tokens are accepted by the grammar
    validation_regex: Option<DerivativeRegex>,
}

impl Tokenizer {
    /// Create a tokenizer from string-based tokens and char delimiters
    /// Converts strings and chars to their byte representations
    pub fn new(
        special_tokens: Vec<String>,
        delimiters: Vec<char>,
        validation_regex: Option<DerivativeRegex>,
    ) -> Self {
        let special_tokens_bytes = special_tokens
            .into_iter()
            .map(|s| s.into_bytes())
            .collect();
        
        // Convert chars to their UTF-8 byte representation
        // For ASCII chars (0-127), this will be a single byte
        let mut delimiter_bytes = Vec::new();
        for ch in delimiters {
            let mut buf = [0u8; 4];
            let bytes = ch.encode_utf8(&mut buf).as_bytes();
            // Only support single-byte delimiters for simplicity
            if bytes.len() == 1 {
                delimiter_bytes.push(bytes[0]);
            }
            debug_trace!("tokenizer","Delimiter '{}' encoded to bytes: {:?}", ch, bytes);
        }
        
        Self {
            special_tokens: special_tokens_bytes,
            delimiters: delimiter_bytes,
            validation_regex,
        }
    }

    /// Create a tokenizer directly from byte-based tokens and delimiters
    pub fn from_bytes(
        special_tokens: Vec<Vec<u8>>,
        delimiters: Vec<u8>,
        validation_regex: Option<DerivativeRegex>,
    ) -> Self {
        Self {
            special_tokens,
            delimiters,
            validation_regex,
        }
    }

    /// Tokenize the input string and return segments with byte-based spans
    pub fn tokenize(&self, input: &str) -> Result<Vec<Segment>, String> {
        let mut segments = Vec::new();
        let bytes = input.as_bytes();
        let mut byte_pos = 0;

        while byte_pos < bytes.len() {
            // Try to match a special token at the current position
            let mut matched: Option<(&[u8], usize)> = None; // (token_bytes, byte_len)
            for special in &self.special_tokens {
                let special_bytes = special.as_slice();
                if byte_pos + special_bytes.len() <= bytes.len()
                    && &bytes[byte_pos..byte_pos + special_bytes.len()] == special_bytes
                {
                    matched = Some((special_bytes, special_bytes.len()));
                    break;
                }
            }
            if let Some((tok_bytes, byte_len)) = matched {
                let index = segments.len();
                segments.push(Segment::with_index(
                    tok_bytes.to_vec(),
                    byte_pos,
                    byte_pos + byte_len,
                    index,
                ));
                byte_pos += byte_len;
                continue;
            }

            // Check if current byte is a delimiter (pure byte comparison)
            if self.delimiters.contains(&bytes[byte_pos]) {
                byte_pos += 1;
                continue;
            }

            // Otherwise, accumulate a normal token
            let start_pos = byte_pos;
            let mut token_bytes = Vec::new();

            while byte_pos < bytes.len() {
                // Check if current byte is a delimiter
                if self.delimiters.contains(&bytes[byte_pos]) {
                    break;
                }

                // Check if a special token starts here
                let special_starts_here = self.special_tokens.iter().any(|s| {
                    let special_bytes = s.as_slice();
                    byte_pos + special_bytes.len() <= bytes.len()
                        && &bytes[byte_pos..byte_pos + special_bytes.len()] == special_bytes
                });
                if special_starts_here {
                    break;
                }

                // Add this byte to the current token
                token_bytes.push(bytes[byte_pos]);
                byte_pos += 1;
            }

            if !token_bytes.is_empty() {
                let index = segments.len();
                let segment = Segment::with_index(token_bytes, start_pos, byte_pos, index);

                // Validate token if validation regex is provided
                if let Some(ref validation_regex) = self.validation_regex {
                    let token_text = segment.text();
                    debug_trace!("tokenizer","Validating token: '{}' against regex: {:?}", token_text, validation_regex.to_pattern());
                    if matches!(
                        validation_regex.prefix_match(&token_text),
                        PrefixStatus::NoMatch
                    ) {
                        return Err(format!("Failed input validation: '{}'", token_text));
                    }
                }

                segments.push(segment);
            }
        }

        Ok(segments)
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;

    #[test]
    fn test_tokenize_with_special_tokens() {
        let input = "x=r+4;print(x)";
        let special_tokens = vec!["+".to_string(), "=".to_string()];
        let delimiters = vec![';', '(', ')'];
        let tokenizer = Tokenizer::new(special_tokens.clone(), delimiters, None);

        let segments = tokenizer.tokenize(input).unwrap();
        let token_strs: Vec<_> = segments.iter().map(|seg| seg.text()).collect();

        assert_eq!(token_strs, vec!["x", "=", "r", "+", "4", "print", "x"]);
    }

    #[test]
    fn test_tokenize_with_spans_positions() {
        let input = "int x = 5;";
        let special_tokens = vec!["int".to_string(), "=".to_string(), ";".to_string()];
        let delimiters = vec![' ', '\t', '\n'];
        let tokenizer = Tokenizer::new(special_tokens.clone(), delimiters, None);
        let segments = tokenizer.tokenize(input).unwrap();

        let strs: Vec<_> = segments.iter().map(|seg| seg.text()).collect();
        assert_eq!(strs, vec!["int", "x", "=", "5", ";"]);

        // Check spans map to substrings using byte positions
        let pieces: Vec<_> = segments
            .iter()
            .map(|seg| &input[seg.start..seg.end])
            .collect();
        assert_eq!(pieces, vec!["int", "x", "=", "5", ";"]);

        // Verify byte positions
        assert_eq!(segments[0].start, 0); // "int" starts at 0
        assert_eq!(segments[0].end, 3); // "int" ends at 3
        assert_eq!(segments[1].start, 4); // "x" starts at 4
        assert_eq!(segments[1].end, 5); // "x" ends at 5
    }

    #[test]
    fn test_byte_based_tokenization() {
        // Test with multi-byte special tokens and single-byte delimiters
        let input = "foo->bar+baz";
        let special_tokens = vec!["->".to_string(), "+".to_string()];
        let delimiters = vec![' '];
        let tokenizer = Tokenizer::new(special_tokens, delimiters, None);
        
        let segments = tokenizer.tokenize(input).unwrap();
        let tokens: Vec<_> = segments.iter().map(|s| s.text()).collect();
        
        assert_eq!(tokens, vec!["foo", "->", "bar", "+", "baz"]);
        
        // Verify all segments use byte-based storage
        for seg in &segments {
            assert_eq!(seg.bytes().len(), seg.end - seg.start);
        }
    }

    #[test]
    fn test_direct_byte_api() {
        // Test the from_bytes API directly
        let input = "a::b";
        let special_tokens = vec![b"::".to_vec()];
        let delimiters = vec![b' '];
        let tokenizer = Tokenizer::from_bytes(special_tokens, delimiters, None);
        
        let segments = tokenizer.tokenize(input).unwrap();
        let tokens: Vec<_> = segments.iter().map(|s| s.text()).collect();
        
        assert_eq!(tokens, vec!["a", "::", "b"]);
    }
}
