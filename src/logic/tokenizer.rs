use crate::regex::{PrefixStatus, Regex as DerivativeRegex};

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
    special_tokens: Vec<String>,
    delimiters: Vec<char>,
    /// Optional regex to validate that tokens are accepted by the grammar
    validation_regex: Option<DerivativeRegex>,
}

impl Tokenizer {
    pub fn new(
        special_tokens: Vec<String>,
        delimiters: Vec<char>,
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
            let mut matched: Option<(&str, usize)> = None; // (token_text, byte_len)
            for special in &self.special_tokens {
                let special_bytes = special.as_bytes();
                if byte_pos + special_bytes.len() <= bytes.len()
                    && &bytes[byte_pos..byte_pos + special_bytes.len()] == special_bytes
                {
                    matched = Some((special.as_str(), special_bytes.len()));
                    break;
                }
            }
            if let Some((tok, byte_len)) = matched {
                let index = segments.len();
                segments.push(Segment::with_index(
                    tok.as_bytes().to_vec(),
                    byte_pos,
                    byte_pos + byte_len,
                    index,
                ));
                byte_pos += byte_len;
                continue;
            }

            // Check if current byte starts a delimiter character
            // We need to decode the UTF-8 character at this position
            if let Some(ch) = std::str::from_utf8(&bytes[byte_pos..])
                .ok()
                .and_then(|s| s.chars().next())
            {
                if self.delimiters.contains(&ch) {
                    byte_pos += ch.len_utf8();
                    continue;
                }
            }

            // Otherwise, accumulate a normal token
            let start_pos = byte_pos;
            let mut token_bytes = Vec::new();

            while byte_pos < bytes.len() {
                // Try to decode the next character
                if let Some(ch) = std::str::from_utf8(&bytes[byte_pos..])
                    .ok()
                    .and_then(|s| s.chars().next())
                {
                    // Check if it's a delimiter
                    if self.delimiters.contains(&ch) {
                        break;
                    }

                    // Check if a special token starts here
                    let special_starts_here = self.special_tokens.iter().any(|s| {
                        let special_bytes = s.as_bytes();
                        byte_pos + special_bytes.len() <= bytes.len()
                            && &bytes[byte_pos..byte_pos + special_bytes.len()] == special_bytes
                    });
                    if special_starts_here {
                        break;
                    }

                    // Add this character to the current token
                    let char_len = ch.len_utf8();
                    token_bytes.extend_from_slice(&bytes[byte_pos..byte_pos + char_len]);
                    byte_pos += char_len;
                } else {
                    // Invalid UTF-8, skip this byte
                    byte_pos += 1;
                    break;
                }
            }

            if !token_bytes.is_empty() {
                let index = segments.len();
                let segment = Segment::with_index(token_bytes, start_pos, byte_pos, index);

                // Validate token if validation regex is provided
                if let Some(ref validation_regex) = self.validation_regex {
                    let token_text = segment.text();
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
}
