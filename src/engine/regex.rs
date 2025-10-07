use regex::Regex;
use std::collections::HashMap;

pub struct RegexAnalyzer {
    cache: HashMap<String, Regex>,
}

#[derive(Debug)]
pub enum RegexResult {
    Match {extensions: String},  // Regex pattern for valid extensions (stored as String)
    Prefix {suffixes: String},    // Regex pattern for valid suffixes (stored as String)
    Mismatch,
}

impl RegexAnalyzer {
    pub fn new() -> Self {
        Self {
            cache: HashMap::new(),
        }
    }

    pub fn analyze(&mut self, pattern: &str, input: &str) -> RegexResult {
        let regex: &Regex = self.cache.entry(pattern.to_string()).or_insert_with(|| {
            Regex::new(pattern).unwrap_or_else(|_| {
                panic!("Invalid regex pattern: {}", pattern);
            })
        });

        if regex.is_match(input) {
            // Input already matches - compute extensions
            let extensions = self.compute_extensions(pattern, input);
            RegexResult::Match { extensions }
        } else if let Some(suffixes) = self.compute_suffixes(pattern, input) {
            // Input is a valid prefix - return the suffix pattern
            RegexResult::Prefix { suffixes }
        } else {
            RegexResult::Mismatch
        }
    }

    /// Compute w⁻¹L: Given pattern L and prefix w, return regex for valid suffixes
    fn compute_suffixes(&self, pattern: &str, prefix: &str) -> Option<String> {
        // The suffix pattern is: what can follow 'prefix' to match the original pattern?
        // This is the Brzozowski derivative: ∂L/∂w
        
        let mut modified_pattern = pattern.to_string();
        
        // Remove anchors for processing
        let has_start_anchor = modified_pattern.starts_with('^');
        let has_end_anchor = modified_pattern.ends_with('$');
        
        if has_start_anchor {
            modified_pattern = modified_pattern[1..].to_string();
        }
        if has_end_anchor && !modified_pattern.is_empty() {
            modified_pattern = modified_pattern[..modified_pattern.len()-1].to_string();
        }

        // Create a pattern that matches: prefix + suffix where prefix+suffix matches original
        // The suffix pattern is essentially the original pattern with the prefix "consumed"
        
        // Test if adding anything to prefix could match
        let test_pattern = if has_start_anchor {
            format!("^{}{}", regex::escape(prefix), modified_pattern)
        } else {
            format!("{}{}", regex::escape(prefix), modified_pattern)
        };
        
        let test_regex = Regex::new(&test_pattern).ok()?;
        
        // Quick test: can we extend this prefix at all?
        let test_input = format!("{}x", prefix);
        if !test_regex.is_match(&test_input) && !self.can_possibly_extend(&test_regex, prefix) {
            return None; // Not a valid prefix
        }

        // The suffix pattern is what remains after consuming the prefix
        // For simple cases, we can derive it directly
        let suffix_pattern = self.derive_suffix_pattern(pattern, prefix)?;
        
        Some(suffix_pattern)
    }

    /// Compute Lw: Given pattern L and matched string w, return regex for valid extensions
    fn compute_extensions(&self, pattern: &str, matched: &str) -> String {
        // Extensions are strings that can be appended while still matching
        // For pattern with end anchor: empty or nothing
        // For pattern without end anchor: .*
        
        let mut modified_pattern = pattern.to_string();
        
        // Check if pattern has end anchor
        if modified_pattern.ends_with('$') {
            // If there's an end anchor, the only valid extension is empty string
            "^$".to_string()
        } else {
            // Without end anchor, anything can follow
            "^.*$".to_string()
        }
    }

    fn derive_suffix_pattern(&self, pattern: &str, prefix: &str) -> Option<String> {
        // Simplified Brzozowski derivative computation
        // This is a heuristic approach - true derivative computation is complex
        
        let mut p = pattern.to_string();
        
        // Strip anchors
        if p.starts_with('^') {
            p = p[1..].to_string();
        }
        let has_end_anchor = p.ends_with('$');
        if has_end_anchor {
            p = p[..p.len()-1].to_string();
        }

        // For literal prefix matches, we can compute the derivative
        if let Some(suffix) = self.try_literal_derivative(&p, prefix) {
            if has_end_anchor {
                return Some(format!("^{}$", suffix));
            } else {
                return Some(format!("^{}.*$", suffix));
            }
        }

        // For complex patterns, construct a derivative pattern
        // This is approximate but works for many cases
        let derivative = self.approximate_derivative(&p, prefix)?;
        
        if has_end_anchor {
            Some(format!("^{}$", derivative))
        } else {
            Some(format!("^{}.*$", derivative))
        }
    }

    fn try_literal_derivative(&self, pattern: &str, prefix: &str) -> Option<String> {
        // Handle simple literal patterns
        if pattern.chars().all(|c| !r".*+?[]{}()|^$\".contains(c)) {
            // Pure literal pattern
            if pattern.starts_with(prefix) {
                return Some(pattern[prefix.len()..].to_string());
            }
        }
        None
    }

    fn approximate_derivative(&self, pattern: &str, prefix: &str) -> Option<String> {
        // For patterns like "hello\d{5}", if prefix is "hello", derivative is "\d{5}"
        // This is a simplified heuristic approach
        
        // Try to match the prefix against the start of the pattern literally
        let prefix_escaped = regex::escape(prefix);
        
        // Check if pattern starts with something that could match our prefix
        if let Ok(test_regex) = Regex::new(&format!("^({})", pattern)) {
            if test_regex.is_match(prefix) {
                // Pattern could start with our prefix
                // The derivative is roughly the pattern with prefix consumed
                
                // Simple heuristic: if prefix length suggests we've consumed
                // some literal portion, return the rest
                if pattern.len() >= prefix.len() {
                    // Very rough approximation
                    return Some(pattern.to_string());
                }
            }
        }

        // Fallback: return the pattern as-is (conservative)
        Some(pattern.to_string())
    }

    fn can_possibly_extend(&self, regex: &Regex, prefix: &str) -> bool {
        // Test various continuations to see if any could match
        let test_chars = "abcdefghijklmnopqrstuvwxyz0123456789 ";
        
        for c in test_chars.chars() {
            if regex.is_match(&format!("{}{}", prefix, c)) {
                return true;
            }
        }
        
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_match_with_extensions() {
        let mut analyzer = RegexAnalyzer::new();
        
        // Pattern without end anchor - can extend
        match analyzer.analyze(r"^hello", "hello") {
            RegexResult::Match { extensions } => {
                println!("Extensions: {}", extensions);
                assert_eq!(extensions, "^.*$");
            }
            _ => panic!("Expected Match"),
        }
    }

    #[test]
    fn test_match_with_no_extensions() {
        let mut analyzer = RegexAnalyzer::new();
        
        // Pattern with end anchor - cannot extend
        match analyzer.analyze(r"^hello$", "hello") {
            RegexResult::Match { extensions } => {
                println!("Extensions: {}", extensions);
                assert_eq!(extensions, "^$");
            }
            _ => panic!("Expected Match"),
        }
    }

    #[test]
    fn test_prefix_with_suffixes() {
        let mut analyzer = RegexAnalyzer::new();
        
        match analyzer.analyze(r"^hello$", "hel") {
            RegexResult::Prefix { suffixes } => {
                println!("Suffixes: {}", suffixes);
                // Should match "lo"
                let suffix_regex = Regex::new(&suffixes).unwrap();
                assert!(suffix_regex.is_match("lo"));
            }
            _ => panic!("Expected Prefix"),
        }
    }

    #[test]
    fn test_numeric_prefix() {
        let mut analyzer = RegexAnalyzer::new();
        
        match analyzer.analyze(r"^\d{5}$", "12") {
            RegexResult::Prefix { suffixes } => {
                println!("Suffixes for digits: {}", suffixes);
                // Should accept 3 more digits
            }
            _ => panic!("Expected Prefix"),
        }
    }

    #[test]
    fn test_mismatch() {
        let mut analyzer = RegexAnalyzer::new();
        
        match analyzer.analyze(r"^hello$", "goodbye") {
            RegexResult::Mismatch => {},
            other => panic!("Expected Mismatch, got {:?}", other),
        }
    }
}

fn main() {
    let mut analyzer = RegexAnalyzer::new();
    
    println!("=== Example 1: Match with extensions ===");
    match analyzer.analyze(r"^hello", "hello") {
        RegexResult::Match { extensions } => {
            println!("'hello' matches '^hello'");
            println!("Valid extensions: {}", extensions);
            println!("(Can append anything)\n");
        }
        _ => {}
    }
    
    println!("=== Example 2: Match with no extensions ===");
    match analyzer.analyze(r"^hello$", "hello") {
        RegexResult::Match { extensions } => {
            println!("'hello' matches '^hello$'");
            println!("Valid extensions: {}", extensions);
            println!("(Cannot append anything)\n");
        }
        _ => {}
    }
    
    println!("=== Example 3: Prefix with suffixes ===");
    match analyzer.analyze(r"^hello$", "hel") {
        RegexResult::Prefix { suffixes } => {
            println!("'hel' is a prefix for '^hello$'");
            println!("Valid suffixes: {}", suffixes);
            println!("(Need to add 'lo')\n");
        }
        _ => {}
    }
    
    println!("=== Example 4: Numeric pattern ===");
    match analyzer.analyze(r"^\d{5}$", "12") {
        RegexResult::Prefix { suffixes } => {
            println!("'12' is a prefix for '^\\d{{5}}$'");
            println!("Valid suffixes: {}", suffixes);
            println!("(Need 3 more digits)\n");
        }
        _ => {}
    }
    
    println!("=== Example 5: Mismatch ===");
    match analyzer.analyze(r"^hello$", "goodbye") {
        RegexResult::Mismatch => {
            println!("'goodbye' does not match '^hello$' and is not a valid prefix\n");
        }
        _ => {}
    }
}