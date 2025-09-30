use regex::Regex;
use std::collections::HashMap;

use super::rank::Ranker;
use crate::logic::grammar::Grammar;
use crate::logic::parser::Parser;
use crate::logic::partial::{CompletionSet, CompletionToken, PartialAST};

/// Context information for token building during synthesis
#[derive(Debug, Clone)]
struct TokenBuildingContext {
    /// Whether we're currently in the middle of building a token
    is_mid_token: bool,
    /// The current partial token being built (if any)
    current_partial_token: String,
    /// Expected completions for the current context
    expected_completions: Vec<CompletionToken>,
    /// Position in the code where current token started
    token_start_position: usize,
    /// Whether the current context allows delimiters
    allows_delimiters: bool,
}

/// The synthesizer: pure/functional w.r.t. state; holds only grammar and ranker
pub struct Synthesizer {
    pub(crate) grammar: Grammar,
    pub(crate) ranker: Box<dyn Ranker>,
    /// Cache of compiled regexes for performance
    regex_cache: HashMap<String, Regex>,
}

// I wanted to do an album with the sounds of the '50s
// The sounds of the '60s, of the '70s
// And then have a sound of the future
// And I said, "Wait a second, I know the synthesizer
// Why don't I use the synthesizer which is the sound of the future?"
impl Synthesizer {
    /// Parse input into a PartialAST with a fresh parser
    fn parse_partial(&self, input: &str) -> Result<PartialAST, String> {
        let mut parser = Parser::new(self.grammar.clone());
        parser.partial(input)
    }

    /// Attempt to append a token and return new code and AST if valid
    fn try_append(&self, code: &str, token: &str) -> Option<(String, PartialAST)> {
        let candidate = format!("{}{}", code, token);
        match self.parse_partial(&candidate) {
            Ok(ast) => Some((candidate, ast)),
            Err(_) => None,
        }
    }

    /// Log current parser state for debugging
    fn log_state(&self, step: usize, code: &str, ast: Option<&PartialAST>) {
        crate::debug_debug!(
            "synthesizer",
            "=== Step {} | code_len={} code='{}' ===",
            step,
            code.len(),
            code
        );
        if let Some(a) = ast {
            crate::debug_debug!(
                "synthesizer",
                "PartialAST present: complete={}",
                a.complete()
            );
        } else {
            crate::debug_debug!("synthesizer", "No PartialAST available");
        }
    }

    /// Analyze the current token building context based on code and AST state
    fn analyze_token_building_context(
        &self,
        code: &str,
        ast: &Option<PartialAST>,
    ) -> Option<TokenBuildingContext> {
        if code.is_empty() {
            return Some(TokenBuildingContext {
                is_mid_token: false,
                current_partial_token: String::new(),
                expected_completions: Vec::new(),
                token_start_position: 0,
                allows_delimiters: true,
            });
        }

        let last_char = code.chars().last().unwrap();

        // If the last character is a delimiter or whitespace, we're not mid-token
        if last_char.is_whitespace() || self.is_delimiter(&last_char.to_string()) {
            return Some(TokenBuildingContext {
                is_mid_token: false,
                current_partial_token: String::new(),
                expected_completions: Vec::new(),
                token_start_position: code.len(),
                allows_delimiters: true,
            });
        }

        // Find the start of the current token
        let suffix_len = code.len().min(50); // Look back up to 50 characters
        let suffix_start = code.len().saturating_sub(suffix_len);

        // Ensure we start at a character boundary
        let safe_suffix_start = (suffix_start..code.len())
            .find(|&i| code.is_char_boundary(i))
            .unwrap_or(code.len());

        let suffix = &code[safe_suffix_start..];

        let current_token_start = suffix
            .rfind(|c: char| c.is_whitespace() || self.is_delimiter(&c.to_string()))
            .map(|i| i + 1)
            .unwrap_or(0);

        let absolute_token_start = safe_suffix_start + current_token_start;
        let current_token = &suffix[current_token_start..];

        if !current_token.is_empty() {
            // We might be in the middle of a token - analyze what completions might apply
            let expected_completions = if let Some(current_ast) = ast {
                let completions = current_ast.completions(&self.grammar, 10);
                completions
                    .candidates
                    .into_iter()
                    .map(|c| c.token)
                    .collect()
            } else {
                Vec::new()
            };

            // Check if current token could match any regex patterns
            let is_regex_prefix = expected_completions.iter().any(|completion| {
                if let CompletionToken::Regex(pattern) = completion {
                    if let Ok(regex) = Regex::new(pattern) {
                        return self.is_valid_regex_prefix(current_token, &regex);
                    }
                }
                false
            });

            Some(TokenBuildingContext {
                is_mid_token: is_regex_prefix,
                current_partial_token: current_token.to_string(),
                expected_completions,
                token_start_position: absolute_token_start,
                allows_delimiters: !is_regex_prefix,
            })
        } else {
            Some(TokenBuildingContext {
                is_mid_token: false,
                current_partial_token: String::new(),
                expected_completions: Vec::new(),
                token_start_position: code.len(),
                allows_delimiters: true,
            })
        }
    }
    /// Enhanced filtering that uses comprehensive context analysis
    fn filter_proposals_with_context(
        &mut self,
        proposals: Vec<(String, f32)>,
        completions: &CompletionSet,
        code: &str,
        token_context: &Option<TokenBuildingContext>,
        step: usize,
    ) -> Vec<(String, f32, f32)> {
        let mut filtered: Vec<(String, f32, f32)> = Vec::new();

        let context = token_context;
        let is_mid_token = context.as_ref().map_or(false, |c| c.is_mid_token);
        let allows_delimiters = context.as_ref().map_or(true, |c| c.allows_delimiters);

        crate::debug_trace!(
            "synthesizer",
            "Step {}: Context-aware filtering - mid_token: {}, allows_delimiters: {}, code: '{}'",
            step,
            is_mid_token,
            allows_delimiters,
            code
        );

        for (token, score) in proposals {
            let completion_boost =
                self.calculate_completion_boost_with_context(&token, completions, token_context);

            // Enhanced filtering logic based on context
            let should_include = if is_mid_token {
                // When building a token, be more restrictive
                completion_boost >= 1.5 || self.could_continue_current_token(&token, token_context)
            } else {
                // When not building a token, allow delimiters even if they have no completion boost
                // This is crucial because delimiters are never in completions but should be allowed
                self.is_delimiter(&token) || completion_boost > 0.0
            };

            if should_include {
                // Ensure delimiters get at least a minimum boost when allowed
                let final_boost = if self.is_delimiter(&token) && !is_mid_token {
                    completion_boost.max(1.0) // Delimiters get at least 1.0 boost when not mid-token
                } else {
                    completion_boost.max(0.1) // Minimum boost to avoid zero
                };

                let boosted_score = score * final_boost;
                crate::debug_trace!(
                    "synthesizer",
                    "Step {}: ACCEPT token='{}' original={:.6} boost={:.2} final={:.6} mid_token={} allows_delim={}",
                    step,
                    token,
                    score,
                    final_boost,
                    boosted_score,
                    is_mid_token,
                    allows_delimiters
                );
                filtered.push((token, score, final_boost));
            } else {
                crate::debug_trace!(
                    "synthesizer",
                    "Step {}: FILTER token='{}' (boost={:.2}, mid_token={}, allows_delim={})",
                    step,
                    token,
                    completion_boost,
                    is_mid_token,
                    allows_delimiters
                );
            }
        }

        // Sort by boosted score
        filtered.sort_by(|a, b| {
            let score_a = a.1 * a.2;
            let score_b = b.1 * b.2;
            score_b.partial_cmp(&score_a).unwrap()
        });

        filtered
    }

    /// Check if a token could continue the current token being built
    fn could_continue_current_token(
        &self,
        token: &str,
        context: &Option<TokenBuildingContext>,
    ) -> bool {
        if let Some(ctx) = context {
            if ctx.is_mid_token {
                let combined = format!("{}{}", ctx.current_partial_token, token);
                // Check if the combined token would be valid for any expected regex patterns
                return ctx.expected_completions.iter().any(|completion| {
                    if let CompletionToken::Regex(pattern) = completion {
                        if let Ok(regex) = Regex::new(pattern) {
                            return regex.is_match(&combined)
                                || self.is_valid_regex_prefix(&combined, &regex);
                        }
                    }
                    false
                });
            }
        }
        false
    }

    /// Enhanced completion boost calculation with context awareness
    fn calculate_completion_boost_with_context(
        &mut self,
        token: &str,
        completions: &CompletionSet,
        context: &Option<TokenBuildingContext>,
    ) -> f32 {
        let mut best_boost = 0.0f32;

        // If we have context and are mid-token, prioritize token continuation
        if let Some(ctx) = context {
            if ctx.is_mid_token {
                if self.could_continue_current_token(token, context) {
                    best_boost = best_boost.max(2.5);
                }
                // Be less generous with delimiters when mid-token
                if self.is_delimiter(token) {
                    return 0.1; // Very low boost for delimiters mid-token
                }
            } else {
                // Not mid-token, normal delimiter handling
                if self.is_delimiter(token) {
                    return 1.0;
                }
            }
        } else {
            // No context, fall back to delimiter check
            if self.is_delimiter(token) {
                return 1.0;
            }
        }

        // Standard completion matching
        for candidate in &completions.candidates {
            match &candidate.token {
                CompletionToken::Literal(expected) => {
                    if token == expected {
                        best_boost = best_boost.max(3.0);
                    } else if expected.starts_with(token) {
                        best_boost = best_boost.max(2.5);
                    } else if token.starts_with(expected) {
                        best_boost = best_boost.max(2.0);
                    }
                }
                CompletionToken::Regex(pattern) => {
                    if self.token_matches_regex_pattern(token, pattern) {
                        best_boost = best_boost.max(1.8);
                    }
                }
            }
        }

        best_boost
    }

    /// Check if a token is a delimiter that should be allowed in most contexts.
    /// Delimiters are tokens that separate other tokens but are not part of the grammar.
    fn is_delimiter(&self, token: &str) -> bool {
        // Only check for actual delimiters (whitespace, separators)
        // Grammar special tokens are handled by completions, not here
        let delimiters = [" ", "\t", "\n", ",", ";"];
        delimiters.contains(&token)
    }

    /// Check if a token matches a regex pattern using proper regex compilation.
    /// Handles both full matches and prefix matches for completion support.
    fn token_matches_regex_pattern(&mut self, token: &str, pattern: &str) -> bool {
        // Get or compile the regex, caching for performance
        let regex = match self.regex_cache.get(pattern) {
            Some(r) => r,
            None => match Regex::new(pattern) {
                Ok(r) => {
                    self.regex_cache.insert(pattern.to_string(), r);
                    self.regex_cache.get(pattern).unwrap()
                }
                Err(_) => {
                    crate::debug_trace!(
                        "synthesizer",
                        "Failed to compile regex pattern: {}",
                        pattern
                    );
                    return false;
                }
            },
        };

        // First check for exact match
        if regex.is_match(token) {
            return true;
        }

        // For completion support, check if the token could be a prefix of a valid match
        // This is done by trying to match the pattern against progressively longer strings
        // starting with the token as a prefix
        self.is_valid_regex_prefix(token, regex)
    }

    /// Check if a token could be a valid prefix for the given regex pattern.
    /// This enables proper completion support for regex-based tokens.
    fn is_valid_regex_prefix(&self, token: &str, regex: &Regex) -> bool {
        // For common patterns, we can do some heuristic checks
        let pattern_str = regex.as_str();

        // Handle identifier patterns like [\p{L}][\p{L}\p{N}_...]*
        if pattern_str.contains("\\p{L}") {
            if token.is_empty() {
                return true; // Empty string can start an identifier
            }
            // Check if first char is a letter and rest are valid identifier chars
            let mut chars = token.chars();
            if let Some(first) = chars.next() {
                if !first.is_alphabetic() {
                    return false;
                }
                return chars.all(|c| c.is_alphanumeric() || c == '_' || "τ₁₂₃₄₅₆₇₈₉₀".contains(c));
            }
        }

        // Handle numeric patterns
        if pattern_str.contains("\\d") || pattern_str.contains("[0-9]") {
            return token.chars().all(|c| c.is_numeric());
        }

        // For other patterns, try a more general approach:
        // Generate test strings by extending the token and see if any match
        if token.len() <= 10 {
            // Avoid infinite loops on very long tokens
            // Try extending with common characters
            let test_chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_τ";
            for test_char in test_chars.chars().take(20) {
                // Limit test cases
                let test_string = format!("{}{}", token, test_char);
                if regex.is_match(&test_string) {
                    return true;
                }
            }
        }

        false
    }

    pub fn new(grammar_spec: &str, ranker: Box<dyn Ranker>) -> Result<Self, String> {
        let grammar = Grammar::load(grammar_spec)?;
        Ok(Self {
            grammar,
            ranker,
            regex_cache: HashMap::new(),
        })
    }

    /// The main synthesis function that handles everything in one place.
    /// This allows for better state management and mid-token processing.
    pub fn synthesize(
        &mut self,
        input: &str,
        k: i32,
        max_steps: usize,
        iterations: i32,
    ) -> Result<String, String> {
        let mut current_input = input.to_string();

        // Run multiple iterations if requested
        for iteration in 0..iterations {
            crate::debug_info!(
                "synthesizer",
                "=== Iteration {}/{} starting with input: '{}' ===",
                iteration + 1,
                iterations,
                current_input
            );

            current_input = self.synthesize_single_pass(&current_input, k, max_steps)?;

            crate::debug_info!(
                "synthesizer",
                "=== Iteration {}/{} completed with output: '{}' ===",
                iteration + 1,
                iterations,
                current_input
            );
        }

        Ok(current_input)
    }

    /// Legacy API compatibility
    pub fn run(&mut self, input: &str, k: i32) -> Result<String, String> {
        self.synthesize(input, k, 128, 1)
    }

    /// Legacy API compatibility
    pub fn run_with(&mut self, input: &str, k: i32, max_steps: usize) -> Result<String, String> {
        self.synthesize(input, k, max_steps, 1)
    }

    /// Main synthesis logic for a single pass with comprehensive state management.
    fn synthesize_single_pass(
        &mut self,
        input: &str,
        k: i32,
        max_steps: usize,
    ) -> Result<String, String> {
        crate::debug_info!(
            "synthesizer",
            "Running completion-guided synthesizer on input: '{}' (k={}, max_steps={})",
            input,
            k,
            max_steps
        );

        // Ensure grammar has a start symbol
        if self.grammar.start_nonterminal().is_none() {
            if let Some(first) = self.grammar.productions.keys().next().cloned() {
                self.grammar.set_start(first);
            }
        }

        // State variables for the synthesis process
        let mut code = input.to_string();
        let mut ast: Option<PartialAST> = None;
        let mut consecutive_failed_attempts = 0;
        let mut last_successful_code = code.clone();
        let mut token_building_context: Option<TokenBuildingContext>;

        // Initial parse attempt
        match self.parse_partial(&code) {
            Ok(initial_ast) => {
                ast = Some(initial_ast);
                crate::debug_debug!("synthesizer", "Initial parse successful");
            }
            Err(_) => {
                crate::debug_debug!("synthesizer", "Initial parse failed, starting without AST");
            }
        }

        for step in 0..max_steps {
            self.log_state(step, &code, ast.as_ref());

            // Update token building context based on current state
            token_building_context = self.analyze_token_building_context(&code, &ast);

            if let Some(context) = &token_building_context {
                crate::debug_trace!(
                    "synthesizer",
                    "Step {}: Token building context - mid_token: {}, current_token: '{}', expected_completions: {}",
                    step,
                    context.is_mid_token,
                    context.current_partial_token,
                    context.expected_completions.len()
                );
            }

            // Get grammar-aware completions
            let completions = self.get_completions(&ast, &code, k as usize)?;

            crate::debug_debug!(
                "synthesizer",
                "Step {}: Found {} completion candidates",
                step,
                completions.candidates.len()
            );

            // Get ranker proposals
            let mut proposals = match self.ranker.rank(&code) {
                Ok(p) => p,
                Err(err) => {
                    return Err(format!("ranker error: {}", err));
                }
            };
            proposals.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap());

            if proposals.is_empty() {
                crate::debug_info!("synthesizer", "No ranker proposals available, stopping");
                break;
            }

            // Filter and score proposals using advanced context-aware logic
            let filtered_proposals = self.filter_proposals_with_context(
                proposals,
                &completions,
                &code,
                &token_building_context,
                step,
            );

            if filtered_proposals.is_empty() {
                consecutive_failed_attempts += 1;
                crate::debug_info!(
                    "synthesizer",
                    "No valid proposals after filtering at step {} (consecutive failures: {})",
                    step,
                    consecutive_failed_attempts
                );

                // If we've failed too many times in a row, consider backing off or trying different strategies
                if consecutive_failed_attempts >= 5 {
                    crate::debug_info!(
                        "synthesizer",
                        "Too many consecutive failures, reverting to last successful state"
                    );
                    code = last_successful_code;
                    break;
                }
                continue;
            }

            crate::debug_debug!(
                "synthesizer",
                "Step {}: {} proposals after filtering",
                step,
                filtered_proposals.len()
            );

            // Try to advance using the filtered proposals
            let mut advanced = false;
            let mut tried_this_step: std::collections::HashSet<String> =
                std::collections::HashSet::new();

            for (tok, score, completion_boost) in filtered_proposals.into_iter() {
                if tried_this_step.contains(&tok) {
                    continue;
                }

                crate::debug_trace!(
                    "synthesizer",
                    "Step {}: TRY token='{}' score={:.6} boost={:.2}",
                    step,
                    tok,
                    score,
                    completion_boost
                );

                // Try to append the token
                if let Some((new_code, new_ast)) = self.try_append(&code, &tok) {
                    crate::debug_trace!(
                        "synthesizer",
                        "Step {}: SELECT token='{}' -> code='{}' (complete={})",
                        step,
                        tok,
                        new_code,
                        new_ast.complete()
                    );

                    // Update state
                    code = new_code;
                    ast = Some(new_ast);
                    last_successful_code = code.clone();
                    consecutive_failed_attempts = 0;
                    advanced = true;
                    break;
                } else {
                    crate::debug_trace!(
                        "synthesizer",
                        "Step {}: REJECT token='{}' (parser error)",
                        step,
                        tok
                    );
                    tried_this_step.insert(tok);
                }
            }

            if !advanced {
                consecutive_failed_attempts += 1;
                crate::debug_info!(
                    "synthesizer",
                    "No token led to progress at step {} (consecutive failures: {})",
                    step,
                    consecutive_failed_attempts
                );

                if consecutive_failed_attempts >= 3 {
                    crate::debug_info!(
                        "synthesizer",
                        "Multiple consecutive failures, returning current state"
                    );
                    break;
                }
            }
        }

        crate::debug_info!(
            "synthesizer",
            "Synthesis completed: final code length={}, complete={}",
            code.len(),
            ast.as_ref().map_or(false, |a| a.complete())
        );

        Ok(code)
    }

    /// Enhanced completion gathering with better error handling
    fn get_completions(
        &mut self,
        ast: &Option<PartialAST>,
        code: &str,
        k: usize,
    ) -> Result<CompletionSet, String> {
        if let Some(current_ast) = ast {
            return Ok(current_ast.completions(&self.grammar, k));
        } else {
            // If no valid AST, try to parse and get completions from any partial result
            match self.parse_partial(code) {
                Ok(partial_ast) => Ok(partial_ast.completions(&self.grammar, k)),
                Err(err) => Err(format!("failed to compute completions: {}", err)),
            }
        }
    }
}

// No saved state struct: synthesizer is kept functional/pure in operation
