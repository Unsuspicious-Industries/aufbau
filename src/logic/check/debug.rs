use crate::logic::ast::{ASTNode, SourceSpan};

/// Debugging utilities for the type checker
pub struct TypeCheckerDebug {
    /// Optional input string for debugging span information
    pub input: Option<String>,
}

impl TypeCheckerDebug {
    /// Create a new debug helper with optional input string
    pub fn new(input: Option<String>) -> Self {
        Self { input }
    }

    /// Format a span location for debugging
    pub fn format_span(&self, span: Option<&SourceSpan>) -> String {
        match (span, &self.input) {
            (Some(span), Some(input)) => {
                // Calculate line and column numbers properly
                let chars_before_start: Vec<char> = input.chars().take(span.start).collect();
                let start_line = chars_before_start.iter().filter(|&&c| c == '\n').count() + 1;
                let start_col = chars_before_start.iter().rev()
                    .take_while(|&&c| c != '\n')
                    .count() + 1;
                
                let chars_before_end: Vec<char> = input.chars().take(span.end).collect();
                let end_line = chars_before_end.iter().filter(|&&c| c == '\n').count() + 1;
                let end_col = chars_before_end.iter().rev()
                    .take_while(|&&c| c != '\n')
                    .count() + 1;
                
                let snippet = self.extract_span_text(span, input);
                
                format!("line {}:{}-{}:{} '{}'", start_line, start_col, end_line, end_col, snippet)
            }
            (Some(span), None) => {
                format!("span {}..{}", span.start, span.end)
            }
            (None, _) => "no span".to_string(),
        }
    }

    /// Extract text from a span, handling UTF-8 character boundaries properly
    fn extract_span_text(&self, span: &SourceSpan, input: &str) -> String {
        // For simplicity, let's assume spans are byte indices and handle them safely
        if span.start <= input.len() && span.end <= input.len() && span.start <= span.end {
            // Try to extract the text, handling potential UTF-8 boundary issues
            match input.get(span.start..span.end) {
                Some(text) => {
                    // Escape special characters for display
                    text.replace('\n', "\\n").replace('\t', "\\t")
                }
                None => {
                    format!("<invalid UTF-8 span: {}..{}>", span.start, span.end)
                }
            }
        } else {
            format!("<invalid span: {}..{}>", span.start, span.end)
        }
    }

    /// Format an error with span information
    pub fn format_error(&self, node: &ASTNode, message: &str) -> String {
        let span_info = self.format_span(node.span());
        format!("{} at {}", message, span_info)
    }

    /// Extract the actual text content from a node using its span
    pub fn extract_text(&self, node: &ASTNode) -> String {
        match (node.span(), &self.input) {
            (Some(span), Some(input)) => {
                self.extract_span_text(span, input)
            }
            (Some(span), None) => {
                format!("<span {}..{}, no input>", span.start, span.end)
            }
            (None, _) => {
                format!("<{}>", node.value()) // fallback to node type name
            }
        }
    }

    /// Log debug information with span context
    pub fn debug_at_span(&self, node: &ASTNode, message: &str) {
        println!("DEBUG: {} at {}", message, self.format_span(node.span()));
    }
}
