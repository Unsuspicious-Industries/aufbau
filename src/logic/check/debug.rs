use crate::logic::ast::ASTNode;
use crate::logic::debug::DebugUtils;

/// Debugging utilities for the type checker
/// Now uses the global debug configuration instead of maintaining its own input
#[derive(Debug, Clone)]
pub struct TypeCheckerDebug;

impl TypeCheckerDebug {
    /// Create a new debug helper (no longer needs input parameter)
    pub fn new(_input: Option<String>) -> Self {
        Self
    }

    /// Format a span location for debugging
    pub fn format_span(&self, span: Option<&crate::logic::ast::SourceSpan>) -> String {
        DebugUtils::format_span(span)
    }

    /// Format an error with span information
    pub fn format_error(&self, node: &ASTNode, message: &str) -> String {
        DebugUtils::format_error(node, message)
    }

    /// Extract the actual text content from a node using its span
    pub fn extract_text(&self, node: &ASTNode) -> String {
        DebugUtils::extract_text(node)
    }

    /// Log debug information with span context
    pub fn debug_at_span(&self, node: &ASTNode, message: &str) {
        println!("DEBUG: {} at {}", message, self.format_span(node.span()));
    }
}
