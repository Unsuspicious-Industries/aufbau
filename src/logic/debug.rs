use std::fmt::{self, Display};
use crate::logic::ast::{ASTNode, SourceSpan};
use crate::logic::typing::Type;

/// Debug level for controlling output verbosity
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum DebugLevel {
    /// No debug output
    None = 0,
    /// Only errors and critical issues
    Error = 1,
    /// Warnings and important events
    Warn = 2,
    /// General information
    Info = 3,
    /// Detailed debugging information
    Debug = 4,
    /// Very verbose tracing
    Trace = 5,
}

impl Display for DebugLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DebugLevel::None => write!(f, "NONE"),
            DebugLevel::Error => write!(f, "ERROR"),
            DebugLevel::Warn => write!(f, "WARN"),
            DebugLevel::Info => write!(f, "INFO"),
            DebugLevel::Debug => write!(f, "DEBUG"),
            DebugLevel::Trace => write!(f, "TRACE"),
        }
    }
}

/// Global debug configuration
pub struct DebugConfig {
    pub level: DebugLevel,
    pub input: Option<String>,
    pub module_filters: Vec<String>,
}

impl Default for DebugConfig {
    fn default() -> Self {
        Self {
            level: DebugLevel::None,
            input: None,
            module_filters: Vec::new(),
        }
    }
}

// Thread-local debug configuration
thread_local! {
    static DEBUG_CONFIG: std::cell::RefCell<DebugConfig> = std::cell::RefCell::new(DebugConfig::default());
}

/// Set the global debug level
pub fn set_debug_level(level: DebugLevel) {
    DEBUG_CONFIG.with(|config| {
        config.borrow_mut().level = level;
    });
}

/// Set the input string for span debugging
pub fn set_debug_input(input: Option<String>) {
    DEBUG_CONFIG.with(|config| {
        config.borrow_mut().input = input;
    });
}

/// Add a module filter (only these modules will output debug info)
pub fn add_module_filter(module: &str) {
    DEBUG_CONFIG.with(|config| {
        config.borrow_mut().module_filters.push(module.to_string());
    });
}

/// Clear all module filters
pub fn clear_module_filters() {
    DEBUG_CONFIG.with(|config| {
        config.borrow_mut().module_filters.clear();
    });
}

/// Check if debugging is enabled for a specific level and module
pub fn is_debug_enabled(level: DebugLevel, module: &str) -> bool {
    DEBUG_CONFIG.with(|config| {
        let config = config.borrow();
        if config.level < level {
            return false;
        }
        if config.module_filters.is_empty() {
            return true;
        }
        config.module_filters.iter().any(|filter| module.contains(filter))
    })
}

#[macro_export]
/// Unified debug macro for all modules
macro_rules! debug {
    ($level:expr, $module:expr, $($arg:tt)*) => {
        if $crate::logic::debug::is_debug_enabled($level, $module) {
            println!("[{}:{}] {}", $level, $module, format!($($arg)*));
        }
    };
}

#[macro_export]
/// Convenience macros for different debug levels
macro_rules! debug_error {
    ($module:expr, $($arg:tt)*) => {
        $crate::debug!($crate::logic::debug::DebugLevel::Error, $module, $($arg)*);
    };
}

#[macro_export]
macro_rules! debug_warn {
    ($module:expr, $($arg:tt)*) => {
        $crate::debug!($crate::logic::debug::DebugLevel::Warn, $module, $($arg)*);
    };
}

#[macro_export]
macro_rules! debug_info {
    ($module:expr, $($arg:tt)*) => {
        $crate::debug!($crate::logic::debug::DebugLevel::Info, $module, $($arg)*);
    };
}

#[macro_export]
macro_rules! debug_debug {
    ($module:expr, $($arg:tt)*) => {
        $crate::debug!($crate::logic::debug::DebugLevel::Debug, $module, $($arg)*);
    };
}

#[macro_export]
macro_rules! debug_trace {
    ($module:expr, $($arg:tt)*) => {
        $crate::debug!($crate::logic::debug::DebugLevel::Trace, $module, $($arg)*);
    };
}

/// Debug utilities for working with AST nodes and spans
pub struct DebugUtils;

impl DebugUtils {
    /// Format a span location for debugging
    pub fn format_span(span: Option<&SourceSpan>) -> String {
        DEBUG_CONFIG.with(|config| {
            let config = config.borrow();
            match (span, &config.input) {
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
                    
                    let snippet = Self::extract_span_text(span, input);
                    
                    format!("line {}:{}-{}:{} '{}'", start_line, start_col, end_line, end_col, snippet)
                }
                (Some(span), None) => {
                    format!("span {}..{}", span.start, span.end)
                }
                (None, _) => "no span".to_string(),
            }
        })
    }

    /// Extract text from a span, handling UTF-8 character boundaries properly
    fn extract_span_text(span: &SourceSpan, input: &str) -> String {
        if span.start <= input.len() && span.end <= input.len() && span.start <= span.end {
            match input.get(span.start..span.end) {
                Some(text) => {
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
    pub fn format_error(node: &ASTNode, message: &str) -> String {
        let span_info = Self::format_span(node.span());
        format!("{} at {}", message, span_info)
    }

    /// Extract the actual text content from a node using its span
    pub fn extract_text(node: &ASTNode) -> String {
        DEBUG_CONFIG.with(|config| {
            let config = config.borrow();
            match (node.span(), &config.input) {
                (Some(span), Some(input)) => {
                    Self::extract_span_text(span, input)
                }
                (Some(span), None) => {
                    format!("<span {}..{}, no input>", span.start, span.end)
                }
                (None, _) => {
                    format!("<{}>", node.value()) // fallback to node type name
                }
            }
        })
    }

    /// Get a compact representation of a node for debugging
    pub fn node_summary(node: &ASTNode) -> String {
        match node {
            ASTNode::Terminal(t) => format!("T({})", t.value),
            ASTNode::Nonterminal(nt) => format!("NT({})", nt.value),
        }
    }



    /// Pretty print an AST node tree with indentation
    pub fn pretty_print_ast(node: &ASTNode, indent: usize) -> String {
        let indent_str = "  ".repeat(indent);
        match node {
            ASTNode::Terminal(t) => {
                format!("{}T: {}", indent_str, t.value)
            }
            ASTNode::Nonterminal(nt) => {
                let mut result = format!("{}NT: {}", indent_str, nt.value);
                for child in &nt.children {
                    result.push('\n');
                    result.push_str(&Self::pretty_print_ast(child, indent + 1));
                }
                result
            }
        }
    }
}

/// Debug context for tracking state during operations
pub struct DebugContext {
    pub operation: String,
    pub data: std::collections::HashMap<String, String>,
}

impl DebugContext {
    pub fn new(operation: &str) -> Self {
        Self {
            operation: operation.to_string(),
            data: std::collections::HashMap::new(),
        }
    }

    pub fn add(&mut self, key: &str, value: &str) {
        self.data.insert(key.to_string(), value.to_string());
    }

    pub fn debug_dump(&self, level: DebugLevel, module: &str) {
        if is_debug_enabled(level, module) {
            println!("[{}:{}] === {} ===", level, module, self.operation);
            for (key, value) in &self.data {
                println!("[{}:{}]   {}: {}", level, module, key, value);
            }
            println!("[{}:{}] === END {} ===", level, module, self.operation);
        }
    }
}
