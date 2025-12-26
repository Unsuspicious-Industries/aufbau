use crate::logic::grammar::Segment;
use crate::logic::segment::SegmentRange;

use std::fmt::{self, Display};

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
        config
            .module_filters
            .iter()
            .any(|filter| module.contains(filter))
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
        $crate::debug!($crate::logic::debug::DebugLevel::Info, $module, $($arg)*)
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
    /// Format a segment range location for debugging
    pub fn format_span(span: Option<&SegmentRange>, segments: &[Segment]) -> String {
        match span {
            Some(range) => {
                let start_seg = segments.get(range.start);
                let end_seg = segments.get(range.end);

                match (start_seg, end_seg) {
                    (Some(start), Some(end)) => {
                        let start_byte = start.start;
                        let end_byte = end.end;
                        format!(
                            "segs[{}..{}] bytes[{}..{}]",
                            range.start, range.end, start_byte, end_byte
                        )
                    }
                    _ => format!(
                        "invalid segment range: segs[{}..{}]",
                        range.start, range.end
                    ),
                }
            }
            None => "no span".to_string(),
        }
    }

    /// Extract text from a segment range
    pub fn extract_span_text(span: &SegmentRange, segments: &[Segment]) -> String {
        let mut result = String::new();
        for idx in span.start..=span.end {
            if let Some(seg) = segments.get(idx) {
                result.push_str(&seg.text());
                if idx < span.end {
                    result.push(' '); // Add space between segments
                }
            }
        }
        result
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
