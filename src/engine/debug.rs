use std::fmt::{self, Display};

/// Debug level for controlling output verbosity
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum DebugLevel {
    None = 0,
    Error = 1,
    Warn = 2,
    Info = 3,
    Debug = 4,
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
    pub module_filters: Vec<String>,
}

impl Default for DebugConfig {
    fn default() -> Self {
        Self {
            level: DebugLevel::None,
            module_filters: Vec::new(),
        }
    }
}

thread_local! {
    static DEBUG_CONFIG: std::cell::RefCell<DebugConfig> = std::cell::RefCell::new(DebugConfig::default());
}

/// Set the global debug level
pub fn set_debug_level(level: DebugLevel) {
    DEBUG_CONFIG.with(|config| {
        config.borrow_mut().level = level;
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
#[must_use]
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
macro_rules! debug {
    ($level:expr, $module:expr, $($arg:tt)*) => {
        if $crate::engine::debug::is_debug_enabled($level, $module) {
            println!("[{}:{}] {}", $level, $module, format!($($arg)*));
        }
    };
}

#[macro_export]
macro_rules! debug_error {
    ($module:expr, $($arg:tt)*) => {
        $crate::debug!($crate::engine::debug::DebugLevel::Error, $module, $($arg)*);
    };
}

#[macro_export]
macro_rules! debug_warn {
    ($module:expr, $($arg:tt)*) => {
        $crate::debug!($crate::engine::debug::DebugLevel::Warn, $module, $($arg)*);
    };
}

#[macro_export]
macro_rules! debug_info {
    ($module:expr, $($arg:tt)*) => {
        $crate::debug!($crate::engine::debug::DebugLevel::Info, $module, $($arg)*)
    };
}

#[macro_export]
macro_rules! debug_debug {
    ($module:expr, $($arg:tt)*) => {
        $crate::debug!($crate::engine::debug::DebugLevel::Debug, $module, $($arg)*);
    };
}

#[macro_export]
macro_rules! debug_trace {
    ($module:expr, $($arg:tt)*) => {
        $crate::debug!($crate::engine::debug::DebugLevel::Trace, $module, $($arg)*);
    };
}
