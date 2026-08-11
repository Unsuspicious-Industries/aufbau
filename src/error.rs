//! Shared error objects for logic-layer subsystems.

/// Prefix-level parse failure carrying the consumed input length.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PrefixError {
    pub input_len: usize,
    pub message: String,
}

impl PrefixError {
    pub fn rejected(input_len: usize, message: impl Into<String>) -> Self {
        Self {
            input_len,
            message: message.into(),
        }
    }
}

impl std::fmt::Display for PrefixError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} at input length {}", self.message, self.input_len)
    }
}

impl std::error::Error for PrefixError {}

/// What went unresolvable during a typing-rule descent: the obligation value
/// (the runtime never learned what text the bound child parsed to) or the type
/// expression (the grammar's structural parse of the extension type failed).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UnresolvedKind {
    Value,
    Type,
}

impl std::fmt::Display for UnresolvedKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnresolvedKind::Value => write!(f, "value"),
            UnresolvedKind::Type => write!(f, "type"),
        }
    }
}

/// Generic rejection of a semantic transition.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TransitionError {
    Rejected,
    /// A setting extension in a typing premise could not be resolved while
    /// descending into a child. The rule name and the binding being descended
    /// into are diagnostic; the setting name is the offending extension; the
    /// kind names which side failed (value or type).
    Unresolved {
        rule: String,
        binding: String,
        setting: String,
        kind: UnresolvedKind,
    },
}

impl std::fmt::Display for TransitionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TransitionError::Rejected => write!(f, "transition rejected"),
            TransitionError::Unresolved { rule, binding, setting, kind } => write!(
                f,
                "in rule '{rule}', descending into '{binding}': setting extension '{setting}' has no resolvable {kind}"
            ),
        }
    }
}

impl std::error::Error for TransitionError {}

pub type TransitionResult<T> = Result<T, TransitionError>;
