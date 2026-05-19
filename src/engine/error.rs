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

/// Generic rejection of a semantic transition.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TransitionError {
    Rejected,
}

pub type TransitionResult<T> = Result<T, TransitionError>;
