//! Beam Search Configuration

/// Beam search configuration parameters
#[derive(Debug, Clone)]
pub struct BeamConfig {
    /// Number of states to keep at each depth level
    ///
    /// Lower values = faster but less thorough
    /// Higher values = slower but more comprehensive
    pub beam_width: usize,

    /// Maximum depth to explore
    pub max_depth: usize,

    /// Maximum total states to explore
    ///
    /// Set to `None` for unlimited exploration
    pub max_states: Option<usize>,

    /// Weight for completeness score in ranking (0.0 to 1.0)
    pub completeness_weight: f64,

    /// Weight for typing score in ranking (0.0 to 1.0)
    pub typing_weight: f64,

    /// Weight for simplicity (0.0 to 1.0)
    ///
    /// Prefers shorter paths when higher
    pub simplicity_weight: f64,
}

impl Default for BeamConfig {
    fn default() -> Self {
        Self {
            beam_width: 10,
            max_depth: 10,
            max_states: None,
            completeness_weight: 1.0,
            typing_weight: 1.0,
            simplicity_weight: 0.5,
        }
    }
}

impl BeamConfig {
    /// Create a fast configuration for quick completion
    ///
    /// Uses narrow beam and shallow depth
    pub fn fast() -> Self {
        Self {
            beam_width: 5,
            max_depth: 5,
            max_states: Some(1000),
            completeness_weight: 1.0,
            typing_weight: 0.5,
            simplicity_weight: 1.0,
        }
    }

    /// Create a balanced configuration
    ///
    /// Moderate beam and depth for good trade-off
    pub fn balanced() -> Self {
        Self::default()
    }

    /// Create a thorough configuration
    ///
    /// Wide beam and deep depth for comprehensive search
    pub fn thorough() -> Self {
        Self {
            beam_width: 20,
            max_depth: 15,
            max_states: Some(10000),
            completeness_weight: 0.5,
            typing_weight: 1.5,
            simplicity_weight: 0.3,
        }
    }
}
