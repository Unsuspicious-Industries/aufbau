// Removing legacy OG bound over complete AST; the system is centered on PartialAST now
// pub mod rule; // removed
pub mod partial;
pub mod typing;
pub mod utils;
pub use typing::BoundType;
mod display;

#[cfg(test)]
mod tests;

// Re-export the main types and traits for easy access
// Legacy exports removed

pub use utils::{bind_type, extract_terminal_value, get_nt_binding, get_var_binding};

// Re-export partial binding API (now canonical)
pub use partial::{
    BindablePartialNonTerminal, BindingResolver, BoundConclusion, BoundConclusionContext,
    BoundConclusionKind, BoundPremise, BoundTypeAscription, BoundTypeSetting, BoundTypingJudgment,
    BoundTypingRule, DefaultBindingResolver, bind_type_partial,
    collect_nt_bindings_same_level_partial, get_nt_binding_partial, get_var_binding_partial,
};
