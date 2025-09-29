// Removing legacy OG bound over complete AST; the system is centered on PartialAST now
// pub mod rule; // removed
pub mod utils;
pub mod typing;
pub mod partial;
pub use typing::BoundType;
mod display;

#[cfg(test)]
mod tests;

// Re-export the main types and traits for easy access
// Legacy exports removed

pub use utils::{
    bind_type,
    get_nt_binding, 
    get_var_binding,
    extract_terminal_value
};

// Re-export partial binding API (now canonical)
pub use partial::{
    BoundTypingRule,
    BoundPremise,
    BoundTypeSetting,
    BoundTypeAscription,
    BoundTypingJudgment,
    BoundConclusion,
    BoundConclusionKind,
    BoundConclusionContext,
    BindingResolver,
    DefaultBindingResolver,
    BindablePartialNonTerminal,
    bind_type_partial,
    get_nt_binding_partial,
    get_var_binding_partial,
    collect_nt_bindings_same_level_partial,
};
