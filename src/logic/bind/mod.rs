pub mod rule;
pub mod utils;
pub mod typing;
pub use typing::BoundType;
mod display;

#[cfg(test)]
mod tests;

// Re-export the main types and traits for easy access
pub use rule::{
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
    BindableNonTerminal, 
    BindableASTNode
};

pub use utils::{
    bind_type,
    get_nt_binding, 
    get_var_binding,
    extract_terminal_value
};
