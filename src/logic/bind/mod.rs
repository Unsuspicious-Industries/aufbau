pub mod rule;
pub mod utils;
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
    BindingResolver, 
    DefaultBindingResolver,
    BindableNonTerminal, 
    BindableASTNode
};

pub use utils::{
    get_type_binding,
    get_nt_binding, 
    get_var_binding,
    extract_terminal_value
};
