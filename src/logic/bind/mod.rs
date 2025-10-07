//! Binding module for resolving typing rules over PartialAST nodes.
//!
//! This module provides the binding infrastructure for the type system,
//! allowing typing rules to be resolved against partial AST nodes.
//!
//! The main types are:
//! - `BoundTypingRule`: A typing rule with all variables resolved to concrete nodes
//! - `BoundType`: A type expression with all type variables resolved
//! - `BindingResolver`: Trait for resolving typing rules

pub mod partial;
pub mod typing;
mod display;

#[cfg(test)]
mod tests;

// Re-export the core types and traits
pub use typing::BoundType;

// Re-export partial binding API (main API)
pub use partial::{
    BindablePartialNonTerminal, BindingResolver, BoundConclusion, BoundConclusionContext,
    BoundConclusionKind, BoundPremise, BoundTypeAscription, BoundTypeSetting, BoundTypingJudgment,
    BoundTypingRule, DefaultBindingResolver, bind_type_partial,
    collect_nt_bindings_same_level_partial, get_nt_binding_partial, get_var_binding_partial,
};
