//! Semantic type objects used by typing and completion.

use crate::logic::path::TreePath;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Type {
    Meta(String),
    Raw(String),
    Arrow(Box<Type>, Box<Type>),
    Array(Box<Type>),
    Object(Vec<(String, Type)>),
    ObjectExtend(String, Box<Type>, Box<Type>),
    Union(Vec<Type>),
    Not(Box<Type>),
    ContextCall(String, String),
    Any,
    None,
    Partial(Box<Type>, String),
    Path(TreePath),
    PathOf(Box<Type>, TreePath),
}
