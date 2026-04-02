use crate::logic::grammar::Segment;
use std::ops::Add;

use super::typing::BindingValue;
use crate::logic::parse::arena::{PathId, Span, TypeId, TypeStatus};

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Bindings(pub Vec<BindingValue>);

impl Add<&[BindingValue]> for Bindings {
    type Output = Self;

    fn add(mut self, inner: &[BindingValue]) -> Self {
        for binding in inner {
            if let Some(existing) = self
                .0
                .iter_mut()
                .find(|e| e.name == binding.name && e.path == binding.path)
            {
                if existing.value.is_none() {
                    existing.value = binding.value.clone();
                }
                if existing.ty.is_none() {
                    existing.ty = binding.ty;
                }
            } else {
                self.0.push(binding.clone());
            }
        }
        self
    }
}

impl From<Vec<BindingValue>> for Bindings {
    fn from(value: Vec<BindingValue>) -> Self {
        Self(value)
    }
}

impl From<Bindings> for Vec<BindingValue> {
    fn from(value: Bindings) -> Self {
        value.0
    }
}

/// Update binding at `path` with terminal value and type.
///
/// Bindings are created by `descend` when entering a symbol with a binding name.
/// This function updates that binding with the actual parsed value and type.
///
/// Returns the full binding list with the binding at `path` updated (if present).
///
/// Note: leak prevention is handled at *propagation* time (filtering bindings to
/// the current symbol's path subtree), not by dropping unrelated bindings here.
pub fn bind_terminal(
    bindings: &[BindingValue],
    path: PathId,
    value: Option<String>,
    ty: Option<TypeId>,
) -> Vec<BindingValue> {
    bindings
        .iter()
        .map(|b| {
            if b.path != path {
                return b.clone();
            }
            BindingValue {
                name: b.name.clone(),
                path,
                value: value.clone().or_else(|| b.value.clone()),
                ty: ty.or(b.ty),
            }
        })
        .collect()
}

/// Update binding at `path` with node span text and type.
///
/// Same behavior as `bind_terminal`: update the binding at `path`, preserve the rest.
pub fn bind_node(
    bindings: &[BindingValue],
    path: PathId,
    span: Span,
    ty: TypeStatus,
    segments: &[Segment],
) -> Vec<BindingValue> {
    let text = span_text(span, segments);
    bindings
        .iter()
        .map(|b| {
            if b.path != path {
                return b.clone();
            }
            BindingValue {
                name: b.name.clone(),
                path,
                value: text.clone().or_else(|| b.value.clone()),
                ty: Some(type_id(ty)),
            }
        })
        .collect()
}

fn span_text(span: Span, segments: &[Segment]) -> Option<String> {
    let parts = (span.start as usize..span.end as usize)
        .filter_map(|idx| {
            segments
                .get(idx)
                .map(|segment| segment.as_str().to_string())
        })
        .collect::<Vec<_>>();
    if parts.is_empty() {
        None
    } else {
        Some(parts.join(" "))
    }
}

fn type_id(status: TypeStatus) -> TypeId {
    match status {
        TypeStatus::Valid(ty) | TypeStatus::Partial(ty) => ty,
    }
}
