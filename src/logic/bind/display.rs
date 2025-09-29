use std::fmt;

use super::{
    BoundConclusion,
    BoundPremise,
    BoundType,
    BoundTypeAscription,
    BoundTypeSetting,
    BoundTypingJudgment,
    BoundTypingRule,
};
use super::partial::extract_terminal_value_partial;

impl fmt::Display for BoundType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BoundType::Atom(name) => write!(f, "{}", name),
            BoundType::Arrow(from, to) => write!(f, "{} → {}", from, to),
            BoundType::Tuple(types) => {
                let type_strs: Vec<String> = types.iter().map(|t| t.to_string()).collect();
                write!(f, "({})", type_strs.join(", "))
            }
            BoundType::Pointer(inner) => write!(f, "*{}", inner),
            BoundType::Array(inner, size) => write!(f, "{}[{}]", inner, size),
            BoundType::Not(inner) => write!(f, "¬{}", inner),
            BoundType::Intersection(left, right) => write!(f, "{} ∧ {}", left, right),
            BoundType::Union(left, right) => write!(f, "{} ∨ {}", left, right),
            BoundType::ContextCall(ctx, var) => write!(f, "{}({})", ctx, var),
            BoundType::Universe => write!(f, "⊤"),
            BoundType::Empty => write!(f, "∅"),
        }
    }
}

impl fmt::Display for BoundTypeAscription {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let term = extract_terminal_value_partial(&self.node.as_node()).unwrap_or_else(|| self.node.value.clone());
        write!(f, "{} : {}", term, self.ty)
    }
}

impl fmt::Display for BoundTypingJudgment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BoundTypingJudgment::Ascription(ascr) => write!(f, "{}", ascr),
            BoundTypingJudgment::Membership(var_node, ctx) => {
                let term = extract_terminal_value_partial(&var_node.as_node()).unwrap_or_else(|| var_node.value.clone());
                write!(f, "{} ∈ {}", term, ctx)
            }
        }
    }
}

impl fmt::Display for BoundTypeSetting {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.extensions.is_empty() {
            write!(f, "{}", self.name)
        } else {
            let parts: Vec<String> = self.extensions.iter().map(|e| {
                let term = extract_terminal_value_partial(&e.node.as_node()).unwrap_or_else(|| e.node.value.clone());
                format!("{}:{}", term, e.ty)
            }).collect();
            write!(f, "{}[{}]", self.name, parts.join(", "))
        }
    }
}

impl fmt::Display for BoundPremise {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match (&self.setting, &self.judgment) {
            (Some(setting), Some(judgment)) => write!(f, "{} ⊢ {}", setting, judgment),
            (Some(setting), None) => write!(f, "{}", setting),
            (None, Some(judgment)) => write!(f, "{}", judgment),
            (None, None) => Ok(()),
        }
    }
}

impl fmt::Display for BoundConclusion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use super::partial::BoundConclusionKind;
        match &self.kind {
            BoundConclusionKind::Type(ty) => {
                match (&self.context.input, &self.context.output) {
                    (i, Some(o)) => write!(f, "{} -> {} ⊢ {}", i, o, ty),
                    (i, None) => write!(f, "{}[] ⊢ {}", i, ty),
                }
            }
            BoundConclusionKind::ContextLookup(ctx, var_node) => {
                let term = extract_terminal_value_partial(&var_node.as_node()).unwrap_or_else(|| var_node.value.clone());
                write!(f, "{}({})", ctx, term)
            }
        }
    }
}

impl fmt::Display for BoundTypingRule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.premises.is_empty() {
            write!(f, "{} ⇒ {}", self.name, self.conclusion)
        } else {
            let parts: Vec<String> = self.premises.iter().map(|p| p.to_string()).collect();
            write!(f, "{} ⇒ {}", parts.join(", "), self.conclusion)
        }
    }
}

impl fmt::Debug for BoundTypingRule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let n = self.premises.len();
        // Verbose, stable debug representation for tests and logs
        write!(
            f,
            "BOUND:{} ({} premises): {}",
            self.name,
            n,
            self
        )
    }
}
