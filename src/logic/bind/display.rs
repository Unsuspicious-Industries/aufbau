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
use super::utils::{extract_terminal_value,extract_terminals};

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
            BoundType::Universe => write!(f, "⊤"),
            BoundType::Empty => write!(f, "∅"),
        }
    }
}

impl fmt::Display for BoundTypeAscription {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let term = extract_terminals(&self.node.as_node()).join("");
        write!(f, "{} : {}", term, self.ty)
    }
}

impl fmt::Display for BoundTypingJudgment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BoundTypingJudgment::Ascription(ascr) => write!(f, "{}", ascr),
            BoundTypingJudgment::Membership(var_node, ctx) => {
                let term = extract_terminal_value(&var_node.as_node()).unwrap_or_else(|| var_node.value.clone());
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
                let term = extract_terminal_value(&e.node.as_node()).unwrap_or_else(|| e.node.value.clone());
                format!("{}:{}", term, e.ty)
            }).collect();
            write!(f, "{}[{}]", self.name, parts.join(", "))
        }
    }
}

impl fmt::Display for BoundPremise {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.setting {
            Some(setting) => write!(f, "{} ⊢ {}", setting, self.judgment),
            None => write!(f, "{}", self.judgment),
        }
    }
}

impl fmt::Display for BoundConclusion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BoundConclusion::Type(ty) => write!(f, "{}", ty),
            BoundConclusion::ContextLookup(ctx, var_node) => {
                let term = extract_terminal_value(&var_node.as_node()).unwrap_or_else(|| var_node.value.clone());
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
