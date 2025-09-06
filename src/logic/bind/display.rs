use std::fmt;

use super::{
    BoundConclusion,
    BoundPremise,
    BoundTypeAscription,
    BoundTypeSetting,
    BoundTypingJudgment,
    BoundTypingRule,
};
use super::utils::extract_terminal_value;

impl fmt::Display for BoundTypeAscription {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let term = extract_terminal_value(&self.node.as_node()).unwrap_or_else(|| self.node.value.clone());
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
            write!(f, "[BOUND:{}] 0 premises ⇒ {}", self.name, self.conclusion)
        } else {
            let parts: Vec<String> = self.premises.iter().map(|p| p.to_string()).collect();
            write!(f, "[BOUND:{}] {} premises ⇒ {} ⇒ {}", self.name, self.premises.len(), parts.join(", "), self.conclusion)
        }
    }
}
