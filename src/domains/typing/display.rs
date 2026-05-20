//! Display impls for rule types (extracted from rule.rs — STYLE.md budget).
use super::rule::{
    Conclusion, Premise, TypeAscription, TypeOperation, TypeSetting, TypingJudgment, TypingRule,
};
use std::fmt;

impl fmt::Display for TypeOperation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeOperation::Equality => write!(f, "="),
            TypeOperation::Inclusion => write!(f, "⊆"),
        }
    }
}

impl fmt::Display for TypeSetting {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let base = if self.extensions.is_empty() {
            self.name.clone()
        } else {
            let exts: Vec<String> = self
                .extensions
                .iter()
                .map(|(t, ty)| format!("{}:{}", t, ty))
                .collect();
            format!("{}[{}]", self.name, exts.join(", "))
        };
        if self.no_propagate {
            write!(f, "[{}]", base)
        } else {
            write!(f, "{}", base)
        }
    }
}

impl fmt::Display for TypingJudgment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypingJudgment::Ascription(TypeAscription(term, ty)) => write!(f, "{} : {}", term, ty),
            TypingJudgment::Membership(var, ctx) => write!(f, "{} ∈ {}", var, ctx),
            TypingJudgment::Operation { left, op, right } => write!(f, "{} {} {}", left, op, right),
            TypingJudgment::Equality { left, right } => write!(f, "{} = {}", left, right),
        }
    }
}

impl fmt::Display for Premise {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match (&self.setting, &self.judgment) {
            (Some(s), Some(j)) => write!(f, "{} ⊢ {}", s, j),
            (Some(s), None) => write!(f, "{}", s),
            (None, Some(j)) => write!(f, "{}", j),
            (None, None) => Ok(()),
        }
    }
}

impl fmt::Display for Conclusion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.context.is_empty() {
            return write!(f, "{}", self.kind);
        }
        let input = &self.context.input;
        match (input.is_empty(), &self.context.output) {
            (false, Some(o)) => write!(f, "{} → {} ⊢ {}", input, o, self.kind),
            (false, None) => write!(f, "{} ⊢ {}", input, self.kind),
            (true, Some(o)) => write!(f, "{} → {} ⊢ {}", o.name, o, self.kind),
            (true, None) => write!(f, "{}", self.kind),
        }
    }
}

impl fmt::Display for TypingRule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.premises.is_empty() {
            write!(f, "[{}] {}", self.name, self.conclusion)
        } else {
            let premises: Vec<String> = self.premises.iter().map(|p| p.to_string()).collect();
            write!(
                f,
                "[{}] {} ⇒ {}",
                self.name,
                premises.join(", "),
                self.conclusion
            )
        }
    }
}
