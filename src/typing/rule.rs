//! Typing rules — data structures and parsing (§2).
//!
//! Rule text is parsed into a `TypingRule` carrying `TypeExpr` patterns in its
//! premises and conclusion. Holes (`?A`) are first-class; there is no
//! compilation pass. `domain::eval` resolves and unifies them.

use super::{Type, TypeExpr};
use crate::typing::ContextTransition;
use std::fmt;

// =============================================================================
// DATA STRUCTURES
// =============================================================================

/// Operations that can appear in premises (e.g., τ₁ = τ₂, τ₁ ⊆ τ₂)
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TypeOperation {
    Equality,
    Inclusion,
}

/// Term representation (binding name in the grammar)
pub type Term = String;

/// A type ascription: term : `type_expr`
pub type TypeAscription = (Term, TypeExpr);

/// Typing context with optional extensions: Γ or Γ[x:τ]
#[derive(Debug, Clone)]
pub struct TypeSetting {
    pub name: String,
    pub extensions: Vec<(String, TypeExpr)>,
    pub no_propagate: bool,
}

/// A judgment that can appear in premises
#[derive(Debug, Clone)]
pub enum TypingJudgment {
    /// Type ascription: e : τ  (τ is a `TypeExpr`)
    Ascription(TypeAscription),
    /// Context membership: x ∈ Γ
    Membership(String, String),
    /// Type operation: τ₁ op τ₂  (both are `TypeExpr`)
    Operation {
        left: TypeExpr,
        op: TypeOperation,
        right: TypeExpr,
    },
}

/// A premise in a typing rule
#[derive(Debug, Clone)]
pub struct Premise {
    pub setting: Option<TypeSetting>,
    pub judgment: Option<TypingJudgment>,
}

/// Verdict after premise evaluation.
/// Lattice structure: Contradiction < Unknown < Satisfied
pub enum PremiseStatus {
    Satisfied,
    Unknown,
    Contradiction,
}

/// Context transform specification in a conclusion
#[derive(Debug, Clone, Default)]
pub struct ConclusionContext {
    pub input: String,
    pub output: Option<TypeSetting>,
}

impl ConclusionContext {
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.input.is_empty() && self.output.is_none()
    }
}

/// The conclusion of a typing rule
#[derive(Debug, Clone)]
pub struct Conclusion {
    pub context: ConclusionContext,
    /// The result type expression (elaborated to evidence at eval time)
    pub kind: TypeExpr,
}

/// A complete typing rule with premises and conclusion.
#[derive(Debug, Clone)]
pub struct TypingRule {
    pub name: String,
    pub premises: Vec<Premise>,
    pub conclusion: Conclusion,
}

/// Rule evaluation result
pub enum RuleResult {
    Success((Type, Option<ContextTransition>)),
    Partial(Type),
    Contradiction,
}

// =============================================================================
// RULE ANALYSIS
// =============================================================================

impl TypingRule {
    /// Every flat `TypeExpr` appearing in this rule, in no particular order. The
    /// runtime parses each into a `TyExpr` tree once (it holds the grammar).
    #[must_use]
    pub fn type_exprs(&self) -> Vec<&TypeExpr> {
        let mut out = Vec::new();
        for premise in &self.premises {
            if let Some(setting) = &premise.setting {
                out.extend(setting.extensions.iter().map(|(_, t)| t));
            }
            match &premise.judgment {
                Some(TypingJudgment::Ascription((_, t))) => out.push(t),
                Some(TypingJudgment::Operation { left, right, .. }) => {
                    out.push(left);
                    out.push(right);
                }
                Some(TypingJudgment::Membership(_, _)) | None => {}
            }
        }
        if let Some(output) = &self.conclusion.context.output {
            out.extend(output.extensions.iter().map(|(_, t)| t));
        }
        out.push(&self.conclusion.kind);
        out
    }

    /// Get the set of binding names referenced by this rule.
    #[must_use]
    pub fn used_bindings(&self) -> std::collections::HashSet<&str> {
        let mut bindings = std::collections::HashSet::new();

        for premise in &self.premises {
            if let Some(setting) = &premise.setting {
                for (var, ty) in &setting.extensions {
                    bindings.insert(var.as_str());
                    bindings.extend(ty.refs());
                }
            }
            if let Some(judgment) = &premise.judgment {
                match judgment {
                    TypingJudgment::Ascription((term, t)) => {
                        bindings.extend(t.refs());
                        bindings.insert(term.as_str());
                    }
                    TypingJudgment::Membership(var, _) => {
                        bindings.insert(var.as_str());
                    }
                    TypingJudgment::Operation { left, right, .. } => {
                        bindings.extend(left.refs());
                        bindings.extend(right.refs());
                    }
                }
            }
        }

        if let Some(output) = &self.conclusion.context.output {
            for (var, ty) in &output.extensions {
                bindings.insert(var.as_str());
                bindings.extend(ty.refs());
            }
        }
        bindings.extend(self.conclusion.kind.refs());

        bindings
    }

    /// Pretty multiline formatting as an inference rule
    #[must_use]
    pub fn pretty(&self, indent: usize) -> String {
        let indent_str = "  ".repeat(indent);
        let mut out = String::new();
        let conclusion_str = format!("{}", self.conclusion);

        if self.premises.is_empty() {
            out.push_str(&format!(
                "{}{}  [{}]",
                indent_str, conclusion_str, self.name
            ));
            return out;
        }

        let premise_lines: Vec<String> = self
            .premises
            .iter()
            .map(|p| format!("{indent_str}{p}"))
            .collect();
        let max_width = premise_lines
            .iter()
            .map(|l| l.trim_start().len())
            .chain([conclusion_str.len()])
            .max()
            .unwrap_or(0);
        let bar = format!("{}{}", indent_str, "─".repeat(max_width.max(4)));

        out.push_str(&premise_lines.join("\n"));
        out.push('\n');
        out.push_str(&format!("{} [{}]", bar, self.name));
        out.push('\n');
        out.push_str(&format!("{indent_str}{conclusion_str}"));
        out
    }
}

// =============================================================================
// PARSING
// =============================================================================

/// Parser for typing rules
pub struct RuleParser;

impl RuleParser {
    /// Parse a complete typing rule from premises string, conclusion string, and name
    pub fn parse(
        premises_str: &str,
        conclusion_str: &str,
        name: &str,
    ) -> Result<TypingRule, String> {
        let premises = premises_str
            .split(',')
            .map(str::trim)
            .filter(|s| !s.is_empty())
            .filter_map(|p| match Self::parse_premise(p) {
                Ok(Some(pr)) => Some(Ok(pr)),
                Ok(None) => None,
                Err(e) => Some(Err(e)),
            })
            .collect::<Result<Vec<_>, _>>()?;

        let conclusion = Self::parse_conclusion(conclusion_str)?;

        Ok(TypingRule {
            name: name.to_string(),
            premises,
            conclusion,
        })
    }

    /// Parse a conclusion: `Γ → Γ[x:τ] ⊢ τ` | `τ`
    pub fn parse_conclusion(s: &str) -> Result<Conclusion, String> {
        let s = s.trim();

        // Case 1: Contains ⊢ → context-transforming type conclusion
        if let Some((lhs, rhs)) = s.split_once('⊢') {
            let ty = TypeExpr::parse(rhs.trim())?;
            let ctx = Self::parse_conclusion_context(lhs.trim())?;
            return Ok(Conclusion {
                context: ctx,
                kind: ty,
            });
        }

        // Case 2: Bare type expression
        let ty = TypeExpr::parse(s)?;
        Ok(Conclusion {
            context: ConclusionContext::default(),
            kind: ty,
        })
    }

    /// Parse a premise: `x ∈ Γ` | `Γ ⊢ e : τ` | `τ₁ = τ₂` | `Γ[x:τ]`
    pub fn parse_premise(s: &str) -> Result<Option<Premise>, String> {
        let s = s.trim();
        if s.is_empty() {
            return Ok(None);
        }

        // Membership: x ∈ Γ
        if let Some((var, ctx)) = s.split_once('∈') {
            let var = var.trim().to_string();
            let ctx = ctx.trim().to_string();
            if var.is_empty() || ctx.is_empty() {
                return Err(format!("Invalid membership premise: '{s}'"));
            }
            return Ok(Some(Premise {
                setting: None,
                judgment: Some(TypingJudgment::Membership(var, ctx)),
            }));
        }

        // Typing judgment: Γ ⊢ e : τ
        if let Some((setting_part, ascr_part)) = s.split_once('⊢') {
            let setting = Self::parse_setting(setting_part.trim())?;
            let ascription = Self::parse_ascription(ascr_part.trim())?;
            return Ok(Some(Premise {
                setting: Some(setting),
                judgment: Some(TypingJudgment::Ascription(ascription)),
            }));
        }

        // Type operation: τ₁ = τ₂ or τ₁ ⊆ τ₂
        if let Some((left, op, right)) = Self::try_parse_operation(s) {
            return Ok(Some(Premise {
                setting: None,
                judgment: Some(TypingJudgment::Operation { left, op, right }),
            }));
        }

        // Setting-only premise
        let setting = Self::parse_setting(s)?;
        Ok(Some(Premise {
            setting: Some(setting),
            judgment: None,
        }))
    }

    /// Parse a type setting: `Γ` or `Γ[x:τ₁][y:τ₂]`
    pub fn parse_setting(s: &str) -> Result<TypeSetting, String> {
        let s = s.trim();
        if s.starts_with('[') && s.ends_with(']') && s.len() >= 2 {
            let inner = s[1..s.len() - 1].trim();
            let mut setting = Self::parse_setting(inner)?;
            setting.no_propagate = true;
            return Ok(setting);
        }
        if !s.contains('[') {
            return Ok(TypeSetting {
                name: s.to_string(),
                extensions: Vec::new(),
                no_propagate: false,
            });
        }

        let first_bracket = s
            .find('[')
            .ok_or_else(|| "Invalid setting: expected name before '['".to_string())?;
        let name = s[..first_bracket].trim().to_string();
        if name.is_empty() {
            return Err("Invalid setting: expected name before '['".to_string());
        }

        let mut extensions = Vec::new();
        let rest = &s[first_bracket..];
        let mut pos = 0;
        while let Some(bracket_start) = rest[pos..].find('[') {
            let abs_start = pos + bracket_start + 1;
            if let Some(colon) = rest[abs_start..].find(':') {
                let key = rest[abs_start..abs_start + colon].trim();
                let val_start = abs_start + colon + 1;
                if let Some(bracket_end) = rest[val_start..].find(']') {
                    let val = rest[val_start..val_start + bracket_end].trim();
                    let ty = TypeExpr::parse(val)?;
                    extensions.push((key.to_string(), ty));
                    pos = val_start + bracket_end + 1;
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        Ok(TypeSetting {
            name,
            extensions,
            no_propagate: false,
        })
    }

    /// Parse a type ascription: `e : τ`
    fn parse_ascription(s: &str) -> Result<TypeAscription, String> {
        let parts: Vec<&str> = s.splitn(2, ':').map(str::trim).collect();
        if parts.len() != 2 {
            return Err(format!(
                "Invalid ascription: expected 'term : type', got '{s}'"
            ));
        }
        let term = parts[0].to_string();
        let ty = TypeExpr::parse(parts[1])?;
        Ok((term, ty))
    }

    /// Try to parse a type operation: `τ₁ = τ₂` or `τ₁ ⊆ τ₂`
    fn try_parse_operation(s: &str) -> Option<(TypeExpr, TypeOperation, TypeExpr)> {
        // Try equality
        if let Some((l, r)) = s.split_once('=')
            && let (Ok(left), Ok(right)) = (TypeExpr::parse(l.trim()), TypeExpr::parse(r.trim()))
        {
            return Some((left, TypeOperation::Equality, right));
        }
        // Try inclusion (⊆ or <=)
        for sep in ["⊆ ", "<="] {
            if let Some((l, r)) = s.split_once(sep)
                && let (Ok(left), Ok(right)) =
                    (TypeExpr::parse(l.trim()), TypeExpr::parse(r.trim()))
            {
                return Some((left, TypeOperation::Inclusion, right));
            }
        }
        None
    }

    /// Parse the context part of a conclusion: `Γ → Γ[x:τ]`
    fn parse_conclusion_context(s: &str) -> Result<ConclusionContext, String> {
        if s.is_empty() {
            return Ok(ConclusionContext::default());
        }

        // Check for arrow (context transform)
        for arrow in ["→", "->"] {
            if let Some((l, r)) = s.split_once(arrow) {
                let input = if l.trim().is_empty() {
                    String::new()
                } else {
                    Self::parse_setting(l.trim())?.name
                };
                let output = if r.trim().is_empty() {
                    None
                } else {
                    Some(Self::parse_setting(r.trim())?)
                };
                return Ok(ConclusionContext { input, output });
            }
        }

        // No arrow: just input context
        let input = Self::parse_setting(s)?.name;
        Ok(ConclusionContext {
            input,
            output: None,
        })
    }
}

// Legacy API
impl TypingRule {
    pub fn new(premises: String, conclusion: String, name: String) -> Result<Self, String> {
        RuleParser::parse(&premises, &conclusion, &name)
    }

    pub fn parse_conclusion(s: &str) -> Result<Conclusion, String> {
        RuleParser::parse_conclusion(s)
    }
}

impl Conclusion {
    #[must_use = "discarding parse errors masks ill-formed conclusions"]
    pub fn try_from_str(s: &str) -> Result<Self, String> {
        RuleParser::parse_conclusion(s)
    }

    #[must_use = "discarding parse errors masks ill-formed conclusions"]
    pub fn try_from_string(s: String) -> Result<Self, String> {
        Self::try_from_str(&s)
    }
}

// =============================================================================
// DISPLAY
// =============================================================================

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
                .map(|(t, ty)| format!("{t}:{ty}"))
                .collect();
            format!("{}[{}]", self.name, exts.join(", "))
        };

        if self.no_propagate {
            write!(f, "[{base}]")
        } else {
            write!(f, "{base}")
        }
    }
}

impl fmt::Display for TypingJudgment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypingJudgment::Ascription((term, ty)) => write!(f, "{term} : {ty}"),
            TypingJudgment::Membership(var, ctx) => write!(f, "{var} ∈ {ctx}"),
            TypingJudgment::Operation { left, op, right } => {
                write!(f, "{left} {op} {right}")
            }
        }
    }
}

impl fmt::Display for Premise {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match (&self.setting, &self.judgment) {
            (Some(s), Some(j)) => write!(f, "{s} ⊢ {j}"),
            (Some(s), None) => write!(f, "{s}"),
            (None, Some(j)) => write!(f, "{j}"),
            (None, None) => Ok(()),
        }
    }
}

impl fmt::Display for Conclusion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.context.is_empty() {
            write!(f, "{}", self.kind)
        } else {
            let input = &self.context.input;
            match (input.is_empty(), &self.context.output) {
                (false, Some(o)) => write!(f, "{} → {} ⊢ {}", input, o, self.kind),
                (false, None) => write!(f, "{} ⊢ {}", input, self.kind),
                (true, Some(o)) => write!(f, "{} → {} ⊢ {}", o.name, o, self.kind),
                (true, None) => write!(f, "{}", self.kind),
            }
        }
    }
}

impl fmt::Display for TypingRule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.premises.is_empty() {
            write!(f, "[{}] {}", self.name, self.conclusion)
        } else {
            let premises: Vec<String> = self
                .premises
                .iter()
                .map(std::string::ToString::to_string)
                .collect();
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

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::typing::Atom;

    #[test]
    fn parse_app_rule_holes() {
        let rule = TypingRule::new(
            "Γ ⊢ l : ?A -> ?B, Γ ⊢ r : ?A".into(),
            "?B".into(),
            "app".into(),
        )
        .unwrap();
        assert_eq!(rule.name, "app");
        assert_eq!(rule.premises.len(), 2);
        assert!(rule.conclusion.kind.has_holes());
        assert_eq!(rule.conclusion.kind.holes(), vec!["B"]);
    }

    #[test]
    fn parse_var_rule_ctx_conclusion() {
        let rule = TypingRule::new("x ∈ Γ".into(), "Γ(x)".into(), "var".into()).unwrap();
        assert_eq!(rule.name, "var");
        assert_eq!(rule.premises.len(), 1);
        assert_eq!(rule.conclusion.kind.0, vec![Atom::Ctx("x".into())]);
    }

    #[test]
    fn parse_lambda_rule_context_extension() {
        let rule = TypingRule::new(
            "Γ[a:'A'] ⊢ e : ?R".into(),
            "'A' -> ?R".into(),
            "lambda".into(),
        )
        .unwrap();
        let setting = rule.premises[0].setting.as_ref().unwrap();
        assert_eq!(setting.name, "Γ");
        assert_eq!(setting.extensions[0].0, "a");
        assert_eq!(setting.extensions[0].1.0, vec![Atom::Lit("A".into())]);
    }

    #[test]
    fn parse_define_rule_context_transform() {
        let rule = TypingRule::new(
            "Γ ⊢ value : ?T".into(),
            "Γ → Γ[name:?T] ⊢ 'Unit'".into(),
            "define".into(),
        )
        .unwrap();
        let ctx = &rule.conclusion.context;
        assert_eq!(ctx.input, "Γ");
        let output = ctx.output.as_ref().unwrap();
        assert_eq!(output.extensions[0].0, "name");
        assert_eq!(output.extensions[0].1.0, vec![Atom::Hole("T".into())]);
        assert_eq!(rule.conclusion.kind.0, vec![Atom::Lit("Unit".into())]);
    }

    #[test]
    fn used_bindings_collects_refs_and_terms() {
        let rule = TypingRule::new("Γ ⊢ e : τ -> ?R".into(), "?R".into(), "single".into()).unwrap();
        let used = rule.used_bindings();
        assert!(used.contains("e"));
        assert!(used.contains("τ"));
    }
}
