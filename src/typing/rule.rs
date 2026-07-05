//! Typing rules — data structures and parsing (§2).
//!
//! The model is small: a rule is a list of premises and a conclusion. A
//! premise is a judgment under premise-local context extensions; a judgment is
//! an ascription (`Γ ⊢ b : τ`), a membership (`x ∈ Γ`), or an equation
//! (`τ₁ = τ₂`). A conclusion is a type, optionally with right-bound effects
//! (`Γ → Γ[x:τ] ⊢ σ`). There is exactly one ambient context; names like `Γ`
//! are notation, not data. Every judgment is discharged by the same elementary
//! operation: unification of type terms.

use super::TypeExpr;
use crate::typing::ContextTransition;
use crate::typing::term::Evidence;
use std::fmt;

// =============================================================================
// DATA STRUCTURES
// =============================================================================

/// A judgment: the three premise forms, all discharged by unification.
#[derive(Debug, Clone)]
pub enum Judgment {
    /// `Γ ⊢ b : τ` — the type of the child bound to `b` unifies with `τ`.
    Ascription { binding: String, ty: TypeExpr },
    /// `x ∈ Γ` — `x`'s text is bound in the context.
    Membership { binding: String },
    /// `τ₁ = τ₂` — the two type expressions unify.
    Equation { left: TypeExpr, right: TypeExpr },
}

/// A premise: a judgment under premise-local context extensions
/// (`Γ[x:τ₁][y:τ₂] ⊢ …`). Extensions scope over this premise only.
#[derive(Debug, Clone)]
pub struct Premise {
    pub extensions: Vec<(String, TypeExpr)>,
    pub judgment: Judgment,
}

/// Verdict after premise evaluation.
/// Lattice structure: Contradiction < Unknown < Satisfied
pub enum PremiseStatus {
    Satisfied,
    Unknown,
    Contradiction,
}

/// The conclusion of a typing rule: the node's type, and the context
/// extensions exported to right siblings once the node is exact
/// (`Γ → Γ[x:τ] ⊢ σ`).
#[derive(Debug, Clone)]
pub struct Conclusion {
    pub ty: TypeExpr,
    pub effects: Vec<(String, TypeExpr)>,
}

/// A complete typing rule with premises and conclusion.
#[derive(Debug, Clone)]
pub struct TypingRule {
    pub name: String,
    pub premises: Vec<Premise>,
    pub conclusion: Conclusion,
}

/// Rule evaluation result. Evidence carries the node's type term together with
/// the residual equations its premises established (see [`Evidence`]).
pub enum RuleResult {
    Success((Evidence, Option<ContextTransition>)),
    Partial(Evidence),
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
            out.extend(premise.extensions.iter().map(|(_, t)| t));
            match &premise.judgment {
                Judgment::Ascription { ty, .. } => out.push(ty),
                Judgment::Equation { left, right } => {
                    out.push(left);
                    out.push(right);
                }
                Judgment::Membership { .. } => {}
            }
        }
        out.extend(self.conclusion.effects.iter().map(|(_, t)| t));
        out.push(&self.conclusion.ty);
        out
    }

    /// Get the set of binding names referenced by this rule.
    #[must_use]
    pub fn used_bindings(&self) -> std::collections::HashSet<&str> {
        let mut bindings = std::collections::HashSet::new();

        for premise in &self.premises {
            for (var, ty) in &premise.extensions {
                bindings.insert(var.as_str());
                bindings.extend(ty.refs());
            }
            match &premise.judgment {
                Judgment::Ascription { binding, ty } => {
                    bindings.extend(ty.refs());
                    bindings.insert(binding.as_str());
                }
                Judgment::Membership { binding } => {
                    bindings.insert(binding.as_str());
                }
                Judgment::Equation { left, right } => {
                    bindings.extend(left.refs());
                    bindings.extend(right.refs());
                }
            }
        }

        for (var, ty) in &self.conclusion.effects {
            bindings.insert(var.as_str());
            bindings.extend(ty.refs());
        }
        bindings.extend(self.conclusion.ty.refs());

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
            .map(Self::parse_premise)
            .collect::<Result<Vec<_>, _>>()?;

        let conclusion = Self::parse_conclusion(conclusion_str)?;

        Ok(TypingRule {
            name: name.to_string(),
            premises,
            conclusion,
        })
    }

    /// Parse a conclusion: `τ` | `Γ ⊢ τ` | `Γ → Γ[x:τ] ⊢ σ`
    pub fn parse_conclusion(s: &str) -> Result<Conclusion, String> {
        let s = s.trim();
        let Some((lhs, rhs)) = s.split_once('⊢') else {
            return Ok(Conclusion {
                ty: TypeExpr::parse(s)?,
                effects: Vec::new(),
            });
        };
        let ty = TypeExpr::parse(rhs.trim())?;
        let lhs = lhs.trim();
        let effects = match lhs.split_once('→').or_else(|| lhs.split_once("->")) {
            Some((_, out)) if !out.trim().is_empty() => Self::parse_extensions(out.trim())?,
            _ => Vec::new(),
        };
        Ok(Conclusion { ty, effects })
    }

    /// Parse a premise: `x ∈ Γ` | `Γ[x:τ]… ⊢ b : σ` | `τ₁ = τ₂`.
    /// Anything else is an error: there is no fourth judgment form.
    pub fn parse_premise(s: &str) -> Result<Premise, String> {
        let s = s.trim();

        // Membership: x ∈ Γ
        if let Some((var, ctx)) = s.split_once('∈') {
            let var = var.trim().to_string();
            if var.is_empty() || ctx.trim().is_empty() {
                return Err(format!("invalid membership premise: '{s}'"));
            }
            return Ok(Premise {
                extensions: Vec::new(),
                judgment: Judgment::Membership { binding: var },
            });
        }

        // Ascription: Γ[x:τ]… ⊢ b : σ
        if let Some((setting, ascr)) = s.split_once('⊢') {
            let extensions = Self::parse_extensions(setting.trim())?;
            let (binding, ty) = Self::parse_ascription(ascr.trim())?;
            return Ok(Premise {
                extensions,
                judgment: Judgment::Ascription { binding, ty },
            });
        }

        if s.contains('⊆') || s.contains("<=") {
            return Err(format!(
                "inclusion premise '{s}': there is no subtyping; every relation is unification (=)"
            ));
        }

        // Equation: τ₁ = τ₂
        if let Some((l, r)) = s.split_once('=') {
            return Ok(Premise {
                extensions: Vec::new(),
                judgment: Judgment::Equation {
                    left: TypeExpr::parse(l.trim())?,
                    right: TypeExpr::parse(r.trim())?,
                },
            });
        }

        Err(format!(
            "unrecognized premise '{s}': expected 'x ∈ Γ', 'Γ ⊢ b : τ', or 'τ₁ = τ₂'"
        ))
    }

    /// Parse a context with extensions, `Γ[x:τ₁][y:τ₂]…`, into the extension
    /// list. The context name is notation for the one ambient context and is
    /// not kept; the whole string must be consumed.
    pub fn parse_extensions(s: &str) -> Result<Vec<(String, TypeExpr)>, String> {
        let s = s.trim();
        let Some(first_bracket) = s.find('[') else {
            if s.is_empty() {
                return Err("invalid setting: expected a context name".to_string());
            }
            return Ok(Vec::new());
        };
        if s[..first_bracket].trim().is_empty() {
            return Err(format!(
                "invalid setting '{s}': expected a context name before '['"
            ));
        }

        let mut extensions = Vec::new();
        let mut rest = s[first_bracket..].trim();
        while !rest.is_empty() {
            let inner = rest
                .strip_prefix('[')
                .and_then(|r| r.split_once(']'))
                .ok_or_else(|| format!("invalid setting '{s}': malformed extension '{rest}'"))?;
            let (ext, tail) = inner;
            let (key, val) = ext
                .split_once(':')
                .ok_or_else(|| format!("invalid setting '{s}': extension '{ext}' has no ':'"))?;
            let key = key.trim();
            if key.is_empty() {
                return Err(format!("invalid setting '{s}': empty binder in '{ext}'"));
            }
            extensions.push((key.to_string(), TypeExpr::parse(val.trim())?));
            rest = tail.trim();
        }
        Ok(extensions)
    }

    /// Parse a type ascription: `b : τ`
    fn parse_ascription(s: &str) -> Result<(String, TypeExpr), String> {
        let (binding, ty) = s
            .split_once(':')
            .ok_or_else(|| format!("invalid ascription: expected 'term : type', got '{s}'"))?;
        Ok((binding.trim().to_string(), TypeExpr::parse(ty.trim())?))
    }
}

impl TypingRule {
    /// Parse a rule from its inference-notation parts.
    pub fn new(premises: String, conclusion: String, name: String) -> Result<Self, String> {
        RuleParser::parse(&premises, &conclusion, &name)
    }
}

// =============================================================================
// DISPLAY
// =============================================================================

fn write_extensions(f: &mut fmt::Formatter<'_>, extensions: &[(String, TypeExpr)]) -> fmt::Result {
    write!(f, "Γ")?;
    for (x, ty) in extensions {
        write!(f, "[{x}:{ty}]")?;
    }
    Ok(())
}

impl fmt::Display for Judgment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Judgment::Ascription { binding, ty } => write!(f, "{binding} : {ty}"),
            Judgment::Membership { binding } => write!(f, "{binding} ∈ Γ"),
            Judgment::Equation { left, right } => write!(f, "{left} = {right}"),
        }
    }
}

impl fmt::Display for Premise {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Judgment::Ascription { .. } = &self.judgment {
            write_extensions(f, &self.extensions)?;
            write!(f, " ⊢ ")?;
        }
        write!(f, "{}", self.judgment)
    }
}

impl fmt::Display for Conclusion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.effects.is_empty() {
            write!(f, "Γ → ")?;
            write_extensions(f, &self.effects)?;
            write!(f, " ⊢ ")?;
        }
        write!(f, "{}", self.ty)
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
        assert!(rule.conclusion.ty.has_holes());
        assert_eq!(rule.conclusion.ty.holes(), vec!["B"]);
    }

    #[test]
    fn parse_var_rule_ctx_conclusion() {
        let rule = TypingRule::new("x ∈ Γ".into(), "Γ(x)".into(), "var".into()).unwrap();
        assert_eq!(rule.name, "var");
        assert_eq!(rule.premises.len(), 1);
        assert_eq!(rule.conclusion.ty.0, vec![Atom::Ctx("x".into())]);
    }

    #[test]
    fn parse_lambda_rule_context_extension() {
        let rule = TypingRule::new(
            "Γ[a:'A'] ⊢ e : ?R".into(),
            "'A' -> ?R".into(),
            "lambda".into(),
        )
        .unwrap();
        let exts = &rule.premises[0].extensions;
        assert_eq!(exts[0].0, "a");
        assert_eq!(exts[0].1.0, vec![Atom::Lit("A".into())]);
    }

    #[test]
    fn parse_define_rule_context_transform() {
        let rule = TypingRule::new(
            "Γ ⊢ value : ?T".into(),
            "Γ → Γ[name:?T] ⊢ 'Unit'".into(),
            "define".into(),
        )
        .unwrap();
        let effects = &rule.conclusion.effects;
        assert_eq!(effects[0].0, "name");
        assert_eq!(effects[0].1.0, vec![Atom::Hole("T".into())]);
        assert_eq!(rule.conclusion.ty.0, vec![Atom::Lit("Unit".into())]);
    }

    #[test]
    fn used_bindings_collects_refs_and_terms() {
        let rule = TypingRule::new("Γ ⊢ e : τ -> ?R".into(), "?R".into(), "single".into()).unwrap();
        let used = rule.used_bindings();
        assert!(used.contains("e"));
        assert!(used.contains("τ"));
    }

    #[test]
    fn inclusion_premise_is_rejected() {
        let err = TypingRule::new("τ ⊆ ?A".into(), "?A".into(), "sub".into()).unwrap_err();
        assert!(err.contains("unification"), "got: {err}");
    }

    #[test]
    fn unrecognized_premise_is_rejected() {
        let err = TypingRule::new("Δ".into(), "?A".into(), "junk".into()).unwrap_err();
        assert!(err.contains("unrecognized premise"), "got: {err}");
    }

    #[test]
    fn equation_premise_parses() {
        let rule = TypingRule::new("τ = ?A list".into(), "?A".into(), "elem".into()).unwrap();
        assert!(matches!(
            rule.premises[0].judgment,
            Judgment::Equation { .. }
        ));
    }

    #[test]
    fn display_round_trips_through_parse() {
        let rule = TypingRule::new(
            "Γ[a:τ] ⊢ e : ?B, x ∈ Γ".into(),
            "Γ → Γ[name:?B] ⊢ τ -> ?B".into(),
            "rt".into(),
        )
        .unwrap();
        let p0 = rule.premises[0].to_string();
        let reparsed = RuleParser::parse_premise(&p0).unwrap();
        assert_eq!(reparsed.extensions.len(), 1);
        let c = rule.conclusion.to_string();
        let recon = RuleParser::parse_conclusion(&c).unwrap();
        assert_eq!(recon.effects.len(), 1);
    }
}
