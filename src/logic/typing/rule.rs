//! Typing Rules - Data Structures and Parsing
//!
//! This module defines the "AST" for typing rules and provides parsing.
//!
//! # Structure
//! - **Data types**: TypingRule, Premise, Conclusion, etc.
//! - **Parsing**: Converting text to rule structures
//! - **Display**: Pretty-printing rules

use super::Type;
use regex::Regex;
use std::fmt;

// =============================================================================
// DATA STRUCTURES (Rule AST)
// =============================================================================

/// Operations that can appear in premises (e.g., τ₁ = τ₂, τ₁ ⊆ τ₂)
#[derive(Debug, Clone, PartialEq)]
pub enum TypeOperation {
    /// Type equality: τ₁ = τ₂
    Equality,
    /// Subtyping/inclusion: τ₁ ⊆ τ₂
    Inclusion,
}

/// Term representation (binding name in the grammar)
pub type Term = String;

/// A type ascription: term : type
pub type TypeAscription = (Term, Type);

/// Typing context with optional extensions: Γ or Γ[x:τ]
#[derive(Debug, Clone)]
pub struct TypeSetting {
    /// Context name (typically "Γ")
    pub name: String,
    /// Local extensions: [x:τ₁][y:τ₂]...
    pub extensions: Vec<TypeAscription>,
    /// Disable upward context propagation from nested checks
    pub no_propagate: bool,
}

/// A judgment that can appear in premises
#[derive(Debug, Clone)]
pub enum TypingJudgment {
    /// pure check
    Check(Term),
    /// Type ascription: e : τ
    Ascription(TypeAscription),
    /// Context membership: x ∈ Γ
    Membership(String, String),
    /// Type operation: τ₁ op τ₂
    Operation {
        left: Type,
        op: TypeOperation,
        right: Type,
    },
}

/// A premise in a typing rule
#[derive(Debug, Clone)]
pub struct Premise {
    /// Optional context setting (Γ or Γ[x:τ])
    pub setting: Option<TypeSetting>,
    /// Optional judgment
    pub judgment: Option<TypingJudgment>,
}

/// Context transform specification in a conclusion
#[derive(Debug, Clone, Default)]
pub struct ConclusionContext {
    /// Input context name
    pub input: String,
    /// Output context (possibly extended)
    pub output: Option<TypeSetting>,
}

impl ConclusionContext {
    pub fn is_empty(&self) -> bool {
        self.input.is_empty() && self.output.is_none()
    }
}

/// What a conclusion produces
#[derive(Debug, Clone)]
pub enum ConclusionKind {
    /// A type result
    Type(Type),
    /// A context lookup: Γ(x)
    ContextLookup(String, String),
}

/// The conclusion of a typing rule
#[derive(Debug, Clone)]
pub struct Conclusion {
    /// Optional context transform (Γ → Γ[x:τ])
    pub context: ConclusionContext,
    /// The result kind
    pub kind: ConclusionKind,
}

/// A complete typing rule with premises and conclusion
#[derive(Debug, Clone)]
pub struct TypingRule {
    /// Rule name (e.g., "var", "lambda", "app")
    pub name: String,
    /// Premises (conditions)
    pub premises: Vec<Premise>,
    /// Conclusion (result)
    pub conclusion: Conclusion,
}

// =============================================================================
// RULE ANALYSIS
// =============================================================================

impl TypingRule {
    /// Get the set of binding names referenced by this rule.
    /// Used to determine which grammar bindings are needed during evaluation.
    pub fn used_bindings(&self) -> std::collections::HashSet<&str> {
        let mut bindings = std::collections::HashSet::new();

        // From premises
        for premise in &self.premises {
            if let Some(setting) = &premise.setting {
                for (var, _) in &setting.extensions {
                    bindings.insert(var.as_str());
                }
            }
            if let Some(judgment) = &premise.judgment {
                match judgment {
                    TypingJudgment::Check(term) => {
                        bindings.insert(term.as_str());
                    }
                    TypingJudgment::Ascription((term, t)) => {
                        if let Type::Raw(b) = t {
                            bindings.insert(b.as_str());
                        }
                        bindings.insert(term.as_str());
                    }
                    TypingJudgment::Membership(var, _) => {
                        bindings.insert(var.as_str());
                    }
                    TypingJudgment::Operation { .. } => {}
                }
            }
        }

        // From conclusion
        if let Some(output) = &self.conclusion.context.output {
            for (var, _) in &output.extensions {
                bindings.insert(var.as_str());
            }
        }
        if let ConclusionKind::ContextLookup(_, var) = &self.conclusion.kind {
            bindings.insert(var.as_str());
        }

        bindings
    }

    /// Pretty multiline formatting as an inference rule
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
            .map(|p| format!("{}{}", indent_str, p))
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
        out.push_str(&format!("{}{}", indent_str, conclusion_str));
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

    /// Parse a conclusion: Γ → Γ[x:τ] ⊢ τ | Γ(x) | τ
    pub fn parse_conclusion(s: &str) -> Result<Conclusion, String> {
        let s = s.trim();

        // Case 1: Contains ⊢ → context-transforming type conclusion
        if let Some((lhs, rhs)) = s.split_once('⊢') {
            let ty = Type::parse(rhs.trim())?;
            let ctx = Self::parse_conclusion_context(lhs.trim())?;
            return Ok(Conclusion {
                context: ctx,
                kind: ConclusionKind::Type(ty),
            });
        }

        // Case 2: Context lookup Γ(x)
        if let Some((ctx, var)) = Self::try_parse_context_lookup(s) {
            return Ok(Conclusion {
                context: ConclusionContext::default(),
                kind: ConclusionKind::ContextLookup(ctx, var),
            });
        }

        // Case 3: Bare type
        let ty = Type::parse(s)?;
        Ok(Conclusion {
            context: ConclusionContext::default(),
            kind: ConclusionKind::Type(ty),
        })
    }

    /// Parse a premise: x ∈ Γ | Γ ⊢ e : τ | τ₁ = τ₂ | Γ[x:τ]
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
                return Err(format!("Invalid membership premise: '{}'", s));
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
        // Check judgment: Γ ▷ e
        else if let Some((setting_part, ascr_part)) = s.split_once('▷') {
            let setting = Self::parse_setting(setting_part.trim())?;
            if ascr_part.trim().is_empty() {
                return Err(format!(
                    "Invalid check premise: missing term after '▷' in '{}'",
                    s
                ));
            }
            return Ok(Some(Premise {
                setting: Some(setting),
                judgment: Some(TypingJudgment::Check(ascr_part.trim().to_string())),
            }));
        }
        // Type operation: τ₁ = τ₂ or τ₁ ⊆ τ₂
        else if let Some((left, op, right)) = Self::try_parse_operation(s) {
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

    /// Parse a type setting: Γ or Γ[x:τ₁][y:τ₂]
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

        let name_re = Regex::new(r"^\s*([^\[\s]+)\s*\[").map_err(|e| e.to_string())?;
        let name = name_re
            .captures(s)
            .and_then(|c| c.get(1))
            .map(|m| m.as_str().to_string())
            .ok_or_else(|| "Invalid setting: expected name before '['".to_string())?;

        let ext_re = Regex::new(r"\[([^:\]]+):([^\]]+)\]").map_err(|e| e.to_string())?;
        let mut extensions = Vec::new();
        for cap in ext_re.captures_iter(s) {
            let var = cap[1].trim().to_string();
            let ty = Type::parse(cap[2].trim())?;
            extensions.push((var, ty));
        }

        Ok(TypeSetting {
            name,
            extensions,
            no_propagate: false,
        })
    }

    /// Parse a type ascription: e : τ
    fn parse_ascription(s: &str) -> Result<TypeAscription, String> {
        let parts: Vec<&str> = s.splitn(2, ':').map(str::trim).collect();
        if parts.len() != 2 {
            return Err(format!(
                "Invalid ascription: expected 'term : type', got '{}'",
                s
            ));
        }
        let term = parts[0].to_string();
        let ty = Type::parse(parts[1])?;
        Ok((term, ty))
    }

    /// Try to parse a type operation: τ₁ = τ₂ or τ₁ ⊆ τ₂
    fn try_parse_operation(s: &str) -> Option<(Type, TypeOperation, Type)> {
        // Try equality
        if let Some((l, r)) = s.split_once("=") {
            if let (Ok(left), Ok(right)) = (Type::parse(l.trim()), Type::parse(r.trim())) {
                return Some((left, TypeOperation::Equality, right));
            }
        }
        // Try inclusion (⊆ or <=)
        for sep in ["⊆ ", "<="] {
            if let Some((l, r)) = s.split_once(sep) {
                if let (Ok(left), Ok(right)) = (Type::parse(l.trim()), Type::parse(r.trim())) {
                    return Some((left, TypeOperation::Inclusion, right));
                }
            }
        }
        None
    }

    /// Try to parse a context lookup: Γ(x)
    fn try_parse_context_lookup(s: &str) -> Option<(String, String)> {
        let paren_start = s.find('(')?;
        let paren_end = s.find(')')?;
        if paren_end > paren_start && paren_end == s.len() - 1 {
            let ctx = s[..paren_start].trim();
            let var = s[paren_start + 1..paren_end].trim();
            if !ctx.is_empty() && !var.is_empty() {
                return Some((ctx.to_string(), var.to_string()));
            }
        }
        None
    }

    /// Parse the context part of a conclusion: Γ → Γ[x:τ]
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

// Legacy API: keep TypingRule::new for backward compatibility
impl TypingRule {
    pub fn new(premises: String, conclusion: String, name: String) -> Result<Self, String> {
        RuleParser::parse(&premises, &conclusion, &name)
    }

    pub fn parse_conclusion(s: &str) -> Result<Conclusion, String> {
        RuleParser::parse_conclusion(s)
    }
}

impl Conclusion {
    pub fn try_from_str(s: &str) -> Result<Self, String> {
        RuleParser::parse_conclusion(s)
    }

    pub fn try_from_string(s: String) -> Result<Self, String> {
        Self::try_from_str(&s)
    }
}

// =============================================================================
// DISPLAY (Pretty Printing)
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
            TypingJudgment::Check(term) => write!(f, "check({})", term),
            TypingJudgment::Ascription((term, ty)) => write!(f, "{} : {}", term, ty),
            TypingJudgment::Membership(var, ctx) => write!(f, "{} ∈ {}", var, ctx),
            TypingJudgment::Operation { left, op, right } => {
                write!(f, "{} {} {}", left, op, right)
            }
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
        match &self.kind {
            ConclusionKind::Type(ty) => {
                if self.context.is_empty() {
                    write!(f, "{}", ty)
                } else {
                    let input = &self.context.input;
                    match (input.is_empty(), &self.context.output) {
                        (false, Some(o)) => write!(f, "{} → {} ⊢ {}", input, o, ty),
                        (false, None) => write!(f, "{} ⊢ {}", input, ty),
                        (true, Some(o)) => write!(f, "{} → {} ⊢ {}", o.name, o, ty),
                        (true, None) => write!(f, "{}", ty),
                    }
                }
            }
            ConclusionKind::ContextLookup(ctx, var) => write!(f, "{}({})", ctx, var),
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
