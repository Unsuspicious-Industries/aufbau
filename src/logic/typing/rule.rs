use super::Type;
use regex::Regex;
use std::fmt; // added

/// Term representation placeholder (extend later with structured terms)
pub type Term = String;
/// A type ascription (term : type)
pub type TypeAscription = (Term, Type);

/// Typing context (setting) possibly extended with new bindings.
#[derive(Debug, Clone, PartialEq)]
pub struct TypeSetting {
    pub name: String, // Γ usually, not supported for other things iirc
    pub extensions: Vec<TypeAscription>, //  [x:τ] extends context but locally
}

/// A typing judgment Γ ⊢ e : τ or membership x ∈ Γ
#[derive(Debug, Clone, PartialEq)]
pub enum TypingJudgment {
    Ascription(TypeAscription), // (term, type)
    Membership(String, String), // (variable, context) for x ∈ Γ
}

/// Premises in a typing rule (currently only typing judgments are supported for the "simple" phase).
#[derive(Debug, Clone, PartialEq)]
pub struct Premise {
    pub setting: Option<TypeSetting>,
    pub judgment: Option<TypingJudgment>,
}

/// Context specification for a conclusion (optional input/output context transforms)
#[derive(Debug, Clone, PartialEq, Default)]
pub struct ConclusionContext {
    pub input: String, // context variable name (previously Option<TypeSetting>)
    pub output: Option<TypeSetting>, // possibly enriched context after transformation
}

impl ConclusionContext {
    pub fn is_empty(&self) -> bool {
        self.input.is_empty() && self.output.is_none()
    }
}

/// The kind of conclusion: either a type or a context lookup Γ(x)
#[derive(Debug, Clone, PartialEq)]
pub enum ConclusionKind {
    Type(Type),
    ContextLookup(String, String), // (context, var) for Γ(x)
}

/// A conclusion consisting of an optional context transform and a concrete kind
#[derive(Debug, Clone, PartialEq)]
pub struct Conclusion {
    pub context: ConclusionContext,
    pub kind: ConclusionKind,
}

impl Conclusion {
    /// Try to convert a string to a Conclusion, returning an error if parsing fails
    pub fn try_from_str(s: &str) -> Result<Self, String> {
        TypingRule::parse_conclusion(s)
    }

    /// Try to convert a String to a Conclusion, returning an error if parsing fails
    pub fn try_from_string(s: String) -> Result<Self, String> {
        Self::try_from_str(&s)
    }
}

/// A typing rule (inference rule) with premises and a conclusion.
#[derive(Debug, Clone, PartialEq)]
pub struct TypingRule {
    pub name: String,
    pub premises: Vec<Premise>,
    pub conclusion: Conclusion,
}

impl TypingRule {
    /// Construct a rule from a comma-separated premises string and a conclusion, with a name.
    pub fn new(str_premises: String, conclusion: String, name: String) -> Result<Self, String> {
        let premises = str_premises
            .split(',')
            .map(str::trim)
            .filter(|s| !s.is_empty())
            .filter_map(|p| match Self::parse_premise(p) {
                Ok(Some(pr)) => Some(Ok(pr)),
                Ok(None) => None,
                Err(e) => {
                    println!("DEBUG: Failed to parse premise '{}': {}", p, e);
                    Some(Err(e))
                },
            })
            .collect::<Result<Vec<_>, _>>()?;
        let conclusion = match Self::parse_conclusion(&conclusion) {
            Ok(c) => c,
            Err(e) => {
                println!("DEBUG: Failed to parse conclusion '{}': {}", conclusion, e);
                return Err(e);
            }
        };
        Ok(Self {
            name,
            premises,
            conclusion,
        })
    }

    /// Get set of binding names used by this rule
    pub fn used_bindings(&self) -> std::collections::HashSet<&str> {
        let mut bindings = std::collections::HashSet::new();
        
        // Collect from premises
        for premise in &self.premises {
            // From setting extensions
            if let Some(setting) = &premise.setting {
                for (var, _) in &setting.extensions {
                    bindings.insert(var.as_str());
                }
            }
            // From judgments
            if let Some(judgment) = &premise.judgment {
                match judgment {
                    TypingJudgment::Ascription((term, _)) => {
                        bindings.insert(term.as_str());
                    }
                    TypingJudgment::Membership(var, _) => {
                        bindings.insert(var.as_str());
                    }
                }
            }
        }
        
        // Collect from conclusion
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

    /// Parse a conclusion string into a Conclusion struct
    pub fn parse_conclusion(conclusion_str: &str) -> Result<Conclusion, String> {
        let s = conclusion_str.trim();

        // 1) If it contains ⊢ then it's a (possibly context-transforming) type conclusion
        if let Some((lhs, rhs)) = s.split_once('⊢') {
            let lhs = lhs.trim();
            let rhs = rhs.trim();
            let ty = Type::parse(rhs)?;

            // Helper to parse an optional context setting (Γ or Γ[...])
            let parse_ctx = |part: &str| -> Result<Option<TypeSetting>, String> {
                let t = part.trim();
                if t.is_empty() {
                    return Ok(None);
                }
                Self::parse_setting(t).map(Some)
            };

            let mut ctx = ConclusionContext::default();
            if !lhs.is_empty() {
                if let Some((l, r)) = lhs.split_once('→') {
                    ctx.input = parse_ctx(l)?.map(|ts| ts.name).unwrap_or_default();
                    ctx.output = parse_ctx(r)?;
                } else if let Some((l, r)) = lhs.split_once("->") {
                    ctx.input = parse_ctx(l)?.map(|ts| ts.name).unwrap_or_default();
                    ctx.output = parse_ctx(r)?;
                } else {
                    // No arrow: treat as only input provided (Γ_in ⊢ τ)
                    ctx.input = parse_ctx(lhs)?.map(|ts| ts.name).unwrap_or_default();
                }
            }
            return Ok(Conclusion {
                context: ctx,
                kind: ConclusionKind::Type(ty),
            });
        }

        // 2) Check for context lookup pattern: Γ(x)
        if let Some(paren_start) = s.find('(') {
            if let Some(paren_end) = s.find(')') {
                if paren_end > paren_start && paren_end == s.len() - 1 {
                    let context = s[..paren_start].trim().to_string();
                    let var = s[paren_start + 1..paren_end].trim().to_string();
                    if !context.is_empty() && !var.is_empty() {
                        return Ok(Conclusion {
                            context: ConclusionContext::default(),
                            kind: ConclusionKind::ContextLookup(context, var),
                        });
                    }
                }
            }
        }

        // 3) Otherwise, parse as a bare type (no context transform)
        let ty = Type::parse(s)?;
        Ok(Conclusion {
            context: ConclusionContext::default(),
            kind: ConclusionKind::Type(ty),
        })
    }

    fn parse_setting(setting_str: &str) -> Result<TypeSetting, String> {
        let setting_str = setting_str.trim();
        if !setting_str.contains('[') {
            return Ok(TypeSetting {
                name: setting_str.to_string(),
                extensions: Vec::new(),
            });
        }
        let name_re = Regex::new(r"^\s*([^\[\s]+)\s*\[").map_err(|e| e.to_string())?;
        let name = if let Some(cap) = name_re.captures(setting_str) {
            cap.get(1).unwrap().as_str().to_string()
        } else {
            return Err("Invalid setting: expected a name before '[' (e.g., Γ[...])".to_string());
        };
        let re = Regex::new(r"\[([^:\]]+):([^\]]+)\]").map_err(|e| e.to_string())?;
        let mut extensions: Vec<TypeAscription> = Vec::new();
        for cap in re.captures_iter(setting_str) {
            let variable = cap[1].trim().to_string();
            let type_expr = cap[2].trim();
            let ty = Type::parse(type_expr)?; // uses syntax module
            extensions.push((variable, ty));
        }
        Ok(TypeSetting { name, extensions })
    }

    fn parse_ascription(ascr_str: &str) -> Result<TypeAscription, String> {
        let parts: Vec<&str> = ascr_str.split(':').map(str::trim).collect();
        if parts.len() != 2 {
            return Err(format!(
                "Invalid ascription, expected 'term : type', got '{}'",
                ascr_str
            ));
        }
        let term = parts[0].to_string();
        let ty = Type::parse(parts[1])?;
        Ok((term, ty))
    }

    fn parse_premise(premise_str: &str) -> Result<Option<Premise>, String> {
        let s = premise_str.trim();
        if s.is_empty() {
            return Ok(None);
        }

        // Membership judgment: x ∈ Γ
        if let Some((var_part, ctx_part)) = s.split_once('∈') {
            let var = var_part.trim().to_string();
            let ctx = ctx_part.trim().to_string();
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
            let setting = Some(Self::parse_setting(setting_part.trim())?);
            let ascription = Self::parse_ascription(ascr_part.trim())?;
            return Ok(Some(Premise {
                setting,
                judgment: Some(TypingJudgment::Ascription(ascription)),
            }));
        }

        // Premise without explicit judgment – treat as setting
        let setting = Some(Self::parse_setting(s)?);
        Ok(Some(Premise {
            setting,
            judgment: None,
        }))
    }

    /// Pretty multiline formatting of the rule as an inference rule with indentation.
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
        out.push_str(&format!("{} {}", bar, format!("[{}]", self.name)));
        out.push('\n');
        out.push_str(&format!("{}{}", indent_str, conclusion_str));
        out
    }
}

impl fmt::Display for Conclusion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            ConclusionKind::Type(ty) => {
                // Formatting helpers
                let fmt_output_ctx = |s: &TypeSetting| -> String { format!("{}", s) };

                if self.context.is_empty() {
                    return write!(f, "{}", ty);
                }
                let input = self.context.input.as_str();
                match (input.is_empty(), &self.context.output) {
                    (false, Some(o)) => write!(f, "{} -> {} ⊢ {}", input, fmt_output_ctx(o), ty),
                    (false, None) => write!(f, "{}[] ⊢ {}", input, ty), // unchanged context
                    (true, Some(o)) => write!(f, "{} -> {} ⊢ {}", o.name, fmt_output_ctx(o), ty),
                    (true, None) => write!(f, "{}", ty),
                }
            }
            ConclusionKind::ContextLookup(context, var) => write!(f, "{}({})", context, var),
        }
    }
}

impl fmt::Display for TypeSetting {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.extensions.is_empty() {
            write!(f, "{}", self.name)
        } else {
            let mut parts: Vec<String> = Vec::new();
            for (term, ty) in &self.extensions {
                parts.push(format!("{}:{}", term, ty));
            }
            write!(f, "{}[{}]", self.name, parts.join(", "))
        }
    }
}

impl fmt::Display for TypingJudgment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypingJudgment::Ascription((term, ty)) => write!(f, "{} : {}", term, ty),
            TypingJudgment::Membership(var, ctx) => write!(f, "{} ∈ {}", var, ctx),
        }
    }
}

impl fmt::Display for Premise {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match (&self.setting, &self.judgment) {
            (Some(setting), Some(judgment)) => write!(f, "{} ⊢ {}", setting, judgment),
            (Some(setting), None) => write!(f, "{}", setting),
            (None, Some(judgment)) => write!(f, "{}", judgment),
            (None, None) => Ok(()),
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
