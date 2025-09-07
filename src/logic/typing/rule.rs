use regex::Regex;
use super::Type;
use std::fmt; // added

/// Term representation placeholder (extend later with structured terms)
pub type Term = String;
/// A type ascription (term : type)
pub type TypeAscription = (Term, Type);

/// Typing context (setting) possibly extended with new bindings.
#[derive(Debug, Clone, PartialEq)]
pub struct TypeSetting {
    pub name: String,                 // e.g. Γ
    pub extensions: Vec<TypeAscription>, // e.g. [x:τ]
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

#[derive(Debug, Clone, PartialEq)]
pub enum Conclusion {
    Type(Type),
    ContextLookup(String, String), // (context, var) for Γ(x)
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
            .filter_map(|p| match Self::parse_premise(p) { Ok(Some(pr)) => Some(Ok(pr)), Ok(None) => None, Err(e) => Some(Err(e)) })
            .collect::<Result<Vec<_>, _>>()?;
        let conclusion = Self::parse_conclusion(&conclusion)?;
        Ok(Self { name, premises, conclusion })
    }

    /// Parse a conclusion string into a Conclusion enum
    pub fn parse_conclusion(conclusion_str: &str) -> Result<Conclusion, String> {
        let s = conclusion_str.trim();
        
        // Check for context lookup pattern: Γ(x)
        if let Some(paren_start) = s.find('(') {
            if let Some(paren_end) = s.find(')') {
                if paren_end > paren_start && paren_end == s.len() - 1 {
                    let context = s[..paren_start].trim().to_string();
                    let var = s[paren_start + 1..paren_end].trim().to_string();
                    if !context.is_empty() && !var.is_empty() {
                        return Ok(Conclusion::ContextLookup(context, var));
                    }
                }
            }
        }
        
        // Otherwise, parse as a type
        let ty = Type::parse(s)?;
        Ok(Conclusion::Type(ty))
    }



    fn parse_setting(setting_str: &str) -> Result<TypeSetting, String> {
        let setting_str = setting_str.trim();
        if !setting_str.contains('[') {
            return Ok(TypeSetting { name: setting_str.to_string(), extensions: Vec::new() });
        }
        let name_re = Regex::new(r"^\s*([^\[\s]+)\s*\[").map_err(|e| e.to_string())?;
        let name = if let Some(cap) = name_re.captures(setting_str) { cap.get(1).unwrap().as_str().to_string() } else {
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
        if parts.len() != 2 { return Err(format!("Invalid ascription, expected 'term : type', got '{}'", ascr_str)); }
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
        Ok(Some(Premise { setting, judgment: None }))
    }

    /// Pretty multiline formatting of the rule as an inference rule with indentation.
    pub fn pretty(&self, indent: usize) -> String {
        let indent_str = "  ".repeat(indent);
        let mut out = String::new();
        let conclusion_str = format!("{}", self.conclusion);
        
        if self.premises.is_empty() {
            out.push_str(&format!("{}{}  [{}]", indent_str, conclusion_str, self.name));
            return out;
        }
        
        let premise_lines: Vec<String> = self.premises.iter().map(|p| format!("{}{}", indent_str, p)).collect();
        let max_width = premise_lines.iter().map(|l| l.trim_start().len()).chain([conclusion_str.len()]).max().unwrap_or(0);
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
        match self {
            Conclusion::Type(ty) => write!(f, "{}", ty),
            Conclusion::ContextLookup(context, var) => write!(f, "{}({})", context, var),
        }
    }
}

impl fmt::Display for TypeSetting {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.extensions.is_empty() { write!(f, "{}", self.name) } else {
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
            write!(f, "[{}] {} ⇒ {}", self.name, premises.join(", "), self.conclusion)
        }
    }
}
