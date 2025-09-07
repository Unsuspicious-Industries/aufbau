use std::fmt;
use super::{Type, ArraySize};

#[derive(Debug, Clone)]
pub struct TypeSyntaxConfig {
    pub empty_kw: &'static str,
    pub universe_kw: &'static str,
    pub arrow: Vec<&'static str>,
    pub union: Vec<&'static str>,
    pub intersection: Vec<&'static str>,
    pub negation: Vec<&'static str>,
    pub refinement_kw: &'static str,
    pub pointer: Vec<&'static str>,
    pub array_open: &'static str,
    pub array_close: &'static str,
}

impl Default for TypeSyntaxConfig {
    fn default() -> Self {
        Self {
            empty_kw: "∅",
            universe_kw: "⊤",
            arrow: vec!["->", "→"],
            union: vec!["∨", "v", "|"],
            intersection: vec!["∧", "^", "&"],
            negation: vec!["¬", "!"],
            refinement_kw: "where",
            pointer: vec!["*"],
            array_open: "[",
            array_close: "]",
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Atom(s) => write!(f, "{}", s),
            Type::Arrow(l, r) => write!(f, "{} → {}", l, r),
            Type::Fn { params, ret } => {
                write!(f, "(")?;
                for (i, p) in params.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", p)?;
                }
                write!(f, ") → {}", ret)
            }
            Type::Pointer(t) => write!(f, "*{}", t),
            Type::Array(t, size) => match size {
                ArraySize::Dynamic => write!(f, "{}[]", t),
                ArraySize::Const(n) => write!(f, "{}[{}]", t, n),
                ArraySize::Var(v) => write!(f, "{}[{}]", t, v),
            },
            Type::Not(t) => write!(f, "¬{}", t),
            Type::Intersection(l, r) => write!(f, "{} ∧ {}", l, r),
            Type::Union(l, r) => write!(f, "{} ∨ {}", l, r),
            Type::Refinement { base, predicate } => write!(f, "{} where {}", base, predicate),
            Type::Universe => write!(f, "⊤"),
            Type::Empty => write!(f, "∅"),
        }
    }
}

impl TypeSyntaxConfig {
    pub fn allowed_chars(&self) -> String {
        let mut chars = String::from("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_λτ₁₂₃₄₅₆₇₈₉₀ ()[],");
        for token in self.arrow.iter().chain(self.union.iter()).chain(self.intersection.iter()).chain(self.negation.iter()).chain(self.pointer.iter()) {
            for c in token.chars() { if !chars.contains(c) { chars.push(c); } }
        }
        for c in self.refinement_kw.chars() { if !chars.contains(c) { chars.push(c); } }
        for c in self.array_open.chars() { if !chars.contains(c) { chars.push(c); } }
        for c in self.array_close.chars() { if !chars.contains(c) { chars.push(c); } }
        chars
    }
}

impl Type {
    pub fn parse(type_str: &str) -> Result<Self, String> { Self::parse_with_config(type_str, &TypeSyntaxConfig::default()) }

    pub fn parse_with_config(type_str: &str, cfg: &TypeSyntaxConfig) -> Result<Self, String> {
        let s = type_str.trim();
        if s.is_empty() { return Err("Type expression cannot be empty".into()); }
        if s == cfg.universe_kw  { return Ok(Type::Universe); }
        if s == cfg.empty_kw { return Ok(Type::Empty); }
        if s.starts_with('(') && s.ends_with(')') && is_outer_paren_pair(s) { return Self::parse_with_config(&s[1..s.len()-1], cfg); }
        
        // Parse array types (e.g., "int[10]", "int[]", or "int[N]")
        if let Some(open_bracket) = s.rfind(cfg.array_open) {
            if s.ends_with(cfg.array_close) {
                let base_type = s[..open_bracket].trim();
                let size_str = s[open_bracket+1..s.len()-1].trim();
                let base = Self::parse_with_config(base_type, cfg)?;
                let size = if size_str.is_empty() {
                    ArraySize::Dynamic
                } else if let Ok(n) = size_str.parse::<u64>() {
                    ArraySize::Const(n)
                } else if size_str.chars().all(|c| c.is_alphanumeric() || c == '_') {
                    ArraySize::Var(size_str.to_string())
                } else {
                    return Err(format!("Invalid array size: {}", size_str));
                };
                return Ok(Type::Array(Box::new(base), size));
            }
        }
        
        if let Some((pos, tok_len)) = find_last_outside_parens(s, &cfg.arrow) { return Ok(Type::Arrow(Box::new(Self::parse_with_config(&s[..pos], cfg)?), Box::new(Self::parse_with_config(&s[pos+tok_len..], cfg)?))); }
        
        // Parse pointer types (e.g., "*int", "*char")
        if let Some(tok) = cfg.pointer.iter().find(|t| s.starts_with(**t)) { 
            return Ok(Type::Pointer(Box::new(Self::parse_with_config(&s[tok.len()..], cfg)?))); 
        }
        
        if let Some(tok) = cfg.negation.iter().find(|t| s.starts_with(**t)) { return Ok(Type::Not(Box::new(Self::parse_with_config(&s[tok.len()..], cfg)?))); }
        if let Some((pos, tok_len)) = find_first_outside_parens(s, &cfg.intersection) { return Ok(Type::Intersection(Box::new(Self::parse_with_config(&s[..pos], cfg)?), Box::new(Self::parse_with_config(&s[pos+tok_len..], cfg)?))); }
        if let Some((pos, tok_len)) = find_first_outside_parens(s, &cfg.union) { return Ok(Type::Union(Box::new(Self::parse_with_config(&s[..pos], cfg)?), Box::new(Self::parse_with_config(&s[pos+tok_len..], cfg)?))); }
        if let Some((pos, _)) = find_keyword_outside_parens(s, cfg.refinement_kw) { let base = &s[..pos]; let predicate = &s[pos+cfg.refinement_kw.len()..].trim(); return Ok(Type::Refinement { base: Box::new(Self::parse_with_config(base, cfg)?), predicate: predicate.to_string() }); }
        if s.chars().all(|c| c.is_alphanumeric() || c == '_') { return Ok(Type::Atom(s.to_string())); }
        Err(format!("Invalid type expression: {}", s))
    }
}

const TYPE_CHARS: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_λτ→₁₂₃₄₅₆₇₈₉₀ ∧∨()!¬*[] where,";
pub fn validate_type_expr(expr: &str) -> bool { !expr.is_empty() && expr.chars().all(|c| TYPE_CHARS.contains(c)) }

fn is_outer_paren_pair(s: &str) -> bool {
    if !s.starts_with('(') || !s.ends_with(')') { return false; }
    let mut depth = 0; for (i, c) in s.chars().enumerate() { match c { '(' => depth += 1, ')' => { depth -= 1; if depth == 0 && i != s.len()-1 { return false; } }, _ => {} } } depth == 0 }

fn find_last_outside_parens(s: &str, tokens: &[&str]) -> Option<(usize, usize)> {
    let mut depth = 0;
    let mut last: Option<(usize, usize)> = None;
    for (i, c) in s.char_indices() {
        match c {
            '(' => depth += 1,
            ')' if depth > 0 => depth -= 1,
            _ if depth == 0 => {
                for tok in tokens {
                    if s[i..].starts_with(tok) {
                        last = Some((i, tok.len()));
                    }
                }
            }
            _ => {}
        }
    }
    last
}

fn find_first_outside_parens(s: &str, tokens: &[&str]) -> Option<(usize, usize)> {
    let mut depth = 0;
    for (i, c) in s.char_indices() {
        match c {
            '(' => depth += 1,
            ')' if depth > 0 => depth -= 1,
            _ if depth == 0 => {
                for tok in tokens {
                    if s[i..].starts_with(tok) {
                        return Some((i, tok.len()));
                    }
                }
            }
            _ => {}
        }
    }
    None
}

fn find_keyword_outside_parens(s: &str, kw: &str) -> Option<(usize, usize)> {
    let mut depth = 0;
    for (i, c) in s.char_indices() {
        match c {
            '(' => depth += 1,
            ')' if depth > 0 => depth -= 1,
            _ if depth == 0 && s[i..].starts_with(kw) => return Some((i, kw.len())),
            _ => {}
        }
    }
    None
}

