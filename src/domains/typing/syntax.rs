//! Type and `TypeExpr` parsing — matching §3 of the draft.
//!
//! # Two parsers
//!
//! - `Type::parse` / `Type::parse_raw` — closed types only (Raw, Arrow, Union,
//!   Not, Any, None, Partial).
//! - `TypeExpr::parse` — the full constraint-language expression including
//!   Meta (`?A`), `ContextExt` (`Γ(x)`), and `TypeOf` (`typeof(b)`).
//!
//! # Partial parsing
//!
//! `Type::parse_partial` remains for incremental type-annotation input
//! (regex-derivative based, §3 Lemma 1).

use super::{Type, TypeExpr};

// ============================================================================
// Constants
// ============================================================================

const NONE_KW: &str = "∅";
const ANY_KW: &str = "⊤";
const ARROW_TOKENS: &[&str; 3] = &["->", "=>", "→"];
const NEGATION_TOKENS: &[&str; 2] = &["¬", "!"];

// ============================================================================
// Type parsing (closed types only)
// ============================================================================

impl Type {
    pub fn parse(type_str: &str) -> Result<Self, String> {
        Self::parse_impl(type_str, false)
    }

    pub fn parse_raw(type_str: &str) -> Result<Self, String> {
        Self::parse_impl(type_str, true)
    }

    pub fn parse_partial(type_str: &str) -> Result<Type, String> {
        let trimmed = type_str.trim();
        if trimmed.is_empty() {
            return Ok(Type::Partial(Box::new(Type::Any), type_str.to_string()));
        }
        match Self::parse(trimmed) {
            Ok(ty) => {
                if Self::is_incomplete(&ty, trimmed) {
                    return Ok(Type::Partial(Box::new(ty), type_str.to_string()));
                }
                Ok(ty)
            }
            Err(_) => Self::analyze_partial(trimmed, type_str),
        }
    }

    fn is_incomplete(ty: &Type, input: &str) -> bool {
        let ends_with_arrow = ARROW_TOKENS
            .iter()
            .any(|&arrow| input.trim_end().ends_with(arrow));
        if ends_with_arrow && Self::has_rightmost_any(ty) {
            return true;
        }
        let is_just_negation = NEGATION_TOKENS.iter().any(|&neg| input.trim() == neg);
        if is_just_negation && matches!(ty, Type::Not(_)) {
            return true;
        }
        false
    }

    fn has_rightmost_any(ty: &Type) -> bool {
        match ty {
            Type::Arrow(_, right) => Self::has_rightmost_any(right),
            Type::Any => true,
            _ => false,
        }
    }

    fn analyze_partial(s: &str, original_input: &str) -> Result<Type, String> {
        if s.starts_with('\'') && !s.ends_with('\'') {
            let content = s.trim_start_matches('\'');
            return Ok(Type::Partial(
                Box::new(Type::Raw(content.to_string())),
                original_input.to_string(),
            ));
        }
        if let Some(&tok) = NEGATION_TOKENS.iter().find(|t| s.starts_with(**t)) {
            let rest = s[tok.len()..].trim_start();
            if rest.is_empty() {
                return Ok(Type::Partial(
                    Box::new(Type::Not(Box::new(Type::Any))),
                    original_input.to_string(),
                ));
            }
            if let Ok(sub) = Type::parse(rest) {
                return Ok(Type::Not(Box::new(sub)));
            }
            if let Ok(Type::Partial(pt, _)) = Self::analyze_partial(rest, original_input) {
                return Ok(Type::Partial(
                    Box::new(Type::Not(pt)),
                    original_input.to_string(),
                ));
            }
        }
        if let Some(inner) = s.strip_prefix('(') {
            if let Ok(inner_ty) = Type::parse(inner.trim_end_matches(')')) {
                return Ok(Type::Partial(
                    Box::new(inner_ty),
                    original_input.to_string(),
                ));
            }
            if let Ok(Type::Partial(pt, _)) = Self::analyze_partial(inner, original_input) {
                return Ok(Type::Partial(pt, original_input.to_string()));
            }
        }
        if let Some((pos, tok_len)) = find_first_outside_parens(s, &ARROW_TOKENS[..]) {
            let left_str = s[..pos].trim();
            let right_str = s[pos + tok_len..].trim_start();
            if left_str.is_empty() {
                return Err("Left side of arrow missing".into());
            }
            let left = Type::parse(left_str)?;
            if right_str.is_empty() {
                return Ok(Type::Partial(
                    Box::new(Type::Arrow(Box::new(left), Box::new(Type::Any))),
                    original_input.to_string(),
                ));
            }
            if let Ok(right_ty) = Type::parse(right_str) {
                return Ok(Type::Arrow(Box::new(left), Box::new(right_ty)));
            }
            if let Ok(Type::Partial(pt, _)) = Self::analyze_partial(right_str, original_input) {
                return Ok(Type::Partial(
                    Box::new(Type::Arrow(Box::new(left), pt)),
                    original_input.to_string(),
                ));
            }
        }
        if s.chars()
            .all(|c| c.is_alphanumeric() || c == '_' || c == '?')
        {
            let name = s.trim_start_matches('?').to_string();
            return Ok(Type::Partial(
                Box::new(Type::Raw(name)),
                original_input.to_string(),
            ));
        }
        Err(format!("Cannot parse as partial type: {s}"))
    }

    pub(crate) fn parse_impl(type_str: &str, raw_mode: bool) -> Result<Self, String> {
        let s = type_str.trim();
        if s.is_empty() {
            return Ok(Type::Any);
        }
        if s == ANY_KW {
            return Ok(Type::Any);
        }
        if s == NONE_KW {
            return Ok(Type::None);
        }

        if s.starts_with('?') {
            return Err(format!(
                "'?' meta-variables are not valid closed types: '{s}'"
            ));
        }
        if let Some(pos) = s.find('(')
            && s[..pos].trim() == "typeof" {
                return Err(format!(
                    "typeof is a type expression, not a closed type: '{s}'"
                ));
            }

        if is_single_quoted_raw_literal(s) {
            return Ok(Type::Raw(s[1..s.len() - 1].to_string()));
        }

        if let Some(parts) = split_top_level_union(s) {
            let members: Result<Vec<Type>, String> = parts
                .into_iter()
                .map(|part| Self::parse_impl(part.trim(), raw_mode))
                .collect();
            return Ok(Type::Union(flatten_unions(members?)));
        }

        if let Some(inner_suffix) = s.strip_prefix('(') {
            let depth = missing_closing_parens(s)?;
            if depth > 0 {
                let inner = Self::parse_impl(inner_suffix, raw_mode)?;
                return Ok(Type::Partial(Box::new(inner), s.to_string()));
            }
            let mut d: isize = 0;
            let mut wrapper_ends_at: Option<usize> = None;
            for (i, c) in s.char_indices() {
                match c {
                    '(' => d += 1,
                    ')' => {
                        d -= 1;
                        if d == 0 {
                            wrapper_ends_at = Some(i);
                            break;
                        }
                    }
                    _ => {}
                }
            }
            if let Some(end) = wrapper_ends_at
                && end == s.len() - 1
            {
                return Self::parse_impl(&s[1..s.len() - 1], raw_mode);
            }
        }

        if let Some((pos, tok_len)) = find_first_outside_parens(s, &ARROW_TOKENS[..]) {
            let right = Self::parse_impl(&s[pos + tok_len..], raw_mode)?;
            if let Some(params) = parse_parenthesized_type_list(&s[..pos], raw_mode)? {
                return Ok(arrow_from_params(params, right));
            }
            return Ok(Type::Arrow(
                Box::new(Self::parse_impl(&s[..pos], raw_mode)?),
                Box::new(right),
            ));
        }

        if let Some(&tok) = NEGATION_TOKENS.iter().find(|t| s.starts_with(**t)) {
            return Ok(Type::Not(Box::new(Self::parse_impl(
                &s[tok.len()..],
                raw_mode,
            )?)));
        }

        if s.chars()
            .all(|c| c.is_alphanumeric() || c == '_' || c == '?')
        {
            if raw_mode {
                return Ok(Type::Raw(s.to_string()));
            }
            return Ok(Type::Raw(s.to_string()));
        }

        if raw_mode {
            return Ok(Type::Raw(s.to_string()));
        }
        Self::analyze_partial(s, type_str)
    }
}

// ============================================================================
// TypeExpr parsing (full constraint language)
// ============================================================================

impl TypeExpr {
    pub fn parse(type_str: &str) -> Result<Self, String> {
        Self::parse_impl(type_str)
    }

    fn parse_impl(s_: &str) -> Result<Self, String> {
        let s = s_.trim();
        if s.is_empty() {
            return Ok(TypeExpr::Any);
        }
        if s == ANY_KW {
            return Ok(TypeExpr::Any);
        }
        if s == NONE_KW {
            return Ok(TypeExpr::None);
        }

        // typeof(b) — first `)` after `typeof(` is the matching close
        if let Some(inner) = s.strip_prefix("typeof(")
            && let Some(pos) = inner.find(')') {
                let binding = inner[..pos].trim();
                if !binding.is_empty() {
                    return Ok(TypeExpr::TypeOf(binding.to_string()));
                }
                return Err("typeof requires a binding name".into());
            }

        // Γ(x) — must have matching parens and valid ctx name; only when no arrow/union
        if let Some(paren_start) = s.find('(')
            && let Some(paren_end) = s.find(')')
            && paren_end > paren_start
            && paren_end == s.len() - 1
        {
            let prefix = s[..paren_start].trim();
            let var = s[paren_start + 1..paren_end].trim();
            if is_ctx_name(prefix) && !var.is_empty() {
                return Ok(TypeExpr::ContextExt(var.to_string()));
            }
        }

        // 'lit'
        if is_single_quoted_raw_literal(s) {
            return Ok(TypeExpr::Lit(s[1..s.len() - 1].to_string()));
        }

        // Union (checks after literals/ctx, before arrows/negation)
        if let Some(parts) = split_top_level_union(s) {
            let members: Result<Vec<TypeExpr>, String> = parts
                .into_iter()
                .map(|part| Self::parse_impl(part.trim()))
                .collect();
            return Ok(TypeExpr::Union(members?));
        }

        // Parentheses
        if let Some(_inner_suffix) = s.strip_prefix('(')
            && let Some(end) = find_matching_paren(s)
                && end == s.len() - 1 {
                    return Self::parse_impl(&s[1..s.len() - 1]);
                }

        // Arrow (RIGHT-associative) — check BEFORE ? meta prefix
        if let Some((pos, tok_len)) = find_first_outside_parens(s, &ARROW_TOKENS[..]) {
            let left = Self::parse_impl(&s[..pos])?;
            let right = Self::parse_impl(&s[pos + tok_len..])?;
            return Ok(TypeExpr::Arrow(Box::new(left), Box::new(right)));
        }

        // Negation
        if let Some(&tok) = NEGATION_TOKENS.iter().find(|t| s.starts_with(**t)) {
            return Ok(TypeExpr::Not(Box::new(Self::parse_impl(&s[tok.len()..])?)));
        }

        // ?A — checked LAST since metas are the most ambiguous form
        if let Some(rest) = s.strip_prefix('?') {
            if rest.is_empty() {
                return Err("empty meta variable".into());
            }
            if rest
                .chars()
                .all(|c| c.is_alphanumeric() || c == '_' || c.is_numeric())
            {
                return Ok(TypeExpr::Meta(rest.to_string()));
            }
            return Err(format!("invalid meta variable: {s_}"));
        }

        // Bare unquoted identifier → TypeOf(binding)
        if s.chars().all(|c| {
            c.is_alphanumeric() || c == '_' || c.is_numeric() || "τ₁₂₃₄₅₆₇₈₉₀αβγδ".contains(c)
        }) {
            return Ok(TypeExpr::TypeOf(s.to_string()));
        }

        Err(format!("Cannot parse type expression: '{s_}'"))
    }
}

// ============================================================================
// Shared helpers
// ============================================================================

fn is_ctx_name(s: &str) -> bool {
    !s.is_empty()
        && s.chars().all(|c| {
            c.is_alphanumeric() || c == '_' || "ΓΔΘΛΣΦΨΩΞΠΡΤΥΧδγτλσφψωξπρυχ₁₂₃₄₅₆₇₈₉₀".contains(c)
        })
}

fn find_matching_paren(s: &str) -> Option<usize> {
    find_matching_paren_from(s, 0)
}

fn find_matching_paren_from(s: &str, start: usize) -> Option<usize> {
    let mut d: isize = 0;
    for (i, c) in s.char_indices() {
        if i < start {
            continue;
        }
        match c {
            '(' => d += 1,
            ')' => {
                d -= 1;
                if d == 0 {
                    return Some(i);
                }
            }
            _ => {}
        }
    }
    None
}

fn missing_closing_parens(s: &str) -> Result<usize, String> {
    if !s.starts_with('(') {
        return Err(format!("Missing opening parenthesis: {s}"));
    }
    let mut depth: isize = 0;
    for c in s.chars() {
        match c {
            '(' => depth += 1,
            ')' => depth -= 1,
            _ => {}
        }
    }
    if depth < 0 {
        Err(format!("Too many closing parentheses: {s}"))
    } else {
        Ok(depth as usize)
    }
}

fn find_first_outside_parens(s: &str, tokens: &[&str]) -> Option<(usize, usize)> {
    let mut depth = 0;
    for (i, c) in s.char_indices() {
        match c {
            '(' | '{' | '[' => depth += 1,
            ')' | '}' | ']' if depth > 0 => depth -= 1,
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

fn is_single_quoted_raw_literal(s: &str) -> bool {
    s.len() > 2 && s.starts_with('\'') && s.ends_with('\'') && !s[1..s.len() - 1].contains('\'')
}

fn split_top_level_union(s: &str) -> Option<Vec<&str>> {
    split_top_level_on(s, '|')
}

fn split_top_level_on(s: &str, sep: char) -> Option<Vec<&str>> {
    let mut depth = 0isize;
    let mut starts = vec![0usize];
    let mut found = false;
    for (i, c) in s.char_indices() {
        match c {
            '(' | '{' | '[' => depth += 1,
            ')' | '}' | ']' if depth > 0 => depth -= 1,
            _ if c == sep && depth == 0 => {
                found = true;
                starts.push(i + 1);
            }
            _ => {}
        }
    }
    if !found {
        return None;
    }
    let mut parts = Vec::with_capacity(starts.len());
    for idx in 0..starts.len() {
        let start = starts[idx];
        let end = if idx + 1 < starts.len() {
            starts[idx + 1] - 1
        } else {
            s.len()
        };
        parts.push(&s[start..end]);
    }
    Some(parts)
}

fn parse_parenthesized_type_list(s: &str, raw_mode: bool) -> Result<Option<Vec<Type>>, String> {
    let trimmed = s.trim();
    if !trimmed.starts_with('(') || !trimmed.ends_with(')') {
        return Ok(None);
    }
    let inner = &trimmed[1..trimmed.len() - 1];
    if inner.trim().is_empty() {
        return Ok(Some(vec![Type::Raw("void".to_string())]));
    }
    let parts = split_top_level_commas(inner);
    if parts.len() <= 1 {
        return Ok(None);
    }
    parts
        .into_iter()
        .map(|part| Type::parse_impl(part.trim(), raw_mode))
        .collect::<Result<Vec<_>, _>>()
        .map(Some)
}

fn split_top_level_commas(s: &str) -> Vec<&str> {
    let mut depth = 0isize;
    let mut starts = vec![0usize];
    for (i, c) in s.char_indices() {
        match c {
            '(' | '{' | '[' => depth += 1,
            ')' | '}' | ']' if depth > 0 => depth -= 1,
            ',' if depth == 0 => starts.push(i + 1),
            _ => {}
        }
    }
    let mut parts = Vec::with_capacity(starts.len());
    for idx in 0..starts.len() {
        let start = starts[idx];
        let end = if idx + 1 < starts.len() {
            starts[idx + 1] - 1
        } else {
            s.len()
        };
        parts.push(&s[start..end]);
    }
    parts
}

fn arrow_from_params(params: Vec<Type>, ret: Type) -> Type {
    params.into_iter().rev().fold(ret, |acc, param| {
        Type::Arrow(Box::new(param), Box::new(acc))
    })
}

fn flatten_unions(members: Vec<Type>) -> Vec<Type> {
    let mut flat = Vec::new();
    for t in members {
        match t {
            Type::Union(nested) => flat.extend(nested),
            other => flat.push(other),
        }
    }
    flat
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use crate::domains::typing::{Type, TypeExpr};

    #[test]
    fn closed_type_parses_raw_literals() {
        let t = Type::parse("'Int'").unwrap();
        assert_eq!(t, Type::Raw("Int".into()));
    }
    #[test]
    fn closed_type_parses_arrows() {
        let t = Type::parse("'Int' -> 'Bool'").unwrap();
        assert_eq!(
            t,
            Type::Arrow(
                Box::new(Type::Raw("Int".into())),
                Box::new(Type::Raw("Bool".into()))
            )
        );
    }
    #[test]
    fn closed_type_arrow_right_associative() {
        let t = Type::parse("'A' -> 'B' -> 'C'").unwrap();
        match &t {
            Type::Arrow(left, right) => {
                assert_eq!(**left, Type::Raw("A".into()));
                assert!(matches!(**right, Type::Arrow(_, _)));
            }
            _ => panic!(),
        }
    }
    #[test]
    fn closed_type_parses_unions() {
        let t = Type::parse("'Int' | 'Bool'").unwrap();
        match t {
            Type::Union(parts) => {
                assert_eq!(parts.len(), 2);
            }
            other => panic!("{:?}", other),
        }
    }
    #[test]
    fn closed_type_parses_negation() {
        assert!(matches!(Type::parse("¬'Int'").unwrap(), Type::Not(_)));
    }
    #[test]
    fn closed_type_parses_any_and_none() {
        assert_eq!(Type::parse("⊤").unwrap(), Type::Any);
        assert_eq!(Type::parse("∅").unwrap(), Type::None);
    }
    #[test]
    fn closed_type_rejects_meta() {
        assert!(Type::parse("?A").is_err());
    }
    #[test]
    fn closed_type_rejects_typeof() {
        assert!(Type::parse("typeof(x)").is_err());
    }

    #[test]
    fn type_expr_parses_meta() {
        assert_eq!(TypeExpr::parse("?A").unwrap(), TypeExpr::Meta("A".into()));
    }
    #[test]
    fn type_expr_parses_typeof() {
        assert_eq!(
            TypeExpr::parse("typeof(x)").unwrap(),
            TypeExpr::TypeOf("x".into())
        );
    }
    #[test]
    fn type_expr_parses_ctx() {
        assert_eq!(
            TypeExpr::parse("Γ(x)").unwrap(),
            TypeExpr::ContextExt("x".into())
        );
    }
    #[test]
    fn type_expr_parses_arrow_metas() {
        assert_eq!(
            TypeExpr::parse("?A -> ?B").unwrap(),
            TypeExpr::Arrow(
                Box::new(TypeExpr::Meta("A".into())),
                Box::new(TypeExpr::Meta("B".into()))
            )
        );
    }
    #[test]
    fn type_expr_parses_lit() {
        assert_eq!(
            TypeExpr::parse("'Int'").unwrap(),
            TypeExpr::Lit("Int".into())
        );
    }

    #[test]
    fn type_expr_roundtrip_meta() {
        let o = TypeExpr::Meta("A".into());
        assert_eq!(TypeExpr::parse(&o.to_string()).unwrap(), o);
    }
    #[test]
    fn type_expr_roundtrip_arrow() {
        let o = TypeExpr::Arrow(
            Box::new(TypeExpr::Meta("A".into())),
            Box::new(TypeExpr::Meta("B".into())),
        );
        assert_eq!(TypeExpr::parse(&o.to_string()).unwrap(), o);
    }

    #[test]
    fn type_parse_raw_bare_identifiers() {
        assert_eq!(
            Type::parse_raw("number").unwrap(),
            Type::Raw("number".into())
        );
    }
    #[test]
    fn multi_arg_fn_type_curried() {
        let t = Type::parse_raw("(number, string) => boolean").unwrap();
        assert_eq!(
            t,
            Type::Arrow(
                Box::new(Type::Raw("number".into())),
                Box::new(Type::Arrow(
                    Box::new(Type::Raw("string".into())),
                    Box::new(Type::Raw("boolean".into()))
                ))
            )
        );
    }
    #[test]
    fn type_expr_roundtrip_typeof() {
        let o = TypeExpr::TypeOf("x".into());
        assert_eq!(TypeExpr::parse(&o.to_string()).unwrap(), o);
    }
    #[test]
    fn type_expr_roundtrip_ctx() {
        let o = TypeExpr::ContextExt("x".into());
        assert_eq!(TypeExpr::parse(&o.to_string()).unwrap(), o);
    }
}
