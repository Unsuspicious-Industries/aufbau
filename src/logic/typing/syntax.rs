use super::Type;
use std::fmt;

// ============================================================================
// Core type syntax constants
// ============================================================================
// The type language supports:
//   - Atoms: alphanumeric identifiers (treated as type variables)
//   - Raw types: quoted literals like 'int', 'string' (concrete base types)
//   - Arrows: ->, =>, or → (function types, right-associative)
//   - Arrays and structural objects: T[], { x: T }
//   - Negation: ¬ or ! (complement types)
//   - Any: ⊤ (top type, accepts everything)
//   - None: ∅ (bottom type, rejects everything)
//   - Context calls: Γ(x) (lookup variable x in context Γ)
//   - Parens: (τ) for grouping
// ============================================================================

const NONE_KW: &str = "∅";
const ANY_KW: &str = "⊤";
const ARROW_TOKENS: &[&str; 3] = &["->", "=>", "→"];
const NEGATION_TOKENS: &[&str; 2] = &["¬", "!"];

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Meta(s) => write!(f, "{}", s),
            Type::Raw(s) => write!(f, "'{}'", s),
            Type::Arrow(l, r) => write!(f, "{} → {}", l, r),
            Type::Array(inner) => write!(f, "{}[]", inner),
            Type::Object(fields) => {
                let rendered: Vec<String> = fields
                    .iter()
                    .map(|(name, ty)| format!("{}: {}", name, ty))
                    .collect();
                write!(f, "{{{}}}", rendered.join(", "))
            }
            Type::ObjectExtend(name, ty, rest) => {
                write!(f, "{{{}: {}, ...{}}}", name, ty, rest)
            }
            Type::Union(items) => {
                let rendered: Vec<String> = items.iter().map(|t| format!("{}", t)).collect();
                write!(f, "{}", rendered.join(" | "))
            }
            Type::Not(t) => write!(f, "¬{}", t),
            Type::ContextCall(_ctx, var) => write!(f, "lookup({})", var),
            Type::Any => write!(f, "⊤"),
            Type::None => write!(f, "∅"),

            // Internal types: Path, PathOf, and Partial are implementation-level
            // types that should not appear in user-facing output. These Display
            // impls are placeholders; see src/notes.md §11.
            Type::Path(p) => write!(
                f,
                "{}",
                p.iter().map(|s| format!("{}.", s)).collect::<String>()
            ),
            Type::PathOf(t, p) => write!(
                f,
                "{} => typeof({})",
                t,
                p.iter().map(|s| format!("{}.", s)).collect::<String>()
            ),
            Type::Partial(t, _input) => write!(f, "{}", t),
        }
    }
}

// Type parsing implementation
impl Type {
    // Public API: parse a type expression with default syntax (atoms parsed as Atom).
    pub fn parse(type_str: &str) -> Result<Self, String> {
        Self::parse_impl(type_str, false)
    }

    // Parse with raw mode: atoms default to Raw instead of Atom.
    pub fn parse_raw(type_str: &str) -> Result<Self, String> {
        Self::parse_impl(type_str, true)
    }

    // ================================================================================
    // Partial type parser: returns Partial(type, original_input)
    // ================================================================================

    pub fn parse_partial(type_str: &str) -> Result<Type, String> {
        let trimmed = type_str.trim();

        // Empty input → partial Any with empty input string
        if trimmed.is_empty() {
            return Ok(Type::Partial(Box::new(Type::Any), type_str.to_string()));
        }

        // Try full parse first
        match Self::parse(trimmed) {
            Ok(ty) => {
                // Check if the parsed type represents an incomplete expression
                if Self::is_incomplete(&ty, trimmed) {
                    // Treat as partial even though parse succeeded
                    return Ok(Type::Partial(Box::new(ty), type_str.to_string()));
                }
                // Otherwise return the complete parse
                Ok(ty)
            }
            Err(_) => Self::analyze_partial(trimmed, type_str),
        }
    }

    /// Check if a successfully parsed type represents an incomplete expression
    fn is_incomplete(ty: &Type, input: &str) -> bool {
        // Case 1: Input ends with arrow operator and rightmost type is Any
        let ends_with_arrow = ARROW_TOKENS
            .iter()
            .any(|&arrow| input.trim_end().ends_with(arrow));

        if ends_with_arrow && Self::has_rightmost_any(ty) {
            return true;
        }

        // Case 2: Input is just a negation operator (like "¬")
        let is_just_negation = NEGATION_TOKENS.iter().any(|&neg| input.trim() == neg);

        if is_just_negation && matches!(ty, Type::Not(_)) {
            return true;
        }

        false
    }

    /// Check if the rightmost type in an arrow chain is Any
    fn has_rightmost_any(ty: &Type) -> bool {
        match ty {
            Type::Arrow(_, right) => {
                // For arrows, check the right side recursively
                Self::has_rightmost_any(right)
            }
            Type::Any => true,
            _ => false,
        }
    }

    /// Core partial analysis dispatcher.
    fn analyze_partial(s: &str, original_input: &str) -> Result<Type, String> {
        // Case: raw literal starting but not closed
        if s.starts_with('\'') && !s.ends_with('\'') {
            let content = s.trim_start_matches('\'');
            return Ok(Type::Partial(
                Box::new(Type::Raw(content.to_string())),
                original_input.to_string(),
            ));
        }

        // Case: negation prefix
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

            if let Ok(Type::Partial(pt, _input)) = Self::analyze_partial(rest, original_input) {
                return Ok(Type::Partial(
                    Box::new(Type::Not(pt)),
                    original_input.to_string(),
                ));
            }
        }

        // Case: parentheses, possibly unbalanced
        if let Some(inner) = s.strip_prefix('(') {
            // fully balanced but parse failed -> treat as inner partial
            if let Ok(inner_ty) = Type::parse(inner.trim_end_matches(')')) {
                return Ok(Type::Partial(
                    Box::new(inner_ty),
                    original_input.to_string(),
                ));
            }

            if let Ok(Type::Partial(pt, _input)) = Self::analyze_partial(inner, original_input) {
                return Ok(Type::Partial(pt, original_input.to_string()));
            }
        }

        // Case: arrow outside parens
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

            if let Ok(Type::Partial(pt, _input)) = Self::analyze_partial(right_str, original_input)
            {
                return Ok(Type::Partial(
                    Box::new(Type::Arrow(Box::new(left), pt)),
                    original_input.to_string(),
                ));
            }
        }

        // Case: partial operator (prefix of arrow)
        for &op in ARROW_TOKENS {
            // Iterate only over valid UTF-8 boundaries for the operator token.
            // (Important for unicode tokens like "→"; op.len() is bytes, not chars.)
            let mut boundaries: Vec<usize> = op.char_indices().map(|(i, _)| i).collect();
            boundaries.push(op.len());

            for w in boundaries.windows(2) {
                let prefix_len = w[1];
                if prefix_len == op.len() {
                    continue; // full token handled by arrow case
                }
                let prefix = &op[..prefix_len];
                if s.trim_end().ends_with(prefix) {
                    let left_str = s[..s.len() - prefix_len].trim();
                    if let Ok(left) = Type::parse(left_str) {
                        return Ok(Type::Partial(
                            Box::new(Type::Arrow(Box::new(left), Box::new(Type::Any))),
                            original_input.to_string(),
                        ));
                    }
                }
            }
        }

        // Case: context call missing closing paren
        if let Some(paren_pos) = s.find('(') {
            let ctx = s[..paren_pos].trim();
            let var_part = s[paren_pos + 1..].trim();
            if !ctx.is_empty() && !s.contains(')') {
                return Ok(Type::Partial(
                    Box::new(Type::ContextCall(ctx.to_string(), var_part.to_string())),
                    original_input.to_string(),
                ));
            }
        }

        // Case: identifier / atom that can extend (default mode treats as Atom, not Raw)
        if s.chars()
            .all(|c| c.is_alphanumeric() || c == '_' || c == '?')
        {
            // If this is a meta variable (starts with '?'), store it as Meta.
            // Otherwise it's a normal Atom.
            let ty = if let Some(rest) = s.strip_prefix('?') {
                Type::Meta(rest.to_string())
            } else {
                Type::Meta(s.to_string())
            };
            return Ok(Type::Partial(Box::new(ty), original_input.to_string()));
        }

        Err(format!("Cannot parse as partial type: {}", s))
    }

    pub fn parse_impl(type_str: &str, raw_mode: bool) -> Result<Self, String> {
        let s = type_str.trim();
        if s.is_empty() {
            // Empty type expression represents a partial universe (Any)
            return Ok(Type::Any);
        } else if s == ANY_KW {
            return Ok(Type::Any);
        }
        if s == NONE_KW {
            return Ok(Type::None);
        }

        if is_single_quoted_raw_literal(s) {
            let raw_type = &s[1..s.len() - 1]; // Remove quotes
            return Ok(Type::Raw(raw_type.to_string()));
        }

        // Union types have lower precedence than arrows:
        // A -> B | C == (A -> B) | C
        if let Some(parts) = split_top_level_union(s) {
            let members: Result<Vec<Type>, String> = parts
                .into_iter()
                .map(|part| Self::parse_impl(part.trim(), raw_mode))
                .collect();
            let members = members?;
            return Ok(Type::Union(flatten_unions(members)));
        }

        // Parentheses: only peel a wrapping pair if it encloses the *entire* expression.
        // Otherwise, leave it to the arrow/context-call parsing logic below.
        if let Some(inner_suffix) = s.strip_prefix('(') {
            let depth = missing_closing_parens(s)?;
            if depth > 0 {
                // Incomplete parens → partial type expecting a closing ')'
                let inner = Self::parse_impl(inner_suffix, raw_mode)?;
                if let Self::Partial(p, _d) = inner {
                    return Ok(Self::Partial(p, s.to_string()));
                }
                return Ok(Self::Partial(Box::new(inner), s.to_string()));
            }

            // depth == 0, so parens are balanced. Only strip if the first '(' matches
            // the final ')' (i.e. it's a top-level wrapper).
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
            // else: not a full wrapper, fall through
        }

        // Arrow types are RIGHT-associative: A -> B -> C  ==  A -> (B -> C)
        // So we split on the FIRST arrow outside parens
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

        if let Some(inner) = strip_array_suffix(s) {
            return Ok(Type::Array(Box::new(Self::parse_impl(inner, raw_mode)?)));
        }

        if let Some(object) = parse_object_type(s, raw_mode)? {
            return Ok(object);
        }

        if let Some(&tok) = NEGATION_TOKENS.iter().find(|t| s.starts_with(**t)) {
            return Ok(Type::Not(Box::new(Self::parse_impl(
                &s[tok.len()..],
                raw_mode,
            )?)));
        }

        // Parse context calls "Γ(x)", "(y)"
        if let Some(paren_start) = s.find('(')
            && let Some(paren_end) = s.find(')')
            && paren_end > paren_start
            && paren_end == s.len() - 1
        {
            let context = s[..paren_start].trim();
            let var = s[paren_start + 1..paren_end].trim();
            if !context.is_empty() && !var.is_empty() {
                // Validate context name contains only valid characters
                if context.chars().all(|c| {
                    c.is_alphanumeric()
                        || c == '_'
                        || "ΓΔΘΛΣΦΨΩΞΠΡΤΥΧδγτλσφψωξπρυχ₁₂₃₄₅₆₇₈₉₀".contains(c)
                }) {
                    return Ok(Type::ContextCall(context.to_string(), var.to_string()));
                }
            }
        }

        if s.chars()
            .all(|c| c.is_alphanumeric() || c == '_' || c == '?')
        {
            if let Some(rest) = s.strip_prefix('?') {
                // Meta variables are never raw types; they participate in inference.
                return Ok(Type::Meta(rest.to_string()));
            }
            if raw_mode {
                return Ok(Type::Raw(s.to_string()));
            } else {
                return Ok(Type::Meta(s.to_string()));
            }
        }

        // In raw mode, unsupported syntax should remain usable as an opaque raw type.
        // This is important for language frontends whose annotation surface syntax is
        // richer than the core type algebra understood by the engine.
        if raw_mode {
            return Ok(Type::Raw(s.to_string()));
        }

        // Strict parse failed - try partial parse as fallback
        Self::analyze_partial(s, type_str)
    }
}

fn missing_closing_parens(s: &str) -> Result<usize, String> {
    if !s.starts_with('(') {
        return Err(format!(
            "Missing opening parenthesis in type expression: {}",
            s
        ));
    }
    let mut depth: isize = 0;
    for c in s.chars() {
        match c {
            '(' => depth += 1,
            ')' => {
                depth -= 1;
            }
            _ => {}
        }
    }
    if depth < 0 {
        Err(format!(
            "Too many closing parentheses in type expression: {}",
            s
        ))
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
    if s.len() <= 2 || !s.starts_with('\'') || !s.ends_with('\'') {
        return false;
    }
    !s[1..s.len() - 1].contains('\'')
}

fn split_top_level_union(s: &str) -> Option<Vec<&str>> {
    let mut depth = 0isize;
    let mut starts = vec![0usize];
    let mut found = false;

    for (i, c) in s.char_indices() {
        match c {
            '(' | '{' | '[' => depth += 1,
            ')' | '}' | ']' if depth > 0 => depth -= 1,
            '|' if depth == 0 => {
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

fn strip_array_suffix(s: &str) -> Option<&str> {
    let trimmed = s.trim_end();
    let inner = if trimmed.ends_with("[]") {
        trimmed[..trimmed.len() - 2].trim_end()
    } else if trimmed.ends_with(']') {
        let before_close = trimmed[..trimmed.len() - 1].trim_end();
        if !before_close.ends_with('[') {
            return None;
        }
        before_close[..before_close.len() - 1].trim_end()
    } else {
        return None;
    };

    if inner.is_empty() {
        return None;
    }

    let mut depth = 0isize;
    for c in inner.chars() {
        match c {
            '(' | '{' | '[' => depth += 1,
            ')' | '}' | ']' => depth -= 1,
            _ => {}
        }
        if depth < 0 {
            return None;
        }
    }

    if depth != 0 {
        return None;
    }

    Some(inner)
}

fn parse_object_type(s: &str, raw_mode: bool) -> Result<Option<Type>, String> {
    let trimmed = s.trim();
    if !trimmed.starts_with('{') || !trimmed.ends_with('}') || !outer_delims_wrap(trimmed, '{', '}')
    {
        return Ok(None);
    }

    let inner = trimmed[1..trimmed.len() - 1].trim();
    if inner.is_empty() {
        return Ok(Some(Type::Object(Vec::new())));
    }

    let mut fields = Vec::new();
    let mut rest = None;
    for part in split_top_level_commas(inner) {
        let part = part.trim();
        if part.is_empty() {
            return Err(format!("Empty object type field in {}", s));
        }
        if let Some(rest_ty) = part.strip_prefix("...") {
            if rest.is_some() {
                return Err(format!("Object type has multiple spreads: {}", s));
            }
            rest = Some(Type::parse_impl(rest_ty.trim(), raw_mode)?);
            continue;
        }
        if rest.is_some() {
            return Err(format!("Object type field appears after spread: {}", s));
        }

        let Some(colon) = find_top_level_char(part, ':') else {
            return Err(format!("Object type field is missing ':': {}", part));
        };
        let name = parse_object_field_name(part[..colon].trim())?;
        let ty = Type::parse_impl(part[colon + 1..].trim(), raw_mode)?;
        fields.push((name, ty));
    }

    if let Some(rest) = rest {
        let ty = fields.into_iter().rev().fold(rest, |acc, (name, field_ty)| {
            Type::ObjectExtend(name, Box::new(field_ty), Box::new(acc))
        });
        Ok(Some(ty))
    } else {
        Ok(Some(Type::Object(fields)))
    }
}

fn outer_delims_wrap(s: &str, open: char, close: char) -> bool {
    let mut depth = 0isize;
    for (i, c) in s.char_indices() {
        if c == open {
            depth += 1;
        } else if c == close {
            depth -= 1;
            if depth == 0 {
                return i == s.len() - close.len_utf8();
            }
        }
    }
    false
}

fn find_top_level_char(s: &str, target: char) -> Option<usize> {
    let mut depth = 0isize;
    for (i, c) in s.char_indices() {
        match c {
            '(' | '{' | '[' => depth += 1,
            ')' | '}' | ']' if depth > 0 => depth -= 1,
            _ if c == target && depth == 0 => return Some(i),
            _ => {}
        }
    }
    None
}

fn parse_object_field_name(s: &str) -> Result<String, String> {
    if s.is_empty() {
        return Err("Object type field name is empty".into());
    }
    if s.len() >= 2
        && ((s.starts_with('\'') && s.ends_with('\''))
            || (s.starts_with('"') && s.ends_with('"')))
    {
        return Ok(s[1..s.len() - 1].to_string());
    }
    Ok(s.to_string())
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

#[cfg(test)]
mod tests {
    use crate::logic::typing::Type;

    #[test]
    fn arrow_associativity() {
        // Arrow types should be RIGHT-associative:
        // A -> B -> C  should parse as  A -> (B -> C)
        let t = Type::parse("A->B->C").unwrap();
        println!("Parsed A->B->C as: {:?}", t);

        // If right-associative: Arrow(A, Arrow(B, C))
        // If left-associative: Arrow(Arrow(A, B), C)
        match &t {
            Type::Arrow(left, right) => {
                println!("  Left:  {:?}", left);
                println!("  Right: {:?}", right);

                // Right side should be Arrow(B, C) for right-associativity
                match right.as_ref() {
                    Type::Arrow(_, _) => println!("  => RIGHT-associative (correct)"),
                    _ => println!("  => LEFT-associative (WRONG!)"),
                }

                // For right-associativity: left = A, right = B->C
                assert!(
                    matches!(right.as_ref(), Type::Arrow(_, _)),
                    "A->B->C should be right-associative: A -> (B -> C), but got: {:?}",
                    t
                );
            }
            _ => panic!("Expected Arrow type"),
        }
    }

    #[test]
    fn curried_function_application_types() {
        // When f : A -> B -> C and x : A
        // Then (f x) should have type B -> C

        let f_type = Type::parse("A->B->C").unwrap();
        println!("f : {:?}", f_type);

        match &f_type {
            Type::Arrow(domain, codomain) => {
                println!("Domain (should be A): {:?}", domain);
                println!("Codomain (should be B->C): {:?}", codomain);

                // For curried application to work, the domain must be the first
                // argument type and the codomain must remain an arrow chain.
                assert!(matches!(domain.as_ref(), Type::Meta(name) if name == "A"));
                assert!(matches!(codomain.as_ref(), Type::Arrow(_, _)));
            }
            _ => panic!("Expected arrow type"),
        }
    }

    #[test]
    fn union_type_parses() {
        let t = Type::parse("Int | Bool").unwrap();
        match t {
            Type::Union(parts) => {
                assert_eq!(parts.len(), 2);
                assert!(matches!(parts[0], Type::Meta(_)));
                assert!(matches!(parts[1], Type::Meta(_)));
            }
            other => panic!("Expected union type, got {:?}", other),
        }
    }

    #[test]
    fn union_arrow_precedence() {
        let t = Type::parse("A -> B | C").unwrap();
        match t {
            Type::Union(parts) => {
                assert_eq!(parts.len(), 2);
                assert!(matches!(parts[0], Type::Arrow(_, _)));
                assert!(matches!(parts[1], Type::Meta(_)));
            }
            other => panic!("Expected top-level union, got {:?}", other),
        }
    }

    #[test]
    fn multi_arg_function_type_parses_as_curried_arrows() {
        let t = Type::parse_raw("(number, string) => boolean").unwrap();
        assert_eq!(
            t,
            Type::Arrow(
                Box::new(Type::Raw("number".into())),
                Box::new(Type::Arrow(
                    Box::new(Type::Raw("string".into())),
                    Box::new(Type::Raw("boolean".into())),
                )),
            )
        );
    }

    #[test]
    fn zero_arg_function_type_uses_void_domain() {
        let t = Type::parse_raw("() => number").unwrap();
        assert_eq!(
            t,
            Type::Arrow(
                Box::new(Type::Raw("void".into())),
                Box::new(Type::Raw("number".into())),
            )
        );
    }

    #[test]
    fn object_type_parses_fields_and_spread() {
        let t = Type::parse("{ key: ?T, ...?Rest }").unwrap();
        assert_eq!(
            t,
            Type::ObjectExtend(
                "key".into(),
                Box::new(Type::Meta("T".into())),
                Box::new(Type::Meta("Rest".into())),
            )
        );

        let concrete = Type::parse_raw("{ id: number, name: string }").unwrap();
        assert_eq!(
            concrete,
            Type::Object(vec![
                ("id".into(), Type::Raw("number".into())),
                ("name".into(), Type::Raw("string".into())),
            ])
        );
    }
}
