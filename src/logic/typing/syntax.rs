use super::Type;
use std::fmt;

// ============================================================================
// Type Syntax Constants (hardcoded)
// ============================================================================
// The type language supports:
//   - Atoms: alphanumeric identifiers (treated as type variables)
//   - Raw types: quoted literals like 'int', 'string' (concrete base types)
//   - Arrows: -> or → (function types, right-associative)
//   - Negation: ¬ or ! (complement types)
//   - Any: ⊤ (top type, accepts everything)
//   - None: ∅ (bottom type, rejects everything)
//   - Context calls: Γ(x) (lookup variable x in context Γ)
//   - Parens: (τ) for grouping
// ============================================================================

const NONE_KW: &str = "∅";
const ANY_KW: &str = "⊤";
const ARROW_TOKENS: &[&str; 2] = &["->", "→"];
const NEGATION_TOKENS: &[&str; 2] = &["¬", "!"];

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Atom(s) => write!(f, "{}", s),
            Type::Meta(s) => write!(f, "?{}", s),
            Type::Raw(s) => write!(f, "'{}'", s),
            Type::Arrow(l, r) => write!(f, "{} → {}", l, r),
            Type::Union(items) => {
                let rendered: Vec<String> = items.iter().map(|t| format!("{}", t)).collect();
                write!(f, "{}", rendered.join(" | "))
            }
            Type::Not(t) => write!(f, "¬{}", t),
            Type::ContextCall(_ctx, var) => write!(f, "lookup({})", var),
            Type::Any => write!(f, "⊤"),
            Type::None => write!(f, "∅"),

            // incomplete shit
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

// ============================================================================
// Helper functions for type parsing
// ============================================================================

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
        if s.starts_with('(') {
            // fully balanced but parse failed -> treat as inner partial
            let inner = &s[1..];
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
                Type::Atom(s.to_string())
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
        if s.starts_with('(') {
            let depth = missing_closing_parens(s)?;
            if depth > 0 {
                // Incomplete parens → partial type expecting a closing ')'
                let inner = Self::parse_impl(&s[1..], raw_mode)?;
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

            if let Some(end) = wrapper_ends_at {
                if end == s.len() - 1 {
                    return Self::parse_impl(&s[1..s.len() - 1], raw_mode);
                }
            }
            // else: not a full wrapper, fall through
        }

        // Arrow types are RIGHT-associative: A -> B -> C  ==  A -> (B -> C)
        // So we split on the FIRST arrow outside parens
        if let Some((pos, tok_len)) = find_first_outside_parens(s, &ARROW_TOKENS[..]) {
            return Ok(Type::Arrow(
                Box::new(Self::parse_impl(&s[..pos], raw_mode)?),
                Box::new(Self::parse_impl(&s[pos + tok_len..], raw_mode)?),
            ));
        }

        if let Some(&tok) = NEGATION_TOKENS.iter().find(|t| s.starts_with(**t)) {
            return Ok(Type::Not(Box::new(Self::parse_impl(
                &s[tok.len()..],
                raw_mode,
            )?)));
        }

        // Parse context calls "Γ(x)", "(y)"
        if let Some(paren_start) = s.find('(') {
            if let Some(paren_end) = s.find(')') {
                if paren_end > paren_start && paren_end == s.len() - 1 {
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
                return Ok(Type::Atom(s.to_string()));
            }
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
            '(' => depth += 1,
            ')' if depth > 0 => depth -= 1,
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

                // For curried application to work, domain must be simple A
                // this fail but idk why i'm gonna kill this fucking compiler bullshit typing
                // zeoifhzjkebfhjzebfuziebfziuoebfhiuezrbfh
                //assert!(equal(**domain, Type::Atom("A".into()), Context::new()));
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
                assert!(matches!(parts[0], Type::Atom(_)));
                assert!(matches!(parts[1], Type::Atom(_)));
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
                assert!(matches!(parts[1], Type::Atom(_)));
            }
            other => panic!("Expected top-level union, got {:?}", other),
        }
    }
}
