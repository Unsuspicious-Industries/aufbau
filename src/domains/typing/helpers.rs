use crate::domains::typing::Type;

fn trailing_name(s: &str) -> Option<String> {
    let s = s.trim();
    let close = s.rfind(')')?;
    let open = s[..close].rfind('(')?;
    let after = s[close + 1..].trim();
    if !after.is_empty() {
        return None;
    }
    let name = s[open + 1..close].trim();
    if name.is_empty() {
        None
    } else {
        Some(name.to_string())
    }
}

fn strip_name(s: &str) -> Option<String> {
    let s = s.trim();
    let close = s.rfind(')')?;
    let open = s[..close].rfind('(')?;
    if !s[close + 1..].trim().is_empty() {
        return None;
    }
    Some(s[..open].trim().to_string())
}

/// Parse a multi-line inference rule block, e.g. `Γ ⊢ x : τ --- (var)`.
pub fn parse_inference_rule(lines: &[&str]) -> Result<(String, String, String), String> {
    if lines.is_empty() {
        return Err("Empty rule block".into());
    }
    let mut premises = String::new();
    let mut conclusion = String::new();
    let mut name = String::new();
    let mut in_conclusion = false;

    for line in lines {
        let t = line.trim();
        if t.contains("---") {
            name = trailing_name(t).unwrap_or(name);
            in_conclusion = true;
            continue;
        }
        if !in_conclusion {
            premises = t.to_string();
        } else {
            conclusion = t.to_string();
            if name.is_empty() {
                if let Some(n) = trailing_name(t) {
                    name = n;
                    conclusion = strip_name(t).unwrap_or(conclusion);
                }
            }
        }
    }
    if name.is_empty() {
        Err("Typing rule has no name".into())
    } else {
        Ok((premises, conclusion, name))
    }
}

pub(crate) fn context_name(s: &str) -> bool {
    !s.is_empty()
        && s.chars().all(|c| {
            c.is_alphanumeric() || c == '_' || "ΓΔΘΛΣΦΨΩΞΠΡΤΥΧδγτλσφψωξπρυχ₁₂₃₄₅₆₇₈₉₀".contains(c)
        })
}

pub(crate) fn matching_paren(s: &str) -> Option<usize> {
    matching_paren_from(s, 0)
}

pub(crate) fn matching_paren_from(s: &str, start: usize) -> Option<usize> {
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

pub(crate) fn unclosed_parens(s: &str) -> Result<usize, String> {
    if !s.starts_with('(') {
        return Err(format!("Missing opening parenthesis: {}", s));
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
        Err(format!("Too many closing parentheses: {}", s))
    } else {
        Ok(depth as usize)
    }
}

pub(crate) fn first_outside_parens(s: &str, tokens: &[&str]) -> Option<(usize, usize)> {
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

pub(crate) fn single_quoted_literal(s: &str) -> bool {
    s.len() > 2 && s.starts_with('\'') && s.ends_with('\'') && !s[1..s.len() - 1].contains('\'')
}

pub(crate) fn split_top(s: &str, sep: char) -> Option<Vec<&str>> {
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

pub(crate) fn split_commas(s: &str) -> Vec<&str> {
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

pub(crate) fn parened_type_list(s: &str, raw_mode: bool) -> Result<Option<Vec<Type>>, String> {
    let trimmed = s.trim();
    if !trimmed.starts_with('(') || !trimmed.ends_with(')') {
        return Ok(None);
    }
    let inner = &trimmed[1..trimmed.len() - 1];
    if inner.trim().is_empty() {
        return Ok(Some(vec![Type::Raw("void".to_string())]));
    }
    let parts = split_commas(inner);
    if parts.len() <= 1 {
        return Ok(None);
    }
    parts
        .into_iter()
        .map(|part| Type::parse_impl(part.trim(), raw_mode))
        .collect::<Result<Vec<_>, _>>()
        .map(Some)
}

pub(crate) fn arrow(params: Vec<Type>, ret: Type) -> Type {
    params.into_iter().rev().fold(ret, |acc, param| {
        Type::Arrow(Box::new(param), Box::new(acc))
    })
}

pub(crate) fn flatten_unions(members: Vec<Type>) -> Vec<Type> {
    let mut flat = Vec::new();
    for t in members {
        match t {
            Type::Union(nested) => flat.extend(nested),
            other => flat.push(other),
        }
    }
    flat
}
