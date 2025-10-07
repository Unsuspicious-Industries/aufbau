use super::Symbol;
use regex::Regex;

// collection of utils for working with grammar definitions
pub fn is_regex(pattern: &str) -> bool {
    // Only slash-delimited patterns: /regex/
    pattern.starts_with('/') && pattern.ends_with('/') && pattern.len() > 2
}

/// Parse a production line like "Lambda(lambda) ::= 'λ' Variable[x] ':' Type[τ₁] '.' Term[e]"
pub fn parse_production(line: &str) -> Result<(String, String), String> {
    let parts: Vec<&str> = line.splitn(2, "::=").collect();
    if parts.len() != 2 {
        return Err(format!("Invalid production line: {}", line));
    }
    Ok((parts[0].trim().to_string(), parts[1].trim().to_string()))
}

/// Parse nonterminal with optional rule name like "Lambda(lambda)" -> ("Lambda", Some("lambda"))
pub fn parse_nonterminal(nt_str: &str) -> Result<(String, Option<String>), String> {
    if let Some(open_paren) = nt_str.find('(') {
        if let Some(close_paren) = nt_str.rfind(')') {
            if close_paren > open_paren {
                let name = nt_str[..open_paren].trim().to_string();
                let rule_name = nt_str[open_paren + 1..close_paren].trim().to_string();
                return Ok((
                    name,
                    if rule_name.is_empty() {
                        None
                    } else {
                        Some(rule_name)
                    },
                ));
            }
        }
    }
    // No rule name
    Ok((nt_str.trim().to_string(), None))
}

/// Split a RHS string by | but respect quoted strings
fn split_alternatives(rhs: &str) -> Result<Vec<String>, String> {
    let mut alternatives = Vec::new();
    let mut current = String::new();
    let mut in_single_quotes = false;
    let mut in_double_quotes = false;
    let mut depth: i32 = 0; // parenthesis depth
    let mut chars = rhs.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch == '/' && !in_single_quotes && !in_double_quotes {
            // regex literal
            current.push(ch);
            while let Some(regex_ch) = chars.next() {
                current.push(regex_ch);
                if regex_ch == '/' {
                    break;
                }
            }
            continue;
        }
        match ch {
            '\'' if !in_double_quotes => {
                in_single_quotes = !in_single_quotes;
                current.push(ch);
            }
            '"' if !in_single_quotes => {
                in_double_quotes = !in_double_quotes;
                current.push(ch);
            }
            '(' if !in_single_quotes && !in_double_quotes => {
                depth += 1;
                current.push(ch);
            }
            ')' if !in_single_quotes && !in_double_quotes => {
                depth -= 1;
                current.push(ch);
            }
            '|' if !in_single_quotes && !in_double_quotes && depth == 0 => {
                alternatives.push(current.trim().to_string());
                current.clear();
            }
            _ => current.push(ch),
        }
    }
    if in_single_quotes || in_double_quotes {
        return Err(format!("Unclosed quotes in grammar rule: {}", rhs));
    }
    if !current.trim().is_empty() {
        alternatives.push(current.trim().to_string());
    }
    Ok(alternatives)
}

/// Parse RHS with bindings like "'λ' Variable[x] ':' Type[τ₁] '.' Term[e]"
pub fn parse_rhs(rhs: &str) -> Result<Vec<Vec<Symbol>>, String> {
    let mut alternatives = Vec::new();
    let alt_strings = split_alternatives(rhs)?;
    for alt in alt_strings
        .iter()
        .map(|s| s.trim())
        .filter(|alt| !alt.is_empty())
    {
        let mut symbols_in_alt = Vec::new();
        for token in alt.split_whitespace() {
            if is_regex(&token) {
                symbols_in_alt.push(Symbol::new(token.to_string()));
            } else if let Some(open_bracket) = token.find('[') {
                if let Some(close_bracket) = token.rfind(']') {
                    if close_bracket > open_bracket {
                        let value = token[..open_bracket].to_string();
                        let binding = token[open_bracket + 1..close_bracket].to_string();
                        symbols_in_alt.push(Symbol::with_binding(value, binding));
                        continue;
                    }
                }
                symbols_in_alt.push(Symbol::new(token.to_string()));
            } else {
                symbols_in_alt.push(Symbol::new(token.to_string()));
            }
        }
        alternatives.push(symbols_in_alt);
    }
    Ok(alternatives)
}

/// Inline-group aware parser. Ignores parentheses that are inside quoted literals.
pub fn parse_rhs_with_groups(rhs: &str) -> Result<Vec<Vec<Symbol>>, String> {
    let mut alternatives: Vec<Vec<Symbol>> = Vec::new();
    let alt_strings = split_alternatives(rhs)?;
    for alt in alt_strings
        .iter()
        .map(|s| s.trim())
        .filter(|alt| !alt.is_empty())
    {
        let mut symbols_in_alt: Vec<Symbol> = Vec::new();
        let mut chars = alt.chars().peekable();
        let mut in_single = false;
        let mut in_double = false;
        while let Some(ch) = chars.peek().cloned() {
            if ch.is_whitespace() {
                chars.next();
                continue;
            }
            match ch {
                '\'' => {
                    in_single = !in_single;
                }
                '"' => {
                    in_double = !in_double;
                }
                _ => {}
            }
            if ch == '(' && !in_single && !in_double {
                // real group
                chars.next(); // consume '('
                let mut depth = 1usize;
                let mut group_content = String::new();
                while let Some(c2) = chars.next() {
                    if c2 == '(' {
                        depth += 1;
                    } else if c2 == ')' {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        }
                    }
                    if depth > 0 {
                        group_content.push(c2);
                    }
                }
                if depth != 0 {
                    return Err(format!("Unclosed group in RHS: {}", alt));
                }
                let inner = parse_rhs_with_groups(group_content.trim())?;
                let inner_seq = if inner.is_empty() {
                    Vec::new()
                } else {
                    inner.into_iter().next().unwrap()
                };
                symbols_in_alt.push(Symbol::group(inner_seq));
                continue;
            }
            // token path
            let mut token = String::new();
            while let Some(c2) = chars.peek().cloned() {
                if c2.is_whitespace() || (!in_single && !in_double && (c2 == '(' || c2 == ')')) {
                    break;
                }
                token.push(c2);
                chars.next();
                if token.starts_with('\'') {
                    // single quoted literal
                    while let Some(c3) = chars.next() {
                        token.push(c3);
                        if c3 == '\'' {
                            break;
                        }
                    }
                } else if token.starts_with('"') {
                    // double quoted literal
                    while let Some(c3) = chars.next() {
                        token.push(c3);
                        if c3 == '"' {
                            break;
                        }
                    }
                } else if token == "/" {
                    // regex
                    while let Some(c3) = chars.next() {
                        token.push(c3);
                        if c3 == '/' {
                            break;
                        }
                    }
                } else if !token.starts_with('\'')
                    && !token.starts_with('"')
                    && token.contains('[')
                    && !token.contains(']')
                {
                    while let Some(c3) = chars.next() {
                        token.push(c3);
                        if c3 == ']' {
                            break;
                        }
                    }
                }
                if chars
                    .peek()
                    .map(|c| c.is_whitespace() || *c == '(' || *c == ')')
                    .unwrap_or(true)
                {
                    break;
                }
            }
            if token.is_empty() {
                chars.next();
                continue;
            }
            let parsed = parse_rhs(&token)?;
            if parsed.len() != 1 || parsed[0].len() != 1 {
                return Err(format!(
                    "Unexpected token decomposition for '{}': parsed={:?}",
                    token, parsed
                ));
            }
            symbols_in_alt.push(parsed[0][0].clone());
        }
        alternatives.push(symbols_in_alt);
    }
    Ok(alternatives)
}

/// Find special tokens in a right-hand side string.
pub fn special_tokens(rhs: &str) -> Vec<String> {
    let mut found = Vec::new();
    // Regex to capture quoted substrings irrespective of adjacency to parentheses
    let re = Regex::new(r#"'([^']*)'|"([^"]*)""#).unwrap();
    for cap in re.captures_iter(rhs) {
        if let Some(m) = cap.get(1) {
            // single quoted
            let v = m.as_str().to_string();
            if !found.contains(&v) {
                found.push(v);
            }
        } else if let Some(m) = cap.get(2) {
            // double quoted
            let v = m.as_str().to_string();
            if !found.contains(&v) {
                found.push(v);
            }
        }
    }
    found
}

/// =========
/// Type Shit
/// =========
///
///
/// ------------
/// Type Parsing
/// ------------

/// Parse a multi-line inference rule block
pub fn parse_inference_rule(lines: &[&str]) -> Result<(String, String, String), String> {
    if lines.is_empty() {
        return Err("Empty rule block".into());
    }

    let mut premises = String::new();
    let mut conclusion = String::new();
    let mut name = String::new();
    let mut in_conclusion = false;

    // Regex that captures `(name)` only when the parentheses occur at end of string (optional trailing whitespace)
    let name_at_end = Regex::new(r"\(([^)]+)\)\s*$").unwrap();

    for line in lines {
        let trimmed = line.trim();
        if trimmed.contains("---") {
            // dashed separator – start collecting conclusion next
            if let Some(cap) = name_at_end.captures(trimmed) {
                name = cap[1].trim().to_string();
            }
            in_conclusion = true;
            continue;
        }
        if !in_conclusion {
            premises = trimmed.to_string();
        } else {
            // first non-dash line after separator is conclusion
            conclusion = trimmed.to_string();
            // Try to extract rule name if not found yet and present at end of conclusion line
            if name.is_empty() {
                if let Some(cap) = name_at_end.captures(trimmed) {
                    name = cap[1].trim().to_string();
                    conclusion = name_at_end.replace(trimmed, "").trim().to_string();
                }
            }
        }
    }

    if name.is_empty() {
        return Err("Typing rule has no name".into());
    }

    Ok((premises, conclusion, name))
}
pub const RELATION_SYMBOLS: [&str; 8] = ["=", "<", "∈", "⊆", "⊂", "⊃", "⊇", ":"];

pub fn parse_judgement(
    judgment_str: &str,
) -> Result<(Option<Vec<(String, String)>>, String, String), String> {
    let parts: Vec<&str> = judgment_str.split('⊢').map(str::trim).collect();
    if parts.len() != 2 {
        return Err(format!("Invalid typing judgment format: {}", judgment_str));
    }
    let (base, extensions) = parse_context_extensions(parts[0])?;
    if base != "Γ" {
        return Err(format!("Context must start with 'Γ', got '{}'", base));
    }
    // split the second part into expression and type
    let expr_parts: Vec<&str> = parts[1].split(':').map(str::trim).collect();
    if expr_parts.len() != 2 {
        return Err(format!("Invalid typing judgment format: {}", judgment_str));
    }
    Ok((
        if extensions.is_empty() {
            None
        } else {
            Some(extensions)
        },
        expr_parts[0].to_string(),
        expr_parts[1].to_string(),
    ))
}

/// Parses a context string like "Γ[x:τ₁][y:τ₂]" into (base_context, Vec<(variable, type)>)
pub fn parse_context_extensions(
    context_str: &str,
) -> Result<(String, Vec<(String, String)>), String> {
    let context_str = context_str.trim();
    if !context_str.starts_with("Γ") {
        return Err(format!(
            "Context must start with 'Γ', got '{}'",
            context_str
        ));
    }
    let base = "Γ".to_string();
    let mut extensions = Vec::new();
    let re = Regex::new(r"\[([^:\]]+):([^\]]+)\]").unwrap();
    for cap in re.captures_iter(context_str) {
        let var = cap.get(1).unwrap().as_str().trim().to_string();
        let ty = cap.get(2).unwrap().as_str().trim().to_string();
        if var.is_empty() || ty.is_empty() {
            return Err(format!(
                "Invalid context extension format, expected '[var:type]': [{:?}:{:?}]",
                var, ty
            ));
        }
        extensions.push((var, ty));
    }
    Ok((base, extensions))
}

pub fn parse_membership(membership_str: &str) -> Result<(String, String), String> {
    let parts: Vec<&str> = membership_str.split('∈').map(str::trim).collect();
    if parts.len() != 2 {
        return Err(format!("Invalid membership format: {}", membership_str));
    }
    Ok((parts[0].to_string(), parts[1].to_string()))
}

pub fn parse_type_relation(relation_str: &str) -> Result<(String, String, String), String> {
    // we should have three parts
    // left <arbitrary symbols> right
    let mut left = String::new();
    let mut right = String::new();
    let mut relation = String::new();
    for c in relation_str.chars() {
        if RELATION_SYMBOLS.contains(&c.to_string().as_str()) {
            relation.push(c);
        } else {
            if relation.is_empty() {
                left.push(c);
            } else {
                right.push(c);
            }
        }
    }
    if relation.is_empty() {
        return Err(format!("No relation symbol found in: {}", relation_str));
    }
    if left.is_empty() || right.is_empty() {
        return Err(format!("Invalid type relation format: {}", relation_str));
    }
    Ok((left.trim().to_string(), right.trim().to_string(), relation))
}
