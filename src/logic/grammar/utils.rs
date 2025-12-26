use super::Symbol;
use regex::Regex as ExternalRegex;

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

/// Result of parsing a RHS: the concrete symbols plus any literal tokens encountered.
pub struct ParsedRhs {
    pub alternatives: Vec<Vec<Symbol>>,
    pub literal_tokens: Vec<String>,
}

/// Parse RHS with bindings like "'λ' Variable[x] ':' Type[τ₁] '.' Term[e]"
pub fn parse_rhs(rhs: &str) -> Result<ParsedRhs, String> {
    let mut alternatives = Vec::new();
    let mut literal_tokens = Vec::new();
    let alt_strings = split_alternatives(rhs)?;
    for alt in alt_strings
        .iter()
        .map(|s| s.trim())
        .filter(|alt| !alt.is_empty())
    {
        let mut symbols_in_alt = Vec::new();
        let mut is_epsilon_alt = false;
        for token in alt.split_whitespace() {
            let (base_token, binding) = split_binding(token)?;
            ensure_no_repetition_suffix(&base_token)?;
            if base_token == "ε" {
                // epsion is for empty alternative
                if binding.is_some() {
                    return Err("Epsilon production cannot carry a binding".into());
                }
                if is_epsilon_alt || !symbols_in_alt.is_empty() {
                    return Err("Epsilon alternative cannot mix with other symbols".into());
                }
                is_epsilon_alt = true;
                continue;
            }
            if let Some(lit) = literal_token_value(&base_token) {
                if !literal_tokens.contains(&lit) {
                    literal_tokens.push(lit.clone());
                }
            }
            let mut symbol = Symbol::new(base_token);
            if let Some(binding) = binding {
                symbol = symbol.attach_binding(binding);
            }
            symbols_in_alt.push(symbol);
        }
        if is_epsilon_alt {
            alternatives.push(Vec::new());
            continue;
        }
        alternatives.push(symbols_in_alt);
    }
    Ok(ParsedRhs {
        alternatives,
        literal_tokens,
    })
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
    let name_at_end = ExternalRegex::new(r"\(([^)]+)\)\s*$").unwrap();

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

fn split_binding(token: &str) -> Result<(String, Option<String>), String> {
    if !token.ends_with(']') {
        return Ok((token.to_string(), None));
    }

    let close_bracket = token.len() - 1;
    let open_bracket = token[..close_bracket]
        .rfind('[')
        .ok_or_else(|| format!("Malformed binding in token '{}': missing '['", token))?;

    let value = token[..open_bracket].to_string();
    let binding = token[open_bracket + 1..close_bracket].to_string();
    if binding.is_empty() {
        return Err(format!("Empty binding name in token '{}'", token));
    }

    Ok((value, Some(binding)))
}

fn ensure_no_repetition_suffix(token: &str) -> Result<(), String> {
    if let Some(last) = token.chars().last() {
        if matches!(last, '*' | '+' | '?') {
            return Err(format!(
                "Repetition operators (*, +, ?) are not supported anymore (found in '{}')",
                token
            ));
        }
    }
    Ok(())
}

fn literal_token_value(token: &str) -> Option<String> {
    if token.len() >= 2 && token.starts_with('\'') && token.ends_with('\'') {
        Some(token[1..token.len() - 1].to_string())
    } else if token.len() >= 2 && token.starts_with('"') && token.ends_with('"') {
        Some(token[1..token.len() - 1].to_string())
    } else {
        None
    }
}
