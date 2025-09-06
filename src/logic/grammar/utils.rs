use regex::Regex;
use super::{Symbol, RepetitionKind};

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
                return Ok((name, if rule_name.is_empty() { None } else { Some(rule_name) }));
            }
        }
    }
    // No rule name
    Ok((nt_str.trim().to_string(), None))
}

/// Parse repetition suffix from a token and return (base_token, repetition_kind)
pub fn parse_repetition_suffix(token: &str) -> (String, Option<RepetitionKind>) {
    if token.ends_with('*') {
        (token[..token.len()-1].to_string(), Some(RepetitionKind::ZeroOrMore))
    } else if token.ends_with('+') {
        (token[..token.len()-1].to_string(), Some(RepetitionKind::OneOrMore))
    } else if token.ends_with('?') {
        (token[..token.len()-1].to_string(), Some(RepetitionKind::ZeroOrOne))
    } else {
        (token.to_string(), None)
    }
}

/// Parse RHS with bindings like "'λ' Variable[x] ':' Type[τ₁] '.' Term[e]"
pub fn parse_rhs(rhs: &str) -> Result<Vec<Vec<Symbol>>, String> {
    let mut alternatives = Vec::new();
    
    // Split by | for alternatives
    for alt in rhs.split('|').map(str::trim).filter(|alt| !alt.is_empty()) {
        let mut symbols_in_alt = Vec::new();
        for token in alt.split_whitespace() {
            // Parse repetition suffix first
            let (base_token, repetition) = parse_repetition_suffix(token);
            
            // Check if it's a regex pattern first (before checking for bindings)
            if is_regex(&base_token) {
                // It's a regex pattern like /[a-zA-Z]+/ - treat as regular symbol
                if let Some(rep) = repetition {
                    symbols_in_alt.push(Symbol::with_repetition(base_token, rep));
                } else {
                    symbols_in_alt.push(Symbol::new(base_token));
                }
            } else if let Some(open_bracket) = base_token.find('[') {
                if let Some(close_bracket) = base_token.rfind(']') {
                    if close_bracket > open_bracket {
                        // Symbol with binding like "Variable[x]"
                        let value = base_token[..open_bracket].to_string();
                        let binding = base_token[open_bracket + 1..close_bracket].to_string();
                        
                        // Strip quotes from the value if it's a quoted terminal
                        let clean_value = if (value.starts_with('\'') && value.ends_with('\'')) || 
                                           (value.starts_with('"') && value.ends_with('"')) {
                            value.trim_matches('\'').trim_matches('"').to_string()
                        } else {
                            value
                        };
                        
                        if let Some(rep) = repetition {
                            symbols_in_alt.push(Symbol::with_binding_and_repetition(clean_value, binding, rep));
                        } else {
                            symbols_in_alt.push(Symbol::with_binding(clean_value, binding));
                        }
                        continue;
                    }
                }
                // If we get here, it has brackets but not a valid binding - treat as regular symbol
                let clean_token = if (base_token.starts_with('\'') && base_token.ends_with('\'')) || 
                                   (base_token.starts_with('"') && base_token.ends_with('"')) {
                    base_token.trim_matches('\'').trim_matches('"').to_string()
                } else {
                    base_token
                };
                if let Some(rep) = repetition {
                    symbols_in_alt.push(Symbol::with_repetition(clean_token, rep));
                } else {
                    symbols_in_alt.push(Symbol::new(clean_token));
                }
            } else {
                // Regular symbol without binding - strip quotes if it's a terminal
                let clean_token = if (base_token.starts_with('\'') && base_token.ends_with('\'')) || 
                                   (base_token.starts_with('"') && base_token.ends_with('"')) {
                    base_token.trim_matches('\'').trim_matches('"').to_string()
                } else {
                    base_token
                };
                if let Some(rep) = repetition {
                    symbols_in_alt.push(Symbol::with_repetition(clean_token, rep));
                } else {
                    symbols_in_alt.push(Symbol::new(clean_token));
                }
            }
        }
        alternatives.push(symbols_in_alt);
    }
    
    Ok(alternatives)
}

/// Find special tokens in a right-hand side string.
pub fn special_tokens(rhs: &str) -> Vec<String> {
    let mut found = Vec::new();
    for alt in rhs.split('|').map(str::trim).filter(|alt| !alt.is_empty()) {
        for sym in alt.split_whitespace() {
            // Any token surrounded by single quotes is a special token
            if (sym.starts_with('\'') && sym.ends_with('\'')) || 
                (sym.starts_with('"') && sym.ends_with('"')) {
                let sym_stripped = sym.trim_matches('\'').trim_matches('"');
                if !found.contains(&sym_stripped.to_string()) {
                    found.push(sym_stripped.to_string());
                }
            }
            // Skip regex patterns - they're not special tokens
            else if is_regex(sym) {
                continue;
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
pub fn parse_inference_rule(lines: &[&str]) -> Result<(String,String,String), String> {
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
}pub const RELATION_SYMBOLS: [&str; 8] = ["=", "<", "∈", "⊆", "⊂", "⊃", "⊇", ":"];

pub fn parse_judgement(
    judgment_str: &str,
) -> Result<(Option<Vec<(String,String)>>, String,String), String> {
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
    Ok((if extensions.is_empty() { None } else { Some(extensions) }, expr_parts[0].to_string(), expr_parts[1].to_string()))
}

/// Parses a context string like "Γ[x:τ₁][y:τ₂]" into (base_context, Vec<(variable, type)>)
pub fn parse_context_extensions(context_str: &str) -> Result<(String, Vec<(String, String)>), String> {
    let context_str = context_str.trim();
    if !context_str.starts_with("Γ") {
        return Err(format!("Context must start with 'Γ', got '{}'", context_str));
    }
    let base = "Γ".to_string();
    let mut extensions = Vec::new();
    let re = Regex::new(r"\[([^:\]]+):([^\]]+)\]").unwrap();
    for cap in re.captures_iter(context_str) {
        let var = cap.get(1).unwrap().as_str().trim().to_string();
        let ty = cap.get(2).unwrap().as_str().trim().to_string();
        if var.is_empty() || ty.is_empty() {
            return Err(format!("Invalid context extension format, expected '[var:type]': [{:?}:{:?}]", var, ty));
        }
        extensions.push((var, ty));
    }
    Ok((base, extensions))
}

pub fn parse_membership(
    membership_str: &str,
) -> Result<(String, String), String> {
    let parts: Vec<&str> = membership_str.split('∈').map(str::trim).collect();
    if parts.len() != 2 {
        return Err(format!("Invalid membership format: {}", membership_str));
    }
    Ok((parts[0].to_string(), parts[1].to_string()))
}

pub fn parse_type_relation(
    relation_str: &str,
) -> Result<(String, String, String), String> {
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

