use super::{Grammar, RepetitionKind, Symbol};
use crate::regex::Regex as DerivativeRegex;
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

/// Parse repetition suffix from a token and return (base_token, repetition_kind)
pub fn parse_repetition_suffix(token: &str) -> (String, Option<RepetitionKind>) {
    if token.ends_with('*') {
        (
            token[..token.len() - 1].to_string(),
            Some(RepetitionKind::ZeroOrMore),
        )
    } else if token.ends_with('+') {
        (
            token[..token.len() - 1].to_string(),
            Some(RepetitionKind::OneOrMore),
        )
    } else if token.ends_with('?') {
        (
            token[..token.len() - 1].to_string(),
            Some(RepetitionKind::ZeroOrOne),
        )
    } else {
        (token.to_string(), None)
    }
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
            let (base_token, repetition) = parse_repetition_suffix(token);
            if is_regex(&base_token) {
                if let Some(rep) = repetition {
                    symbols_in_alt.push(Symbol::with_repetition(base_token, rep));
                } else {
                    symbols_in_alt.push(Symbol::new(base_token));
                }
            } else if let Some(open_bracket) = base_token.find('[') {
                if let Some(close_bracket) = base_token.rfind(']') {
                    if close_bracket > open_bracket {
                        let value = base_token[..open_bracket].to_string();
                        let binding = base_token[open_bracket + 1..close_bracket].to_string();
                        if let Some(rep) = repetition {
                            symbols_in_alt
                                .push(Symbol::with_binding_and_repetition(value, binding, rep));
                        } else {
                            symbols_in_alt.push(Symbol::with_binding(value, binding));
                        }
                        continue;
                    }
                }
                if let Some(rep) = repetition {
                    symbols_in_alt.push(Symbol::with_repetition(base_token, rep));
                } else {
                    symbols_in_alt.push(Symbol::new(base_token));
                }
            } else {
                if let Some(rep) = repetition {
                    symbols_in_alt.push(Symbol::with_repetition(base_token, rep));
                } else {
                    symbols_in_alt.push(Symbol::new(base_token));
                }
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
                // Optional repetition suffix
                let repetition = match chars.peek().cloned() {
                    Some('*') => {
                        chars.next();
                        Some(RepetitionKind::ZeroOrMore)
                    }
                    Some('+') => {
                        chars.next();
                        Some(RepetitionKind::OneOrMore)
                    }
                    Some('?') => {
                        chars.next();
                        Some(RepetitionKind::ZeroOrOne)
                    }
                    _ => None,
                };
                let inner = parse_rhs_with_groups(group_content.trim())?;

                if inner.len() > 1 {
                    return Err(format!(
                        "Nested alternatives inside group are not supported: {}",
                        group_content
                    ));
                }

                let inner_seq = if inner.is_empty() {
                    Vec::new()
                } else {
                    inner.into_iter().next().unwrap()
                };
                symbols_in_alt.push(Symbol::group(inner_seq, repetition));
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

/// Extract special tokens from a symbol (more efficient than parsing RHS string again)
pub fn extract_special_tokens_from_symbol(symbol: &Symbol, tokens: &mut Vec<String>) {
    match symbol {
        Symbol::Litteral(lit) => {
            if !lit.is_empty() && !tokens.contains(lit) {
                tokens.push(lit.clone());
            }
        }
        Symbol::Single { value, .. } => {
            extract_special_tokens_from_symbol(value, tokens);
        }
        Symbol::Group { symbols, .. } => {
            for sym in symbols {
                extract_special_tokens_from_symbol(sym, tokens);
            }
        }
        Symbol::Regex(_) | Symbol::Expression(_) => {
            // Not special tokens
        }
    }
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

// build a big regex to validate tokenizer input
pub fn build_accepted_tokens_regex(grammar: &Grammar) -> Option<DerivativeRegex> {
    let mut regexes = Vec::new();

    // Extract both literal tokens and regex patterns from all productions in a single pass
    for productions in grammar.productions.values() {
        for production in productions {
            for symbol in &production.rhs {
                collect_token_regexes(symbol, &mut regexes);
            }
        }
    }

    // If we have no patterns, return None
    if regexes.is_empty() {
        return None;
    }

    // Build union regex using our custom regex type
    // Sort and deduplicate for consistency
    regexes.sort_by(|a, b| a.to_pattern().cmp(&b.to_pattern()));
    regexes.dedup_by(|a, b| a.equiv(b));

    // The union of the regexes would be the alphabet of accepted tokens
    // We want the Kleene star of that to accept sequences of tokens
    let union = DerivativeRegex::union_many(regexes);
    Some(DerivativeRegex::zero_or_more(union))
}

/// Recursively collect regex patterns from a symbol
fn collect_token_regexes(symbol: &Symbol, regexes: &mut Vec<DerivativeRegex>) {
    match symbol {
        Symbol::Litteral(lit) => {
            // Convert literal tokens to regex
            if !lit.is_empty() {
                regexes.push(DerivativeRegex::literal(lit));
            }
        }
        Symbol::Regex(derivative_regex) => {
            // Use the derivative regex directly
            regexes.push(derivative_regex.clone());
        }
        Symbol::Single { value, .. } => {
            collect_token_regexes(value, regexes);
        }
        Symbol::Group { symbols, .. } => {
            for sym in symbols {
                collect_token_regexes(sym, regexes);
            }
        }
        Symbol::Expression(_) => {
            // Expressions are non-terminals, not tokens
        }
    }
}
