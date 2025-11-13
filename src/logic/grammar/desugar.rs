use super::Symbol;
use crate::logic::grammar::utils::{parse_rhs, parse_production, parse_nonterminal};

/// The result of desugaring a single production's RHS for a given LHS.
/// - `alternatives`: the desugared alternatives for the original LHS
/// - `synthetic`: list of synthetic productions introduced (name, rhs symbols)
#[derive(Debug, Clone)]
pub struct DesugaredRhs {
    pub alternatives: Vec<Vec<Symbol>>,         // LHS alternatives
    pub synthetic: Vec<(String, Vec<Symbol>)>,  // (synthetic name, rhs)
}

/// Desugar a RHS string with parentheses groups and + repetition into:
/// - synthetic productions for groups, named `<lhs>.<i>`
/// - expansion of OneOrMore(X) into `X X*`
pub fn desugarify_rhs(lhs: &str, rhs: &str) -> Result<DesugaredRhs, String> {
    // Split alternatives respecting quotes and parentheses depth
    fn split_alternatives(rhs: &str) -> Result<Vec<String>, String> {
        let mut alternatives = Vec::new();
        let mut current = String::new();
        let mut in_single = false;
        let mut in_double = false;
        let mut depth: i32 = 0;
        let mut chars = rhs.chars().peekable();
        while let Some(ch) = chars.next() {
            if ch == '/' && !in_single && !in_double {
                current.push(ch);
                while let Some(rch) = chars.next() {
                    current.push(rch);
                    if rch == '/' { break; }
                }
                continue;
            }
            match ch {
                '\'' if !in_double => { in_single = !in_single; current.push(ch); }
                '"' if !in_single => { in_double = !in_double; current.push(ch); }
                '(' if !in_single && !in_double => { depth += 1; current.push(ch); }
                ')' if !in_single && !in_double => { depth -= 1; current.push(ch); }
                '|' if !in_single && !in_double && depth == 0 => { alternatives.push(current.trim().to_string()); current.clear(); }
                _ => current.push(ch),
            }
        }
        if in_single || in_double { return Err(format!("Unclosed quotes in grammar rule: {}", rhs)); }
        if !current.trim().is_empty() { alternatives.push(current.trim().to_string()); }
        Ok(alternatives)
    }

    // Expand a symbol with + into two symbols: base and base*
    fn expand_one_or_more(sym: Symbol) -> Vec<Symbol> {
        match sym {
            Symbol::Single { value, binding, repetition: Some((1, None)) } => {
                // If no binding, unwrap the base symbol directly
                let base = match &binding {
                    None => *value.clone(),
                    Some(b) => Symbol::Single { value: value.clone(), binding: Some(b.clone()), repetition: None },
                };
                let star = Symbol::Single { value, binding, repetition: Some((0, None)) };
                vec![base, star]
            }
            other => vec![other],
        }
    }

    fn parse_one_alt(
        lhs: &str,
        alt: &str,
        counter: &mut usize,
        synthetic: &mut Vec<(String, Vec<Symbol>)>,
    ) -> Result<Vec<Symbol>, String> {
        let mut symbols: Vec<Symbol> = Vec::new();
        let mut chars = alt.chars().peekable();
        let mut in_single = false;
        let mut in_double = false;
        while let Some(ch) = chars.peek().cloned() {
            if ch.is_whitespace() { chars.next(); continue; }
            match ch { '\'' => in_single = !in_single, '"' => in_double = !in_double, _ => {} }
            if ch == '(' && !in_single && !in_double {
                // Consume '('
                chars.next();
                let mut depth: i32 = 1;
                let mut inner = String::new();
                while let Some(c2) = chars.next() {
                    if c2 == '(' { depth += 1; }
                    else if c2 == ')' { depth -= 1; if depth == 0 { break; } }
                    if depth > 0 { inner.push(c2); }
                }
                if depth != 0 { return Err(format!("Unclosed group in RHS: {}", alt)); }
                // Optional repetition suffix
                let repetition = match chars.peek().cloned() {
                    Some('*') => { chars.next(); Some((0, None)) }
                    Some('+') => { chars.next(); Some((1, None)) }
                    Some('?') => { chars.next(); Some((0, Some(1))) }
                    _ => None,
                };

                // Recursively parse the group's content to allow nested groups
                let synth_idx = *counter;
                *counter += 1;
                let synth_name = format!("{}.{}", lhs, synth_idx);
                let inner_alts = split_alternatives(inner.trim())?;
                if inner_alts.len() > 1 {
                    return Err(format!("Nested alternatives inside group are not supported: {}", inner));
                }
                let inner_seq = if inner_alts.is_empty() { String::new() } else { inner_alts[0].clone() };
                let mut inner_counter = 0usize;
                let expanded = parse_one_alt(
                    &synth_name,
                    &inner_seq,
                    &mut inner_counter,
                    synthetic,
                )?;
                // Register synthetic production for multi-symbol group
                synthetic.push((synth_name.clone(), expanded));
                // Replace group with reference to synthetic nonterminal, carrying repetition
                match repetition {
                    None => symbols.push(Symbol::Expression(synth_name)),
                    Some((0, None)) => symbols.push(Symbol::Single {
                        value: Box::new(Symbol::Expression(synth_name)),
                        binding: None,
                        repetition: Some((0, None)),
                    }),
                    Some((0, Some(1))) => symbols.push(Symbol::Single {
                        value: Box::new(Symbol::Expression(synth_name)),
                        binding: None,
                        repetition: Some((0, Some(1))),
                    }),
                    Some((1, None)) => {
                        // Expand S+ into S S*
                        symbols.push(Symbol::Expression(synth_name.clone()));
                        symbols.push(Symbol::Single {
                            value: Box::new(Symbol::Expression(synth_name)),
                            binding: None,
                            repetition: Some((0, None)),
                        });
                    }
                    Some(_) => {
                        return Err(format!("Unsupported repetition for group in RHS: {}", alt));
                    }
                }
            } else {
                // Parse a token (non-group). Supports quoted literals/strings, regex, optional [binding], and suffixes.
                let mut token = String::new();
            if let Some(c2) = chars.peek().cloned() {
                if c2 == '\'' {
                    // Quoted literal
                    chars.next();
                    token.push('\'');
                    while let Some(c3) = chars.next() { token.push(c3); if c3 == '\'' { break; } }
                    // Optional binding [..]
                    if let Some('[') = chars.peek().cloned() {
                        token.push('[');
                        chars.next();
                        while let Some(c3) = chars.next() { token.push(c3); if c3 == ']' { break; } }
                    }
                    // Optional repetition suffix
                    if let Some(next) = chars.peek().cloned() {
                        if next == '*' || next == '+' || next == '?' { token.push(next); chars.next(); }
                    }
                } else if c2 == '"' {
                    // Quoted string
                    chars.next();
                    token.push('"');
                    while let Some(c3) = chars.next() { token.push(c3); if c3 == '"' { break; } }
                    if let Some('[') = chars.peek().cloned() {
                        token.push('[');
                        chars.next();
                        while let Some(c3) = chars.next() { token.push(c3); if c3 == ']' { break; } }
                    }
                    if let Some(next) = chars.peek().cloned() {
                        if next == '*' || next == '+' || next == '?' { token.push(next); chars.next(); }
                    }
                } else if c2 == '/' {
                    // Regex literal
                    chars.next();
                    token.push('/');
                    while let Some(c3) = chars.next() { token.push(c3); if c3 == '/' { break; } }
                    if let Some('[') = chars.peek().cloned() {
                        token.push('[');
                        chars.next();
                        while let Some(c3) = chars.next() { token.push(c3); if c3 == ']' { break; } }
                    }
                    if let Some(next) = chars.peek().cloned() {
                        if next == '*' || next == '+' || next == '?' { token.push(next); chars.next(); }
                    }
                } else {
                    // Bare token up to whitespace or paren
                    while let Some(c3) = chars.peek().cloned() {
                        if c3.is_whitespace() || c3 == '(' || c3 == ')' { break; }
                        token.push(c3);
                        chars.next();
                        if c3 == '[' {
                            // Read until closing bracket
                            while let Some(c4) = chars.next() { token.push(c4); if c4 == ']' { break; } }
                        }
                    }
                }
            }
            if token.is_empty() { chars.next(); continue; }
            let parsed = parse_rhs(&token)?;
            if parsed.len() != 1 || parsed[0].len() != 1 {
                return Err(format!("Unexpected token decomposition for '{}': parsed={:?}", token, parsed));
            }
            let sym = parsed[0][0].clone();
            let expanded_syms = expand_one_or_more(sym);
            symbols.extend(expanded_syms);
            }
        }
        Ok(symbols)
    }

    let mut synthetic: Vec<(String, Vec<Symbol>)> = Vec::new();
    let mut counter: usize = 0;
    let mut alts = Vec::new();
    for alt in split_alternatives(rhs)? {
        let expanded = parse_one_alt(lhs, &alt, &mut counter, &mut synthetic)?;
        alts.push(expanded);
    }
    Ok(DesugaredRhs { alternatives: alts, synthetic })
}

/// Optional helper to desugar an entire production line like `A ::= ...`.
/// Returns (lhs, rule_name, desugared rhs, synthetic productions)
pub fn desugarify_production(line: &str) -> Result<(String, Option<String>, DesugaredRhs), String> {
    let (lhs_str, rhs_str) = parse_production(line)?;
    let (name, rule_name) = parse_nonterminal(&lhs_str)?;
    let rhs = desugarify_rhs(&name, &rhs_str)?;
    Ok((name, rule_name, rhs))
}
