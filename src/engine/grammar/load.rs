use super::utils::{ParsedRhs, parse_nonterminal, parse_production, parse_rhs};
use crate::engine::grammar::{Production, SPG};

/// EBNF parsing. Returns the partial grammar (productions only, no rules) and
/// the non-production text blocks for the rule loader.
pub fn load_ebnf(source: &str) -> Result<(SPG, Vec<String>), String> {
    let mut grammar = SPG::new();
    let mut nt_order: Vec<String> = Vec::new();
    let mut rule_blocks: Vec<String> = Vec::new();
    let all: Vec<&str> = source.lines().collect();

    for block in all.split(|l| l.trim().is_empty()) {
        let lines: Vec<&str> = block
            .iter()
            .map(|l| l.trim())
            .filter(|line| !line.is_empty() && !line.starts_with("//"))
            .collect();

        if lines.is_empty() {
            continue;
        }

        if lines.iter().any(|line| line.contains("::=")) {
            let mut i = 0;
            while i < lines.len() {
                let line = lines[i];
                if line.contains("::=") {
                    let mut production_lines = vec![line];
                    i += 1;
                    while i < lines.len() && lines[i].starts_with('|') {
                        production_lines.push(lines[i]);
                        i += 1;
                    }
                    let production_str = production_lines.join("\n");
                    let (lhs_str, rhs_str) = parse_production(&production_str.replace('\n', " "))?;
                    let (name, rule_name) = parse_nonterminal(&lhs_str)?;
                    let ParsedRhs {
                        alternatives,
                        literal_tokens,
                    } = parse_rhs(&rhs_str)?;

                    if !nt_order.contains(&name) {
                        nt_order.push(name.clone());
                    }
                    for literal in literal_tokens {
                        grammar.add_special(literal);
                    }
                    // `Ty(*) ::= …` is a kind annotation, not a rule: it
                    // designates the type fragment (`def:term-language`).
                    if rule_name.as_deref() == Some("*") {
                        if let Some(prev) = &grammar.ty {
                            return Err(format!("two type fragments: {prev}, {name}"));
                        }
                        grammar.ty = Some(name.clone());
                    } else if let Some(rule_name) = rule_name.clone() {
                        grammar.bind_nt_rule(name.clone(), rule_name)?;
                    }
                    for alt_symbols in alternatives {
                        grammar.add_production(name.clone(), Production { rhs: alt_symbols });
                    }
                } else {
                    i += 1;
                }
            }
        } else {
            rule_blocks.push(lines.join("\n"));
        }
    }

    if grammar.start().is_none()
        && let Some(last) = nt_order.last()
    {
        grammar.with_start(last.clone());
    }

    Ok((grammar, rule_blocks))
}

impl SPG {
    /// Load a complete grammar from `.auf` source.
    pub fn load(source: &str) -> Result<Self, String> {
        let (mut grammar, rule_blocks) = load_ebnf(source)?;
        let block_refs: Vec<&str> = rule_blocks.iter().map(String::as_str).collect();
        let (rules, rewrites) = crate::typing::loader::load(&block_refs)?;
        for (name, rule) in rules {
            grammar.add_rule(name, rule);
        }
        grammar.rewrites = rewrites;
        grammar.build_bindings();
        grammar.build_tokenizer();
        crate::typing::loader::check(&grammar)?;

        Ok(grammar)
    }
}
