use super::utils::{
    ParsedRhs, build_accepted_tokens_regex, parse_inference_rule, parse_nonterminal,
    parse_production, parse_rhs,
};
use crate::logic::grammar::{Grammar, Production, TypingRule};

impl Grammar {
    /// Parse the textual specification into a `Grammar`.
    pub fn load(input: &str) -> Result<Grammar, String> {
        let mut grammar = Grammar::new();
        // Track first-seen order of nonterminals to pick a deterministic start symbol
        let mut nt_order: Vec<String> = Vec::new();
        // Split input into blocks separated by blank lines
        let blocks: Vec<&str> = input
            .split("\n\n")
            .filter(|b| !b.trim().is_empty())
            .collect();

        for block in blocks {
            let lines: Vec<&str> = block
                .lines()
                .map(str::trim)
                .filter(|line| !line.is_empty() && !line.starts_with("//"))
                .collect();

            if lines.is_empty() {
                continue;
            }

            // Check if this block contains a production rule
            if lines.iter().any(|line| line.contains("::=")) {
                // Production block - may contain multiple productions
                let mut i = 0;
                while i < lines.len() {
                    let line = lines[i];
                    if line.contains("::=") {
                        // Start of a new production
                        let mut production_lines = vec![line];
                        i += 1;

                        // Collect any continuation lines starting with |
                        while i < lines.len() && lines[i].starts_with('|') {
                            production_lines.push(lines[i]);
                            i += 1;
                        }

                        // Parse this production
                        let production_str = production_lines.join("\n");
                        let (lhs_str, rhs_str) =
                            parse_production(&production_str.replace('\n', " "))?;
                        let (name, rule_name) = parse_nonterminal(&lhs_str)?;
                        let parsed_rhs = parse_rhs(&rhs_str)?;
                        let ParsedRhs {
                            alternatives,
                            literal_tokens,
                        } = parsed_rhs;

                        // Record first time we see this nonterminal (declaration order)
                        if !nt_order.contains(&name) {
                            nt_order.push(name.clone());
                            grammar.production_order.push(name.clone());
                        }

                        for literal in literal_tokens {
                            grammar.add_special_token(literal);
                        }

                        // Create productions for each alternative
                        for alt_symbols in alternatives {
                            let production = Production {
                                rule: rule_name.clone(),
                                rhs: alt_symbols,
                            };
                            grammar
                                .productions
                                .entry(name.clone())
                                .or_default()
                                .push(production);
                        }
                    } else {
                        i += 1;
                    }
                }
            } else {
                let (premises, conclusion, name) = parse_inference_rule(&lines)?;
                grammar.add_typing_rule(TypingRule::new(premises, conclusion, name)?);
            }
        }

        // By convention, set the start symbol to the last declared production LHS
        if grammar.start_nonterminal().is_none() {
            if let Some(last) = grammar.production_order.last() {
                grammar.set_start(last.clone());
            }
        }

        // Build the unified accepted tokens regex
        grammar.accepted_tokens_regex = build_accepted_tokens_regex(&grammar);

        // Build the binding map
        grammar.rebuild_bindings();

        Ok(grammar)
    }
}
