use super::utils::{
    ParsedRhs, ParsedSymbol, RepeatKind, parse_inference_rule, parse_nonterminal, parse_production,
    parse_rhs,
};
use crate::logic::grammar::{Grammar, Production, Symbol, TypingRule};

impl Grammar {
    /// Parse the textual specification into a `Grammar`.
    pub fn load(input: &str) -> Result<Grammar, String> {
        let mut grammar = Grammar::new();
        let mut repetition_counter = 0usize;
        // Track first-seen order of nonterminals to pick a deterministic start symbol
        let mut nt_order: Vec<String> = Vec::new();
        // Split input into blocks separated by blank (or whitespace-only) lines
        let mut blocks = Vec::new();
        let mut current = Vec::new();
        for line in input.lines() {
            if line.trim().is_empty() {
                if !current.is_empty() {
                    blocks.push(current.join("\n"));
                    current.clear();
                }
            } else {
                current.push(line);
            }
        }
        if !current.is_empty() {
            blocks.push(current.join("\n"));
        }

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
                        }

                        for literal in literal_tokens {
                            grammar.add_special_token(literal);
                        }

                        // Create productions for each alternative
                        for (alt_idx, alt_symbols) in alternatives.into_iter().enumerate() {
                            let production = Production {
                                rule: rule_name.clone(),
                                rhs: expand_repetitions(
                                    &mut grammar,
                                    &name,
                                    rule_name.as_deref(),
                                    alt_idx,
                                    alt_symbols,
                                    &mut repetition_counter,
                                ),
                            };
                            grammar.add_production(name.clone(), production);
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
        if grammar.start_nonterminal().is_none()
            && let Some(last) = nt_order.last()
        {
            grammar.set_start(last.clone());
        }

        // Build the binding map
        grammar.rebuild_bindings();

        // Prepare the tokenizer regexes
        grammar.prepare_tokenizer();

        Ok(grammar)
    }
}

fn expand_repetitions(
    grammar: &mut Grammar,
    lhs: &str,
    rule_name: Option<&str>,
    alt_idx: usize,
    symbols: Vec<ParsedSymbol>,
    repetition_counter: &mut usize,
) -> Vec<Symbol> {
    symbols
        .into_iter()
        .map(|parsed| match parsed.repetition {
            None => parsed.symbol,
            Some(kind) => {
                let helper = format!(
                    "__rep_{}_{}_{}_{}",
                    sanitize_nt(lhs),
                    rule_name.unwrap_or("_"),
                    alt_idx,
                    *repetition_counter
                );
                *repetition_counter += 1;
                grammar.add_hidden_nonterminal(helper.clone());
                add_repetition_productions(grammar, &helper, parsed.symbol, kind);
                Symbol::Nonterminal {
                    name: helper,
                    binding: None,
                }
            }
        })
        .collect()
}

fn add_repetition_productions(
    grammar: &mut Grammar,
    helper: &str,
    symbol: Symbol,
    kind: RepeatKind,
) {
    // Binding-path safety invariant:
    // - helper productions never carry a rule name
    // - helper nonterminals are marked hidden
    // Therefore binding collection drills through them exactly like any other
    // anonymous structural node, so bindings attached inside the repeated item
    // retain the same observable path modulo hidden-node flattening.
    let self_ref = Symbol::Nonterminal {
        name: helper.to_string(),
        binding: None,
    };

    match kind {
        RepeatKind::Optional => {
            grammar.add_production(
                helper.to_string(),
                Production {
                    rule: None,
                    rhs: Vec::new(),
                },
            );
            grammar.add_production(
                helper.to_string(),
                Production {
                    rule: None,
                    rhs: vec![symbol],
                },
            );
        }
        RepeatKind::ZeroOrMore => {
            grammar.add_production(
                helper.to_string(),
                Production {
                    rule: None,
                    rhs: Vec::new(),
                },
            );
            grammar.add_production(
                helper.to_string(),
                Production {
                    rule: None,
                    rhs: vec![symbol, self_ref],
                },
            );
        }
        RepeatKind::OneOrMore => {
            grammar.add_production(
                helper.to_string(),
                Production {
                    rule: None,
                    rhs: vec![symbol.clone()],
                },
            );
            grammar.add_production(
                helper.to_string(),
                Production {
                    rule: None,
                    rhs: vec![symbol, self_ref],
                },
            );
        }
    }
}

fn sanitize_nt(value: &str) -> String {
    value
        .chars()
        .map(|ch| if ch.is_ascii_alphanumeric() { ch } else { '_' })
        .collect()
}
