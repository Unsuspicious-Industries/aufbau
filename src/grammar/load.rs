use super::utils::{ParsedRhs, parse_nonterminal, parse_production, parse_rhs};
use crate::engine::grammar::{Production, SPG, Symbol};
use crate::regex::Regex;
use crate::typing::TypingRule;

/// A symbol description for structural assembly: what one `.auf` RHS token
/// gives, without `.auf` source. Each optionally binds a name.
pub enum Sym {
    Nt(String, Option<String>),
    Lit(String, Option<String>),
    Re(String, Option<String>),
}

/// A nonterminal definition: name, optional rule name, alternatives.
pub type Def = (String, Option<String>, Vec<Vec<Sym>>);

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
                    // `Ty* ::= …` marks the type fragment: Ty has kind `*`
                    // (`def:term-language`).
                    let name = match name.strip_suffix('*') {
                        Some(n) => {
                            let n = n.trim().to_string();
                            if grammar.ty.as_ref().is_some_and(|prev| *prev != n) {
                                return Err(format!(
                                    "two type fragments: {}, {n}",
                                    grammar.ty.as_deref().unwrap_or("")
                                ));
                            }
                            grammar.ty = Some(n.clone());
                            n
                        }
                        None => name,
                    };
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
                    if let Some(rule_name) = rule_name.clone() {
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

    /// Assemble a grammar structurally, no `.auf` source: the single entry the
    /// FFIs share. Runs the same validation as [`SPG::load`]. The start symbol
    /// defaults to the last definition, as in the surface syntax.
    pub fn assemble(
        defs: Vec<Def>,
        rules: Vec<TypingRule>,
        rewrites: Vec<(String, String)>,
        start: Option<String>,
        ty: Option<String>,
    ) -> Result<Self, String> {
        let mut g = SPG::new();
        let mut last = None;
        for (name, rule, alts) in defs {
            for alt in alts {
                let rhs = alt
                    .into_iter()
                    .map(|s| match s {
                        Sym::Nt(name, binding) => Symbol::Nonterminal { name, binding },
                        Sym::Lit(t, binding) => {
                            g.add_special(t.clone());
                            Symbol::Terminal {
                                regex: Regex::literal(&t),
                                binding,
                            }
                        }
                        Sym::Re(p, binding) => Symbol::Terminal {
                            regex: Regex::from_str(&p).unwrap_or_else(|_| Regex::literal(&p)),
                            binding,
                        },
                    })
                    .collect();
                g.add_production(name.clone(), Production { rhs });
            }
            if let Some(r) = rule {
                g.bind_nt_rule(name.clone(), r)?;
            }
            last = Some(name);
        }
        for rule in rules {
            g.add_rule(rule.name.clone(), rule);
        }
        g.rewrites = rewrites;
        g.ty = ty;
        if let Some(s) = start.or(last) {
            g.with_start(s);
        }
        g.build_tokenizer();
        g.build_bindings();
        crate::typing::loader::check(&g)?;
        Ok(g)
    }
}
