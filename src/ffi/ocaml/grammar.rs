//! Build an SPG from OCaml values and check programs with it.
//!
//! The grammar is constructed structurally — productions over symbols, no `.auf`
//! source. Typing rules are given in the inference notation (`premises`,
//! `conclusion`), parsed by the same rule parser the loader uses.

use crate::engine::grammar::{Production, SPG, Symbol};
use crate::regex::Regex;
use crate::typing::{Context, TypingRule, TypingSynth, render};

/// A grammar symbol on the OCaml side: a nonterminal, a literal token, or a regex
/// terminal — each optionally binding a name. Variant order matches the OCaml type.
#[derive(ocaml::FromValue)]
pub enum OSym {
    Nt(String, Option<String>),
    Lit(String, Option<String>),
    Re(String, Option<String>),
}

/// A nonterminal definition: name, optional rule name, alternatives of symbols.
type ODef = (String, Option<String>, Vec<Vec<OSym>>);

/// A typing rule: name, premises, conclusion (inference notation).
type ORule = (String, String, String);

fn symbol(s: OSym, specials: &mut Vec<String>) -> Symbol {
    match s {
        OSym::Nt(name, binding) => Symbol::Nonterminal { name, binding },
        OSym::Lit(t, binding) => {
            specials.push(t.clone());
            Symbol::Terminal {
                regex: Regex::literal(&t),
                binding,
            }
        }
        OSym::Re(p, binding) => Symbol::Terminal {
            regex: Regex::from_str(&p).unwrap_or_else(|_| Regex::literal(&p)),
            binding,
        },
    }
}

fn build(
    defs: Vec<ODef>,
    rules: Vec<ORule>,
    rewrites: Vec<(String, String)>,
    start: Option<String>,
) -> Result<SPG, String> {
    let mut g = SPG::new();
    let mut last = None;
    for (name, rule, alts) in defs {
        let mut specials = Vec::new();
        for alt in alts {
            let rhs = alt.into_iter().map(|s| symbol(s, &mut specials)).collect();
            g.add_production(name.clone(), Production { rhs });
        }
        for sp in specials {
            g.add_special(sp);
        }
        if let Some(r) = rule {
            g.bind_nt_rule(name.clone(), r)?;
        }
        last = Some(name);
    }
    for (name, premises, conclusion) in rules {
        let r = TypingRule::new(premises, conclusion, name.clone())?;
        g.add_rule(name, r);
    }
    g.rewrites = rewrites;
    if let Some(s) = start.or(last) {
        g.with_start(s);
    }
    g.build_tokenizer();
    g.build_bindings();
    Ok(g)
}

/// Build the grammar and type-check `program`, returning `program : type` or
/// `error: …`.
#[ocaml::func]
#[must_use]
pub fn aufbau_check(
    defs: Vec<ODef>,
    rules: Vec<ORule>,
    rewrites: Vec<(String, String)>,
    start: Option<String>,
    program: String,
) -> String {
    let g = match build(defs, rules, rewrites, start) {
        Ok(g) => g,
        Err(e) => return format!("error: {e}"),
    };
    let mut synth = TypingSynth::new(g, &program);
    match synth.parse_with(&Context::new()) {
        Ok(ast) => {
            let rt = synth.runtime().clone();
            let g = synth.grammar();
            ast.roots()
                .filter(|r| r.is_complete())
                .find_map(|r| rt.evidence_of(r.evidence()).map(|t| render(g, &t)))
                .map_or_else(|| "error: no type".to_string(), |t| format!("{program} : {t}"))
        }
        Err(e) => format!("error: {e}"),
    }
}
