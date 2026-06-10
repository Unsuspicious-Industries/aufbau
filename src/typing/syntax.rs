//! Rule-expression parsing — §2.
//!
//! A `TypeExpr` is tokenized into atoms: `'lit'` → `Lit`, `?A` → `Hole`,
//! `typeof(b)` and bare identifiers → `Ref`, `Γ(x)` → `Ctx`, `⊤`/`∅` →
//! `Top`/`Bot`. Everything between (spaces, `->`, …) is literal separator text.
//! There is no arrow/union/negation grammar: structure is sequencing.
//!
//! [`TyExpr::build`] then recovers the *tree* a `TypeExpr` denotes by parsing it
//! with the grammar: the rule's structure is the grammar's, not a second syntax.
//! Elaboration is explicit, with no guessing from how an identifier is spelled:
//!   - a bare identifier is a binding reference exactly when it is one of the
//!     rule's declared bindings ([`SPG::rule_bindings`]); otherwise it is
//!     object-grammar text (a type keyword such as `list`);
//!   - every meta (a hole, a non-keyword ref, `Γ(x)`, `⊤`/`∅`) is replaced by a
//!     `?…` placeholder the augmented grammar accepts wherever a type atom can
//!     stand, and the unique complete parse is mapped back;
//!   - a derivation that swallows a placeholder as an interior keyword (reading
//!     `?A list` as `fst list`) never reproduces the placeholder as a leaf and is
//!     discarded, so the metavariable genuinely occupies an atom position.
//!
//! [`SPG::rule_bindings`]: crate::engine::grammar::SPG::rule_bindings

use super::{Atom, Term, TyExpr, TypeExpr};
use crate::engine::grammar::{Symbol, SPG};
use crate::engine::structure::{FusionChild, FusionNode};
use crate::regex::Regex;
use crate::typing::{Context, TypingSynth};
use std::collections::{HashMap, HashSet};

impl TyExpr {
    /// Project a type-expression tree to a pure type [`Term`] (the structure a
    /// rewrite rule rewrites). Only `Var`/`Lit`/`Con` survive; `Ref`/`Ctx`/`Inst`/
    /// `⊤`/`∅` are *resolution* constructs (look up an obligation or the context),
    /// which have no meaning in a structural rewrite, so they are refused.
    pub fn to_term(&self) -> Result<Term, String> {
        Ok(match self {
            TyExpr::Var(n) => Term::var(n.clone()),
            TyExpr::Lit(s) => Term::raw(s),
            TyExpr::Con(label, kids) => Term::con(
                label.clone(),
                kids.iter().map(TyExpr::to_term).collect::<Result<_, _>>()?,
            ),
            TyExpr::Ref(n) => return Err(format!("rewrite cannot reference a binding '{n}'")),
            TyExpr::Ctx(v) => return Err(format!("rewrite cannot look up a context 'Γ({v})'")),
            TyExpr::Inst(v) => return Err(format!("rewrite cannot instantiate a context 'inst({v})'")),
            TyExpr::Top => return Err("rewrite cannot use ⊤".into()),
            TyExpr::Bot => return Err("rewrite cannot use ∅".into()),
        })
    }
}

/// Render a type term back to the grammar's surface syntax (the inverse of
/// parsing): a constructor interleaves its children with its production's literal
/// terminals, a leaf is its text, a hole is `?A`. No re-parenthesization, so an
/// ambiguous grammar may not round-trip; unknown constructors fall back to
/// `label(…)`.
#[must_use]
pub fn render(g: &SPG, t: &Term) -> String {
    match t {
        Term::Var(n) => format!("?{n}"),
        Term::Leaf(p) => p.to_string(),
        Term::Con(label, kids) => {
            let prod = g.productions.get(label).and_then(|ps| {
                ps.iter().find(|p| {
                    p.rhs
                        .iter()
                        .filter(|s| matches!(s, Symbol::Nonterminal { .. }))
                        .count()
                        == kids.len()
                })
            });
            let Some(prod) = prod else {
                let inner: Vec<String> = kids.iter().map(|k| render(g, k)).collect();
                return format!("{label}({})", inner.join(", "));
            };
            let mut kids = kids.iter();
            let mut out = Vec::new();
            for sym in &prod.rhs {
                match sym {
                    Symbol::Terminal { regex, .. } => out.push(regex.to_pattern().replace('\\', "")),
                    Symbol::Nonterminal { .. } => {
                        if let Some(k) = kids.next() {
                            out.push(render(g, k));
                        }
                    }
                }
            }
            out.join(" ")
        }
    }
}

/// Replace each `?Ident` with a fresh placeholder mapped back to a `Var`; copy all
/// other text verbatim for the grammar to parse.
fn skeletonize_holes(s: &str) -> (String, HashMap<String, TyExpr>) {
    let chars: Vec<char> = s.chars().collect();
    let mut out = String::new();
    let mut metas = HashMap::new();
    let mut i = 0;
    while i < chars.len() {
        if chars[i] == '?' {
            let (name, next) = take_while(&chars, i + 1, is_ident);
            if !name.is_empty() {
                let ph = format!("?_m{}", metas.len());
                metas.insert(ph.clone(), TyExpr::Var(name));
                out.push_str(&ph);
                i = next;
                continue;
            }
        }
        out.push(chars[i]);
        i += 1;
    }
    (out, metas)
}

impl Term {
    /// Parse a type string into its tree, by parsing it with `grammar`. `?A` is a
    /// metavariable ([`Term::Var`]); every other token is concrete type syntax (a
    /// bare identifier is a type name, *not* a binding ref). Holes are skeletonized
    /// to placeholders the augmented grammar accepts, the unique parse gives the
    /// structure, and the result is projected to a term. The single text→type
    /// entry point: context/annotation types, rewrite sides, CLI/FFI, tests.
    pub fn parse(grammar: &SPG, s: &str) -> Result<Self, String> {
        let (skeleton, metas) = skeletonize_holes(s);
        let g = augment(grammar)?;
        parse_unique(&g, &skeleton, &metas, |root| from_node(root, &metas))
            .map_err(|e| format!("type '{s}' {e}"))?
            .to_term()
    }
}

/// Parse `s` with each nonterminal as the start and return the unique complete
/// derivation, converted by `f`. No designated start: a type-shaped string is
/// derived only by the type productions, and two distinct results is a real
/// ambiguity. A derivation that swallows a placeholder as an interior keyword
/// (so it never reappears as an atom leaf) is not a materialization of the
/// metavariable and is discarded via [`all_holes_present`]. Shared by
/// concrete-type and rule-pattern building.
fn parse_unique<T: PartialEq>(
    g: &SPG,
    s: &str,
    metas: &HashMap<String, TyExpr>,
    f: impl Fn(&FusionNode) -> T,
) -> Result<T, String> {
    let names: Vec<String> = (0..g.production_count())
        .filter_map(|i| g.nt(i).map(str::to_string))
        .collect();
    let mut found: Option<T> = None;
    for name in names {
        let mut g = g.clone();
        g.with_start(name);
        let mut synth = TypingSynth::new(g, s.to_string());
        let Ok(ast) = synth.parse_with(&Context::new()) else {
            continue;
        };
        for root in ast
            .roots()
            .filter(FusionNode::is_complete)
            .filter(|r| all_holes_present(r, metas))
        {
            let t = f(&root);
            match &found {
                None => found = Some(t),
                Some(x) if *x == t => {}
                Some(_) => return Err("is ambiguous".into()),
            }
        }
    }
    found.ok_or_else(|| "has no complete parse".into())
}

/// Does every skeleton placeholder reappear as an atom leaf in this derivation?
/// A placeholder occupies a full atom position, so some node's text is exactly
/// the placeholder; one consumed as an interior keyword (e.g. the `'fst'` of a
/// `Fst` node) is never reproduced, marking that derivation a misparse.
fn all_holes_present(node: &FusionNode, metas: &HashMap<String, TyExpr>) -> bool {
    let mut seen = HashSet::new();
    collect_placeholders(node, metas, &mut seen);
    seen.len() == metas.len()
}

fn collect_placeholders(
    node: &FusionNode,
    metas: &HashMap<String, TyExpr>,
    seen: &mut HashSet<String>,
) {
    let text = node.text();
    if metas.contains_key(text.trim()) {
        seen.insert(text.trim().to_string());
        return;
    }
    for child in node.children() {
        if let FusionChild::Node(n) = child {
            collect_placeholders(&n, metas, seen);
        }
    }
}

impl TypeExpr {
    pub fn parse(s: &str) -> Result<Self, String> {
        let s = s.trim();
        if s.is_empty() || s == "⊤" || s == "Any" {
            return Ok(TypeExpr(vec![Atom::Top]));
        }
        if s == "∅" || s == "None" {
            return Ok(TypeExpr(vec![Atom::Bot]));
        }

        let chars: Vec<char> = s.chars().collect();
        let mut atoms = Vec::new();
        let mut sep = String::new();
        let mut i = 0;

        while i < chars.len() {
            let c = chars[i];
            if c == '\'' {
                flush(&mut atoms, &mut sep);
                let (lit, next) = take_until(&chars, i + 1, '\'')
                    .ok_or_else(|| format!("unterminated literal in '{s}'"))?;
                atoms.push(Atom::Lit(lit));
                i = next + 1;
            } else if c == '?' {
                flush(&mut atoms, &mut sep);
                let (name, next) = take_while(&chars, i + 1, is_ident);
                if name.is_empty() {
                    return Err(format!("empty hole in '{s}'"));
                }
                atoms.push(Atom::Hole(name));
                i = next;
            } else if c.is_alphabetic() || c == '_' {
                let (id, after_id) = take_while(&chars, i, is_ident);
                if chars.get(after_id) == Some(&'(') {
                    let (inner, close) = take_until(&chars, after_id + 1, ')')
                        .ok_or_else(|| format!("unterminated '(' in '{s}'"))?;
                    flush(&mut atoms, &mut sep);
                    atoms.push(call_atom(&id, inner.trim())?);
                    i = close + 1;
                } else {
                    flush(&mut atoms, &mut sep);
                    atoms.push(Atom::Ref(id));
                    i = after_id;
                }
            } else {
                sep.push(c);
                i += 1;
            }
        }
        flush(&mut atoms, &mut sep);
        Ok(TypeExpr(atoms))
    }
}

impl TyExpr {
    /// Recover the tree of a `TypeExpr` by parsing it with `grammar`. `bindings`
    /// are the rule's declared bindings ([`SPG::rule_bindings`]): a bare
    /// identifier names a reference (`τ`) when it is one and object-grammar text
    /// (the keyword `list`) when it is not. A type with no internal separators is
    /// a leaf or a lone meta (handled directly); otherwise the metas (`?A`, `τ`,
    /// `Γ(x)`, `⊤`/`∅`) are replaced by `?`-placeholders that the augmented
    /// grammar accepts as a type atom, the unique complete parse gives the
    /// structure, and the placeholder leaves are mapped back.
    pub fn build(grammar: &SPG, expr: &TypeExpr, bindings: &HashSet<String>) -> Result<Self, String> {
        match expr.0.as_slice() {
            [] | [Atom::Top] => return Ok(Self::Top),
            [Atom::Bot] => return Ok(Self::Bot),
            [Atom::Hole(n)] => return Ok(Self::Var(n.clone())),
            [Atom::Ref(n)] if bindings.contains(n) => return Ok(Self::Ref(n.clone())),
            [Atom::Ctx(n)] => return Ok(Self::Ctx(n.clone())),
            [Atom::Lit(s)] => return Ok(Self::Lit(s.trim().to_string())),
            _ => {}
        }
        let (skeleton, metas) = skeletonize(expr, bindings);
        let g = augment(grammar)?;
        parse_unique(&g, &skeleton, &metas, |root| from_node(root, &metas))
            .map_err(|e| format!("type pattern '{expr}' {e}"))
    }
}

/// `?[A-Za-z0-9_]+` — the hole atom the augmented grammar recognizes. This is the
/// one hardcoded shape; everything else is the grammar's own.
fn hole_regex() -> Regex {
    let idchar = Regex::union_many([
        Regex::Range('A', 'Z'),
        Regex::Range('a', 'z'),
        Regex::Range('0', '9'),
        Regex::Char('_'),
    ]);
    Regex::cat(Regex::Char('?'), Regex::cat(idchar.clone(), Regex::star(idchar)))
}

/// Emit a placeholder string for the meta atoms and a map back to `TyExpr`
/// leaves; literal text (separators, quoted types) is copied verbatim. A bare
/// identifier that is not one of the rule's `bindings` is object-grammar text (a
/// type keyword like `list`), so it too is copied verbatim for the grammar to
/// lex, rather than masked behind a metavariable placeholder.
fn skeletonize(expr: &TypeExpr, bindings: &HashSet<String>) -> (String, HashMap<String, TyExpr>) {
    let mut s = String::new();
    let mut metas = HashMap::new();
    for (i, atom) in expr.0.iter().enumerate() {
        match atom {
            Atom::Lit(t) => {
                s.push_str(t);
                continue;
            }
            Atom::Ref(n) if !bindings.contains(n) => {
                s.push_str(n);
                continue;
            }
            _ => {}
        }
        let ph = format!("?_m{i}");
        let te = match atom {
            Atom::Hole(n) => TyExpr::Var(n.clone()),
            Atom::Ref(n) => TyExpr::Ref(n.clone()),
            Atom::Ctx(n) => TyExpr::Ctx(n.clone()),
            Atom::Inst(n) => TyExpr::Inst(n.clone()),
            Atom::Top => TyExpr::Top,
            Atom::Bot => TyExpr::Bot,
            Atom::Lit(_) => unreachable!(),
        };
        s.push_str(&ph);
        metas.insert(ph, te);
    }
    (s, metas)
}

/// Clone `grammar` so a hole atom parses anywhere a type leaf can. Refuse if the
/// grammar already accepts a `?…` token: the meta marker must be disjoint from
/// the grammar's own alphabet (the same soundness gate as leaf/separator
/// separability). The start is chosen by the caller (each nonterminal in turn).
fn augment(grammar: &SPG) -> Result<SPG, String> {
    let hole = hole_regex();
    for prods in grammar.productions.values() {
        for p in prods {
            for sym in &p.rhs {
                if let Symbol::Terminal { regex, .. } = sym
                    && regex.has_intersection(&hole)
                {
                    return Err(format!(
                        "grammar terminal /{}/ overlaps the hole syntax '?…'",
                        regex.to_pattern()
                    ));
                }
            }
        }
    }
    let mut g = grammar.clone();
    // Skeleton parsing is purely structural; drop the typing *rules* so spinning
    // up a runtime over this grammar does not recurse back into `TyExpr::build`
    // (`type_trees` iterates the rules).
    //
    // Keep only the labels of *type constructors* — rule-less labelled
    // productions like `ListType(list)`. Their label is what marks a node as a
    // constructor rather than a transparent wrapper, so a unary type like
    // `τ list` keeps its structure instead of collapsing to `τ`. A label that
    // carries a typing rule (`Variable(var)`, `Application(app)`) is instead an
    // *expression* constructor, which a type can never contain; dropping its
    // label lets a spurious expression reading of a type string (`Int` as a
    // `Variable`) collapse as a transparent wrapper to the same leaf rather than
    // compete as a distinct tree, which would read as ambiguity.
    g.nonterminal_rules
        .retain(|_, label| !grammar.rules.contains_key(label));
    g.rules.clear();
    for prods in g.productions.values_mut() {
        for p in prods.iter_mut() {
            for sym in p.rhs.iter_mut() {
                // Every terminal also admits the hole shape, so a hole can stand
                // in any atom position — including a keyword-valued one like
                // `Atom ::= 'Int'`. A hole that is instead *swallowed* as an
                // interior keyword (the `'fst'` of `'fst' AtomicExpression`, which
                // would read `?A list` as `fst list`) is rejected downstream by
                // `parse_unique`: such a derivation never reproduces the
                // placeholder as a leaf, so it is not a valid materialization.
                if let Symbol::Terminal { regex, .. } = sym {
                    *regex = Regex::or(regex.clone(), hole.clone());
                }
            }
        }
    }
    g.build_tokenizer();
    g.build_bindings();
    Ok(g)
}

/// Convert a resolved type subtree to a `TyExpr`: a placeholder leaf maps back to
/// its meta, a transparent wrapper collapses, a leaf is `Lit`, and a structural
/// node is `Con` labelled by its nonterminal.
fn from_node(node: &FusionNode, metas: &HashMap<String, TyExpr>) -> TyExpr {
    let text = node.text();
    if let Some(te) = metas.get(text.trim()) {
        return te.clone();
    }
    if node.is_transparent()
        && let Some(child) = node.children().find_map(|c| match c {
            FusionChild::Node(n) => Some(n),
            FusionChild::Terminal { .. } => None,
        })
    {
        return from_node(&child, metas);
    }
    let kids: Vec<TyExpr> = node
        .children()
        .filter_map(|c| match c {
            FusionChild::Node(n) => Some(from_node(&n, metas)),
            FusionChild::Terminal { .. } => None,
        })
        .collect();
    if kids.is_empty() {
        return TyExpr::Lit(text.trim().to_string());
    }
    TyExpr::Con(node.nt_name().unwrap_or("?").to_string(), kids)
}

fn call_atom(id: &str, inner: &str) -> Result<Atom, String> {
    if inner.is_empty() {
        return Err(format!("{id}() requires an argument"));
    }
    if id == "typeof" {
        Ok(Atom::Ref(inner.to_string()))
    } else if id == "inst" {
        Ok(Atom::Inst(inner.to_string()))
    } else if is_ctx_name(id) {
        Ok(Atom::Ctx(inner.to_string()))
    } else {
        Err(format!("unknown form {id}(…)"))
    }
}

/// A context name: `Γ`, `G`, or other Greek-uppercase context symbols.
fn is_ctx_name(s: &str) -> bool {
    !s.is_empty()
        && s.chars()
            .all(|c| c.is_alphanumeric() || c == '_' || "ΓΔΘΛΣΦΨΩΞΠ".contains(c))
}

fn flush(atoms: &mut Vec<Atom>, sep: &mut String) {
    if !sep.is_empty() {
        atoms.push(Atom::Lit(std::mem::take(sep)));
    }
}

fn is_ident(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

/// Collect `chars[start..]` while `pred`, returning the run and the stop index.
fn take_while(chars: &[char], start: usize, pred: impl Fn(char) -> bool) -> (String, usize) {
    let mut i = start;
    while i < chars.len() && pred(chars[i]) {
        i += 1;
    }
    (chars[start..i].iter().collect(), i)
}

/// Collect `chars[start..]` up to (excluding) `delim`, returning the run and
/// the index of `delim`. `None` if `delim` is absent.
fn take_until(chars: &[char], start: usize, delim: char) -> Option<(String, usize)> {
    let mut i = start;
    while i < chars.len() && chars[i] != delim {
        i += 1;
    }
    (i < chars.len()).then(|| (chars[start..i].iter().collect(), i))
}

#[cfg(test)]
mod tests {
    use crate::typing::{Atom, TypeExpr};

    fn parse(s: &str) -> Vec<Atom> {
        TypeExpr::parse(s).unwrap().0
    }

    #[test]
    fn hole() {
        assert_eq!(parse("?A"), vec![Atom::Hole("A".into())]);
    }
    #[test]
    fn literal() {
        assert_eq!(parse("'Int'"), vec![Atom::Lit("Int".into())]);
    }
    #[test]
    fn bare_ident_is_ref() {
        assert_eq!(parse("τ"), vec![Atom::Ref("τ".into())]);
    }
    #[test]
    fn typeof_is_ref() {
        assert_eq!(parse("typeof(x)"), vec![Atom::Ref("x".into())]);
    }
    #[test]
    fn ctx_lookup() {
        assert_eq!(parse("Γ(x)"), vec![Atom::Ctx("x".into())]);
    }
    #[test]
    fn arrow_is_separator() {
        assert_eq!(
            parse("?A -> ?B"),
            vec![
                Atom::Hole("A".into()),
                Atom::Lit(" -> ".into()),
                Atom::Hole("B".into()),
            ]
        );
    }
    #[test]
    fn ref_arrow_hole() {
        assert_eq!(
            parse("τ -> ?R"),
            vec![
                Atom::Ref("τ".into()),
                Atom::Lit(" -> ".into()),
                Atom::Hole("R".into()),
            ]
        );
    }
    #[test]
    fn top_and_bot() {
        assert_eq!(parse("⊤"), vec![Atom::Top]);
        assert_eq!(parse("∅"), vec![Atom::Bot]);
    }
}
