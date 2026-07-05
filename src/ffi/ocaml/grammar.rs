//! Grammars as OCaml values: structural construction, a persistent handle,
//! and structured verdicts. No `.auf` source as the medium; productions,
//! rules, and type expressions are all built inductively. The idiomatic OCaml
//! surface lives in `ocaml/aufbau.ml`; variant orders here match the OCaml
//! type declarations.

use super::term::OTerm;
use crate::engine::grammar::SPG;
use crate::engine::grammar::load::{Def, Sym};
use crate::engine::structure::FusionNode;
use crate::typing::rule::{Conclusion, Judgment, Premise};
use crate::typing::{Atom, Context, TypeExpr, TypingRule, TypingSynth};

/// A grammar symbol: nonterminal, literal token, or regex terminal, each
/// optionally binding a name.
#[derive(ocaml::FromValue)]
pub enum OSym {
    Nt(String, Option<String>),
    Lit(String, Option<String>),
    Re(String, Option<String>),
}

/// A nonterminal definition: name, optional rule name, alternatives.
type ODef = (String, Option<String>, Vec<Vec<OSym>>);

/// One atom of a rule type expression (mirrors [`Atom`]). A `Lit` is both
/// quoted type text (`'Int'`) and separator text (`" -> "`).
#[derive(ocaml::FromValue)]
pub enum OAtom {
    Lit(String),
    Hole(String),
    Ref(String),
    Ctx(String),
    Inst(String),
    Top,
    Bot,
}

/// A premise, structurally: an ascription under premise-local settings, a
/// context membership, or an equation.
#[derive(ocaml::FromValue)]
pub enum OPremise {
    Ascribe(Vec<(String, Vec<OAtom>)>, String, Vec<OAtom>),
    Member(String),
    Equate(Vec<OAtom>, Vec<OAtom>),
}

/// A typing rule: the inference notation, or built from parts (name,
/// premises, right-bound effects, conclusion).
#[derive(ocaml::FromValue)]
pub enum ORule {
    Parsed(String, String, String),
    Made(String, Vec<OPremise>, Vec<(String, Vec<OAtom>)>, Vec<OAtom>),
}

/// The grammar handle: assembled and validated once, checked many times.
pub struct Gram(pub SPG);
ocaml::custom!(Gram);

/// Assembly result; the OCaml surface folds it into a `result`.
#[derive(ocaml::ToValue)]
pub enum OBuild {
    Built(ocaml::Pointer<Gram>),
    Invalid(String),
}

/// A checked program: `Typed` when a complete root has a type, `Live` when
/// the input is a completable prefix, `Dead` with the engine's reason.
#[derive(ocaml::ToValue)]
pub enum OVerdict {
    Typed(OTerm),
    Live,
    Dead(String),
}

fn texpr(atoms: Vec<OAtom>) -> TypeExpr {
    TypeExpr(
        atoms
            .into_iter()
            .map(|a| match a {
                OAtom::Lit(s) => Atom::Lit(s),
                OAtom::Hole(s) => Atom::Hole(s),
                OAtom::Ref(s) => Atom::Ref(s),
                OAtom::Ctx(s) => Atom::Ctx(s),
                OAtom::Inst(s) => Atom::Inst(s),
                OAtom::Top => Atom::Top,
                OAtom::Bot => Atom::Bot,
            })
            .collect(),
    )
}

fn extensions(exts: Vec<(String, Vec<OAtom>)>) -> Vec<(String, TypeExpr)> {
    exts.into_iter().map(|(x, t)| (x, texpr(t))).collect()
}

fn premise(p: OPremise) -> Premise {
    match p {
        OPremise::Ascribe(under, binding, t) => Premise {
            extensions: extensions(under),
            judgment: Judgment::Ascription {
                binding,
                ty: texpr(t),
            },
        },
        OPremise::Member(binding) => Premise {
            extensions: Vec::new(),
            judgment: Judgment::Membership { binding },
        },
        OPremise::Equate(l, r) => Premise {
            extensions: Vec::new(),
            judgment: Judgment::Equation {
                left: texpr(l),
                right: texpr(r),
            },
        },
    }
}

fn rule(r: ORule) -> Result<TypingRule, String> {
    match r {
        ORule::Parsed(name, premises, conclusion) => TypingRule::new(premises, conclusion, name),
        ORule::Made(name, premises, effects, conclusion) => Ok(TypingRule {
            name,
            premises: premises.into_iter().map(premise).collect(),
            conclusion: Conclusion {
                ty: texpr(conclusion),
                effects: extensions(effects),
            },
        }),
    }
}

fn sym(s: OSym) -> Sym {
    match s {
        OSym::Nt(n, b) => Sym::Nt(n, b),
        OSym::Lit(t, b) => Sym::Lit(t, b),
        OSym::Re(p, b) => Sym::Re(p, b),
    }
}

fn build(
    defs: Vec<ODef>,
    rules: Vec<ORule>,
    rewrites: Vec<(String, String)>,
    start: Option<String>,
    ty: Option<String>,
) -> Result<SPG, String> {
    let defs: Vec<Def> = defs
        .into_iter()
        .map(|(n, r, alts)| {
            let alts = alts
                .into_iter()
                .map(|a| a.into_iter().map(sym).collect())
                .collect();
            (n, r, alts)
        })
        .collect();
    let rules = rules.into_iter().map(rule).collect::<Result<_, _>>()?;
    SPG::assemble(defs, rules, rewrites, start, ty)
}

fn built(g: Result<SPG, String>) -> OBuild {
    match g {
        Ok(g) => OBuild::Built(ocaml::Pointer::alloc_custom(Gram(g))),
        Err(e) => OBuild::Invalid(e),
    }
}

/// Assemble a grammar structurally; the handle is reused across checks.
#[ocaml::func]
#[must_use]
pub fn aufbau_grammar(
    defs: Vec<ODef>,
    rules: Vec<ORule>,
    rewrites: Vec<(String, String)>,
    start: Option<String>,
    ty: Option<String>,
) -> OBuild {
    built(build(defs, rules, rewrites, start, ty))
}

/// Load a grammar from `.auf` source.
#[ocaml::func]
#[must_use]
pub fn aufbau_load(source: String) -> OBuild {
    built(SPG::load(&source))
}

/// Render a grammar handle back to `.auf` source.
#[ocaml::func]
#[must_use]
pub fn aufbau_show(g: &Gram) -> String {
    g.0.to_spec_string()
}

/// The realizability class of the grammar: `("syntactic", [])` (no rules,
/// live ⇔ realizable), `("inhabited", [])` (every ascribed sort has a
/// universal inhabitant, live ⇒ realizable), or `("sound", sorts)` (a live
/// prefix may be uninhabited at the listed sorts).
#[ocaml::func]
#[must_use]
pub fn aufbau_completeness(g: &Gram) -> (String, Vec<String>) {
    match crate::typing::completeness(&g.0) {
        crate::typing::Completeness::Syntactic => ("syntactic".into(), vec![]),
        crate::typing::Completeness::Inhabited => ("inhabited".into(), vec![]),
        crate::typing::Completeness::Sound { uninhabited } => ("sound".into(), uninhabited),
    }
}

/// Check a whole program or a prefix against the grammar.
#[ocaml::func]
#[must_use]
pub fn aufbau_check(g: &Gram, program: String) -> OVerdict {
    let mut synth = TypingSynth::new(g.0.clone(), &program);
    match synth.parse_with(&Context::new()) {
        Err(e) => OVerdict::Dead(e),
        Ok(ast) => {
            let rt = synth.runtime().clone();
            ast.roots()
                .filter(FusionNode::is_complete)
                .find_map(|r| rt.evidence_of(r.evidence()))
                .map_or(OVerdict::Live, |t| OVerdict::Typed(OTerm::lift(&t)))
        }
    }
}
