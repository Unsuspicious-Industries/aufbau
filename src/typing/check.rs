//! Static analysis of compiled IR — safety *before* the cut.
//!
//! `compile` fixes a schedule; `run` discharges it. Anything wrong with the
//! schedule itself is therefore a compile-time fact, and belongs here rather
//! than as a runtime surprise. Every check below rules out a class of program
//! that would otherwise fail silently — an unresolved register looks exactly
//! like "still waiting for input", so a rule that can *never* resolve is
//! indistinguishable from one that has not resolved *yet*.
//!
//! Run once per rule at grammar load, never on the hot path.

use std::collections::{HashMap, HashSet};

use crate::typing::ir::{Instr, Program};
use crate::typing::{Key, TyExpr};

/// Everything wrong with a program, reported together: fixing one rule and
/// rediscovering the next is a worse workflow than seeing all of them.
pub fn check(
    program: &Program,
    declared: &HashSet<String>,
    ambient: &HashSet<String>,
) -> Result<(), Vec<String>> {
    let mut errs = Vec::new();
    scopes_balanced(program, &mut errs);
    registers_written_before_read(program, &mut errs);
    splices_well_formed(program, &mut errs);
    emits_once(program, &mut errs);
    bindings_declared(program, declared, ambient, &mut errs);

    if errs.is_empty() { Ok(()) } else { Err(errs) }
}

/// The literal keys `program` *writes* — by a premise setting or an exported
/// effect. Union this over every rule to get the ambient names a grammar
/// makes available; a read of anything else can never resolve.
pub fn writes(program: &Program) -> impl Iterator<Item = &str> {
    program.instrs.iter().filter_map(|i| match i {
        Instr::Extend { key, .. } | Instr::Effect { key, .. } => key.literal(),
        _ => None,
    })
}

/// `PushScope`/`PopScope` nest properly and close. `descend` slices `instrs` by
/// splice range, so an unbalanced program hands a premise the wrong context.
fn scopes_balanced(p: &Program, errs: &mut Vec<String>) {
    let mut depth = 0i32;
    for (pc, instr) in p.instrs.iter().enumerate() {
        match instr {
            Instr::PushScope => depth += 1,
            Instr::PopScope => {
                depth -= 1;
                if depth < 0 {
                    errs.push(format!("{}: pop_scope below zero at {pc}", p.name));
                    return;
                }
            }
            _ => {}
        }
    }
    if depth != 0 {
        errs.push(format!("{}: {depth} scope(s) left open", p.name));
    }
}

/// Every register is written by an `Eval` before it is read. A read of an
/// unwritten register is permanently `UNRESOLVED`, which the executor reports
/// as "not yet known" — so the rule can never be satisfied and never says why.
fn registers_written_before_read(p: &Program, errs: &mut Vec<String>) {
    let mut written: HashSet<usize> = HashSet::new();
    for (pc, instr) in p.instrs.iter().enumerate() {
        let read = |r: usize, errs: &mut Vec<String>| {
            if !written.contains(&r) {
                errs.push(format!(
                    "{}: instruction {pc} ({instr}) reads r{r} before it is written",
                    p.name
                ));
            }
        };
        match instr {
            Instr::Eval { dst, .. } => {
                written.insert(*dst);
            }
            Instr::Ascribe { expected, .. } => read(*expected, errs),
            Instr::Equate { left, right } => {
                read(*left, errs);
                read(*right, errs);
            }
            Instr::Extend { ty, .. } | Instr::Emit { ty } | Instr::Effect { ty, .. } => {
                read(*ty, errs);
            }
            Instr::Member { .. } | Instr::Fresh { .. } | Instr::PushScope | Instr::PopScope => {}
        }
    }
}

/// Splice ranges are in-bounds, ordered, and disjoint. Overlapping splices let
/// one premise's setting leak into another premise's descent.
fn splices_well_formed(p: &Program, errs: &mut Vec<String>) {
    let mut ranges: Vec<_> = p.splices.iter().collect();
    ranges.sort_by_key(|(_, r)| (r.start, r.end));
    for (name, r) in &ranges {
        if r.start > r.end || r.end > p.instrs.len() {
            errs.push(format!(
                "{}: splice {name} = {r:?} is out of bounds ({} instrs)",
                p.name,
                p.instrs.len()
            ));
        }
    }
    for w in ranges.windows(2) {
        let ((an, a), (bn, b)) = (w[0], w[1]);
        if a.end > b.start {
            errs.push(format!(
                "{}: splices {an} = {a:?} and {bn} = {b:?} overlap",
                p.name
            ));
        }
    }
}

/// Exactly one `Emit`, and it is last: the conclusion is the program's result,
/// so a second one silently discards the first.
fn emits_once(p: &Program, errs: &mut Vec<String>) {
    let emits: Vec<usize> = p
        .instrs
        .iter()
        .enumerate()
        .filter(|(_, i)| matches!(i, Instr::Emit { .. }))
        .map(|(pc, _)| pc)
        .collect();
    match emits.len() {
        1 if emits[0] == p.instrs.len() - 1 => {}
        1 => errs.push(format!(
            "{}: emit at {} is not the last instruction",
            p.name, emits[0]
        )),
        0 => errs.push(format!("{}: no emit; the rule produces no type", p.name)),
        n => errs.push(format!("{}: {n} emits, expected exactly one", p.name)),
    }
}

/// Every binding an instruction names is one the rule's productions declare.
///
/// This is the check that would have caught `Γ(r)` silently never resolving:
/// a reference to a name no production binds can never be satisfied, but at
/// runtime it is indistinguishable from a binding whose value has not arrived.
fn bindings_declared(
    p: &Program,
    declared: &HashSet<String>,
    ambient: &HashSet<String>,
    errs: &mut Vec<String>,
) {
    // An empty declared-set means the caller could not determine the bindings;
    // reporting every name as undeclared would be noise, not signal.
    if declared.is_empty() {
        return;
    }
    let mut seen: HashMap<&str, &str> = HashMap::new();
    for instr in &p.instrs {
        let (name, kind) = match instr {
            Instr::Ascribe { binding, .. } => (binding.as_str(), "ascription"),
            // A literal key needs no production. What it *does* need is a
            // writer, which `ambient_read` checks for reads; a setting or an
            // effect is itself a write, so there is nothing to verify here.
            Instr::Member { key } => match key {
                Key::Literal(s) => {
                    ambient_read(p, s, "membership", ambient, errs);
                    continue;
                }
                Key::Binding(b) => (b.as_str(), "membership"),
            },
            Instr::Fresh { key } => match key {
                Key::Literal(s) => {
                    errs.push(format!(
                        "{}: freshness names ambient key '{s}', but freshness is binding-only",
                        p.name
                    ));
                    continue;
                }
                Key::Binding(b) => (b.as_str(), "freshness"),
            },
            Instr::Extend { key, .. } => match key.binding() {
                Some(b) => (b, "setting"),
                None => continue,
            },
            Instr::Effect { key, .. } => match key.binding() {
                Some(b) => (b, "effect"),
                None => continue,
            },
            Instr::Eval { expr, .. } => {
                refs_of(expr, declared, ambient, p, errs);
                continue;
            }
            _ => continue,
        };
        seen.insert(name, kind);
    }
    for (name, kind) in seen {
        if !declared.contains(name) {
            errs.push(format!(
                "{}: {kind} names '{name}', which no production of this rule binds",
                p.name
            ));
        }
    }
}

/// A literal key is only readable if some rule in the grammar writes it.
///
/// This replaces, for literal keys, the guarantee `bindings_declared` gives for
/// binding keys. Without it the surface gains a name that parses, compiles and
/// then silently never resolves — exactly the failure mode this module exists
/// to close, reintroduced through the new syntax.
fn ambient_read(
    p: &Program,
    name: &str,
    kind: &str,
    ambient: &HashSet<String>,
    errs: &mut Vec<String>,
) {
    if !ambient.contains(name) {
        errs.push(format!(
            "{}: {kind} reads ambient key '{name}', which no rule of this grammar sets",
            p.name
        ));
    }
}

/// Binding references inside an evaluated type expression.
fn refs_of(
    expr: &TyExpr,
    declared: &HashSet<String>,
    ambient: &HashSet<String>,
    p: &Program,
    errs: &mut Vec<String>,
) {
    match expr {
        TyExpr::Ref(n) => {
            if !declared.contains(n) {
                errs.push(format!(
                    "{}: type expression references '{n}', which no production of this rule binds",
                    p.name
                ));
            }
        }
        // `Γ(x)` looks up by *x's parsed text*, so `x` must be a real binding;
        // `Γ('k')` looks up a fixed name, so some rule must set it. Both are
        // the shape that otherwise fails silently at runtime.
        TyExpr::Ctx(k) | TyExpr::Inst(k) => match k {
            Key::Literal(s) => ambient_read(p, s, "context lookup", ambient, errs),
            Key::Binding(b) if !declared.contains(b) => errs.push(format!(
                "{}: context lookup Γ({b}) names '{b}', which no production of this rule binds",
                p.name
            )),
            Key::Binding(_) => {}
        },
        TyExpr::Con(_, kids) => {
            for k in kids {
                refs_of(k, declared, ambient, p, errs);
            }
        }
        TyExpr::Var(_) | TyExpr::Top | TyExpr::Bot | TyExpr::Lit(_) => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::grammar::SPG;
    use crate::typing::ir::compile;

    fn program(g: &SPG, rule: &str) -> Program {
        compile(&g.rules[rule], &crate::typing::loader::type_trees(g))
    }

    /// Every rule of every shipped grammar passes. The checks are only worth
    /// having if real grammars satisfy them.
    #[test]
    fn example_grammars_pass() {
        let dir = concat!(env!("CARGO_MANIFEST_DIR"), "/examples");
        for entry in std::fs::read_dir(dir).unwrap() {
            let path = entry.unwrap().path();
            if path.extension().is_none_or(|e| e != "auf") {
                continue;
            }
            let file = path.file_name().unwrap().to_string_lossy().into_owned();
            let g = SPG::load(&std::fs::read_to_string(&path).unwrap())
                .unwrap_or_else(|e| panic!("{file}: {e}"));
            let trees = crate::typing::loader::type_trees(&g);
            let ambient: HashSet<String> = g
                .rules
                .values()
                .flat_map(|r| {
                    writes(&compile(r, &trees))
                        .map(str::to_string)
                        .collect::<Vec<_>>()
                })
                .collect();
            for name in g.rules.keys() {
                let p = program(&g, name);
                if let Err(errs) = check(&p, &g.rule_bindings(name), &ambient) {
                    panic!("{file}/{name}:\n  {}", errs.join("\n  "));
                }
            }
        }
    }

    /// The regression that motivated this module: a context lookup naming a
    /// binding no production declares is never resolvable, and at runtime looks
    /// exactly like "waiting for more input".
    #[test]
    fn undeclared_context_lookup_is_rejected() {
        let src = "Identifier ::= /[a-z]+/\nM(m) ::= Identifier[x]\nE ::= M\n\n----------- (m)\nΓ(nope)\n";
        let err = SPG::load(src).expect_err("should not load");
        assert!(err.contains("nope"), "{err}");
    }

    /// An ambient key nobody sets is unreadable. Without this, the literal-key
    /// syntax would reintroduce exactly the silent-`live` failure that motivated
    /// this module — the check for binding keys does not cover it.
    #[test]
    fn unset_ambient_key_is_rejected() {
        let src = "Identifier ::= /[a-z]+/\nM(m) ::= Identifier[x]\nE ::= M\n\n----------- (m)\nΓ('nope')\n";
        let err = SPG::load(src).expect_err("should not load");
        assert!(err.contains("nope") && err.contains("ambient"), "{err}");
    }

    /// The same key, once some rule sets it, loads fine.
    #[test]
    fn ambient_key_with_a_writer_is_accepted() {
        let src = "Identifier ::= /[a-z]+/\nM(m) ::= Identifier[x]\nE ::= M\n\nΓ['k' : ⊤] ⊢ x : ?A\n----------- (m)\nΓ('k')\n";
        SPG::load(src).expect("a written ambient key is readable");
    }

    #[test]
    fn undeclared_ascription_binding_is_rejected() {
        let src = "Identifier ::= /[a-z]+/\nM(m) ::= Identifier[x]\nE ::= M\n\nΓ ⊢ zzz : ?A\n----------- (m)\n?A\n";
        let err = SPG::load(src).expect_err("should not load");
        assert!(err.contains("zzz"), "{err}");
    }
}
