//! Regressions for `to_spec_string`: a grammar must survive being rendered and
//! reloaded. `SPG::source()` is public API (the Python DSL round-trips through
//! it), so a lossy render silently changes the language.

use crate::grammar::SPG;

/// An epsilon alternative has an empty `rhs`. Rendering it as nothing produced
/// `A ::= 'a' B | `, and the loader drops blank alternatives — so the ε branch
/// vanished on reload and `A` stopped being nullable.
#[test]
fn epsilon_alternatives_survive_round_trip() {
    let src = "A ::= 'a' B | ε\nB ::= 'b' | ε\nstart ::= A B\n";
    let g = SPG::load(src).expect("load");
    let rendered = g.to_spec_string();
    assert!(rendered.contains('ε'), "epsilon not rendered:\n{rendered}");

    let g2 = SPG::load(&rendered).expect("reload");
    for nt in ["A", "B"] {
        assert_eq!(
            g.productions[nt].len(),
            g2.productions[nt].len(),
            "{nt} lost an alternative:\n{rendered}"
        );
        assert!(
            g2.productions[nt].iter().any(|p| p.rhs.is_empty()),
            "{nt} lost its epsilon alternative:\n{rendered}"
        );
    }
}

/// `set_nonterminal_rule` registers a name in `nonterminals` before it has any
/// productions; `add_production` then guarded only on `productions` and pushed
/// it a second time. Every nonterminal carrying a typing rule was listed twice,
/// which doubled `nt_count`/`nt_index` and duplicated its rendered productions.
#[test]
fn rule_bearing_nonterminals_are_listed_once() {
    let src = "\
    Lower ::= /[a-z]+/
    Variable(var) ::= Lower[x]
    Expression ::= Variable

    x ∈ Γ
    ----------- (var)
    Γ(x)
";
    let g = SPG::load(src).expect("load");

    let mut seen = g.nonterminals.clone();
    seen.sort();
    let mut uniq = seen.clone();
    uniq.dedup();
    assert_eq!(seen, uniq, "duplicate nonterminals: {:?}", g.nonterminals);

    // `nt_index` must agree with `nt_name` for every slot.
    for (i, nt) in g.nonterminals.iter().enumerate() {
        assert_eq!(g.nt_index(nt), Some(i), "{nt} resolves to the wrong slot");
    }

    let rendered = g.to_spec_string();
    assert_eq!(
        rendered.matches("Variable(var) ::=").count(),
        1,
        "production emitted more than once:\n{rendered}"
    );
    SPG::load(&rendered).expect("reload");
}
