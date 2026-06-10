//! The typing constraint language. §2.
//!
//! A type is a [`Pattern`](super::pattern::Pattern) — a regular set over the
//! grammar, interned as evidence. A `TypeExpr` is a rule-level pattern with
//! unresolved references: holes (`?A`), binding refs (`τ`, `typeof(b)`), and
//! context lookups (`Γ(x)`). `domain::eval` resolves these against obligations
//! and context into a `Pattern`. There is no arrow, union, or negation
//! construct — structure is the sequencing of literal separators.

use std::fmt;

pub use super::term::Term as Type;

/// One element of a `TypeExpr`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Atom {
    /// Literal text: a quoted type (`'Int'`) or a separator (`" -> "`).
    Lit(String),
    /// A hole `?A` — unification variable and positional capture.
    Hole(String),
    /// A binding reference — the type of the child bound to this name.
    Ref(String),
    /// A context lookup `Γ(x)` — the type bound to `x`'s value.
    Ctx(String),
    /// An instantiating context lookup `inst(x)` — `Γ(x)` with its variables
    /// freshened, i.e. a polymorphic scheme made concrete at this use.
    Inst(String),
    /// `⊤`, the unconstrained type.
    Top,
    /// `⊥`, the contradictory type.
    Bot,
}

/// A rule-level type expression: a sequence of atoms.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct TypeExpr(pub Vec<Atom>);

/// A rule-level type *pattern*: the tree a `TypeExpr` denotes once its structure
/// is recovered by parsing it with the grammar (§2). Leaves are holes (`?A`),
/// binding refs (`τ`), context lookups (`Γ(x)`), `⊤`/`∅`, or literal type text;
/// `Con` is a grammar production. `domain::eval` resolves it to a `Term`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyExpr {
    /// A hole `?A` — a unification variable.
    Var(String),
    /// A binding reference `τ`/`typeof(b)` — the referenced node's type.
    Ref(String),
    /// A context lookup `Γ(x)`.
    Ctx(String),
    /// An instantiating context lookup `inst(x)` — `Γ(x)` with fresh variables.
    Inst(String),
    /// `⊤`, the unconstrained type.
    Top,
    /// `∅`, the contradictory type.
    Bot,
    /// Literal type text (`'Int'`).
    Lit(String),
    /// A constructor: a grammar production labelled by its nonterminal.
    Con(String, Vec<TyExpr>),
}

impl TyExpr {
    /// Binding references (`τ`, `typeof(b)`), in order.
    #[must_use]
    pub fn refs(&self) -> Vec<&str> {
        let mut out = Vec::new();
        self.walk(&mut |t| {
            if let TyExpr::Ref(n) = t {
                out.push(n.as_str());
            }
        });
        out
    }
    /// Hole names (`?A`), in order.
    #[must_use]
    pub fn holes(&self) -> Vec<&str> {
        let mut out = Vec::new();
        self.walk(&mut |t| {
            if let TyExpr::Var(n) = t {
                out.push(n.as_str());
            }
        });
        out
    }
    #[must_use]
    pub fn has_holes(&self) -> bool {
        !self.holes().is_empty()
    }

    fn walk<'a>(&'a self, f: &mut impl FnMut(&'a TyExpr)) {
        f(self);
        if let TyExpr::Con(_, kids) = self {
            for k in kids {
                k.walk(f);
            }
        }
    }
}

impl fmt::Display for TyExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TyExpr::Var(n) => write!(f, "?{n}"),
            TyExpr::Ref(n) => write!(f, "{n}"),
            TyExpr::Ctx(v) => write!(f, "Γ({v})"),
            TyExpr::Inst(v) => write!(f, "inst({v})"),
            TyExpr::Top => write!(f, "⊤"),
            TyExpr::Bot => write!(f, "∅"),
            TyExpr::Lit(s) => write!(f, "'{s}'"),
            TyExpr::Con(label, kids) => {
                write!(f, "{label}(")?;
                for (i, k) in kids.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{k}")?;
                }
                write!(f, ")")
            }
        }
    }
}

impl TypeExpr {
    /// Hole names (`?A`), in order, with repeats.
    #[must_use]
    pub fn holes(&self) -> Vec<&str> {
        self.collect(|a| matches!(a, Atom::Hole(_)))
    }
    /// Binding references (`τ`, `typeof(b)`).
    #[must_use]
    pub fn refs(&self) -> Vec<&str> {
        self.collect(|a| matches!(a, Atom::Ref(_)))
    }
    /// Context-lookup variables (`Γ(x)`).
    #[must_use]
    pub fn ctx_vars(&self) -> Vec<&str> {
        self.collect(|a| matches!(a, Atom::Ctx(_)))
    }

    /// Instantiating-lookup variables (`inst(x)`).
    #[must_use]
    pub fn inst_vars(&self) -> Vec<&str> {
        self.collect(|a| matches!(a, Atom::Inst(_)))
    }

    fn collect(&self, pred: impl Fn(&Atom) -> bool) -> Vec<&str> {
        self.0
            .iter()
            .filter(|a| pred(a))
            .filter_map(Atom::name)
            .collect()
    }

    #[must_use]
    pub fn has_holes(&self) -> bool {
        self.0.iter().any(|a| matches!(a, Atom::Hole(_)))
    }

    /// The single hole `?A`, when that is the whole expression.
    #[must_use]
    pub fn as_single_hole(&self) -> Option<&str> {
        match self.0.as_slice() {
            [Atom::Hole(n)] => Some(n.as_str()),
            _ => None,
        }
    }
}

impl Atom {
    fn name(&self) -> Option<&str> {
        match self {
            Atom::Lit(n) | Atom::Hole(n) | Atom::Ref(n) | Atom::Ctx(n) | Atom::Inst(n) => {
                Some(n.as_str())
            }
            Atom::Top | Atom::Bot => None,
        }
    }
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Atom::Lit(s) => write!(f, "{s}"),
            Atom::Hole(n) => write!(f, "?{n}"),
            Atom::Ref(n) => write!(f, "{n}"),
            Atom::Ctx(v) => write!(f, "Γ({v})"),
            Atom::Inst(v) => write!(f, "inst({v})"),
            Atom::Top => write!(f, "⊤"),
            Atom::Bot => write!(f, "∅"),
        }
    }
}

impl fmt::Display for TypeExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for a in &self.0 {
            write!(f, "{a}")?;
        }
        Ok(())
    }
}
