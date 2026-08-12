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

/// How a context entry is addressed.
///
/// The context maps names to types, and until now a name could only be *read
/// off the input*: `Γ[a:τ]` keys the entry by the runtime text of the token
/// bound to `a`. That works when the writer and the reader both see the same
/// token — a binder and its uses — and not otherwise. Two rules with no token
/// in common cannot agree on a key, so they cannot communicate.
///
/// A `Literal` key is a name the rule fixes: an ambient channel that any rule
/// can write and any rule can read, independent of the input text. Nothing
/// about it is language-specific — it is how a grammar publishes data for its
/// own later use, whether that is a function's return type, the table a query
/// is scoped to, or the schema version a record must match.
///
/// Both kinds share one namespace, deliberately: there is one context, and a
/// literal key names a cell in it on the same terms as a binding-derived key.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Key {
    /// `Γ[a:τ]` — keyed by the runtime text of the token bound to `a`.
    Binding(String),
    /// `Γ['return':τ]` — keyed by a fixed name spelled in the rule.
    Literal(String),
}

impl Key {
    /// Read a key from surface syntax: quoted is literal, bare is a binding.
    #[must_use]
    pub fn parse(s: &str) -> Self {
        let s = s.trim();
        match s.strip_prefix('\'').and_then(|r| r.strip_suffix('\'')) {
            Some(lit) => Key::Literal(lit.to_string()),
            None => Key::Binding(s.to_string()),
        }
    }

    /// The binding this key reads, if it reads one. `None` for a literal, which
    /// is exactly what makes it resolvable without any input.
    #[must_use]
    pub fn binding(&self) -> Option<&str> {
        match self {
            Key::Binding(n) => Some(n),
            Key::Literal(_) => None,
        }
    }

    /// The fixed name this key names, if it is fixed.
    #[must_use]
    pub fn literal(&self) -> Option<&str> {
        match self {
            Key::Literal(s) => Some(s),
            Key::Binding(_) => None,
        }
    }
}

impl From<&str> for Key {
    /// Surface spelling in, key out — the same rule as [`Key::parse`], so a
    /// caller building rules programmatically writes exactly what it would
    /// write in `.auf`.
    fn from(s: &str) -> Self {
        Key::parse(s)
    }
}

impl fmt::Display for Key {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Key::Binding(n) => write!(f, "{n}"),
            // Quoted on the way out or it re-parses as a binding — the same
            // round-trip hazard `needs_quotes` guards for literal types.
            Key::Literal(s) => write!(f, "'{s}'"),
        }
    }
}

/// One element of a `TypeExpr`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Atom {
    /// Literal text: a quoted type (`'Int'`) or a separator (`" -> "`).
    Lit(String),
    /// A hole `?A` — unification variable and positional capture.
    Hole(String),
    /// A binding reference — the type of the child bound to this name.
    Ref(String),
    /// A context lookup `Γ(x)` — the type bound to `x`'s value, or to a fixed
    /// name (`Γ('return')`).
    Ctx(Key),
    /// An instantiating context lookup `inst(x)` — `Γ(x)` with its variables
    /// freshened, i.e. a polymorphic scheme made concrete at this use.
    Inst(Key),
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
    Ctx(Key),
    /// An instantiating context lookup `inst(x)` — `Γ(x)` with fresh variables.
    Inst(Key),
    /// `⊤`, the unconstrained type.
    Top,
    /// `∅`, the contradictory type.
    Bot,
    /// Literal type text (`'Int'`).
    Lit(String),
    /// A constructor: a grammar production labelled by its nonterminal.
    Con(String, Vec<TyExpr>),
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
}

impl Atom {
    fn name(&self) -> Option<&str> {
        match self {
            Atom::Lit(n) | Atom::Hole(n) | Atom::Ref(n) => Some(n.as_str()),
            Atom::Ctx(k) | Atom::Inst(k) => k.binding(),
            Atom::Top | Atom::Bot => None,
        }
    }
}

/// Would this literal survive being written without quotes?
///
/// `Atom::Lit` holds both halves of the surface syntax: a quoted type (`'Int'`)
/// and the separator text between atoms (`" -> "`). Written bare, a separator
/// re-parses to the same `Lit`, but type text does not — `Int` comes back as an
/// `Atom::Ref`.
///
/// Quoting is the safe direction: `'x'` always re-parses to `Lit(x)`. So this
/// only decides *readability*, and erring toward `true` costs nothing but noise.
/// That is deliberate — the predicate mirrors `TypeExpr::parse`'s atom starts
/// without being coupled to them, and `display_round_trips` in `syntax.rs` is
/// what actually holds the two in agreement. It is not a list of known types;
/// nothing here depends on any particular grammar's vocabulary.
///
/// (A literal containing `'` is representable in neither form. `parse` cannot
/// produce one — a quoted literal stops at the closing quote and separator text
/// never accumulates one — so there is nothing to encode.)
fn needs_quotes(s: &str) -> bool {
    s.chars()
        .any(|c| c.is_alphanumeric() || c == '_' || c == '?')
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Atom::Lit(s) if needs_quotes(s) => write!(f, "'{s}'"),
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
