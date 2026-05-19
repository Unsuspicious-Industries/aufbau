//! Type expressions and closed types — matching §3 of the draft.
//!
//! # TypeExpr — constraint-language expressions
//!
//! Appears in typing rule premises and conclusions.  Elaborated to evidence
//! per the draft's elaboration rules (§3):
//!
//!   τ        ↦  Exact(τ)           (literal closed type)
//!   Γ(x)     ↦  evidence stored for x in Γ
//!   typeof(b) ↦ resolved type for node bound to b
//!   ?A       ↦  pattern matching on unified binding evidence
//!
//! Meta variables (`?A`) are compiled away during rule loading into equality
//! constraints on `typeof` projections (see `CompilationPass`).
//!
//! # Type — closed evidence types
//!
//! Interned by `TypingDomain::intern_evidence`.  Closed-type equality and
//! inclusion are decidable (per §3 assumption), with ⊥ ⊆ τ ⊆ ⊤ for all τ.

use std::fmt;

// ── TypeExpr — constraint-language expressions ─────────────────────────────

/// Type expression appearing in typing rule premises and conclusions.
///
/// After compilation (`CompilationPass`), no `Meta` variants remain.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeExpr {
    /// Literal concrete type: `'Int'`, `'Bool'`, `'number'`.
    Lit(String),

    /// Arrow type: `τ₁ → τ₂`.
    Arrow(Box<TypeExpr>, Box<TypeExpr>),

    /// Union type: `τ₁ | τ₂`.
    Union(Vec<TypeExpr>),

    /// Negation: `¬τ`.
    Not(Box<TypeExpr>),

    /// Top type: `⊤` (any).
    Any,

    /// Bottom type: `⊥` (none).
    None,

    /// Meta-variable: `?A`.  Compiled away during rule loading — all
    /// occurrences of the same name are replaced by fresh internal
    /// metas and equality constraints.
    Meta(String),

    /// Context lookup: `Γ(x)`.  Resolved at evaluation time by looking
    /// up `x` in the current typing context.
    ContextExt(String),

    /// Typed binding reference: `typeof(b)`.  Resolved at evaluation
    /// time by looking up the evidence of binding `b` in obligations.
    TypeOf(String),
}

// ── Type — closed evidence types ───────────────────────────────────────────

/// Closed type — the evidence interned by `TypingDomain`.
///
/// Instantiated from the draft's closed-type algebra (§3):
///
///   τ ::= t | τ₁ → τ₂ | τ₁ ∨ τ₂ | ¬τ | ⊤ | ⊥
///
/// with the addition of `Partial` for incremental input (regex-derivative
/// based, §3 Lemma 1 — evidence monotonicity).
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Type {
    /// Concrete type: `Int`, `Bool`, `number`.
    Raw(String),

    /// Arrow type: `τ₁ → τ₂`.
    Arrow(Box<Type>, Box<Type>),

    /// Union type: `τ₁ ∨ τ₂` (written `τ₁ | τ₂`).
    Union(Vec<Type>),

    /// Negation: `¬τ`.
    Not(Box<Type>),

    /// Top type: `⊤` (any).
    Any,

    /// Bottom type: `⊥` (none).
    None,

    /// Incremental-type evidence: `Partial(inner, input)`.
    ///
    /// `inner` is the current best-effort type inferred from the
    /// incomplete input `input`.  Completable types narrow under input
    /// extension (§3 Lemma 1 — evidence monotonicity).
    Partial(Box<Type>, String),
}

// ── TypeExpr helpers ───────────────────────────────────────────────────────

impl TypeExpr {
    /// Check whether this expression contains any `Meta` variants.
    pub fn has_metas(&self) -> bool {
        match self {
            TypeExpr::Meta(_) => true,
            TypeExpr::Arrow(a, b) => a.has_metas() || b.has_metas(),
            TypeExpr::Union(parts) => parts.iter().any(|p| p.has_metas()),
            TypeExpr::Not(inner) => inner.has_metas(),
            TypeExpr::Lit(_)
            | TypeExpr::Any
            | TypeExpr::None
            | TypeExpr::ContextExt(_)
            | TypeExpr::TypeOf(_) => false,
        }
    }

    /// Collect all `Meta` names referenced in this expression.
    pub fn metas(&self) -> Vec<&str> {
        let mut out = Vec::new();
        self.collect_metas_into(&mut out);
        out
    }

    fn collect_metas_into<'a>(&'a self, out: &mut Vec<&'a str>) {
        match self {
            TypeExpr::Meta(name) => out.push(name.as_str()),
            TypeExpr::Arrow(a, b) => {
                a.collect_metas_into(out);
                b.collect_metas_into(out);
            }
            TypeExpr::Union(parts) => {
                for p in parts {
                    p.collect_metas_into(out);
                }
            }
            TypeExpr::Not(inner) => inner.collect_metas_into(out),
            TypeExpr::Lit(_)
            | TypeExpr::Any
            | TypeExpr::None
            | TypeExpr::ContextExt(_)
            | TypeExpr::TypeOf(_) => {}
        }
    }

    /// Collect all `TypeOf` binding names referenced in this expression.
    pub fn typeof_bindings(&self) -> Vec<&str> {
        let mut out = Vec::new();
        self.collect_typeof_into(&mut out);
        out
    }

    fn collect_typeof_into<'a>(&'a self, out: &mut Vec<&'a str>) {
        match self {
            TypeExpr::TypeOf(binding) => out.push(binding.as_str()),
            TypeExpr::Arrow(a, b) => {
                a.collect_typeof_into(out);
                b.collect_typeof_into(out);
            }
            TypeExpr::Union(parts) => {
                for p in parts {
                    p.collect_typeof_into(out);
                }
            }
            TypeExpr::Not(inner) => inner.collect_typeof_into(out),
            TypeExpr::Lit(_)
            | TypeExpr::Any
            | TypeExpr::None
            | TypeExpr::Meta(_)
            | TypeExpr::ContextExt(_) => {}
        }
    }

    /// Collect all `ContextExt` variable names referenced in this expression.
    pub fn context_vars(&self) -> Vec<&str> {
        let mut out = Vec::new();
        self.collect_ctx_into(&mut out);
        out
    }

    fn collect_ctx_into<'a>(&'a self, out: &mut Vec<&'a str>) {
        match self {
            TypeExpr::ContextExt(var) => out.push(var.as_str()),
            TypeExpr::Arrow(a, b) => {
                a.collect_ctx_into(out);
                b.collect_ctx_into(out);
            }
            TypeExpr::Union(parts) => {
                for p in parts {
                    p.collect_ctx_into(out);
                }
            }
            TypeExpr::Not(inner) => inner.collect_ctx_into(out),
            TypeExpr::Lit(_)
            | TypeExpr::Any
            | TypeExpr::None
            | TypeExpr::Meta(_)
            | TypeExpr::TypeOf(_) => {}
        }
    }
}

// ── Type helpers ──────────────────────────────────────────────────────────

impl Type {
    pub fn is_concrete(&self) -> bool {
        matches!(self, Type::Raw(_))
    }

    pub fn is_any(&self) -> bool {
        matches!(self, Type::Any)
    }

    pub fn is_none(&self) -> bool {
        matches!(self, Type::None)
    }

    /// `Type::Raw` constructor for convenience.
    pub fn raw(name: impl Into<String>) -> Self {
        Type::Raw(name.into())
    }

    /// `Type::Arrow` constructor for convenience.
    pub fn arrow(domain: Type, codomain: Type) -> Self {
        Type::Arrow(Box::new(domain), Box::new(codomain))
    }
}

// ── Display ────────────────────────────────────────────────────────────────

impl fmt::Display for TypeExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeExpr::Lit(s) => write!(f, "'{}'", s),
            TypeExpr::Arrow(a, b) => write!(f, "{} → {}", a, b),
            TypeExpr::Union(parts) => {
                for (i, part) in parts.iter().enumerate() {
                    if i > 0 {
                        write!(f, " | ")?;
                    }
                    match part {
                        TypeExpr::Arrow(..) | TypeExpr::Not(_) => write!(f, "({})", part)?,
                        other => write!(f, "{}", other)?,
                    }
                }
                Ok(())
            }
            TypeExpr::Not(inner) => write!(f, "¬{}", inner),
            TypeExpr::Any => write!(f, "⊤"),
            TypeExpr::None => write!(f, "⊥"),
            TypeExpr::Meta(name) => write!(f, "?{}", name),
            TypeExpr::ContextExt(var) => write!(f, "Γ({})", var),
            TypeExpr::TypeOf(binding) => write!(f, "typeof({})", binding),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Raw(s) => write!(f, "'{}'", s),
            Type::Arrow(a, b) => write!(f, "{} → {}", a, b),
            Type::Union(parts) => {
                for (i, part) in parts.iter().enumerate() {
                    if i > 0 {
                        write!(f, " | ")?;
                    }
                    match part {
                        Type::Arrow(..) | Type::Not(_) => write!(f, "({})", part)?,
                        other => write!(f, "{}", other)?,
                    }
                }
                Ok(())
            }
            Type::Not(inner) => write!(f, "¬{}", inner),
            Type::Any => write!(f, "⊤"),
            Type::None => write!(f, "⊥"),
            Type::Partial(inner, _input) => write!(f, "{}", inner),
        }
    }
}
