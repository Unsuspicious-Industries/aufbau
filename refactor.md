# Refactor Plan: SPG Generalization

## 0. Goal

Restructure the codebase to mirror the formal SPG model in `draft/`, so that:

1. The grammar carries no domain-specific machinery — `SPG<R>` is generic over the rule type `R`.
2. The constraint domain is a first-class abstraction — the `ConstraintDomain` trait corresponds directly to Definition `def:constraint-domain` of `draft/sections/01-abstract-spg-definition.tex`.
3. The current typing-rule machinery becomes one instantiation among many (`TypingDomain`), clearing the path for future domains (dependent types, database semantics, etc.).
4. Code and paper are co-evolved: every code construct that has a formal counterpart carries a `///` reference to the paper label (`def:*`, `lem:*`, `thm:*`); engineering choices the paper currently glosses over are noted in the paper.
5. Domain-level realizability proofs (analogous to `lem:evidence-monotone`, `lem:evidence-realizable`, `lem:typeof-realizable`, `lem:rule-realizable`, `thm:typing-realizable`) become straightforward to state, test, and verify.

This file is the single source of truth for the rework. Read it end-to-end before touching code.

---

## 1. Paper ↔ code reference table

This is the contract. Every entry in the right column must exist after the rework with a `///` doc comment citing the left column.

| Paper label | Paper construct | Code construct |
|---|---|---|
| §1.2 `sec:gram-def` | `G = (N, T, P, S, Θ, 𝒯, B, A)` | `engine::grammar::SPG<R>` |
| Definition (Production) | `p^n_a = α₁[b₁]…α_m[b_m]` | `engine::grammar::Production` |
| Definition (Binding signature) | `B(n) ⊆ B` | `engine::binding::BindingSignature` |
| Definition (Binding position) | `pos_{n,a}(b)` | `engine::binding::BindingMap` |
| Property `prop:binding-uniqueness` | binding uniqueness | invariant of `BindingMap`, asserted in `debug_assert!` |
| Definition (Syntactic State) | `σ(s) = (s, t)` | `engine::parse::state::ParserState` |
| Definition `def:evidence-summary` | `ν = ⟨n, [i,j), ρ, τ, ∇, β⟩` | `semantics::EvidenceSummary` |
| Definition `def:beta-resolution` | `β(ν,b)` | `semantics::obligation::Obligation` |
| Definition `def:evidence-graph` | `G(s) = (E(s), C(s))` | implicit (materialized by `engine::parse::arena::ParseArena` + `DomainRuntime`) |
| Definition `def:open-closed` | open/closed graph | `semantics::EvidenceSummary::is_closed()` |
| Definition `def:witness` | witness `r` for `G(s)` | implicit in `Synthesizer::complete` |
| Definition `def:denotation` | `D(G(s))` | not a runtime construct (set-theoretic) |
| Definition `def:constraint-domain` | `D = (Rules, Closed, Ctx, eval, ⊕)` (post-update; see §6.A) | `semantics::domain::ConstraintDomain` |
| Lemma `lem:safe-pruning` | safe pruning | invariant maintained by `DomainRuntime`; documented on `finalize` |
| §2 `sec:prefix-parsing-algorithm` | prefix parser `Ψ_G` | `engine::parse::parser::Parser` |
| Definition `def:obligation-store` | obligation store `Ω` | `semantics::obligation::Obligations` |
| §2.X operations (`descend`, `step`, `finalize`, `apply`, …) | parser semantic interface | `semantics::SemanticRuntime` methods |
| Lemma `lem:evidence-monotone` | type evidence monotonicity | doc on `TypingDomain`; property test in `domains/typing/tests/realizability.rs` |
| Lemma `lem:evidence-realizable` | type evidence realizability | same |
| Lemma `lem:typeof-realizable` | `typeof` realizability | same |
| Lemma `lem:rule-realizable` | rule realizability | doc on `TypingDomain::finalize` |
| Theorem `thm:typing-realizable` | `eval_impl = eval` | top-of-module doc on `domains::typing` |

Any new domain MUST replicate the "Realizability" portion of the table (`lem:*-realizable`-analogs and `thm:*-realizable`-analog), even if only as `// open / not yet proven` markers — see §7.

---

## 2. Pre-work (Phase 0)

Block-required before any other phase. The working tree currently has uncommitted changes (M and ??) across many files. Three categories:

**A. Modified files** (`git status` — M):
- `benches/chart_growth.rs`
- `scripts/collect.sh`
- `src/cli/mod.rs`
- `src/logic/binding.rs`
- `src/logic/grammar/{mod,tests}.rs`
- `src/logic/{mod,structure/ast,structure/display}.rs`
- `src/logic/parse/{arena,display,mod,parser,testing}.rs`
- `src/logic/parse/tests/{dedup,mod,parse,repro,status,utils}.rs`
- `src/logic/synth/mod.rs`
- `src/logic/typing/{mod,runtime,syntax}.rs`
- `src/logic/typing/tests/{invariants,obligation,runtime}.rs`
- `src/validation/{completable,parseable}/mod.rs`

**B. Deleted files** (D):
- `benches/chart_data.rs`
- `examples/typescript.auf`
- `src/logic/grammar/extend.rs`
- `src/logic/typing/obligation.rs`
- `src/validation/completable/typescript.rs`
- `src/validation/parseable/typescript.rs`

**C. Untracked** (??):
- `draft/` (the paper — keep)
- `p7-sas.pdf` (paper export — keep)
- `refactor.md` (this file — keep)
- `src/cli/chart/` (new chart command — keep)
- `src/logic/semantic/` (new semantic module — already on the path of the rework; KEEP and integrate)

**Decisions required from the user before starting**:

1. Commit the M+D set as one "pre-rework checkpoint" commit so the rework starts from a clean tree? **Recommended.** Suggested message: `checkpoint before SPG generalization refactor`.
2. Confirm `src/logic/semantic/` is the in-progress start of `semantics::` and should be folded into Phase 2 rather than discarded.
3. Confirm `src/cli/chart/` is unrelated and just needs `use`-path updates in Phase 6.
4. Confirm the deleted `typescript.{auf,rs}` files should stay deleted (no resurrection during the rework).

**Exit criteria for Phase 0**:
- `git status` is clean.
- `cargo build` succeeds.
- `cargo test` succeeds (or, if currently failing, list of accepted-failing tests is recorded here below).
- This file's "Pre-work decisions" subsection above is updated with the user's answers.

---

## 3. Final architecture (target layout)

Every file listed below is either new (N), moved from somewhere (M ← src), or kept in place but renamed/restructured (R).

```
src/
  engine/                                # §2 — Generic parser algorithm
    mod.rs                               # N
    grammar/                             # SPG syntactic tuple (N, T, P, S, B, A)
      mod.rs                             # R ← logic/grammar/mod.rs   (Grammar → SPG<R>)
      production.rs                      # M ← logic/grammar/production.rs
      symbol.rs                          # M ← logic/grammar/symbol.rs
      tokenizer.rs                       # M ← logic/grammar/tokenizer.rs
      load.rs                            # R ← logic/grammar/load.rs   (load_ebnf only)
      fill.rs                            # M ← logic/grammar/fill.rs
      save.rs                            # M ← logic/grammar/save.rs
      display.rs                         # M ← logic/grammar/display.rs
      utils.rs                           # M ← logic/grammar/utils.rs
      typing.rs                          # DELETE (now in domains/typing/syntax.rs)
    parse/                               # Prefix parser, arena, items, state
      mod.rs                             # M ← logic/parse/mod.rs
      arena.rs                           # M ← logic/parse/arena.rs
      parser.rs                          # M ← logic/parse/parser.rs
      state.rs                           # M ← logic/parse/state.rs
      display.rs                         # M ← logic/parse/display.rs
      testing.rs                         # M ← logic/parse/testing.rs
      tests/                             # M ← logic/parse/tests/
    ast.rs                               # M ← logic/structure/ast.rs (FusionAST, FusionNode)
    ast_display.rs                       # M ← logic/structure/display.rs
    binding.rs                           # R ← logic/binding.rs        (generic over R: HasBindings)
    synth/                               # M ← logic/synth/
    path.rs                              # M ← logic/path.rs
    error.rs                             # M ← logic/error.rs
    exp/                                 # M ← logic/exp/  (or delete if dead)

  semantics/                             # §1 — Abstract SPG + constraint domain
    mod.rs                               # R ← logic/semantic/mod.rs
                                         #     SemanticRuntime, EvidenceSummary,
                                         #     Verdict re-export
    obligation.rs                        # M ← logic/semantic/obligation.rs
    domain.rs                            # N — ConstraintDomain, Verdict, HasBindings
    loader.rs                            # N — ConstraintLoader
    runtime.rs                           # N — DomainRuntime<D>

  domains/                               # §3 — Domain implementations
    mod.rs                               # N
    typing/                              # Typing domain
      mod.rs                             # M ← logic/typing/mod.rs
      types.rs                           # M ← logic/typing/types.rs
      context.rs                         # R ← logic/typing/context.rs (IndexMap-backed)
      rule.rs                            # R ← logic/typing/rule.rs    (+ impl HasBindings)
      ops.rs                             # M ← logic/typing/ops.rs
      syntax.rs                          # M ← logic/typing/syntax.rs
      pool.rs                            # M ← logic/typing/pool.rs
      domain.rs                          # N — TypingDomain: ConstraintDomain
      loader.rs                          # N — TypingRuleLoader: ConstraintLoader
      tests/
        invariants.rs                    # M ← logic/typing/tests/invariants.rs
        obligation.rs                    # M ← logic/typing/tests/obligation.rs
        runtime.rs                       # M ← logic/typing/tests/runtime.rs
        realizability.rs                 # N — property tests for lem:*-realizable analogs

  ffi/
    mod.rs                               # KEEP
    python.rs                            # update use-paths; Phase 7 expands API

  validation/                            # KEEP locations; update use-paths only
  cli/                                   # KEEP locations; update use-paths only
  regex/                                 # KEEP entirely (independent)
  utils.rs                               # KEEP
  testing.rs                             # KEEP (cfg(test))
  main.rs                                # KEEP
  mod.rs / lib.rs                        # Update top-level module declarations
  complexity/                            # KEEP
```

**Modules removed from `src/logic/`**: everything except possibly `debug.rs` (the debug-tracing macros). Decide in Phase 6 whether to move that under `engine/` or keep at crate root.

---

## 4. Resolved design decisions

Each decision is final unless explicitly revisited in a follow-up turn before Phase 2 starts.

### 4.1 `Verdict` is a single concrete enum, not an associated type

The Satisfied/Live/Lost trichotomy is universal across constraint domains (it's the codomain of `eval` in `def:constraint-domain`). It is defined once:

```rust
// semantics/domain.rs

/// Three-valued verdict from `def:constraint-domain`. Universal across all
/// constraint domains.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Verdict {
    /// `G(s) ∈ Closed`.
    Satisfied,
    /// `G(s) ∉ Closed ∧ D(G(s)) ≠ ∅`.
    Live,
    /// `D(G(s)) = ∅`. The parser bridge translates this to `Err(TransitionError::Rejected)`.
    Lost,
}
```

It is **not** an associated type of `ConstraintDomain`. Every domain returns the same enum. `DomainRuntime<D>` translates `Lost` to `Err(TransitionError::Rejected)` when implementing the parser-facing `SemanticRuntime`.

### 4.2 Bindings exposed by the rule type via `HasBindings`

The generic binding map construction (`engine::binding::build_binding_map`) needs to know which bindings each rule references. Rules expose this themselves:

```rust
pub trait HasBindings {
    fn referenced_bindings(&self) -> Box<dyn Iterator<Item = &str> + '_>;
}
```

`TypingRule: HasBindings` walks `self.premises` and yields every binding name appearing in `judgment` and `setting.extensions`. (RPIT-in-trait is fine if MSRV permits; otherwise use `Box<dyn Iterator>`.)

### 4.3 Context is `IndexMap<Identifier, EvidenceId>`-shaped, but the *concrete type* stays as `D::Context`

The abstract paper change (§6.A) standardizes `Ctx = Identifier ⇀ Evidence`. In code, each domain still owns its concrete `Context` type as an associated type, so that:

- `TypingDomain` can keep its existing `Context` struct (already `Vec`-backed and order-preserving).
- A future proof-types domain can use `IndexMap<String, ProofEvidenceId>` or whatever it needs.

The `Hash + Eq + Clone` bound is required because `DomainRuntime<D>` interns `D::Context` into `CtxId`. Domain implementers must use ordered storage internally — `HashMap` does not satisfy `Hash`.

### 4.4 Initial context is user-supplied

The formal identity for `⊕` is just the distinguished empty element of `Ctx` (no separate symbol; `Ctx` is a set, the empty context is an element of it). In code there is one method that returns it:

```rust
fn empty_context(&self) -> Self::Context;
```

The user-facing `Synthesizer` API accepts a non-empty starting context, exactly as `PySynthesizer::add_to_ctx` does today. The flow is:

1. `Synthesizer::new(spec, input)` uses `empty_context()` as the parse-root context by default.
2. `Synthesizer::with_context(ctx)` (or the `ctx` field of `PySynthesizer`) lets the user provide a populated starting context.
3. Formally this is the same as pre-applying some `∇₀ ∈ Ctx → Ctx` to the empty context before the root. The parser sees only the resulting `CtxId`.

The **database domain** is the canonical motivating case: the schema is loaded into the user-supplied initial context before parsing begins. This is engineering, not formal: the paper's `eval(G(s))` is parameterized by some starting context; the implementation just lets the caller specify it. Noted in the paper as an engineering convenience under §6.C.

### 4.5 `ConstraintLoader` is NT-name-agnostic

The loader produces only `HashMap<RuleName, R>`. The `NT → RuleName` mapping is part of EBNF (declared in lines like `Expr ::= … (RuleName)`) and is handled entirely by `engine::grammar::load::load_ebnf`. The top-level orchestrator joins them:

```rust
impl<R> SPG<R> {
    pub fn load_with<L: ConstraintLoader<Domain: ConstraintDomain<Rule = R>>>(
        source: &str,
        loader: &L,
    ) -> Result<Self, String> {
        let (mut spg, rule_blocks) = load_ebnf(source)?;
        let rules = loader.load(&rule_blocks)?;
        spg.attach_rules(rules);  // joins via the NT → RuleName map already in spg
        Ok(spg)
    }
}
```

### 4.6 Segments threaded explicitly

No `set_segs` mutable hook. Every `ConstraintDomain` method that may resolve obligation values takes `segs: &[Segment]`. The `Synthesizer` owns segments and passes them down to `DomainRuntime`, which forwards them to the domain.

This is a departure from the current `SemanticRuntime::set_segs(&mut self, ...)`. After the rework, `SemanticRuntime` methods also take `segs: &[Segment]`; the mutable hook is removed.

### 4.7 Renames table

| Old | New | Rationale |
|---|---|---|
| `logic/grammar/Grammar` | `engine::grammar::SPG<R>` | Matches paper's `G = (N,T,P,…)` and is generic; needs `#[allow(clippy::upper_case_acronyms)]` on the type def |
| `logic/semantic/SemanticSummary` | `semantics::EvidenceSummary` | Matches paper's `ν` |
| `logic/typing/runtime/RuleRuntime` | split into `domains::typing::TypingDomain` + `semantics::runtime::DomainRuntime<D>` | Separates domain (math) from runtime (bridge/interning) |
| `Grammar::load()` | `SPG::load_with(source, loader)` + `engine::grammar::load_ebnf` + `domains::typing::TypingRuleLoader::load` | Separates EBNF from rule loading |
| `SemanticRuntime::set_segs` | removed; segments passed per-call | See §4.6 |

---

## 5. Full trait shapes

These are the exact signatures to land in Phase 2. Doc comments are non-negotiable — they carry the paper references.

```rust
// semantics/domain.rs

use std::hash::Hash;
use crate::engine::grammar::Segment;
use crate::engine::parse::arena::NodeStatus;
use crate::semantics::obligation::Obligations;

/// `def:constraint-domain` (post-update §6.A):
/// `D = (Rules, Closed, Ctx, ε, eval, ⊕)`.
///
/// The trait exposes the *operational* decomposition of `eval(G(s))`. The
/// theoretical single-shot `eval` is unimplementable online; the parser
/// builds `G(s)` incrementally and calls `descend`, `finalize`,
/// `apply_effect`, `compose_effects` per node and per edge.
///
/// ## Realizability obligations for implementers
///
/// Every impl SHOULD carry a module-level doc comment stating the proof
/// status of:
/// - monotonicity of evidence under input extension (`lem:evidence-monotone` analog)
/// - realizability of non-`Lost` premises (`lem:rule-realizable` analog)
/// - `eval_impl = eval` (`thm:typing-realizable` analog)
///
/// See `domains::typing` for a worked example.
pub trait ConstraintDomain {
    /// Elements of `Θ`. Must expose its binding usage so generic code can
    /// build the binding map; see `engine::binding::build_binding_map`.
    type Rule: HasBindings;

    /// `τ` in the evidence summary `ν` (`def:evidence-summary`).
    type Evidence: Hash + Eq + Clone;

    /// `Ctx` in `def:constraint-domain` (post-update). Identifier ⇀ Evidence
    /// in the abstract model; concrete representation is the domain's choice
    /// but must be ordered (HashMap does not satisfy `Hash`).
    type Context: Hash + Eq + Clone;

    /// `∇` in `def:evidence-summary`. Acts on `Ctx` via `apply_effect`.
    type Effect: Hash + Eq + Clone;

    /// `ε` — the empty context, identity for `apply_effect`.
    fn empty_context(&self) -> Self::Context;

    /// The "no evidence yet" sentinel, returned for nodes without an
    /// attached rule. Corresponds to `Type::Any` in the typing domain.
    fn any_evidence(&self) -> Self::Evidence;

    /// Context selected before entering a child position (`descend` op of
    /// §2 semantic interface). Returns the child-bound context.
    fn descend(
        &self,
        rule: &Self::Rule,
        binding: Option<&str>,
        ctx: &Self::Context,
        obligations: &Obligations,
        segs: &[Segment],
    ) -> Self::Context;

    /// Verdict + evidence + optional effect for a node (`finalize` op of
    /// §2 semantic interface). Operational embodiment of one node-level
    /// case of `eval(G(s))`. Live nodes never export an effect; only
    /// `Verdict::Satisfied` with `status == NodeStatus::Closed` may return
    /// `Some(effect)`.
    ///
    /// Implementers MUST satisfy `lem:rule-realizable`-analog: a non-`Lost`
    /// verdict implies the existence of a continuation that drives the
    /// verdict to `Satisfied`.
    fn finalize(
        &self,
        rule: &Self::Rule,
        ctx: &Self::Context,
        obligations: &Obligations,
        segs: &[Segment],
        status: NodeStatus,
    ) -> (Verdict, Self::Evidence, Option<Self::Effect>);

    /// `⊕` from §6.A: apply an exported right-bound effect to a context.
    fn apply_effect(&self, ctx: Self::Context, effect: &Self::Effect) -> Self::Context;

    /// Left-to-right composition of effects for transparent productions.
    /// `None` ↔ the composed effect is the identity (no observable change).
    fn compose_effects(&self, effects: &[&Self::Effect]) -> Option<Self::Effect>;
}

/// Three-valued verdict from `def:constraint-domain`. Universal across
/// all constraint domains.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Verdict { Satisfied, Live, Lost }

/// Lets generic code introspect a rule's binding usage without knowing the
/// rule type. Used by `engine::binding::build_binding_map`.
pub trait HasBindings {
    fn referenced_bindings(&self) -> Box<dyn Iterator<Item = &str> + '_>;
}
```

```rust
// semantics/loader.rs

use std::collections::HashMap;
use crate::semantics::domain::ConstraintDomain;

/// `𝒯 : N ⇀ Θ` is split into two halves:
/// - the NT-name → rule-name map (in EBNF, handled by `load_ebnf`)
/// - the rule-name → rule body map (this trait)
pub trait ConstraintLoader {
    type Domain: ConstraintDomain;

    /// Receives rule-source blocks (everything in the `.auf` file that
    /// isn't a `::=` production block) and returns the rule table by name.
    fn load(
        &self,
        blocks: &[&str],
    ) -> Result<
        HashMap<String, <Self::Domain as ConstraintDomain>::Rule>,
        String,
    >;
}
```

```rust
// semantics/runtime.rs

/// Generic bridge: holds an `SPG<D::Rule>` plus interners for D's evidence,
/// context, and effect types. Implements the parser-facing
/// `SemanticRuntime` by interning D's outputs into opaque IDs.
pub struct DomainRuntime<D: ConstraintDomain> {
    domain: D,
    spg: SPG<D::Rule>,
    evidence: Interner<D::Evidence, EvidenceId>,
    contexts: Interner<D::Context, CtxId>,
    effects:  Interner<D::Effect, EffectId>,
}

impl<D: ConstraintDomain> DomainRuntime<D> {
    pub fn new(domain: D, spg: SPG<D::Rule>) -> Self { /* interns ε as CTX_0, any_evidence as EVIDENCE_ANY */ }

    pub fn intern_context(&self, ctx: D::Context) -> CtxId { ... }

    pub fn context(&self, id: CtxId) -> Option<&D::Context> { ... }

    // ...
}

impl<D: ConstraintDomain> SemanticRuntime for DomainRuntime<D> {
    fn descend(
        &self,
        prod: ProdId,
        binding: Option<&str>,
        ctx: CtxId,
        obligations: &Obligations,
        segs: &[Segment],          // NEW (§4.6)
    ) -> Result<CtxId, TransitionError> {
        let Some(rule) = self.spg.rule_for_prod(prod) else { return Ok(ctx); };
        let ctx_val = self.context(ctx).ok_or(TransitionError::Rejected)?;
        let next = self.domain.descend(rule, binding, ctx_val, obligations, segs);
        Ok(self.intern_context(next))
    }

    fn finalize(
        &self,
        prod: ProdId,
        ctx: CtxId,
        obligations: &Obligations,
        segs: &[Segment],          // NEW
        status: NodeStatus,
    ) -> Result<EvidenceSummary, TransitionError> {
        let Some(rule) = self.spg.rule_for_prod(prod) else {
            return Ok(EvidenceSummary::new(self.evidence.intern(self.domain.any_evidence()), None, true));
        };
        let ctx_val = self.context(ctx).ok_or(TransitionError::Rejected)?;
        let (verdict, evidence, effect) = self.domain.finalize(rule, ctx_val, obligations, segs, status);
        match verdict {
            Verdict::Lost => Err(TransitionError::Rejected),
            Verdict::Live => Ok(EvidenceSummary::new(self.evidence.intern(evidence), None, false)),
            Verdict::Satisfied => Ok(EvidenceSummary::new(
                self.evidence.intern(evidence),
                effect.map(|e| self.effects.intern(e)),
                true,
            )),
        }
    }

    fn apply_effect(&self, ctx: CtxId, effect: EffectId, segs: &[Segment]) -> Result<CtxId, TransitionError> { ... }
    fn compose_effects(&self, effects: Vec<EffectId>) -> Result<Option<EffectId>, TransitionError> { ... }
}
```

`SemanticSummary` is renamed to `EvidenceSummary` and its `evidence` field stays as `EvidenceId` (the parser sees only opaque IDs — this is unchanged).

---

## 6. Paper changes

Each subsection has (a) the change to make and (b) the proposed wording or sketch to drop in.

### 6.A Lift context into the abstract constraint domain

**Problem**: `def:evidence-summary` includes `∇`, but the abstract model never says what `∇` acts on. §3 silently introduces context Γ as if it were typing-domain-specific. Without naming `Ctx` and `⊕` in `def:constraint-domain`, the operational hooks `descend` and `apply_effect` have no formal home.

**Change**: Update `def:constraint-domain` to:

> A *constraint domain* is a tuple
> $$D = (\mathsf{Rules},\, \mathsf{Closed},\, \mathsf{Ctx},\, \mathit{eval},\, \oplus)$$
> where, in addition to the previous components,
> - $\mathsf{Ctx}$ is the *context type* of $D$, a set of finite partial maps $\mathit{Identifier} \rightharpoonup \mathit{Evidence}$ containing a distinguished empty element;
> - $\oplus : \mathsf{Ctx} \times \nabla \to \mathsf{Ctx}$ applies a right-bound effect, with $\nabla$ typed as $\mathsf{Ctx} \to \mathsf{Ctx}$.

The empty context is the identity for $\oplus$. It is an element of $\mathsf{Ctx}$, not a separate component of the tuple — naming it inside the tuple would clutter the notation without adding content.

Then update §3's introduction of Γ to say "the typing domain instantiates Ctx as an ordered finite map $\mathit{Identifier} \rightharpoonup \mathit{Type}$" instead of introducing context as if domain-local.

This is not a model expansion — it makes explicit what `∇` already requires. No proofs of §3 change.

### 6.B Operational decomposition of `eval` (new short subsection)

**Where**: end of §1.3 (or as a new §1.4 between abstract definition and parser).

**Sketch**:

> The evaluator $\mathit{eval}(\mathcal G(s))$ in Definition `def:constraint-domain` is defined on a fully constructed evidence graph. An online prefix parser cannot compute $\mathit{eval}$ in a single step; it must accumulate the verdict as $\mathcal G(s)$ grows. We therefore expose $\mathit{eval}$ through four operational hooks, each computing the verdict-contribution of a single node or edge as $\mathcal G(s)$ is constructed:
>
> - $\mathsf{descend}(p, b, \Gamma, \Omega)$ — selects the context entering child binding $b$;
> - $\mathsf{finalize}(p, \Gamma, \Omega, \rho)$ — closes a node, computing its $\tau$, its $\nabla$ if any, and its verdict in $\{\mathsf{Satisfied}, \mathsf{Live}, \mathsf{Lost}\}$;
> - $\mathsf{apply}(\Gamma, \nabla)$ — folds an exported right-bound effect into the context of the next sibling;
> - $\mathsf{compose}(\nabla_1, \dots, \nabla_k)$ — left-to-right composition of effects through transparent productions.
>
> Theorem `thm:typing-realizable` may be read as the statement that the join of these per-node verdicts, taken at the root, agrees with the single-shot $\mathit{eval}(\mathcal G(s))$ for the typing domain. We expect analogous theorems for every constraint domain in $\mathsf{SPG}(D)$.

### 6.C Engineering-decision footnotes in §2

Add three short paragraphs in §2.X:

1. *Spans, not strings.* The implementation segments input once via `seg` and represents every binding value as a span `[i, j)` into the segment array. All occurrences of "text" in the paper are realized as spans in code; this changes nothing semantically because $\mathsf{seg}$ is deterministic.
2. *Interning.* The runtime interns evidence, contexts, and effects into opaque integer handles before storing them in the arena. The parser observes only handles; the semantic-domain-specific types are recovered by lookup when needed. This is invisible to the formal model: $\mathcal E(s)$ is still the same finite set.
3. *Initial context.* The user may provide a non-empty starting context (e.g., a pre-loaded database schema). Formally this is the empty context updated by some user-supplied $\nabla_0 \in \mathsf{Ctx} \to \mathsf{Ctx}$ applied before the root.

### 6.D Cross-references

Every definition / lemma / theorem in `draft/` gets a footnote of the form:

> *In code:* `engine::grammar::SPG` (`src/engine/grammar/mod.rs`).

The Rust side gets `///` doc comments naming the paper label (already encoded in §1's table above).

To detect drift, add a `scripts/check-paper-refs.sh` that:
- greps `///.*\b(def|lem|thm):[a-z-]+\b` in `src/` for every reference;
- greps `\\label\{(def|lem|thm):[^}]+\}` in `draft/` for every label;
- reports any reference without a matching label or vice versa.

(Optional — add only if upkeep proves painful.)

---

## 7. Realizability conventions

The paper's §3 chain — `lem:evidence-monotone` → `lem:evidence-realizable` → `lem:typeof-realizable` → `lem:rule-realizable` → `thm:typing-realizable` — is the worked example of what every constraint domain must establish for the SPG framework to give correct pruning (`lem:safe-pruning`). Realizability for any new domain must be:

1. **Stated** in the domain's module-level doc comment.
2. **Argued** in the matching paper section (new subsections in §3 or new sections).
3. **Tested** in `domains/<name>/tests/realizability.rs`.

### 7.1 Domain module-level checklist (mandatory)

Every `ConstraintDomain` impl module ships with this block at the top:

```rust
//! # Realizability status for `<DomainName>`
//!
//! ## Monotonicity (`lem:evidence-monotone`-analog)
//! Status: PROVEN | OPEN | PARTIAL — <one-line>
//! Paper reference: §X.Y
//!
//! ## Evidence realizability (`lem:evidence-realizable`-analog)
//! Status: ...
//! Paper reference: ...
//!
//! ## Premise realizability (`lem:typeof-realizable`-analog)
//! Status: ...
//! Paper reference: ...
//!
//! ## Rule realizability (`lem:rule-realizable`-analog)
//! Status: ...
//! Paper reference: ...
//!
//! ## eval_impl = eval (`thm:typing-realizable`-analog)
//! Status: ...
//! Paper reference: ...
```

`OPEN` is acceptable; silently absent is not. The block is grep-able so the test suite can sanity-check every domain declares its status.

### 7.2 Property tests (`domains/<name>/tests/realizability.rs`)

Two patterns, both generic across domains via a small `RealizabilityHarness` helper (lives in `semantics/runtime.rs` or `engine/synth/`):

**Monotonicity**: given a reachable state `σ(s)` from a fuzzed grammar, extend the input by one segment to obtain `σ(s · x)`. Assert:
- no node's verdict transitions from `Lost` to anything else;
- evidence for any node either stays equal or strictly narrows (domain-specific notion of "narrows" via a `Evidence: Narrows` helper trait, optional for now — for typing it's `comp(e, s·x) ⊆ comp(e, s)`).

**Witness existence**: given a reachable state `σ(s)` with root verdict `Live`, use the `Synthesizer`'s completion machinery to produce a candidate continuation `r`. Assert root verdict at `σ(s · r)` is `Satisfied` within a bounded depth (configurable, default 64 segments). On failure, dump the offending state for inspection.

These are property tests with deterministic seeds (for reproducibility in CI). They are **not** proofs — they catch regressions, they do not establish correctness.

### 7.3 Why this matters

Safe pruning (`lem:safe-pruning`) is conditional on the domain's evaluator never returning `Lost` for a prefix that has a continuation. If a domain breaks realizability, the parser silently rejects valid prefixes. The convention above forces every domain to declare and (ideally) verify the property that makes the framework sound.

---

## 8. Phases — concrete acceptance criteria

Each phase ends in one commit; do not interleave phases. Every phase must pass `cargo test` (with the test exclusions, if any, recorded in Phase 0) before moving on.

### Phase 0 — Pre-work
Done when:
- `git status` clean
- Pre-work decisions (§2) recorded back into this file
- `cargo test` green

### Phase 1 — Add new traits (additive, no moves)
Create:
- `src/semantics/` (new directory, alongside existing `src/logic/semantic/` if necessary)
  - `mod.rs` (re-exports)
  - `domain.rs` (`ConstraintDomain`, `Verdict`, `HasBindings`)
  - `loader.rs` (`ConstraintLoader`)
  - `runtime.rs` (`DomainRuntime<D>` skeleton — methods can panic until Phase 3)
- New top-level module declared in `src/mod.rs` (or `src/lib.rs`)
- Existing `src/logic/semantic/` remains; nothing else changes

Done when:
- New traits compile.
- Nothing currently uses them.
- `cargo test` still green.

### Phase 2 — Implement `TypingDomain`
- New: `src/domains/typing/{domain.rs, loader.rs}` (the rest stays at `src/logic/typing/` for now)
- `TypingDomain` implements `ConstraintDomain` using logic extracted from `RuleRuntime`
- `TypingRule` gains `impl HasBindings`
- `TypingRuleLoader` implements `ConstraintLoader` using the non-EBNF half of `Grammar::load`
- `DomainRuntime<TypingDomain>` fully implements `SemanticRuntime` and is wired through `Synthesizer` as a feature-flagged alternative to `RuleRuntime` (so old tests keep passing)

Done when:
- `DomainRuntime<TypingDomain>` passes every test that `RuleRuntime` passes (run both side-by-side under a cargo feature).

### Phase 3 — Cutover
- Replace `RuleRuntime` with `DomainRuntime<TypingDomain>` everywhere.
- Delete `RuleRuntime`.
- Delete the rule-loading half of `Grammar::load`; replace call sites with `SPG::load_with(source, &TypingRuleLoader)`.

Done when:
- `RuleRuntime` is gone.
- `cargo test` green.

### Phase 4 — Genericize `Grammar` → `SPG<R>`
- Rename `Grammar` to `SPG<R>`; rules field is `HashMap<String, R>`.
- `engine::binding::build_binding_map` becomes generic over `R: HasBindings`.
- Audit every reader of `grammar.rules` outside `domains::typing` and `DomainRuntime` — none should remain.

Done when:
- `cargo test` green.
- `grep -r 'grammar.rules' src/engine src/cli` returns nothing.

### Phase 5 — Module restructure
- Execute every move from §3 (the "Final architecture" tree).
- Update every `use` in `validation/`, `cli/`, `ffi/`, `benches/`.
- Update top-level module declarations.
- `cargo test` green.

### Phase 6 — Paper updates
- Apply §6.A, §6.B, §6.C, §6.D to `draft/sections/*.tex`.
- Add the realizability checklist (§7.1) to `domains/typing/mod.rs`.
- Add the property tests (§7.2) to `domains/typing/tests/realizability.rs`.

Done when:
- Paper builds.
- New property tests pass.

### Phase 7 — FFI expansion (separate task; not part of this rework)
- `PyGrammar` wrapping `SPG<TypingRule>` with inspection/mutation methods.
- `PyAst` exposing opaque `EvidenceId` and structural traversal.
- `PyRuntime` for interpreting evidence IDs.
- Eventual `Box<dyn ConstraintDomain<...>>` if/when multi-domain Python use shows up.

---

## 9. Out of scope

- Phase 7 (FFI expansion) — separate task after rework lands.
- Implementing a second `ConstraintDomain` (proof types, database). The rework only clears the path; landing such a domain is follow-on work that exercises §7.
- Hash-consing evidence values deeply (`Type::Arrow(Box<Type>, Box<Type>)` interns shallowly today; same after rework).
- Performance tuning of the interning layer.
- Bringing back the deleted `typescript.*` files.

---

## 10. Pre-work decisions

- [x] Q1 (commit checkpoint): **accepted** — message: `WIP new model`
- [x] Q2 (`src/logic/semantic/` folded into Phase 1): **accepted**
- [x] Q3 (`src/cli/chart/` is independent): **accepted** — `use`-path updates only
- [x] Q4 (`typescript.*` stays deleted): **accepted**
- [x] Q5 (currently failing tests): **none** — `cargo test` must be green at the end of every phase, with no exclusions
