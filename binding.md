# Beam Binding System Implementation

## Summary

Binding resolution produces `BoundTypingRule` instances at parse time. The parser attaches a fully resolved rule (`bound_typing_rule: Option<Box<BoundTypingRule>>`) directly onto each `NonTerminal` whose production references a typing rule. The type checker consumes only these bound rules; it performs no meta-variable lookup.

## Core Concepts

- **BoundTypingRule**: A typing rule with every schematic term / type variable resolved to concrete AST nodes and `BoundType` values.
- **BoundPremise / BoundTypingJudgment**: Premises where:
  - Ascription: node already resolved; expected type already resolved (no late binding)
  - Membership: node is a concrete variable occurrence; membership checked against current Γ
- **BoundTypeSetting / BoundTypeAscription**: Context extension data (variable node + concrete type) used for premises and conclusion context transforms.
- **BoundConclusion**: Either a concrete type or a context lookup plus an optional (Γ_in → Γ_out) transform represented structurally (no string parsing at check time).
- **BindingResolver (DefaultBindingResolver)**: Walks an AST node + original `TypingRule` and constructs the bound form. It also expands premises over repeated bindings (e.g. a schematic `Γ ⊢ e : τ` becomes N premises if `e` is bound to a repeated sequence at the same AST level).

## Lifecycle

1. Grammar spec parsed → `TypingRule` objects stored in `Grammar`.
2. Parser builds AST. When a production carries a rule annotation, it:
   - Creates the `NonTerminal`
   - Invokes `DefaultBindingResolver::resolve_rule`
   - Attaches the resulting `BoundTypingRule` to `nonterminal.bound_typing_rule` (if well‑formed).
3. Type checking:
   - `TypeChecker::check_nt` finds `bound_typing_rule` and calls `apply_bound_rule`.
   - Premises evaluated in temporary child contexts; no global side effects until conclusion.
   - Conclusion optionally threads context (Γ_in → Γ_out) and yields a `BoundType`.

## Display & Debug

- `Display` for bound entities prints math-like concise forms used in messages/specs.
- `Debug` for `BoundTypingRule` prints diagnostic: `BOUND:<name> (<n> premises): <display>`.

## Serialization Notes

- AST serialization includes `(rule ruleName)` metadata and a `;!rules:` header listing rule names.
- Bound rule internals (resolved nodes/types) are NOT serialized. Deserializing an AST yields nodes with `bound_typing_rule: None`.
- Re-binding after deserialization requires re-running the resolver with access to the original `TypingRule` definitions.

## Advantages

- Single-pass binding: meta-variable resolution cost paid once during parsing.
- Deterministic checking: the checker only evaluates already-ground premises.
- Better errors: premises and conclusion point to concrete AST nodes (spans available).
- Extensible: new premise forms or conclusion kinds can be added in the bound model without changing the checker’s core loop.

## Implementation Status

✅ Done:
- Parser integration (rules bound on-the-fly)
- Bound rule data structures & display
- Premise expansion for repeated bindings
- Type checker support (`apply_bound_rule` implemented)
- Context transform threading in conclusions
- Serialization updated (rule names only, no bound internals)
- Tests exercising bound rule evaluation (incrementally expanding)

🛠 In Progress / Next:
- Systematic re-binding helper for deserialized ASTs
- Additional diagnostics for ill-formed rules (stronger `is_well_formed`)
- Performance profiling on large grammars / deeply nested repetitions
- Broader test coverage for edge cases (empty repetitions, nested transforms)

🚧 Future:
- Multiple named contexts (Γ, Δ, …)
- Polymorphism / type schemes
- Context joins (e.g. branching constructs)

## Minimal Example

Given rule (lambda):
```
Γ[x:τ₁] ⊢ body : τ₂
------------------- (lambda)
τ₁ → τ₂
```
Parsing `(λ x : τ₁ . body)` with binding annotations resolves `x`, `body`, `τ₁`, `τ₂` to concrete nodes/types; the resulting nonterminal holds a `BoundTypingRule` ready for checking.

## Well-Formedness

`DefaultBindingResolver` rejects a rule if any schematic variable in premises/conclusion cannot be matched to a bound AST subtree or concrete type. A bound rule must reference at least one node unless it has zero premises (axiom-like).

## Repetition Expansion

A single schematic premise over a variable appearing in a repeated RHS position expands into multiple concrete premises (one per occurrence). If zero occurrences, the premise silently vanishes (matching the optional nature of Kleene-star sections).

---
Concise: Binding turns schematic inference rules into ground rules at parse time; the checker only executes ground logic.
