#[D] Grammar Specification

The grammar is composed of production rules with syntactic meaning and attached typing rules conveying semantic information about the language.

The core data structures are defined in [`src/logic/grammar/mod.rs`](../../src/logic/grammar/mod.rs).

## Production Rules

Production rules define the syntactic structure of the language. Each rule maps a nonterminal to one or more alternative right-hand sides.

>D Production Rule Syntax
```spec
Nonterminal(rule_name) ::= symbol₁ symbol₂ ... | alternative₂ | ...
```

- `Nonterminal`: symbol being defined.
- `(rule_name)`: optional typing rule name.
- `::=`: separator.
- `|`: alternative separator.
- `symbols`: sequence of terminals and nonterminals.
<

Bindings are resolved via **grammar paths** during type-checking. See [`src/logic/binding/grammar.rs`](../../src/logic/binding/grammar.rs) for the path construction algorithm.

### Epsilon Productions

An epsilon production represents an empty alternative, written as `ε`.

```spec
// Optional tail matching zero or more Terms
ProgramTail ::= Term ProgramTail | ε
```

## Typing Rules

Typing rules attach semantic information to productions using an inference-rule format.

```spec
premise₁, premise₂, ...
------------------------ (rule_name)
conclusion
```

The rule name in parentheses must match a `(rule_name)` annotation on a production.

### Premises

Premises express conditions for rule application.

| Syntax | Meaning |
| :--- | :--- |
| `Γ ⊢ e : τ` | Term `e` has type `τ` in context `Γ` (inference) |
| `Γ ▷ e` | Check that term `e` is well-typed in context `Γ` (checking) |
| `x ∈ Γ` | Variable `x` is bound in context `Γ` |
| `τ₁ = τ₂` | Types must unify |
| `τ₁ ⊆ τ₂` | Type `τ₁` is a subtype of `τ₂` |
| `Γ[x:τ]` | Extend context with binding `x:τ` |

### Conclusions

Conclusions specify the rule's result:

| Syntax | Meaning |
| :--- | :--- |
| `τ` | Return type `τ` |
| `Γ(x)` | Look up type of `x` in context |
| `Γ → Γ[x:τ] ⊢ τ` | Set type of `x` in context and return `τ` |

Checking mode (`▷`) is not implemented as a conclusion; use the check premise `Γ ▷ e` instead.

### Meta-Variables

A **meta-variable** (written `?A`, `?B`, etc.) is a placeholder unified during type checking, allowing rules to express polymorphic constraints.

```spec
// ?A must unify with both the function's domain and the argument's type.
Γ ⊢ f : ?A → ?B, Γ ⊢ e : ?A
-------------------------------- (app)
?B
```

### Context Operations

The typing context `Γ` maps variable names to types:
- `Γ(x)`: Look up type of variable `x`.
- `Γ[x:τ]`: Extend context with binding `x:τ`.
- `x ∈ Γ`: Check if `x` is bound in context.

## Type Syntax

Types in typing rules follow this syntax:

| Syntax | Description |
| :--- | :--- |
| `τ` | Type variable (from binding) |
| `!τ` | Not Type |
| `?A` | Meta-variable (unified during checking) |
| `'int'` | Raw/concrete type literal |
| `τ₁ → τ₂` | Function type |
| `τ₁ \| τ₂` | Union type |
| `⊤` | Top type (any) |
| `∅` | Bottom type (none) |
| `Γ(x)` | Context lookup |

## Example: Simply-Typed Lambda Calculus

A complete specification for STLC from [`examples/stlc.auf`](../../examples/stlc.auf):

```spec
{{#include ../../examples/stlc.auf}}
```

## Source Files

| Component | Path |
| :--- | :--- |
| Grammar structures | [`src/logic/grammar/mod.rs`](../../src/logic/grammar/mod.rs) |
| Spec file loading | [`src/logic/grammar/load.rs`](../../src/logic/grammar/load.rs) |
| Typing rules | [`src/logic/typing/rule.rs`](../../src/logic/typing/rule.rs) |
| Type definitions | [`src/logic/typing/mod.rs`](../../src/logic/typing/mod.rs) |
| Binding map | [`src/logic/binding/grammar.rs`](../../src/logic/binding/grammar.rs) |
