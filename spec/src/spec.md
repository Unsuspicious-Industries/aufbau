# Grammar Specification

The grammar is composed of a set of **production rules** with syntactic meaning, and attached **typing rules** conveying semantic information about the structure of the language.

This document formally defines the grammar specification language as implemented in P7.

The core data structures are defined in [`src/logic/grammar/mod.rs`](~/src/logic/grammar/mod.rs).

## Overall Structure

Grammar files for P7 are written in a simple BNF-like format with human-readable, inference-style typing rules. 

The goal of this specification is to be able to define the grammar $G$ as presented in [our conceptual framework](concepts/theory.md) in a nice and human readable way. This spec is a **data oriented** view of our rules. 



## Production Rules

Production rules define the syntactic structure of the language. Each rule maps a nonterminal to one or more alternative right-hand sides.

>D Production Rule Syntax
```
Nonterminal(rule_name) ::= symbol₁ symbol₂ ... | alternative₂ | ...
```

Where:
- `Nonterminal` is the name of the symbol being defined
- `(rule_name)` is the optional name of the typing rule attached to this production
- `::=` is the separator between left-hand side and right-hand side
- `|` separates alternative productions
- `symbols` is a sequence of terminals and nonterminals
<

>E Lambda Calculus Productions
```spec
Variable(var) ::= Identifier[x]
Lambda(lambda) ::= 'λ' Variable[x] ':' Type[τ] '.' Term[e]
Application(app) ::= BaseTerm[f] BaseTerm[e]
Term ::= Application | BaseTerm
```
<

>R Start Symbol 
By default, the start symbol is the **last declared nonterminal** in the specification file.
<


## Symbols

### Terminals

>D Terminal Symbol
A **terminal** matches concrete input text. There are three forms:

| Form | Description | Example |
|------|-------------|---------|
| `'literal'` | Single-quoted string literal | `'λ'`, `'->'`, `'let'` |
| `"literal"` | Double-quoted string literal | `"fn"`, `"=>"` |
| `/regex/` | Regex pattern | `/[0-9]+/`, `/[a-z][a-zA-Z0-9]*/` |
<

>E Terminal Forms
```spec
// String literals (single or double quotes)
Keyword ::= 'let' | 'in' | "if" | "then" | "else"

// Regex patterns
Number ::= /[0-9]+/
Identifier ::= /[A-Za-z_][A-Za-z0-9_]*/
```
<

### Nonterminals

A **nonterminal** is a reference to another production rule.


>E Nonterminal References
```spec
Term ::= Variable | Lambda | Application
Expression ::= Term '+' Term | Term
```
<

### Bindings

A **binding** attaches a name to a symbol using bracket notation `[name]`. Bindings are used to reference matched subtrees in typing rules. They are basically the link between the syntax and semantics of a laguage.

>E Binding Syntax
```spec
Lambda(lambda) ::= 'λ' Variable[x] ':' Type[τ] '.' Term[e]
```

Here:
- `[x]` binds the matched `Variable` node
- `[τ]` binds the matched `Type` node  
- `[e]` binds the matched `Term` node

These names can then be referenced in the `lambda` typing rule.
<

>R Binding Resolution
Bindings are resolved via **grammar paths** at type-checking time. 

See [`src/logic/binding/grammar.rs`](~/src/logic/binding/grammar.rs) for the path construction algorithm.
<

### Epsilon Productions

We can represent an **epsilon production**, meaning an empty alternative,  with the symbol `ε`.


>E Optional Tail Pattern
```spec
ProgramTail ::= Term ProgramTail | ε
```

This allows `ProgramTail` to match zero or more `Term`s.
<



## Typing Rules

Typing rules attach semantic information to productions. They follow an inference-rule format.


```spec
premise₁, premise₂, ...
------------------------ (rule_name)
conclusion
```

The rule name in parentheses must match a `(rule_name)` annotation on a production.


### Premises

We have several (hardcoded) premise types that can express conditions that must hold for the rule to apply:

| Syntax | Meaning |
|--------|---------|
| `Γ ⊢ e : τ` | Term `e` has type `τ` in context `Γ` (inference) |
| `Γ ▷ e` | Check that term `e` is well-typed in context `Γ` (checking) |
| `x ∈ Γ` | Variable `x` is bound in context `Γ` |
| `τ₁ = τ₂` | Types must unify |
| `τ₁ ⊆ τ₂` | Type `τ₁` is a subtype of `τ₂` |
| `Γ[x:τ]` | Extend context with binding `x:τ` |


>E Premise Examples
```spec
// Check that f is a function and e matches its domain (inference)
Γ ⊢ f : ?A → ?B, Γ ⊢ e : ?A

// Check that e is well-typed (checking mode)
Γ ▷ e

// Variable must be in scope
x ∈ Γ

// Type equality constraint
τ₁ = τ₂
```
<

### Conclusions

>D Conclusion Types
Conclusions specify what the rule produces:

| Syntax | Meaning |
|--------|---------|
| `τ` | Return type `τ` |
| `Γ(x)` | Look up type of `x` in context and return it |
| `Γ → Γ[x:τ] ⊢ τ` | Set type of `x` to `τ` in context and return `τ` |
| `▷` | Return void (checking mode) |
<

### Meta-Variables

>D Meta-Variable
A **meta-variable** (written `?A`, `?B`, etc.) is a placeholder that gets unified during type checking. Meta-variables allow typing rules to express polymorphic constraints.
<

>E Meta-Variable Usage
```spec
Γ ⊢ f : ?A → ?B, Γ ⊢ e : ?A
-------------------------------- (app)
?B
```

Here `?A` must unify with both the function's domain and the argument's type.
<

### Context Operations

>D Typing Context
The typing context `Γ` is a mapping from variable names to types. Context operations include:

- `Γ(x)` — Look up type of variable `x`
- `Γ[x:τ]` — Extend context with binding `x:τ`
- `x ∈ Γ` — Check if `x` is bound in context
<



## Type Syntax

Types in typing rules follow this syntax:

>D Type Forms
| Syntax | Description |
|--------|-------------|
| `τ` | Type variable (from binding) |
| `!τ` | Not Type |
| `?A` | Meta-variable (unified during checking) |
| `'int'` | Raw/concrete type literal |
| `τ₁ → τ₂` | Function type |
| `Γ(x)` | Context lookup |
<

>E Function Type
```spec
τ → ?B
```
Represents a function from type `τ` to type `?B`.
<



## Complete Example

>E Simply-Typed Lambda Calculus
A complete specification for STLC from [`examples/stlc.spec`](~/examples/stlc.spec):

```spec
{{#include ../../examples/stlc.spec}}
```
<


## Source Files

| Component | Path |
|-----------|------|
| Grammar structures | [`~/src/logic/grammar/mod.rs`](~/src/logic/grammar/mod.rs) |
| Spec file loading | [`~/src/logic/grammar/load.rs`](~/src/logic/grammar/load.rs) |
| Typing rules | [`~/src/logic/typing/rule.rs`](~/src/logic/typing/rule.rs) |
| Type definitions | [`~/src/logic/typing/mod.rs`](~/src/logic/typing/mod.rs) |
| Binding map | [`~/src/logic/binding/grammar.rs`](~/src/logic/binding/grammar.rs) |
