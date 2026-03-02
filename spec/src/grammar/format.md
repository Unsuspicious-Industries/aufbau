#[D] Grammar File Format

A `.auf` file is plain text, block-structured. Blank lines separate blocks. Lines beginning with `//` are comments and are stripped before parsing. Every block is either a **production block** (contains `::=`) or an **inference rule block** (does not).

Source: [`src/logic/grammar/load.rs`](~/src/logic/grammar/load.rs), [`src/logic/grammar/utils.rs`](~/src/logic/grammar/utils.rs)

## Production Blocks

A production block declares one or more alternatives for a nonterminal.

>D Production Syntax
A production block has the form:

$$\texttt{LHS} \;::=\; \alpha_1 \;\mid\; \alpha_2 \;\mid\; \cdots$$

where `LHS` is either a bare nonterminal name or `Name(rule_name)` linking to a typing rule. Each $\alpha_i$ is a sequence of **symbols**. Continuation alternatives may appear on subsequent lines starting with `|`.
<

### Symbol Forms

A symbol in an alternative is one of:

- **Nonterminal**: bare `Name` — references another nonterminal.
- **Literal terminal**: `'text'` — exact string match; the literal is also added to the special-token table so the tokenizer recognizes it as an atomic token.
- **Regex terminal**: `/pattern/` — matched via Brzozowski derivative; see [Regex Engine](../regex.md).
- **Epsilon**: `ε` — the empty alternative. Cannot be mixed with other symbols.

Any symbol may carry a **binding annotation** in square brackets: `Symbol[name]`. The binding makes the matched subtree accessible under that name in the corresponding typing rule. Bindings are resolved at load time into tree-path references by `rebuild_bindings`.

>N epsilon vs empty alternatives
`ε` is not the same as an empty alternative line. You must write it explicitly. A continuation line with just `|` and nothing after it is a parse error.
<

### Start Symbol Convention

The start symbol is set to the **last** nonterminal whose LHS appears for the first time (by declaration order). Write the top-level entry point last.

## Inference Rule Blocks

An inference rule block encodes a typing judgement in natural-deduction style. It has no `::=`.

>D Inference Rule Layout
An inference rule block consists of:

1. Zero or more **premise** lines.
2. A **separator** line of three or more dashes, optionally carrying `(rule_name)` at the end.
3. A **conclusion** line — the type returned when all premises hold.

The rule name in `(rule_name)` must match the annotation on the corresponding production.
<

### Premise Forms

| Form | Meaning |
|------|---------|
| `Γ ⊢ x : T` | Infer type of bound node `x`; must equal `T` |
| `x ∈ Γ` | Bound name `x` must be in scope |
| `Γ[name:τ] ⊢ body : ?R` | Extend context with `name:τ`, then infer type of `body` |

### Type Expressions

Types in conclusions and premises are written as atoms (`'Int'`), arrows (`τ → ?R`), or unification variables (`?R`). A `?`-prefixed name is a fresh meta-variable unified during type checking.

## Example

The `fun.auf` grammar for simply-typed lambda calculus illustrates all forms:

```
// Types
Type(ty-arrow) ::= '(' Type[a] '->' Type[b] ')'
Type(ty-base)  ::= /[A-Z][a-zA-Z0-9]*/[t]

// Terms
Term(lam)  ::= 'λ' /[a-z]+/[x] ':' Type[τ] '.' Term[e]
Term(app)  ::= Term[f] Term[arg]
Term(var)  ::= /[a-z]+/[x]
Term(bool) ::= 'true' | 'false'

// Typing rules

x ∈ Γ
--------------  (var)
Γ(x)

Γ[x:τ] ⊢ e : ?R
--------------  (lam)
τ → ?R

Γ ⊢ f : ?A → ?R
Γ ⊢ arg : ?A
--------------  (app)
?R
```

## Loading Pipeline

`Grammar::load` processes a file in one pass:

1. Split on blank lines; strip comment lines.
2. For each production block: parse LHS (name + optional rule name), parse RHS by splitting on `|` (respecting quoted strings and `/regex/` delimiters), construct `Production` entries. Register all `'literal'` values with `add_special_token`.
3. For each inference rule block: locate the dashes separator, extract premises, conclusion, and rule name; construct a `TypingRule`.
4. Set the start symbol to the last-seen LHS name.
5. Call `rebuild_bindings` — compiles binding annotations into grammar-path references for the typing engine.
6. Call `prepare_tokenizer` — initializes the `Tokenizer` with all collected special tokens.

>W ambiguous rule names
If two production blocks declare the same `(rule_name)`, only the last one survives — `grammar.add_typing_rule` replaces by name. Keep rule names unique.
<
