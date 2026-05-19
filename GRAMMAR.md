# Grammar Specification Language

Aufbau grammars live in `.auf` files and define both a concrete syntax (productions)
and a constraint domain (typing rules) for context-dependent parsing. The two halves
are separated by blank lines.



## Quick Tour

```
// Productions — EBNF with bindings
Identifier ::= /[A-Za-z_][A-Za-z0-9]*/
Variable(var) ::= Identifier[x]
Expression ::= Lambda | Application | Variable

Lambda(lambda) ::= 'λ' Identifier[a] ':' Type[τ] '.' Expression[e]
Application(app) ::= Expression[l] Expression[r]

// Typing Rules — inference rule notation
x ∈ Γ
----------- (var)
Γ(x)

Γ[a:τ] ⊢ e : ?B
--------------------------- (lambda)
τ → ?B
```

- `::=` introduces a production. Use `|` for alternatives.
- `name[binding]` binds a sub-expression to a name for use in rules.
- `'literal'` matches a literal token. `/regex/` matches a regex.
- A block without `::=` is treated as an inference rule.
- Blank lines separate blocks.



## Productions

### Syntax

```
Nonterminal(rule_name) ::= symbol symbol ... | alternative ...
```

- **LHS**: The nonterminal name, optionally followed by `(rule_name)` in
  parentheses to attach a typing rule.
- **RHS**: A sequence of symbols. Multiple alternatives are separated by `|`.
  Continuation lines (starting with `|`) are supported for readability.

### Symbols

| Syntax | Meaning | Example |
|--------|---------|---------|
| `Name` | Nonterminal reference | `Expression` |
| `Name[b]` | Nonterminal with binding | `Expression[l]` |
| `'text'` | Literal token | `'λ'`, `'('` |
| `/regex/` | Regex terminal | `/[0-9]+/` |
| `ε` | Epsilon (empty) | `ε` |

Bindings (`[b]`) attach a name to the subtree so the typing rule can reference
its evidence via `typeof(b)`.

Regex terminals must be valid Rust `regex` crate patterns. Literal tokens
must not contain the delimiter characters (space, `|`, `[`, `]`).

### Nonterminal ordering and start symbol

Nonterminals are declared in the order they appear. The **last** nonterminal
with productions is the start symbol (unless explicitly set via
`grammar.set_start`).



## Typing Rules

Rules use inference rule notation. A rule block looks like:

```
premise_1, premise_2, ... , premise_n
---------------------------------------------- (rule_name)
conclusion
```

The dashed line must contain at least three dashes. The rule name (in
parentheses) may appear anywhere on the dashed line or at the end of the
conclusion line. It must be non-empty and match `^[A-Za-z_][A-Za-z0-9_]*$`.

### Premises

| Form | Judgment | Meaning |
|------|----------|---------|
| `x ∈ Γ` | Membership | Variable `x` is in the context |
| `Γ ⊢ e : τ` | Typing ascription | Term `e` has type `τ` under context `Γ` |
| `Γ[x:τ] ⊢ e : σ` | Extended ascription | Term `e` has type `σ` under `Γ` extended with `x:τ` |
| `τ₁ = τ₂` | Equality constraint | Types must be equal |
| `τ₁ ⊆ τ₂` | Inclusion constraint | `τ₁` is a subtype of `τ₂` |
| `τ₁ | τ₂` | Union type | Either `τ₁` or `τ₂` |

Multiple extensions on a context (`Γ[x:τ₁][y:τ₂] ⊢ e : σ`) are supported.

A setting-only premise (bare `Γ[x:τ]` without `⊢`) declares a context
extension without a typing judgment.

### Conclusions

| Form | Meaning |
|------|---------|
| `τ` | The result type |
| `Γ ⊢ τ` | Result type under context |
| `Γ → Γ[x:τ] ⊢ σ` | Context transform: input `Γ`, output `Γ[x:τ]`, result `σ` |

Axioms (rules with no premises) just have the conclusion below the line.

### Meta variables

`?A`, `?B`, `?X` are meta variables that the compiler will resolve. They
desugar into fresh variables plus equality constraints:

```
Γ[a:τ] ⊢ e : ?B         ← before compilation (user sees this)
==========================
typeof(e) = _0           ← after compilation (eliminated metas)
_0 = _1 → _2
========================================
_2
```

When the same meta name appears in multiple positions, the compiler generates
equality constraints linking the corresponding fresh variables. This is the
key mechanism for pattern-based constraint propagation — you write `?A`
wherever you expect the same type, and the compiler wires it up.

### Type expressions

| Syntax | TypeExpr | Meaning |
|--------|----------|---------|
| `'Int'`, `'Bool'` | `Lit("Int")` | Named type literal |
| `τ₁ → τ₂` | `Arrow(τ₁, τ₂)` | Function type (right-associative) |
| `τ₁ = τ₂` | `Equality` | Equality constraint |
| `⊤` | `Any` | Top type / any |
| `∅`, `⊥` | `None` | Bottom type / contradiction |
| `?A` | `Meta("A")` | Meta variable (compiled away) |
| `Γ(x)` | `ContextExt("x")` | Context variable lookup |
| `typeof(b)` | `TypeOf("b")` | Binding evidence lookup |
| `¬τ`, `!τ` | `Not(τ)` | Negation |

Arrow (`→` / `->` / `=>`) is right-associative: `A → B → C` parses as
`A → (B → C)`.

---

## Transparent Nonterminals and Bridge Rules

A nonterminal is **transparent** if every alternative has exactly one
nonterminal child and no bound terminal symbols. For example:

```
Wrapped ::= '(' Expression[e] ')'
```

When no rule is explicitly assigned to a transparent nonterminal,
`fill_and_compile` generates a **bridge rule** that passes through the child's
type:

```
__br_Wrapped ::= '(' Expression[__Wrapped_0] ')'

typeof(__Wrapped_0) : ?__Wrapped_0
--------------------------------- (__br_Wrapped)
typeof(__Wrapped_0)
```

This means the wrapper inherits the type of its inner expression
automatically.

To opt out, assign an explicit rule name:

```
Wrapped(foo) ::= '(' Expression[e] ')'
```

---

## Full Example: Simply Typed Lambda Calculus

```
// Lexical form
Identifier ::= /[A-Za-z_][A-Za-z0-9_]*/
TypeName ::= /[A-Za-z0-9_τ₁₂₃₄₅₆₇₈₉₀]+/

// Variables
Variable(var) ::= Identifier[x]

// Types
BaseType ::= TypeName | '(' Type ')'
AtomicType ::= BaseType | '(' Type ')'
FunctionType ::= AtomicType '->' Type
Type ::= AtomicType | FunctionType

// Terms
Lambda(lambda) ::= 'λ' Identifier[a] ':' Type[τ] '.' Expression[e]
AtomicExpression ::= Variable | '(' Expression ')' | Lambda
Application(app) ::= Expression[l] AtomicExpression[r]
Expression ::= AtomicExpression | Application

// Typing rules
x ∈ Γ
----------- (var)
Γ(x)

Γ[a:τ] ⊢ e : ?B
--------------------------- (lambda)
τ → ?B

Γ ⊢ l : ?A → ?B, Γ ⊢ r : ?A
--------------------------------- (app)
?B
```



## Loading Pipeline

```
.auf source
  │
  ├─ Blocks with "::="  ──→ load_ebnf()  ──→ Productions + NT→rule-name map
  │
  └─ Other blocks  ──→ ConstraintLoader::load()  ──→ Rule table by rule name
  │
  ▼
postprocess (fill_and_compile)  ──→ auto-bridge rules + compile metas
  │
  ▼
grammar ready for use
```



## Debugging

Use `aufbau check -s grammar.auf < program.txt` to run the parser and see the
result tree. Add `--ast` for the full tree print and `--all` to see ambiguous
candidates.

Use `aufbau chart -s grammar.auf` to load and dump the parsed grammar as an
S-node/ParseTree which you can [visualize online](https://unsuspicious.org/proposition-7/chart).
