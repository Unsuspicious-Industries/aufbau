# Grammar Specification Language

Aufbau grammars live in `.auf` files and define both a concrete syntax (productions)
and a constraint domain (typing rules) for context-dependent parsing. The two halves
are separated by blank lines.



## Quick Tour

```
// Productions: EBNF with bindings
Identifier ::= /[A-Za-z_][A-Za-z0-9]*/
Variable(var) ::= Identifier[x]
Expression ::= Lambda | Application | Variable

// The type fragment: types are part of the grammar; the * sigil is its kind
Type* ::= Identifier | Type '->' Type | '(' Type ')'

Lambda(lambda) ::= 'λ' Identifier[a] ':' Type[τ] '.' Expression[e]
Application(app) ::= Expression[l] Expression[r]

// Typing rules: inference rule notation
x ∈ Γ
----------- (var)
Γ(x)

Γ[a:τ] ⊢ e : ?B
--------------------------- (lambda)
τ -> ?B
```

- `::=` introduces a production. Use `|` for alternatives.
- `Name[binding]` binds a sub-expression to a name for use in rules.
- `'literal'` matches a literal token. `/regex/` matches a regex.
- The parenthesized slot on the LHS attaches semantics; a `*` sigil on the
  name (`Type*`) designates the type fragment (see below).
- A block without `::=` is a typing rule or a rewrite rule.
- Blank lines separate blocks.



## Productions

### Syntax

```
Nonterminal(annotation) ::= symbol symbol ... | alternative ...
```

The LHS is the nonterminal name, optionally carrying one of two markers:

| Form | Meaning |
|------|---------|
| `Name(rule)` | This node carries the typing rule `rule` |
| `Name(label)` with no rule `label` | A labeled type constructor: the label keeps the node's structure (e.g. `ListType(list)`) |
| `Name*` | The type fragment: terms of this nonterminal are the types (the sigil is the kind `*`) |

The RHS is a sequence of symbols. Multiple alternatives are separated by `|`;
continuation lines (starting with `|`) are supported.

### Symbols

| Syntax | Meaning | Example |
|--------|---------|---------|
| `Name` | Nonterminal reference | `Expression` |
| `Name[b]` | Nonterminal with binding | `Expression[l]` |
| `'text'` | Literal token | `'λ'`, `'('` |
| `/regex/` | Regex terminal | `/[0-9]+/` |
| `ε` | Epsilon (empty) | `ε` |

Bindings (`[b]`) attach a name to the subtree so the typing rule can reference
its evidence: bare `b` (or `typeof(b)`) is its type, `Γ(b)` looks its text up
in the context.

Regex terminals must be valid Rust `regex` crate patterns. Literal tokens
must not contain the delimiter characters (space, `|`, `[`, `]`).

### Nonterminal ordering and start symbol

Nonterminals are declared in the order they appear. The **last** nonterminal
with productions is the start symbol (unless explicitly set via
`grammar.with_start`).

### The type fragment

Types are not a separate language: they are terms of the grammar's own type
fragment, the nonterminal marked `Name*`. Every type expression appearing in
a rule (`?A -> ?B`, `?A list`, `'Int'`) is parsed by the grammar at that sort,
so the type constructors are exactly the productions the grammar writes.
A grammar that marks no type fragment falls back to trying every nonterminal, which
is slower and admits accidental cross-category ambiguity; declare the fragment.

Loading rejects a rule pattern with no unique parse at the type sort, rather
than silently weakening it to `⊤`.



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

There are exactly three judgment forms, and one semantic operation behind
all of them: unification of type terms.

| Form | Judgment | Meaning |
|------|----------|---------|
| `x ∈ Γ` | Membership | Binding `x`'s text is in the context |
| `Γ ⊢ e : τ` | Ascription | `e`'s type unifies with `τ` |
| `τ₁ = τ₂` | Equation | The two type expressions unify |

An ascription's context may carry extensions (`Γ[x:τ] ⊢ e : σ`, multiple as
`Γ[x:τ₁][y:τ₂]`): the judgment holds under `Γ` extended for `e`'s subtree
only. A setting is premise-local: it never leaks to sibling premises. There
is exactly one ambient context; `Γ` is notation, not a name. Anything else
(a fourth judgment form, a subtyping `⊆`) is a load error.

### Conclusions

| Form | Meaning |
|------|---------|
| `τ` | The node's type |
| `Γ ⊢ τ` | The node's type under context |
| `Γ → Γ[x:τ] ⊢ σ` | Right-bound effect: later siblings see `Γ[x:τ]`; the node's type is `σ` |

Axioms (rules with no premises) just have the conclusion below the line.
Effects export only from exact (closed, unextendable) nodes.

### Type expressions

A type expression is a sequence of atoms; everything between atoms is literal
type syntax handed to the grammar. There is no built-in arrow, product, or
union: `?A -> ?B` means whatever the type fragment's productions make of the
text `->` between the two holes.

| Atom | Meaning |
|------|---------|
| `'Int'` | Literal type text |
| `?A` | A hole: a unification variable |
| `b`, `typeof(b)` | The type of the child bound to `b` |
| `Γ(x)` | The type bound to `x`'s text in the context |
| `inst(x)` | `Γ(x)` with fresh variables: a polymorphic scheme instantiated at this use |
| `⊤`, `Any` | The unconstrained type (top) |
| `∅`, `None` | The contradictory type (bottom) |

A bare identifier is a binding reference exactly when it names one of the
rule's declared bindings; otherwise it is object-grammar text (a type keyword
such as `list` in `?A list`).

### Holes and unification

One semantic operation discharges every premise: unification of type terms.
A hole `?A` is a unification variable; writing the same hole in two positions
is the one and only way to require two types to be equal. Each rule's holes
are renamed fresh per node, so `?A` in two different `app` nodes never
collides; a binding discovered for a hole anywhere in the tree reaches every
other use of that hole. `⊤` unifies with everything and binds nothing; `∅`
unifies with nothing.

Context lookups (`Γ(x)`, bare refs) share metavariable identity: an unknown
stays one unknown across all its uses. Only `inst(x)` freshens, which is what
makes a context entry behave as a polymorphic scheme.

### Rewrite rules (normalization theory)

A block of `lhs ⇝ rhs` lines (ASCII `~>` also works) declares oriented type
rewrites. Both sides are type expressions over the type fragment; `?A` marks a
metavariable. Types are compared modulo the rewrites: unification normalizes
both sides first, so `Bool` and `Unit + Unit` are one type under

```
Bool ⇝ Unit + Unit
```

The theory must be confluent and terminating; that is the grammar author's
obligation. Loading rejects a rewrite whose sides do not parse at the type
sort or whose right side invents a variable absent from the left.



## Completeness

Pruning is sound unconditionally: a `dead` verdict means no continuation
exists. The converse — every `live` prefix has a continuation — depends on
the grammar, and every grammar carries its own certificate
(`completeness()` in the Python and OCaml surfaces):

| Class | Guarantee |
|-------|-----------|
| `syntactic` | No typing rules. Liveness is Earley liveness: live ⇔ realizable. |
| `inhabited` | Every sort an ascription can constrain has a *universal inhabitant*: live ⇒ realizable. |
| `sound` | A live prefix may demand a type the language cannot inhabit at the listed sorts. |

A universal inhabitant is a closed derivation whose type is a bare hole, so
it inhabits every demanded type — OCaml's `assert false`. The certificate is
a least fixpoint: an axiom concluding `?A` over derivable syntax certifies
its sort; transparent alternation propagates it; a rule all of whose
premises are ascriptions of universal children and whose conclusion is a
bare hole propagates it further. The condition is sufficient, not
necessary: `sound` grammars may still be complete in fact (STLC's
`λf:A→B. f(` is the canonical live-but-uninhabited prefix; adding a
diverging term moves the grammar into `inhabited`).



## Transparent Nonterminals

A nonterminal is **transparent** if it carries no rule, has exactly one
nonterminal child, and no bound terminal:

```
Wrapped ::= '(' Expression ')'
```

A transparent node has no semantics of its own: it inherits its child's
evidence and effects, and collapses out of type terms (so `(Int)` and `Int`
are one type). To keep the structure of a unary type constructor, label it:
`ListType(list) ::= Factor 'list'` stays a `list` node rather than collapsing
to `Factor`.



## Full Example: Simply Typed Lambda Calculus

```
// Lexical form
Identifier ::= /[A-Za-z_][A-Za-z0-9_]*/
TypeName ::= /[A-Za-z0-9_]+/

// Variables
Variable(var) ::= Identifier[x]

// Types
AtomicType ::= TypeName | '(' Type ')'
FunctionType ::= AtomicType '->' Type
Type* ::= AtomicType | FunctionType

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
τ -> ?B

Γ ⊢ l : ?A -> ?B, Γ ⊢ r : ?A
--------------------------------- (app)
?B
```



## Loading Pipeline

```
.auf source
  │
  ├─ Blocks with "::="  ──→ load_ebnf()            ──→ productions, annotations
  │
  └─ Other blocks       ──→ typing::loader::load() ──→ rule table, rewrites
  │
  ▼
typing::loader::check(): every rule pattern parses uniquely at the type
sort; every rewrite parses and invents no variables. Failures are load errors.
```



## Debugging

Use `aufbau check -s grammar.auf < program.txt` to run the parser and see the
result tree. Add `--ast` for the full tree print and `--all` to see ambiguous
candidates.

Use `aufbau chart -s grammar.auf` to load and dump the parsed grammar as an
S-node/ParseTree which you can [visualize online](https://unsuspicious.org/proposition-7/chart).
