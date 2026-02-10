# P7 — Complete Context Dump

## 0. What Is This

P7 is a **type-safe constrained generation** engine. Given a `.spec` file defining a language's syntax (CFG) and semantics (inference-rule typing), it can:

1. **Partially parse** any prefix into a forest of partial ASTs
2. **Type-check** trees against declarative typing rules
3. **Complete** partial inputs to well-typed programs via BFS/beam search
4. **Validate** prefix soundness: every prefix of a completable string is itself completable

Primary application: constraining LLM token generation to only produce well-typed code.

---

## 1. Theory

### 1.1 Formal Language & Completability

**Alphabet** $\Sigma$, **Kleene closure** $\Sigma^*$, **language** $L \subset \Sigma^*$.

**Completability**: $s$ is completable in $L$ iff $\exists s' \in \Sigma^* : ss' \in L$.

**Completability set**: $\mathcal{C}_L(s) = \{a \in \Sigma : \exists s'. \; sas' \in L\}$

**Equivalence**: $s$ completable $\iff$ $\mathcal{C}_L(s) \neq \emptyset$

**Edge case (nullable)**: $s \in L \implies \mathcal{C}_L(s) = \{\epsilon\}$

### 1.2 Grammar

$G = (N, T, P, S, \Theta, A)$

| Symbol | Meaning |
|--------|---------|
| $N$ | Non-terminals |
| $T$ | Terminals (regex or literal) |
| $P$ | Productions with binding annotations |
| $S \in N$ | Start symbol (last declared NT) |
| $\Theta: P \to \theta \cup \{\varepsilon\}$ | Optional typing rule per production |
| $A: N \to P^*$ | Alternative productions per NT |

**Production**: $\alpha_0[b_0] \; \alpha_1[b_1] \cdots \alpha_n[b_n]$ where $\alpha_k \in T \cup N$, $b_k \in \mathcal{B} \cup \{\varepsilon\}$.

### 1.3 Partial Trees & Forests

**Partial tree**: $t = (V, E, \lambda, \pi, \alpha, \text{root})$

- $V$ — nodes, $E \subseteq V \times \mathbb{N} \times V$ — position-indexed edges
- $\lambda: V \to N \cup T$ — label, $\pi: V \to P$ — production, $\alpha: V \to \mathbb{N}$ — alternative index
- A tree that consumed all input but may have unsatisfied symbols

**Forest**: $\mathcal{F}(s) = \{t_1, \ldots, t_n\}$ — all valid partial trees for input $s$, ordered by alternative definition order.

**Tree path**: $\mathcal{P} = \mathbb{N}^*$ (de Bruijn-style). $\text{path}(v) = \text{path}(\text{parent}(v)) \cdot i$. Paths are **injective**.

**Completeness**: terminal complete iff matched full token; NT complete iff all symbols satisfied. Forest complete iff $\exists t_k$ complete.

**Frontier**: path to rightmost incomplete node. Unique in partial trees. Absent in complete trees.

**Frontier monotonicity**: $\text{front}(\mathcal{F}(s \cdot t)) > \text{front}(\mathcal{F}(s))$ — extending input always advances the frontier.

### 1.4 Binding Resolution

**Binding map** (computed at grammar-load time, input-independent):
$$\beta: (\mathcal{B} \times \text{Rule}) \to \mathcal{P}^*$$

**Grammar path**: sequence of steps $i@a$ where $i$ = child index, $a$ = expected alternative. Navigate from rule-carrying production to bound symbol.

**Regular grammar paths**: for recursive structures, grammar paths become regex over $\Sigma = \mathbb{N} \times \mathbb{N}$. E.g., binding through nested parens: $3@0 \cdot (0@1 \cdot 1@0)^* \cdot 0@0 \cdot 0$.

**Construction**: BFS from each typing-rule production, tracking (child-index, alt-index) pairs. Acyclic paths are finite sequences; cyclic paths use regular expressions. `MAX_RECURSION_DEPTH = 16`.

**Runtime resolution**: walk AST following grammar path; at each step verify alternative matches. Results: `Match(node)` | `Partial(node)` | `AlternativeMismatch` | `MissingNode`.

**Theorems**:
1. $\beta(b, p)$ uniquely determined by grammar (input-independent)
2. $|\beta(b, p)| > 1 \implies$ tuple-typed binding `(b...)`

### 1.5 Type Language

$$\tau ::= \text{atom} \mid \tau_1 \to \tau_2 \mid \neg\tau \mid \top \mid \bot \mid \Gamma(x) \mid \text{'raw'}$$

- **Atom**: type variables resolved from grammar bindings
- **Arrow** $\tau_1 \to \tau_2$: function types (right-assoc)
- **Meta** $?A, ?B$: inference variables for pattern matching in rules
- **Raw** `'Int'`: concrete literal types
- **⊤** (Any): top type — accepts all; **∅** (None): bottom type — rejects all
- **Γ(x)**: context lookup

### 1.6 Typing Rules

Standard inference-rule format:

$$\frac{\text{premise}_1, \quad \text{premise}_2, \quad \ldots}{\text{conclusion}} \; (\text{rule\_name})$$

**Premise forms**:

| Form | Meaning |
|------|---------|
| $\Gamma \vdash e : \tau$ | Ascription — $e$ has type $\tau$ |
| $x \in \Gamma$ | Membership — $x$ bound in context |
| $\tau_1 = \tau_2$ | Equality constraint |
| $\tau_1 \subseteq \tau_2$ | Subtype constraint |
| $\Gamma[x:\tau]$ | Local context extension (premise-scoped) |

**Conclusion forms**:

| Form | Meaning |
|------|---------|
| $\tau$ | Bare type result |
| $\Gamma(x)$ | Context lookup — return type of $x$ |
| $\Gamma \to \Gamma[x:\tau] \vdash \sigma$ | Context transform — propagates binding upward |

**Critical scoping distinction**: premise extensions $\Gamma[x:\tau] \vdash \ldots$ are **local** to that premise. Conclusion extensions $\Gamma \to \Gamma[x:\tau]$ **propagate to parent/siblings**.

### 1.7 Unification (Formal)

$$\text{UNIFY}(\tau_1, \tau_2, \sigma) = \begin{cases}
\sigma; \;\text{true} & \tau_1 = \tau_2 \\
\sigma[?X := \tau_2]; \;\text{true} & \tau_1 = ?X, \; ?X \notin \text{FV}(\tau_2) \\
\sigma[?X := \tau_1]; \;\text{true} & \tau_2 = ?X, \; ?X \notin \text{FV}(\tau_1) \\
\text{UNIFY}(l_1,l_2,\sigma) \land \text{UNIFY}(r_1,r_2,\sigma) & \tau_1 = l_1 \to r_1, \; \tau_2 = l_2 \to r_2 \\
\text{true} & \tau_1 = \top \lor \tau_2 = \top \\
\text{false} & \text{otherwise}
\end{cases}$$

### 1.8 Type-Constrained Completion

$$S'_{\text{typed}} = \{s' \in S' \mid \text{well-typed}(\Psi_L(s \cdot s'))\}$$

Forest well-typed iff $\exists t_k$ passing type evaluation.

**FIRST sets** for completion tokens:
$$\text{FIRST}(\alpha) = \begin{cases} \{\alpha\} & \alpha \in T \\ \bigcup_{p \in A(\alpha)} \text{FIRST}(p_0) & \alpha \in N \end{cases}$$

**Prefix soundness**: $\forall$ prefix $p$ of completable string $s$: $p$ is also completable.

### 1.9 Subtyping

- $\bot \subseteq \tau$ (None ⊆ everything)
- $\tau \subseteq \top$ (everything ⊆ Any)
- $\tau \subseteq \tau$ (reflexivity)
- $\tau_1 \to \tau_2 \subseteq \sigma_1 \to \sigma_2 \iff \sigma_1 \subseteq \tau_1 \land \tau_2 \subseteq \sigma_2$ (contravariant domain)

---

## 2. Spec Format

### 2.1 Productions

```
Name(rule_name) ::= Symbol₁[bind] Symbol₂ ... | Alt₂ | ...
```

- **Terminals**: `'λ'` (literal), `"/[a-z]+/"` (regex)
- **Bindings**: `Identifier[x]` — attaches name `x` for typing rules
- **Rule annotation**: `Lambda(lambda)` — links to `(lambda)` typing rule
- **Epsilon**: `ε` alone — nullable production
- **Alternatives**: `|`-separated, can span multiple lines
- **No EBNF** (`*`, `+`, `?`): use recursive productions instead

### 2.2 Typing Rules

```
premise₁, premise₂
------------------- (rule_name)
conclusion
```

Separated from grammar by blank lines. Rule name must match a production annotation.

### 2.3 Type Expressions in Rules

| Syntax | Type |
|--------|------|
| `τ`, `Int` | Atom (resolved from binding) |
| `'Int'`, `'void'` | Raw (concrete literal) |
| `?A`, `?B` | Meta/inference variable |
| `τ₁ → τ₂`, `τ₁ -> τ₂` | Arrow (function, right-assoc) |
| `¬τ`, `!τ` | Negation |
| `Γ(x)` | Context lookup |
| `⊤` | Top/Any |
| `∅` | Bottom/None |

Precedence: `¬` > `→`. Parens for grouping.

---

## 3. Existing Specs

### 3.1 STLC (`examples/stlc.spec`)

```
Identifier ::= /[A-Za-z_][A-Za-z0-9]*/
Variable(var) ::= Identifier[x]
TypeName ::= /[A-Za-z0-9_τ₁₂₃₄₅₆₇₈₉₀]+/
BaseType ::= TypeName | '(' Type ')'
AtomicType ::= BaseType | '(' Type ')'
FunctionType ::= AtomicType '->' Type
Type ::= AtomicType | FunctionType
Lambda(lambda) ::= 'λ' Identifier[a] ':' Type[τ] '.' Expression[e]
AtomicExpression ::= Variable | '(' Expression ')' | Lambda
Application(app) ::= AtomicExpression[l] Expression[r]
Expression ::= AtomicExpression | Application

x ∈ Γ            Γ[a:τ] ⊢ e : ?B           Γ ⊢ r : ?A → ?B, Γ ⊢ l : ?A
----- (var)       --------------- (lambda)   ----------------------------- (app)
Γ(x)             τ → ?B                     ?B
```

**Note on `app` bindings**: `[l]` = `AtomicExpression` (left/function), `[r]` = `Expression` (right/argument). The typing rule checks `r : ?A → ?B` — this means the **argument** position has function type, which looks backwards. Either the naming is intentionally swapped or this is a bug to investigate.

### 3.2 IMP (`examples/imp.spec`)

```
Identifier ::= /[a-z]+/
TypeName ::= /[A-Z][a-z]*/
Integer(int) ::= /[0-9]+/[n]
Variable(var) ::= Identifier[x]
Value ::= Integer | Variable
Assignment(assign) ::= Identifier[x] ':' TypeName[τ] '=' Value[v] ';'
Operator ::= '+' | '-' | '*' | '/'
Operation(op) ::= Value[lhs] Operator[op] Value[rhs] ';'
Expression(expr) ::= Assignment | Operation
Program(prog) ::= Expression[a] Program[b] | ε

x ∈ Γ         Γ ⊢ v : τ              Γ ⊢ lhs : ?A, Γ ⊢ rhs : ?A
----- (var)   ------------- (assign)  ----------------------------- (op)
Γ(x)          Γ → Γ[x:τ] ⊢ ∅        ?A

              ----------- (int)       Γ ⊢ a : ?A, Γ ⊢ b : ?B
              'Int'                   ------------------------ (prog)
                                      ?A -> ?B
```

Context transform in `assign`: `Γ → Γ[x:τ]` propagates binding to subsequent statements.

---

## 4. Architecture

```
src/
├── logic/
│   ├── grammar/          # G = (N,T,P,S,Θ,A) representation
│   │   ├── mod.rs        # Symbol, Production, Grammar
│   │   ├── load.rs       # Grammar::load() — .spec → Grammar
│   │   ├── tokenizer.rs  # Input segmentation by terminals
│   │   └── utils.rs      # BNF + inference rule parsing
│   │
│   ├── partial/          # Ψ_L: Σ* → Forest
│   │   ├── parse.rs      # Parser::partial() — Earley-style
│   │   ├── structure.rs  # Node, NonTerminal, Terminal
│   │   └── completion.rs # FIRST-set completion tokens
│   │
│   ├── typing/           # Θ evaluation engine
│   │   ├── mod.rs        # Type enum (Atom|Meta|Raw|Arrow|Not|Any|None|...)
│   │   ├── core.rs       # Context(Γ), TreeStatus, TreeRef, TreePath
│   │   ├── eval.rs       # ★ check_tree, apply_rule, check_premise (925L)
│   │   ├── rule.rs       # TypingRule, Premise, Conclusion parsing (515L)
│   │   ├── syntax.rs     # Type::parse, Type::parse_partial (480L)
│   │   ├── ops.rs        # equal() → Option<bool>, subtype() → bool
│   │   ├── tree.rs       # PartialAST → TypedAST via type cache
│   │   ├── binding.rs    # resolve_bindings() — runtime β resolution
│   │   └── tests/        # lc, imp, contradiction, redeclaration, partial
│   │
│   ├── binding/          # β: (B × Rule) → P* construction
│   │   ├── grammar.rs    # BindingMap, GrammarPath, PathStep, build_binding_map
│   │   └── eval.rs       # resolve_binding_path() — AST traversal
│   │
│   └── beam/             # Beam search completer
│       ├── search.rs     # beam_complete()
│       ├── config.rs     # BeamConfig
│       └── state.rs      # BeamState, BeamResult
│
├── regex/                # Brzozowski derivative regex engine
│   ├── dfa.rs/nfa.rs     # Compilation
│   ├── ops.rs            # derivative(c), nullable(), valids()
│   └── parse.rs          # Regex parser
│
├── validation/           # Test/proof infrastructure
│   ├── completability.rs # complete(), sound_complete() — BFS completion
│   ├── completable/      # Slow thorough tests (STLC, IMP, arithmetic, weird)
│   └── parseable/        # Fast O(n²) prefix parseability tests
│
└── viz/                  # Debug visualization (graph server)
```

### Key Data Flow

```
.spec text → Grammar::load() → Grammar{productions, typing_rules, binding_map, tokenizer}
                                    │
input string → Parser::partial() → PartialAST{roots: Vec<NonTerminal>}
                                    │
                    ┌───────────────┼───────────────────┐
                    ▼               ▼                   ▼
              .completions()   .typed(g)          beam_complete()
              → next tokens    → TypedAST          → BeamResult
                               (filters by         (search for
                                check_tree)         well-typed completion)
```

### Type Evaluation Pipeline

```
check_tree(root, grammar)
  └→ check_node(tref, grammar, ctx, depth, type_cache)
       ├─ Terminal → Valid(Any) | Partial(Any)
       └─ NonTerminal → check_nt()
            ├─ Has rule → apply_rule()
            │   ├─ resolve_bindings(nt, rule, grammar) → Bindings{full, partial}
            │   ├─ for each premise: check_premise()
            │   │   ├─ build_ctx_extension() — Γ[x:τ] local scope
            │   │   ├─ Ascription: check_node() on bound child → actual type
            │   │   │   ├─ has_meta? → solve_meta() or set_meta() into map
            │   │   │   └─ equal(actual, expected) → Some(true|false) | None
            │   │   ├─ Membership: lookup in ctx
            │   │   └─ Operation: solve_meta both sides, equal/subtype
            │   ├─ eval_conclusion() → TreeStatus (resolve metas + bindings)
            │   └─ extract_context_transform() — Γ → Γ[x:τ] propagation
            └─ No rule → drill_only_child()
                 ├─ 1 NT child → recurse (transparent wrapper)
                 ├─ 0 NT children → Any
                 └─ 2+ NT children → Any (LOSSY!)
```

---

## 5. Key Types

| Type | Module | Role |
|------|--------|------|
| `Type` | `typing::mod` | `Atom\|Meta\|Raw\|Arrow\|Not\|Any\|None\|ContextCall\|Path\|PathOf\|Partial` |
| `Context` | `typing::core` | $\Gamma$: `bindings: HashMap<String,Type>` + `unresolved_bindings: HashMap<TreePath,Type>` |
| `TreeStatus` | `typing::core` | `Valid(Type)\|Partial(Type)\|Malformed\|TooDeep` |
| `TreeRef<'a>` | `typing::core` | Zero-copy ref into AST by root + path |
| `Bindings` | `typing::binding` | `full: HashMap<String,TreePath>`, `partial: HashMap<String,TreePath>` |
| `TypingRule` | `typing::rule` | `name + Vec<Premise> + Conclusion` |
| `Grammar` | `grammar::mod` | `productions + typing_rules + binding_map + tokenizer` |
| `BindingMap` | `binding::grammar` | `rule → {binding → [GrammarPath]}` |
| `GrammarPath` | `binding::grammar` | `Vec<PathStep{i,a}>` |
| `PartialAST` | `partial::structure` | `roots: Vec<NonTerminal>, input: String` |
| `NonTerminal` | `partial::structure` | `name, production, alternative_index, children: Vec<Node>` |
| `TypedAST` | `typing::tree` | Forest filtered to well-typed trees, with type annotations |

---

## 6. Meta Variable System (The Messy Part)

The `map: HashMap<String, Type>` is built incrementally across premises:

- **`has_meta(ty)`** — does type contain `Meta(_)`?
- **`set_meta(pattern, actual, map)`** — destructure pattern against actual, insert into map. `Arrow(?A,?B)` vs `Arrow(Int,Bool)` → `?A=Int, ?B=Bool`
- **`solve_meta(ty, map)`** — substitute all `Meta(name)` → `map[name]`. `Err` if unbound.
- **`solve_binding(tref, ty, bound)`** — resolve `Atom("τ")` → look up grammar path → get text → `Type::parse_raw()` → `Raw("Int")`

### Check Premise Ascription Flow

```
check_premise(Γ ⊢ term : expected_ty):
  1. Resolve `term` binding → Full(path) | Partial(path) | None
  2. If Full: check_node(child_at_path) → var_ty (actual type)
  3. If expected_ty has meta:
     a. Try solve_meta(expected_ty, map) → right_ty
        - If success: solve_binding, then equal(var_ty, right_ty)
        - If fail (unbound metas): set_meta(expected_ty, var_ty, map)
          → EXTENDS the map, then allow pass
  4. If no meta: solve_binding(expected_ty) → right_ty
  5. resolve_ctx_calls on both sides
  6. equal(var_ty, right_ty) → Some(true)=Ok, Some(false)=Fail, None=Partial
```

---

## 7. Equality & Subtyping Ops

**`equal(t1, t2) → Option<bool>`** — three-valued:
- `Raw(a) vs Raw(b)` → `Some(a==b)`
- `Arrow(l1,r1) vs Arrow(l2,r2)` → `Some(equal(l1,l2)? && equal(r1,r2)?)`
- `Any vs _` or `_ vs Any` → `Some(true)` ⚠️
- `None vs _` or `_ vs None` → `Some(false)`
- `ContextCall/Path/PathOf` vs anything → `None` (can't decide)

**`subtype(t1, t2) → bool`**: None ⊆ τ, τ ⊆ Any, reflexive, contravariant arrow.

---

## 8. Validation Framework

### 8.1 Parseable Tests (`validation::parseable`)

Fast O(n²): for input of length n, check all n+1 prefixes parse.

```rust
ParseTestCase::valid(desc, input)      // all prefixes must parse
ParseTestCase::invalid(desc, input)    // full input must NOT parse
ParseTestCase::type_error(desc, input) // parses but fails typing
ParseTestCase::structural(desc, input) // parses structurally only
```

### 8.2 Completable Tests (`validation::completable`)

Slow BFS: verify partial input completes to well-typed full AST.

```rust
TypedCompletionTestCase::new(desc, input, xfail)
    .with_depth(d)
    .with_context(vec![("x", "A")])
```

- `xfail=false` → `sound_complete()` (checks ALL prefixes completable)
- `xfail=true` → `complete()` (expects failure for full input)

### 8.3 Completion Algorithm

`complete(grammar, input, max_depth, ctx, max_states)`:
1. Parse input → PartialAST
2. BFS: compute valid completions (next tokens) per tree
3. Extend input with each token, re-parse, type-check
4. Complete + well-typed → `Success`
5. Exhausted → `Failure` | `StateOverflow` | `Invalid`

`sound_complete(input)` runs `complete()` on every prefix.

### 8.4 Test Commands

```bash
cargo test --lib validation::completable::stlc -- --nocapture
cargo test --lib validation::parseable::stlc -- --nocapture
cargo test --lib logic::typing::tests -- --nocapture
cargo test --lib validation::completable::imp -- --nocapture
```

---

## 9. Known Bugs & Smell Zones

### 9.1 `equal(Any, X) = Some(true)` — Over-permissive

Any is treated as **equal to** everything, not just **supertype of**. This means `equal(Any, Int→Bool)` returns `Some(true)`, allowing nonsensical type matches through. `Any` should only satisfy subtype checks, not equality.

### 9.2 `set_meta(Arrow(?A,?B), Any, map)` — Pollutes Map

Matching a structured meta pattern against `Any` sets `?A=Any, ?B=Any`. Combined with bug 9.1, this makes `?A→?B = Any→Any = anything`. Over-permissive cascade.

### 9.3 No Occurs Check in `set_meta`

Could create infinite types. The formal spec requires $?X \notin \text{FV}(\tau)$ but the implementation doesn't enforce it.

### 9.4 Premise Order Sensitivity

The `map` is built sequentially. A premise using `?A` must come AFTER the premise that binds `?A`. Rule premise order is **semantic**, not just syntactic.

### 9.5 Context Shadowing Blocked

`Context::extend()` returns `Err` on duplicate keys. This prevents legitimate variable shadowing like `λx:A.λx:B.x`. Meanwhile `Context::add()` silently overwrites — inconsistent.

### 9.6 `drill_only_child` Masks Errors

With 2+ NT children and no typing rule, returns `Any`. Type errors in children become invisible. Should probably be `Malformed` or require explicit rules.

### 9.7 `Partial` Type Unwrapping in `set_meta`

`set_meta` handles `PathOf(t, _)` and `Partial(t, _)` by unwrapping to inner type `t`. This loses the partial/path status. Could cause premature type decisions on incomplete trees.

### 9.8 STLC `app` Rule — Binding Name Swap?

Grammar: `Application(app) ::= AtomicExpression[l] Expression[r]` → `l`=function, `r`=argument.
Rule: `Γ ⊢ r : ?A → ?B, Γ ⊢ l : ?A` → `r` has function type.
This checks the **argument** for function type. Either intentional naming swap or genuine bug.

### 9.9 `solve_meta` on Unbound Meta Returns `Err`

When a meta variable hasn't been bound yet, `solve_meta` fails. The caller catches this and falls back to `set_meta`. This error-as-control-flow pattern is fragile and the branching logic in `check_premise` is convoluted.

---

## 10. Regex Engine

Custom **Brzozowski derivative** implementation:
- `Regex::derivative(c)` → regex after consuming char `c`
- `Regex::nullable()` → can match empty string?
- `Regex::valids()` → set of valid starting chars
- Used for tokenization and completion token generation
- Supports: literals, char classes, alternation, concat, Kleene star, empty

---

## 11. Python Bindings

`python/` — PyO3/maturin bindings exposing core engine to Python:
- `proposition_7/` package: `environment.py`, `grammars.py`, `inference.py`, `llm.py`, `sampler.py`
- Examples: `gpt2.py`, `phi.py` — constrained LLM decoding
- `src/lib.rs` — Rust FFI bridge

---

## 12. Spec Book

`spec/` — mdBook formal specification:
- `basic.md` — alphabet, Kleene closure, completability definitions
- `concepts/theory.md` — grammar definition, partial parser, completability
- `concepts/trees.md` — partial trees, forests, paths, frontiers, completeness
- `concepts/binding.md` — grammar paths, regular grammar paths, binding map construction, resolution
- `spec.md` — full .spec format BNF, typing rules, context operations
- Custom preprocessors: proofs, status tracking
