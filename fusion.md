# Fusion Parser System

## Overview

The Fusion parser is a **typed-by-construction incremental parser** that replaces the old SPPF-based parser. It performs parsing and type checking simultaneously, building a typed AST in an append-only arena as it goes.

The system lives in `src/logic/fusion/` and consists of these modules:

| Module      | Purpose |
|-------------|---------|
| `parser.rs` | Core recursive-descent parser (`TypedParser<T>`) |
| `arena.rs`  | Append-only arena for nodes, alternatives, and paths |
| `typing.rs` | `TypingRuntime` trait and state types |
| `runtime.rs`| `RuleRuntime` — grammar-based type checking implementation |
| `binding.rs`| Binding creation, filtering, and merging |
| `ast.rs`    | `FusionAST` / `FusionNode` — zero-materialization typed AST |
| `advance.rs`| Incremental parsing — extend existing roots with new tokens |
| `meta.rs`   | `MetaTypedParser` — iterative deepening wrapper |
| `synth.rs`  | `Synthesizer` — high-level incremental parsing interface |
| `state.rs`  | `TypedPrefixState` / `TypedPrefixError` — result types |
| `display.rs`| Debug printing |

---

## Architecture

### `TypedParser<T>`

The core parser, parameterized by a `TypingRuntime`:

```rust
pub struct TypedParser<T> {
    grammar: Grammar,
    typing: T,
    arena: ParseArena,
    frontier: Vec<FrontierItem>,
    prod_offsets: Vec<usize>,
    max_depth: u16,
    depth_failures: u32,
}
```

- **`grammar`** — the context-free grammar with typing rule annotations
- **`typing`** — the typing runtime (usually `RuleRuntime`)
- **`arena`** — append-only storage for all parse nodes
- **`frontier`** — reserved for future incremental use (currently unused)
- **`prod_offsets`** — precomputed offsets mapping `(nt, alt)` → `ProdId`
- **`max_depth`** — recursion depth budget
- **`depth_failures`** — count of branches rejected by depth limit

### `ParseArena`

An append-only arena using `RefCell` for interior mutability:

```rust
pub struct ParseArena {
    nodes: RefCell<Vec<ArenaNode>>,
    alts: RefCell<Vec<PackedAlt>>,
    paths: RefCell<Vec<PathNode>>,
}
```

Three storage vectors:

- **`nodes`** — `ArenaNode` entries, each representing a parsed non-terminal
- **`alts`** — `PackedAlt` entries (production + children), stored contiguously
- **`paths`** — `PathNode` entries forming a parent-linked tree identifying grammar positions

Each `ArenaNode` carries:

```rust
pub struct ArenaNode {
    pub nt: NtId,
    pub span: Span,              // token range [start, end)
    pub status: NodeStatus,      // Complete or Partial
    pub ty: TypeStatus,          // Valid(TypeId) or Partial(TypeId)
    pub env_in: CtxId,           // typing context on entry
    pub env_out: CtxId,          // typing context on exit
    pub bindings: Vec<BindingValue>,
    pub alts: AltRange,          // index into the alts vector
}
```

Nodes are identified by `NodeId` (index into the nodes vector). Paths are identified by `PathId` (index into the paths vector). Both are stable across arena mutations because the arena only appends.

### `TypingRuntime` Trait

The interface between parsing and type checking:

```rust
pub trait TypingRuntime {
    fn enter_nonterminal(&self, nt: NtId, summary: &TypingContextSummary) -> Vec<TypingState>;
    fn prepare_child(&self, prod: ProdId, child_idx: usize, binding: Option<&str>,
                     state: &TypingState, parsed_children: &[TypingState]) -> TransitionResult<TypingState>;
    fn descend(&self, state: &TypingState, path: PathId, binding: Option<&str>) -> TransitionResult<TypingState>;
    fn consume_terminal(&self, state: &TypingState, regex: &Regex, segment: Option<&Segment>) -> TransitionResult<TypingState>;
    fn finish_production(&self, prod: ProdId, state: &TypingState, children: &[TypingState],
                         status: NodeStatus) -> TransitionResult<TypingState>;
}
```

### `RuleRuntime`

The concrete implementation of `TypingRuntime` that performs grammar-based type checking:

```rust
pub struct RuleRuntime {
    grammar: Grammar,
    types: RefCell<Vec<Type>>,
    type_ids: RefCell<HashMap<Type, TypeId>>,
    contexts: RefCell<Vec<Context>>,
}
```

Manages interned types and contexts, evaluates typing rule premises (membership, ascription, operations), and performs unification for meta variables.

### `TypedPrefixState` / `TypedPrefixError`

The result types for parsing:

```rust
pub struct TypedPrefixState {
    pub input_len: usize,
    pub roots: Vec<NodeId>,       // surviving parse roots
    pub frontier: Vec<FrontierId>, // reserved for incremental
    pub depth: DepthMeta,
}

pub struct TypedPrefixError {
    pub input_len: usize,
    pub depth: DepthMeta,
    pub message: String,
}
```

### `FusionAST` / `FusionNode`

Arena-backed typed AST wrappers with zero materialization overhead:

```rust
pub struct FusionAST {
    arena: ParseArena,
    segments: Vec<Segment>,
    roots: Vec<NodeId>,
    input: String,
}

pub struct FusionNode<'a> {
    ast: &'a FusionAST,
    node_id: NodeId,
}
```

`FusionAST` owns the arena and computes everything on-demand. `FusionNode` is a borrowed view into a specific arena node. No intermediate tree is ever cloned — text, types, children, and scoring signals are all computed by traversing the arena.

### `MetaTypedParser`

Wraps `TypedParser` with iterative deepening:

```rust
pub struct MetaTypedParser<T> {
    parser: TypedParser<T>,
    start_depth: u16,    // default: 4
    max_depth: u16,      // default: 128
    depth_factor: f64,   // default: 1.5
}
```

Starts with a small depth budget and increases geometrically (`depth *= 1.5`) until parsing succeeds or `max_depth` is reached. This avoids wasting time on deep parses when a shallow one would succeed.

### `Synthesizer`

High-level interface wrapping `MetaTypedParser`:

```rust
pub struct Synthesizer {
    grammar: Grammar,
    runtime: RuleRuntime,
    meta: MetaTypedParser<RuleRuntime>,
    input: String,
    tree: Option<FusionAST>,
    parse_memo: RefCell<FusionMemoMap>,
    typed_memo: RefCell<TypedMemoMap>,
    // ... counters
}
```

Provides:
- `partial()` / `partial_typed()` — parse current input
- `completions()` / `completions_ctx()` — get valid next tokens
- `extend()` / `try_extend()` — add a token and re-parse
- Memoization with LRU eviction (limit: 32 entries)

---

## Parsing Flow

### Entry Point: `parse()`

```
parse(input, ctx)
  ├── tokenize input into segments
  ├── get start non-terminal
  ├── start_states(ctx) → typing.enter_nonterminal(start_nt, ...)
  └── for each start state:
        parse_nonterminal(start_nt, segments, 0, 0, state)
  └── filter roots: keep Complete (full input) or Partial
```

### `parse_nonterminal()`

Handles a non-terminal symbol:

1. Check depth budget — reject if exceeded
2. Compute the **left-recursion component** (mutually left-recursive non-terminals)
3. If component has >1 member or self-left-recursion, use `parse_component()` (fixed-point algorithm)
4. Otherwise, iterate productions:
   - Skip left-recursive productions initially (collect them for later)
   - Parse each non-left-recursive production via `parse_production()`
   - Deduplicate by `(span_start, span_end, alt_idx)`
5. Iteratively apply left-recursive productions using existing nodes as seeds (fixed-point)

### `parse_production()`

Parses a single production alternative:

```
parse_production(nt, alt_idx, production, ...)
  ├── parse_symbols(production.rhs, ...)
  └── for each branch:
        typing.finish_production(prod_id, state, branch.states, branch.status)
  └── push_node(arena, packed_alts)
```

### `parse_symbols()`

The core recursive symbol sequence parser. Accumulates branches with typing states:

```
parse_symbols([sym0, sym1, ...], input_idx, state, ...)
  ├── prepare_child(sym0, state, parsed_prefix)  → typing gate
  ├── parse_symbol(sym0, input_idx, state)       → first_branches[]
  └── for each first_branch:
        if partial or no rest → yield first_branch
        else → parse_symbols(rest, first_branch.end, last_state)
               combine children, states, status
```

Key behavior:
- Calls `prepare_child()` before parsing each symbol — this is where the typing runtime can reject a branch based on already-parsed children
- Recursively parses the rest of the symbol sequence, combining branches
- Partial branches short-circuit (don't try to parse remaining symbols)

### `parse_symbol()`

Creates a binding via `descend()`, then delegates:

```
parse_symbol(symbol, input_idx, state, ...)
  ├── push_path(parent_path, child_idx, alt_idx)  → path
  ├── typing.descend(state, path, symbol.binding)  → new state with binding slot
  └── match symbol:
        Terminal  → parse_terminal(regex, segments, input_idx, state)
        Nonterminal → parse_nonterminal(nt, segments, input_idx, depth, state)
                      for each child node → create Branch with ChildRef::Node
```

### `parse_terminal()`

```
parse_terminal(regex, segments, input_idx, state)
  ├── typing.consume_terminal(state, regex, segment)
  ├── match segment:
        Some → prefix_match(regex, text) → complete branch
        None → partial branch (input exhausted)
  └── bind_terminal(state.bindings, path, value, type)
```

### `finish_production()`

After all symbols in a production are parsed, `finish_production()` is called:

```
finish_production(prod, state, children, status)
  ├── look up typing rule for production
  ├── apply_rule(premises...) → evaluate all premises
  │     ├── membership: is binding value in context?
  │     ├── ascription: does child's inferred type match expected?
  │     ├── operation: equality/inclusion between resolved types
  │     └── unify meta variables
  ├── if rule fails and status is Partial → return partial state
  ├── if rule fails and status is Complete → reject
  └── compute inferred type from conclusion
```

---

## Binding System

Bindings connect grammar symbols to values and types. They flow through the parse tree and are used by the typing runtime to resolve meta variables and check premises.

### Lifecycle

1. **Creation** — `descend()` creates a binding slot when entering a symbol with a binding name:
   ```rust
   fn descend(state, path, binding) {
       if let Some(name) = binding {
           state.bindings.push(BindingValue {
               name, path, value: None, ty: None,
           });
       }
   }
   ```

2. **Terminal binding** — `bind_terminal()` updates the binding with the parsed token:
   ```rust
   fn bind_terminal(bindings, path, value, ty) {
       bindings.iter()
           .filter(|b| b.path == path)  // only bindings at this path
           .map(|b| BindingValue { value: value.clone(), ty, ..b })
           .collect()
   }
   ```

3. **Node binding** — `bind_node()` updates with the child node's span text:
   ```rust
   fn bind_node(bindings, path, span, ty, segments) {
       let text = span_text(span, segments);
       bindings.iter()
           .filter(|b| b.path == path)
           .map(|b| BindingValue { value: text.clone(), ty: Some(type_id(ty)), ..b })
           .collect()
   }
   ```

4. **Merging** — `merge_bindings()` combines parent and child bindings by name:
   ```rust
   fn merge_bindings(outer, inner) {
       for binding in inner {
           if let Some(existing) = outer.iter_mut().find(|e| e.name == binding.name) {
               // fill in missing value/type from inner
           } else {
               outer.push(binding.clone());
           }
       }
   }
   ```

### Path-Based Scoping

Bindings are scoped to productions via `PathId`. A `PathId` encodes the full path from root to the current position in the grammar (sequence of `child_idx` + `alt_idx` steps). The `bind_terminal`/`bind_node` functions filter by path, so only bindings belonging to the current production position are returned.

This is intended to prevent bindings from leaking across sibling productions.

---

## Typing Runtime

### `RuleRuntime` Implementation

#### Context Management

`RuleRuntime` maintains interned pools of `Type` and `Context` values. Contexts are immutable — extending a context creates a new `CtxId`.

#### Premise Evaluation

Typing rules have premises that must be satisfied:

- **Membership** (`x ∈ Γ`) — checks if a binding's value exists in the current context
- **Ascription** (`e : τ`) — checks if a child's inferred type matches the expected type, with unification for meta variables
- **Operations** — equality or inclusion between types
- **Context extension** — premises can extend the context with new bindings before checking judgments

#### Unification

Meta variables (`Type::Meta(name)`) and atoms (`Type::Atom(name)`) are unified during premise evaluation:

```rust
fn unify(expected, actual, subst) {
    match expected {
        Type::Meta(name) => {
            if subst.contains(name) → check bound == actual
            else → subst.insert(name, actual)
        }
        Type::Arrow(a, b) => unify(a, x) && unify(b, y)  // for actual = Arrow(x, y)
        _ => equal(expected, actual)
    }
}
```

#### Type Resolution

Types can reference binding values. `resolve_type()` substitutes meta variables and atoms with their bound values:

```rust
fn resolve_type(ty, subst, state, children, ctx) {
    Type::Meta(name) → subst.get(name) or binding_type(name) or Type::Meta(name)
    Type::ContextCall(_, var) → ctx.lookup(binding_value(var))
    Type::Arrow(l, r) → Arrow(resolve(l), resolve(r))
    ...
}
```

#### Inferred Type (no explicit rule)

When a production has no typing rule, `inferred_type()` applies a default:
- Single typed child → inherit child's type
- No children or multiple children → `Type::Any`

---

## Incremental Parsing

### `advance()` Module

The `advance` module provides incremental state updates:

```rust
pub fn advance(&mut self, prev: &TypedPrefixState, input: &str, ctx: CtxId)
    -> Result<TypedPrefixState, TypedPrefixError>
```

**Invariant**: `advance` can only **reduce** or **extend** existing roots. New roots are never created — they must be extensions of previous partial parses.

### Algorithm

1. Tokenize the full new input
2. For each previous root:
   - If complete and covers full new input → keep as-is
   - If partial → try to extend from `root.span.end` with new tokens
   - If complete but doesn't cover full input → dead (filtered out)
3. Filter: keep only roots that are partial at the new input boundary or complete covering full input

### Arena Reuse

The arena is never cleared during `advance()` — new nodes are appended. This means old node IDs remain valid, and the arena grows monotonically.

### Implementation Note

The `advance` module replicates much of the parsing logic from `parser.rs` (duplicate `parse_symbols`, `parse_symbol`, `parse_terminal`, `parse_nonterminal`, `finish_branches`, etc.) because the parser's methods are private. This is a known code duplication issue.

---

## Current Issues

### 1. Binding Resolution — Bindings Leak Across Productions

**Symptom**: Bindings from one production can appear in sibling productions' `parsed_children`.

**Root cause**: The `bind_terminal`/`bind_node` filtering by `path` is intended to prevent this, but the filtering happens *after* bindings are collected. When a non-terminal child is parsed, its bindings are attached to the branch state and then merged into the parent via `merge_bindings`. If the same binding name appears at different paths, `merge_bindings` merges them by name, causing cross-production contamination.

**Evidence**: In `parser.rs:898-906`, when a non-terminal child is parsed:
```rust
out.push(Branch {
    states: vec![TypingState {
        bindings: node.bindings.clone(),  // all bindings from child, not filtered
        ...
    }],
    ...
});
```
The child's full binding set is propagated, not filtered to the current path.

### 2. Context Extension in `prepare_child` Doesn't Work for Nested Lambdas

**Symptom**: For nested lambda expressions, the context extension that should add bound variables to the typing context fails to propagate correctly.

**Root cause**: `prepare_child` in `RuleRuntime` (runtime.rs:381-442) checks premises for missing bindings and returns partial if any are missing. However, the context extension logic in `apply_premise` (runtime.rs:233-367) operates on a local copy of the context (`premise_ctx`), and this extended context is only committed back if the premise succeeds. For nested lambdas, the inner lambda's binding isn't available when the outer lambda's `prepare_child` runs, so the context extension is skipped.

**Evidence**: In `prepare_child`:
```rust
for premise in &rule.premises {
    if let Some(TypingJudgment::Ascription((term, _))) = &premise.judgment {
        if binding == Some(term.as_str()) {
            for (name, _) in &setting.extensions {
                let Some(bound) = self.child_binding_value(parsed_children, name) else {
                    return Ok(state.clone());  // returns partial, context not extended
                };
            }
        }
    }
}
```

### 3. `bind_terminal`/`bind_node` Filtering by Path Causes Bindings to Be Lost

**Symptom**: When a non-terminal is involved in a production, the bindings from that non-terminal's children are lost.

**Root cause**: `bind_terminal` and `bind_node` filter bindings to only those matching the current `path`:
```rust
bindings.iter().filter(|b| b.path == path)
```
When a non-terminal child is parsed, its internal bindings have different paths (deeper in the tree). When `bind_node` is called for the non-terminal, it filters to the non-terminal's path, losing all the child bindings. The child's bindings are then supposed to be merged via `merge_bindings`, but the merge happens by name, not by path, causing the issues described in #1 and #4.

### 4. `merge_bindings` by Name Causes Duplicate Bindings

**Symptom**: The same binding name can appear multiple times in a binding list, with different values.

**Root cause**: `merge_bindings` updates existing bindings by name:
```rust
if let Some(existing) = outer.iter_mut().find(|e| e.name == binding.name) {
    if existing.value.is_none() { existing.value = binding.value.clone(); }
    if existing.ty.is_none() { existing.ty = binding.ty; }
} else {
    outer.push(binding.clone());
}
```
The problem is that `outer` may already contain multiple bindings with the same name (from different paths/productions). The `find()` returns the first match, so only one is updated, while others remain with `None` values. This leads to duplicate bindings where some have values and some don't, confusing the premise evaluation logic that searches for binding values.

### 5. Code Duplication in `advance.rs`

The `advance` module replicates ~600 lines of parsing logic from `parser.rs` because the parser's methods are private. This includes duplicate implementations of `parse_symbols`, `parse_symbol`, `parse_terminal`, `parse_nonterminal`, `parse_production`, `parse_component`, `parse_left_recursive_production`, `parse_recursive_with_seed`, `finish_branches`, and all helper functions. Any bug fix or improvement must be applied in both places.

### 6. `advance.rs` Passes Empty Bindings to `bind_terminal`

In the incremental parsing path (`advance.rs:488-494`), `bind_terminal` is called with an empty bindings slice:
```rust
let bindings = bind_terminal(
    &[],  // empty! should be state.bindings
    state.path.unwrap_or(PathId(0)),
    segment.map(|s| s.as_str().to_string()),
    next.inferred,
);
```
This means terminal bindings are never actually created during incremental parsing — the returned binding list is always empty.

---

## Data Flow Summary

```
Input string
    │
    ▼
tokenize → segments[]
    │
    ▼
TypedParser::parse()
    ├── start_states() ──→ enter_nonterminal() ──→ initial TypingState
    │
    ▼
parse_nonterminal(start_nt, segments, 0, 0, state)
    │
    ├── parse_production(nt, alt, prod, ...)
    │       │
    │       ├── parse_symbols(rhs, ..., state, prod_id, ...)
    │       │       │
    │       │       ├── prepare_child(sym, state, parsed_prefix)
    │       │       │
    │       │       ├── parse_symbol(sym, ...)
    │       │       │       ├── descend(state, path, binding)  → creates binding slot
    │       │       │       │
    │       │       │       ├── parse_terminal() ──→ bind_terminal()
    │       │       │       │
    │       │       │       └── parse_nonterminal() ──→ bind_node()
    │       │       │
    │       │       └── (recursive for rest of symbols)
    │       │
    │       └── finish_production(prod, state, children, status)
    │               ├── apply_rule(premises)
    │               ├── unify meta variables
    │               └── compute inferred type
    │
    └── push_node(arena, packed_alts)  → NodeId

NodeId roots ──→ FusionAST(arena, segments, roots, input)
    │
    ▼
FusionNode traversal (on-demand, zero materialization)
```

---

## Key Design Decisions

1. **Typed-by-construction**: Type checking happens during parsing, not after. This means ill-typed parses are rejected early, reducing the search space.

2. **Arena-backed AST**: The parse tree lives in an append-only arena. `FusionAST` and `FusionNode` are thin wrappers that compute properties on-demand. No cloning of intermediate structures.

3. **Path-based binding identity**: Bindings are identified by `PathId` (grammar position), not by arena node ID. This is deterministic and unique within a production.

4. **Partial parsing support**: Nodes can be `Partial` (input exhausted mid-parse), allowing the system to provide completions for incomplete input.

5. **Iterative deepening**: `MetaTypedParser` starts shallow and increases depth geometrically, avoiding wasted work on deep parses.

6. **Incremental by extension**: `advance()` only extends existing roots, never creates new ones. The arena grows monotonically.
