#[D] Typed AST

The **typed AST** is the output of applying type-checking to a partial parse forest. It transforms a `PartialAST` (whose nodes carry no type information) into a tree of `TypedNode`s annotated with inferred types, discarding any trees that fail type-checking.

Source: [`src/logic/typing/tree.rs`](~/src/logic/typing/tree.rs)

## Node Types

A `TypedNode` is one of two variants:

| Variant | Fields | Meaning |
|---------|--------|---------|
| `Term` | `val: String`, `ty: Type` | A terminal token with its inferred type |
| `Expr` | `name: String`, `children: Vec<TypedNode>`, `ty: Type`, `complete: bool` | A nonterminal with typed children |

Only nonterminal children appear in `Expr.children`. Terminal children are folded in directly as `Term` leaves. The `complete` flag mirrors the source `NonTerminal.is_complete()`.

## TypedAST Structure

| Field | Type | Meaning |
|-------|------|---------|
| `roots` | `Vec<TypedNode>` | Well-typed parse trees (malformed and too-deep trees are dropped) |
| `input` | `String` | The input string that was parsed |

A `TypedAST` is never empty: construction fails with `Err("No well-typed trees")` if no tree survives type-checking.

## Construction

### From PartialAST

`PartialAST::typed(g)` and `typed_ctx(g, ctx)` are the primary entry points. Both:

1. Call `check_node` on each root, which walks the tree running typing rules and accumulates a `type_cache: HashMap<TreePath, Type>`.
2. Discard roots whose status is `Malformed` or `TooDeep`.
3. For surviving roots, build `TypedNode` trees by reading types out of the cache (falling back to `Type::Any` for nodes not in the cache).

The cache-first approach avoids redundant type evaluation: types are computed once during the `check_node` pass and then simply copied during `TypedNode` construction.

### Composition helpers

| Method | Behaviour |
|--------|-----------|
| `typed(g)` | Type-check with empty context |
| `typed_ctx(g, ctx)` | Type-check with provided context |
| `typed_complete(g)` | `typed(g)` then `complete()` |
| `typed_complete_ctx(g, ctx)` | `typed_ctx(g, ctx)` then `complete()` |
| `filter_typed(g)` | Returns a `PartialAST` keeping only well-typed roots |
| `filter_typed_ctx(g, ctx)` | Same, with context |
| `has_well_typed(g)` | Predicate: any well-typed root exists? |

`complete()` filters `roots` to only complete trees (those where `is_complete()` is true). It returns `Err("No complete trees")` if none remain.

`filter_typed` and `filter_typed_ctx` return a `PartialAST`, not a `TypedAST` — they preserve the original untyped structure but drop ill-typed trees. This is used by `Parser::partial_typed`.

## TreeStatus and Filtering

Type-checking returns a `TreeStatus` per root. The typed AST uses this to decide what to keep:

| Status | Kept? | Meaning |
|--------|-------|---------|
| `Valid(Type)` | Yes | Fully type-checked, complete derivation |
| `Partial(Type)` | Yes | Partial derivation, type assignment still valid |
| `Malformed` | No | Rule mismatch; typing rule preconditions failed |
| `TooDeep` | No | Recursion limit exceeded during type evaluation |

`Partial` trees are retained because the input itself may be partial (incomplete input is the normal case during guided synthesis). A `Partial` root becomes a `Term` or `Expr` node with a tentative type.

## Type Fallback

When a node has no entry in the type cache — which happens for nonterminals that are covered by a rule that assigns `Type::Any`, or for which no rule fires — the construction assigns `Type::Any`. This prevents construction from failing due to missing type information on interior nodes.

## Display

`TypedAST` displays as an IDE-style annotated tree, with types shown inline using `: type` notation:

```
Input: "42"

Tree 0:
└─ start
   └─ Num : int
      └─ 42 : int
```

`Type::Any` is suppressed in display (the annotation is omitted if the type is `Any`).

>N on type any
`Type::Any` is not an error state. It means "no typing rule assigned a more specific type to this node." Many intermediate nonterminals in a grammar carry `Any` because the grammar author only typed the leaves and the root.
<
