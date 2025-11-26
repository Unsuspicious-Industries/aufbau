# Binding System

Connects grammar bindings to AST nodes for typing rules.

## Binding Map

Constructed at grammar load time:

$$\beta: (\mathcal{B} \times \text{Rule}) \to \mathcal{P}^*$$

Maps (binding, rule) pairs to grammar paths.

## Grammar Paths

Sequence of steps from rule-carrying node to bound node:

```rust
PathStep { child_index: usize, alternative_index: Option<usize> }
```

Example: `β(x, abs) = [{child: 1, alt: None}]`

## Resolution

At runtime, traverse AST following path steps:

```
RESOLVE(node, path):
    if path = []: return node
    step = path[0]
    child = node.children[step.child_index]
    if step.alt ≠ None ∧ child.alt ≠ step.alt:
        return AlternativeMismatch
    return RESOLVE(child, path[1:])
```

Results:
- `Match(node)` — complete node found
- `Partial(node)` — incomplete node at path end
- `AlternativeMismatch` — tree took different alternative
- `MissingNode` — path extends beyond tree

## Construction

```
BUILD-MAP(G):
    for (nt, prods) ∈ G.productions:
        for (alt, prod) ∈ prods:
            if prod.rule:
                COLLECT(G, nt, alt, prod, prod.rule, [], {}, map)
    return map

COLLECT(G, nt, alt, prod, rule, path, visited, map):
    if (nt, alt) ∈ visited: return
    visited ← visited ∪ {(nt, alt)}
    
    for (i, sym) ∈ prod.rhs:
        if sym.binding:
            map[rule, sym.binding] ← path · [i]
        if sym ∈ N ∧ ¬has_rule(sym):
            for (j, child_prod) ∈ G[sym]:
                COLLECT(G, sym, j, child_prod, rule, path·[(i,j)], visited, map)
```

## See Also

- [grammar.md](grammar.md) — Binding syntax
- [challenges.md](challenges.md) — Formal definition
