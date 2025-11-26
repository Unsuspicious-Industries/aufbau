# Partial Parsing

Partial parser exploring all valid/completable parse paths.

## Structure

```
Grammar + Input → Parser → PartialAST{roots: [NonTerminal], input: String}
```

**Completeness**: `∃ t ∈ roots : complete(t)`

## Node Types

```rust
NonTerminal {
    name: String,
    production: Production,
    alternative_index: usize,
    children: Vec<Node>,
    binding: Option<String>,
    consumed_segments: usize,
}

Terminal::Complete { value, binding, extension }
Terminal::Partial { value, binding, remainder }
```

## Completeness Predicate

$$\text{complete}(v) = \begin{cases}
\text{matched full token} & v \in T \\
\forall i: \text{complete}(v[i]) \land |children| = |rhs| & v \in N
\end{cases}$$

## Frontier

Path to rightmost incomplete node. Unique in partial trees, absent in complete trees.

**Monotonicity**: $\text{front}(\Psi(s \cdot t)) > \text{front}(\Psi(s))$

## Algorithm

1. Tokenize input
2. Parse from start symbol, trying all alternatives
3. Collect trees consuming all segments
4. Return forest

## See Also

- [grammar.md](grammar.md) — Grammar spec
- [binding.md](binding.md) — Binding resolution
- [completion.md](completion.md) — Completion algorithm
