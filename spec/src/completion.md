#[D] Completion

The **completion** pipeline takes a partial input string $s$ and produces a complete, well-typed expression in the language $L$ defined by grammar $G$. It is the core synthesis loop of Aufbau, the mechanism by which partial expressions are extended into valid programs.

The theoretical foundation for completability over two-level languages (where the grammar alphabet is itself a regular language over bytes) is developed in [Completability and Regular Expressions](https://unsuspicious.org/blog/completing-regex/).

This page is the roadmap for the completion subsection of the book. It is meant to be read linearly with the next three chapters in order: [Synthesizer](./completion/synthesizer.md), [Search](./completion/search.md), then [Scoring](./completion/scoring.md). To avoid duplication, those chapters focus on one stage each instead of re-explaining the whole pipeline.

## Pipeline Overview

The completion pipeline composes three components:

1. **Synthesizer** $\Sigma_s$: maintains incremental parsing state and exposes typed extension operations. Given input $s$ and a typing context $\Gamma$, it produces the set of type-valid next tokens.

2. **Search**: explores the space of token sequences that extend $s$ to a complete expression. The current implementation tries a greedy fast path first, then falls back to a priority-queue frontier.

3. **Scoring**: a heuristic function $|\sigma|: Q \to \mathbb{R}$ that ranks search states by estimated proximity to a valid completion. Guides the search toward promising branches.

The composition is:

$$\text{complete}(G, s, \Gamma) = \text{Search}(\Sigma_s(G, s), |\sigma|, \Gamma)$$

where $\Gamma$ is an optional external typing context.

## Typed Completability

Recall from [Basic Concepts](./basic.md) that the completability set $\mathcal{C}_L(s) = \\{a \in \Sigma : \exists s' \in \Sigma^*, sas' \in L\\}$ is the set of valid next symbols. The completion pipeline computes a refined version:

$$\mathcal{C}_{L,\Theta}(s) = \\{a \in \mathcal{C}_L(s) : \Gamma \vdash\_\Theta \mathcal{F}(sa) \neq \text{Invalid}\\}$$

This is the **typed completability set**, the set of next tokens that are both syntactically valid and consistent with all typing constraints. The [type system](./concepts/typing_system.md) explains why this refinement is always a subset of the syntactic completability set.

## Data Flow

```
Input s ──→ Synthesizer ──→ completions_ctx ──→ Search loop
                │                                      │
                │            extend_with_regex ←───────┘
                │                   │
                └───────────────────┘
                        │
                  find_valid_completion ──→ Success / Exhausted
```

This diagram is intentionally high-level. The current implementation's greedy fast path, beam-style child truncation, and local grounding preference are search-policy details described in [Search](./completion/search.md).

Each iteration of the search loop:
1. Pops the highest-scored state from the priority queue
2. Checks whether the current typed forest already contains a complete root
3. If not, asks the synthesizer for `completions_ctx` at the current state
4. Extends the state with candidate tokens, scores and prunes the children, and pushes the survivors onto the queue

For the formal definition of completability over two-level languages (where terminals include regex patterns over $\Sigma_\text{vocab}$), including the Brzozowski derivative mechanism for partial token validation, see [Completability and Regular Expressions](https://unsuspicious.org/blog/completing-regex/).

## Source Files

| Component | Path |
|-----------|------|
| Synthesizer | [`src/logic/partial/synth.rs`](~/src/logic/partial/synth.rs) |
| Search | [`src/logic/search/mod.rs`](~/src/logic/search/mod.rs) |
| Scoring | [`src/logic/search/scoring.rs`](~/src/logic/search/scoring.rs) |
