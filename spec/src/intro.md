# Introduction

In the current context of a rapidly evolving research effort towards LLM-based formal reasoning and theorem proving, our aim is to contribute a novel approach that complements existing methods with a focus on formal guarantees of correctness.

In this blog post [Proposition 7: Truth by Construction](https://unsuspicious.org/blog/proposition-7/),
we introduced the core idea of our approach: guide LLMs to produce only tokens that are guaranteed to be part of a correct expression in a language.  Such approaches already exist in various forms, but our contribution lies in the generalization of this idea to a larger and extensible class of formal languages, along with a concrete algorithm to achieve this goal.

This document serves as a formal specification of the theoretical foundations, as documentation of the algorithm, and as a reference for future work building upon this approach.

Our specific system focuses on a subset of the class of **context-dependent languages**.  

## The Aufbau Engine

**Aufbau** is the grammar-driven completion engine powering the *Proposition 7* project. 
Given a **grammar** and a **partial expression** it computes the set of next tokens that can extend the expression toward a complete, well-typed program. The central guarantee is that every suggested token keeps the expression on a valid trajectory that can then be completed until it reaches a **complete** (a *word* in the language defined by the grammar)

The name comes from the German word for "construction" or "build-up" from Carnap's *Der logische Aufbau der Welt*

## Structure

The specification follows the layered structure of the implementation:


2. **Core Ideas**: Our theoretical framework along with our theorems, and implementations outlines

3. **Parsing**: typed arena parsing, packed alternatives, and incremental extension of prefix states.

4. **Typing**: the type system, the inference rules encoded in grammar specs, context propagation, and the typed AST overlay that is built on top of the partial parse forest.

5. **Completion**: how the engine synthesizes and scores completion candidates, and how the beam synthesizer explores the space of completions.

6. **Verification**: All the utils we built and use to ensure the system is working as intended

## Invariant

The key correctness property that runs through every layer of the specification:

>T Completion Soundness
For any partial expression $s$ and any token $a$ returned by the completion
engine,

$$s \cdot a \text{ is completable}$$

That is, every suggested token keeps the expression on a path toward at least one complete, well-typed program. We'll see better deinfiiotn of completability later on.
<
## Related Work

The Aufbau engine synthesizes ideas from incremental parsing, packed forest representations, and syntax-directed type checking.

### Packed Forest Representation

The arena-based packed alternative hypergraph is conceptually related to Shared Packed Parse Forests (SPPF) from generalized parsing literature. However, where SPPF packs ambiguity across all parse states, our arena is **typing-filtered**: alternatives are pruned eagerly by the `TypingRuntime` during construction, ensuring that only well-typed branches survive. This trades full ambiguity preservation for tractable completion search.

### Incremental Parsing

The `advance` operation provides a **monotonic resume interface**: given a previous parse state and new input, it extends existing roots without reparsing from scratch. This is similar to tree-sitter's incremental reparse but operates at a finer granularity (segment-level) and integrates tightly with typing constraints. The monotonicity invariant (arena only grows, roots only extend) ensures cache coherence across edits.

### Syntax-Directed Typing with Executable Oracles

The `TypingRuntime` trait formalizes syntax-directed checking as a step interface synchronized with the parser. Unlike traditional two-phase systems (parse then typecheck), our engine interleaves the two, using typing judgments as **branch filters**. The Coq oracle provides an independent reference implementation of the typing semantics, allowing cross-validation without coupling the mechanized proof to the Rust codebase.
