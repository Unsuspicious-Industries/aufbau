# Introduction

In the current context of a rapidly evolving research effort towards LLM-based formal reasoning and theorem proving, our aim is to contribute a novel approach that complements existing methods with a focus on formal guarantees of correctness.

In this blog post [Proposition 7: Truth by Construction](https://unsuspicious.org/blog/proposition-7/),
we introduced the core idea of our approach: guide LLMs to produce only tokens that are guaranteed to be part of a correct expression in a language.  Such approaches already exist in various forms, but our contribution lies in the generalization of this idea to a larger and extensible class of formal languages, along with a concrete algorithm to achieve this goal.

This document serves as a formal specification of the theoretical foundations, as documentation of the algorithm, and as a reference for future work building
upon this approach.

## The Aufbau Engine

**Aufbau** is the grammar-driven completion engine powering the *Proposition 7* project. 
Given a **grammar** and a **partial expression** it computes the set of next tokens that can extend the expression toward a complete, well-typed program. The central guarantee is that every suggested token keeps the expression on a valid trajectory that can then be completed until it reaches a **complete** (a *word* in the language defined by the grammar)

The name comes from the German word for "construction" or "build-up" from Carnap's *Der logische Aufbau der Welt*

## Structure

The specification follows the layered structure of the implementation:

1. **Basic Concepts** : alphabets, Kleene closure, formal languages, and the completability set $\mathcal{C}_L(s)$ at the heart of the engine

2. **Concepts**: the more advanced theoretical framework

3. **Parsing**:  the chart parser, the segment model, the span cache and its invalidation strategy, and the meta-parser that reads grammar specifications.

4. **Typing**: the type system, the inference rules encoded in grammar specs, context propagation, and the typed AST overlay that is built on top of the partial parse forest.

5. **Completion**: how the engine synthesizes and scores completion candidates, and how the beam synthesizer explores the space of completions.

6. **Visualization**: the HTTP server and browser UI that expose the engine core interactively.

7. **CLI Reference**: the `aufbau` command-line tool and its subcommands.

## Invariant

The key correctness property that runs through every layer of the specification:

>T Completion Soundness
For any partial expression $s$ and any token $a$ returned by the completion
engine,

$$s \cdot a \text{ is completable}$$

That is, every suggested token keeps the expression on a path toward at least one complete, well-typed program. We'll see better deinfiiotn of completability later on.
<
