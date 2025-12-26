# Introduction

In the current context of a rapidly evolving research effort towards LLM-based formal reasoning and theorem proving, our aim is to contribute a novel approach that complements existing methods with a focus on formal guarantees of correctness. 

In this blog post [Proposition 7: Truth by Construction](https://unsuspicious.org/blog/proposition-7/), we introduced the core idea of our approach: guide LLMs to produce only tokens that are guaranteed to be part of a correct expression in a language. Such approaches already exist in various forms, but our contribution lies in the generalization of this idea to a larger and extensible class of formal languages, along with a concrete algorithm to achieve this goal.

This document will serve as a formal specification of the theoretical foundations, as documentation of the algorithm and as a reference for future work building upon this approach.
