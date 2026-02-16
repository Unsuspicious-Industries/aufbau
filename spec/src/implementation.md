# Implementation Details

Our module is structured as follows:
 - [`grammar`](~/src/logic/grammar/) where we define the grammar data structures and loading logic
 - [`partial`](~/src/logic/partial/) where we implement the partial parser $\Psi_L$ that interprets grammars to collect **partial parse forests** (see next section)
 - [`bind`](~/src/logic/binding/) where we implement the binding resolution logic
 - [`typing`](~/src/logic/typing/) where the core typing engine is implemented, used after parsing to validate well-typedness of parse trees in a forrest. It's mainly used a filtering mechanism for completability.

We also have auxiliary utils such as the [`validation`](~/src/logic/validation/) module which check `aufbau` for correctness and the complex custom regex engine [`regex`](~/src/regex/) that is used for completeability over *two-level languages* (see here: [Completability and Regular Expressions](https://unsuspicious.org/blog/completing-regex/)).


# Proof and structured

In this document we will go through each algorithm in detail, and try to prove their correctness. We will also provide a structured overview of the implementation, including the data structures and algorithms used.
