# Implementation notes

 - Explicit the Bot/Top rules  and behavior
 - Define a preper pattern-matching semantic core
 - Fix the fucking transparent handling with 
   + generalizable Elaborations rules [ref](https://www.cs.cmu.edu/~fp/courses/15814-f21/lectures/11-elab.pdf)
   + Clear cut Semantic separation for elaboration mecanisms
   + exclude from proofs
 - Add specialized module testing utilities (stlc, bigger langs)
   + Ocaml FFI (https://github.com/bquiring/well-typed-term-generator/tree/main/lib)
   + Python verification module

## Open: `extend()` rejects candidates `gather()` proposed (2026-04-07)

Kept from the deleted `docs/issues/soundness_issues.md`; unverified against the
current engine. On `A ::= 'a' A | 'b'`, BFS search saw `extend()` fail with
`no typed branches survived` for a candidate `gather()` had just offered, on an
untyped grammar. If it still reproduces it is a real divergence between the
candidate generator and the parser, and `bfs.rs` swallowing the `Err` hides it.
Check before trusting search completeness; the message itself is also wrong,
naming typing where the failure was syntactic.
