# corpora/ml

A corpus of ML **expressions**, certified against `examples/ml.auf`.

`start` holds `Expression`, which is *not* the grammar's own start symbol.
`ml.auf` starts at `Program` — a list of `Define` structure items of the form
`let name (param : T) : T = body` — because that is what constrained generation
has to target: the grader compiles the text and calls `solve` from a driver
appended after it, so a program must bind a name that outlives its own
right-hand side.

The entries here predate that top-level section and are expressions, which is
the right unit for certifying the typing rules. Without `start` every entry is
rejected at its second token: `valid.txt` fails wholesale, and — worse —
`invalid.txt` and `beyond.txt` keep *passing* while proving nothing, because
their entries are still rejected, just for the wrong reason.

**The OCaml harness does not read this file yet.** `ocaml/lang_ml.ml` calls
`Grammar.load` and the OCaml FFI exposes no way to override a loaded grammar's
start, so its ML certification has the same latent breakage. Fixing it means
adding that to the FFI; until then the two harnesses disagree about ML, and the
Rust side is the correct one.
