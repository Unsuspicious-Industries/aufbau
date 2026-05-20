# STYLE

> *"If you can (cleanly) refactor to the point that the feature is a 3-line change, this is great."* — tinygrad

This is how we write code in aufbau. Read it once. Then write less.

The project contains
 - `draft/` with the spec. 
 - `src/` with the implementation.
 Property tests and `///` doc labels bridge them.


## 0. One rule

**Every line is a liability.** Adding lines must justify itself. Deleting lines almost never has to. If a diff shrinks and `make test` still passes, ship it.

Corollary: dead code is a bug.



## 1. Budgets

Hard caps, checked by `scripts/lc.sh` in CI from `./style.toml`


`extra/`, `experiments/`, `data/`, `validation/` and all test code are exempt. Nothing in those directories ships.


## 2. Naming scheme

The paper writes `σ(s)`, the code writes `σ`. The paper writes `Ψ_G`, the code writes `Ψ`. The paper writes `pos_n,a(b)`, the code writes `pos(n, a, b)`. If you have to translate between LaTeX and Rust to read the code, the code is wrong.

```rust
// good : matches §2.2 of the paper
fn match_term(t: &Term, x: &Seg) -> Option<(Match, &str)> { ... }

// good : matches Definition 4
struct Graph<'a> { E: Vec<Evidence>, C: Vec<Constraint>, _p: PhantomData<&'a ()> }

// bad : paper says Γ and code says context_at_dot
fn descend(nt: NT, prod: &Production, dot: usize,
           context_at_dot: &TypingContext, ...) { ... }
```

Unicodes can be replaced by simple words. 
Gamma, universally read as "context" in a type theory setting can be replace by `Ctx` or `ctx` in the code.

Rules:

- **Unicode is encouraged** `Γ`, `Ω`, `σ`, `τ`, `∇`, `φ`, `η`, `ν`, `ξ`, `χ`, `Ψ`, `Θ`, `λ`, `ε`, `⊥`, `⊤`, `∈`, `⊆`, `⊢`. Match `draft/macros.tex`.
- **Short scope → one letter.** Inside a 10-line function, `i`, `j`, `k`, `n`, `p`, `ω`, `ν` are perfect. Inside a 200-line function, you have bigger problems.
- **Long scope → short names ascii, paper-style.** `obligations` for `Ω`. `Graph` for `G`. `production` for `p`. `nt`.
- **Acronyms uppercase.** `SPG`, `NT`, `AST`, `FFI`, `EOF`, `CSP`. Not `Spg`, `Nt`, `Ast`. 
- **No `get_`/`set_`.** Field access is field access. `obligations()` not `get_obligations()`.
- **Files are single word, one concept per file.** If a file name needs `_and_`, split it.
- **Use the filesystem hierarchy**, we are not in C. mode module::sub instead of module_sub

The one place to be *more* verbose: `pub` API exposed across `ffi/python.rs`. Python users don't read the paper; `Synthesizer.try_feed(token)` stays `try_feed`, not `tf`.


## 3. Rust

aufbau is a parser. Parsers are data transformations. Write them that way.

**Reach for:** iterators, `match` on exhaustive enums, `?`, `From`/`Into`, `const fn`, slice patterns, `#[derive(...)]`.

**Reach away from:** `Box<dyn Trait>` where a generic works, trait hierarchies that mirror an OO design, builder structs for things with ≤ 3 fields, `Default` impls that lie, deep `pub use` chains.

```rust
// good 
enum Status { Prefix, Exact, Extensible }
impl Term {
  fn step(&self, x: &Seg) -> Option<(Status, &str)> { match self { ... } }
}

// bad 
trait Terminal { fn step(&self, x: &Seg) -> Option<(Status, &str)>; }
struct RegexTerm(Regex); impl Terminal for RegexTerm { ... }
struct LiteralTerm(String); impl Terminal for LiteralTerm { ... }
// ...
```

Rules that aren't negotiable:

- **`clippy::pedantic` on, `-D warnings` in CI.** If clippy fights you, the code is wrong.
- **No `unwrap` / `expect` outside `#[cfg(test)]` and `main.rs`.** In tests, `unwrap` is fine and shorter than `?`. In library code, an `unwrap` is a panic the type system told you was possible.
- **No `unsafe` without two things directly above it:** (a) a `// SAFETY:` block stating the invariant, (b) the name of the `proptest` that establishes it. If you can't name one, you can't write the `unsafe`.
- **One crate-level `Error` enum**, `thiserror`-derived, in `src/engine/error.rs` (it's already there — extend it, don't add siblings). `anyhow` is fine in `main.rs` and tests, banned in library code.
- **`#[must_use]` on every constructor and every `try_*` function.** Discarding a `try_feed` result is always a bug.
- **No re-exports that hide where things live.** `pub use engine::parse::Parser` at the crate root is fine; chains of three `pub use` are not.

### Comment density

Sparse. Comments answer *why*. The code answers *what*. If a 5-line function needs 8 lines of comments, the function name is wrong or the function is wrong.

```rust
// good
// Prefix scan: terminal is open on the last segment, so we route to F (EOF frontier)
// per ScanOpen in §4.2. The token can still be refined by later input.
if j == m - 1 && open(ξ) { F.push(...); }

// bad
// Increment k
k += 1;
```

- **`///` on every `pub` item.** First line is one sentence. Blank line, then details if any.
- **Cite the paper.** A function implementing rule X writes `/// Implements §4.2 ScanOpen.` and *that's it*. The reader opens `draft/` if they need more.
- **No commented-out code.** Ever.
- **`TODO(name, #issue)` is fine. `FIXME` means it ships broken and blocks merge.**


## 4. Property testing : enforcing the theory

The paper proves Theorem 2 (Completability Soundness) and Theorem 3 (Prefix Monotonicity). Those are not abstract, they correspond to functions in `src/engine/parse/parser.rs` and `src/domains/typing/`. Every paper lemma should be:

1. Covered by a property test in the corresponding `tests/` directory, or
2. Marked `// PAPER ONLY: stated, not mechanized` with a clear todo

There is no third option. "Obvious from the code" is not a third option.

### Layout

```
src/domains/typing/
  ops.rs            // exact filters: init, resolve, finalize
  context.rs        // Γ, ∇, descend
  obligation.rs     // Ω store
  rule.rs           // typing rule evaluation
  tests/
    correctness.rs   // eval determinism, unifier properties, subtype lattice
    realizability.rs // safe pruning, monotonicity, witness existence
    invariants.rs    // grammar-level invariants (transparent wrapping, context membership)
```

`///` doc labels (e.g. `/// \`def:constraint-domain\``, `/// \`lem:safe-pruning\``) are the canonical bridge between LaTeX `\label{}` and Rust code. `scripts/gen-code-refs.sh` scans these and produces `draft/code-refs-gen.tex`. CI runs it in check mode; stale refs fail the build. Keep it green.

### Writing property tests

```rust
proptest! {
    /// Verdict never downgrades under additional obligations
    /// (§3 — Lemma 3.1 analog)
    #[test]
    fn verdict_monotonicity(input in grammar_strategy()) {
        // ...exercise semantic operations and assert invariants...
    }
}
```

- **Test names describe the property, not the mechanism.** `verdict_monotonicity`, not `test_3`.
- **`proptest` for everything algebraic.** Sampling with high case counts catches violations that hand-written tests miss.
- **If a property test fails, fix the code, not the test.** Loosening assertions to make the test pass *is* a bug.
- **Red test = red build = blocked merge.** Same gate as any other test failure.



## 5. Python FFI 

The Python module exists because users want `pip install aufbau` and `SPG("...").tokenize("...")`. Nothing else. Every line of Python is a line that could have been Rust.

It can only be used for prototyping logic.

```python
# good : pyo3 export, exactly one line of Python
from ._native import Synthesizer  # re-exported

# bad : logic in Python
def parse(spec, input):
    grammar = load_grammar(spec)         # should be Rust
    tokens = tokenize_input(input)        # should be Rust
    return run_parser(grammar, tokens)    # should be Rust
```

- **`pyo3` + `maturin`, nothing else.** No `ctypes`, no `cffi`, no hand-rolled `extern "C"`.
- **The Python surface is what's in `README.md`'s "Exported API" section.** That list is the contract. Adding to it requires a PR that also updates `README.md` and adds a `.pyi` stub.
- **`.pyi` stubs are mandatory** for every public function. They are the docs Python users actually read.
- **Numpy interop via the buffer protocol, on the Rust side.** Zero-copy or it's wrong.
- **No loops in Python over tokens / characters / parser items.** If you wrote `for token in ...`, ask whether `feed_many` should exist in Rust.
- **`__init__.py` does one thing: re-export from `_native`.** No metaclasses, no `*` imports, no decorators.
- **`src/ffi/test.py` is the integration test surface.** `pytest` runs it via `make test-py`. Tests cover the boundary, not the math — Rust property tests do the math.



## 6. LaTeX (`./draft/`)

The paper is the spec. Treat it like code.

- **One section per file**, already established: `01-abstract-spg-definition.tex`, `02-generic-parser-algorithm.tex`, `03-typing-domain-implementation.tex`. Numbered prefix for ordering, `\input{}` in `main.tex`.
- **Every theorem/lemma/definition gets a `\label{}`.** Naming convention: `thm:completability`, `lem:type-monotone`, `def:evidence-graph`, `prop:store-exactness`. Lowercase, hyphenated, category-prefixed.
- **Every label is cited in a `///` doc comment** on its Rust counterpart. `scripts/gen-code-refs.sh` enforces linkage (or mark `paper_only = true` in prose).
- **Macros in one file** (`draft/macros.tex` once you create it; currently inlined — fix in a refactor PR): `\R`, `\N`, `\norm{x}`, `\inner{x}{y}`, `\eval`, `\Closed`, `\Live`, `\Lost`. Never redefine inline.
- **No `\textbf` for emphasis. Use `\emph`. No `\\` for line breaks in paragraphs.**
- **Build with `latexmk -pdf` from `draft/`.** CI builds the PDF; a broken build blocks merge.
- **Bibliography stays in `references.bib`.** Existing `CITE` placeholders in §6 are `FIXME`s — they block merge per §3.


## 7. Tests

The paper claims things. Tests check them. Mismatches between the two are the highest-priority bugs in the repo.

### What goes where

```
src/engine/parse/tests/        // unit + property tests for parser
src/engine/grammar/tests/      // grammar loading, tokenization
src/domains/typing/tests/      // CSP solver, obligation store
  realizability.rs             // ← directly tests §3 Realizability claim
  correctness.rs               // ← Theorem 1 (impl = ideal evaluator)
  invariants.rs                // ← Lemma 3 (type evidence monotone)
src/validation/parseable/      // golden tests per grammar: fun.rs, imp.rs, stlc.rs
src/ffi/test.py                // FFI boundary only
```

### Rules

- **Every bug fix ships with a regression test that fails on the parent commit.** No exceptions. A bug fix without a test means the bug comes back.
- **Property tests (`proptest`) for everything algebraic.** Many properties are already stated in the paper — port them directly. `add_is_commutative`, `monotone_under_extension`, `feed_then_unfeed_is_identity` (if applicable). Test names *describe the property*, not the mechanism.
- **Test names**: `<subject>_<property>`. Not `test_1`, not `test_parser_works`. `parser_accepts_valid_stlc_prefix`. `obligation_store_is_monotone_under_resolve`.
- **A test that needs a mock is a test of the wrong thing.** The parser doesn't need mocks; it takes strings and returns parse states. If you're mocking, refactor until you aren't.
- **Validation grammars (`src/validation/parseable/{fun,imp,stlc,...}.rs`) are the empirical anchor of §6.** Adding a new grammar is great. Removing one needs a PR justifying it.
- **Tests live in test files, not inline.** `#[cfg(test)] mod tests { ... }` blocks inside source files are banned. Tests go in `tests/` subdirectories (for large suites) or sibling `tests.rs` files (for small unit tests). The only exception: single-statement `#[cfg(test)]` debug guards — e.g. `#[cfg(test)] debug_trace!(...)` — which are fine and don't count against the budget. Every test move makes `scripts/lc.sh` happier and the code shorter.


## 8. Dependencies

`Cargo.toml` is short and stays short. Adding a dep requires:

1. A line in the PR explaining why a 50-line in-house version isn't enough.
2. License: MIT / Apache-2.0 / BSD. No exceptions.
3. Last release within 12 months.
4. No transitive bloat — `cargo tree -d` shows no duplicates we didn't already have.

Group and comment:

```toml
# core parsing
regex      = "..."

# property testing
proptest = { ... }

# ffi
pyo3       = { version = "...", features = ["extension-module"] }
numpy      = "..."

# error / debug
thiserror  = "..."
```

Python: `pyproject.toml` lists exactly what users need. Test/dev deps under `[project.optional-dependencies]`.

---

## 9. PRs

Inherited from tinygrad, verbatim because it works:

- **Bug fixes are the best.** Always welcome, always reviewed first.
- **If your PR looks "complex", is a big diff, or adds lots of lines, it won't be merged.** Break it up. Prerequisite refactors before feature. 
- **Code golf PRs are closed. Conceptual cleanups are great.** The distinction: code golf trades clarity for line count; cleanup buys clarity *and* line count.
- **Dead code removal from `src/` is great.** Less for the next reader to be confused by.
- **"Exploratory" PRs are closed.** Prototype in `experiments/`. Open the PR when you have a plan.
- **If you don't understand the code you're changing, don't change it.** Read `draft/` first.

PR description template:

```
What:    one sentence
Why:     one sentence
Paper:   §N.M / Lemma X / — (none)
Prop:    tests pass / new property test `foo` added / N/A
LC:      +X −Y  (cargo run --bin lc, or wc -l)
```

If `LC` is positive, the PR description must justify it. Negative `LC` PRs that pass CI need no justification beyond "fewer lines."

---

## 10. The asymmetry

When in doubt:

- Delete > add.
- Match the paper > clever rename.
- Inline > extract (until the function exceeds the budget).
- One enum + `match` > trait hierarchy.
- Rust > Python.
- Property-tested > hand-tested > asserted.
- Boring > clever.

The goal is that someone reading the draft and then opening `src/` finds the same names, the same structure, and the same theorems.