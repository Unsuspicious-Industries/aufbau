# Incremental Recursive-Descent Plan

This plan keeps the current recursive-descent parser as the semantic core.
It does not replace it with a chart parser. Instead it reifies the parser's
existing memoized search state so that a prefix parse can be resumed.

The design goal is:

- preserve current behavior and depth semantics
- preserve the current notion of partial trees
- make memoized subproblems reusable across prefix growth
- approach chart-like reuse as a consequence of state persistence, not by
  rewriting the parser into a different algorithm

## Axioms

1. The recursive-descent search is the reference semantics.
2. Depth remains first-class. It is part of the parse state, not a tuning hack.
3. Partial trees are proof objects for future completion and must be preserved.
4. Incrementality must be expressed as resuming old search states, not by
   reparsing and hoping memo hits save us.
5. Any new module must reduce conceptual load. If a helper adds indirection
   without clarifying invariants, it should not exist.

## Core idea

Today the parser computes memoized recursive subproblems, but throws away the
most valuable operational information: which recursive calls were still open at
the prefix boundary, and what remainder they were waiting for.

The new design keeps three things from a prefix parse:

- the memo table of solved recursive subproblems
- the frontier of suspended recursive calls at the boundary
- the produced SPPF forest for already-proven structure

Incremental parsing then becomes:

1. parse prefix with ordinary recursive descent
2. retain memo + frontier + forest
3. on append-only growth, resume only frontier states against the new suffix
4. keep all interior solved states and forest fragments unchanged

This preserves the algorithm while making it persistent.

## New semantic objects

The parser should produce a persistent prefix result:

```text
PrefixState =
  { input            : String
  , segments         : Vec<Segment>
  , max_depth        : usize
  , forest           : SppfForest
  , solved           : MemoTable
  , frontier         : Vec<SuspendedCall>
  , hit_depth_limit  : bool
  }
```

Where:

- `solved` is the current memo table, but retained after parse
- `frontier` is the set of recursive calls that reached the boundary and could
  continue if more input appears

The frontier element should be as small as possible:

```text
SuspendedCall =
  { key              : ParseKey
  , call             : CallShape
  , abs_pos          : usize
  , level            : usize
  , local_suffix     : SuffixState
  }
```

With:

- `ParseKey`: same logical identity as current memo subproblems
- `CallShape`: whether we suspended in `parse_nonterminal`, `parse_symbols`, or
  terminal matching
- `SuffixState`: the remaining symbol sequence or regex derivative needed to
  continue

This is not a new parsing algorithm. It is the old one with continuations made
explicit.

## Required invariants

These invariants must be written into the code as doc comments near the types.

### Solved-state soundness

Every entry in `solved[key]` is a result that the current recursive-descent
parser would have returned for that exact subproblem at the current depth bound.

### Frontier soundness

Every `SuspendedCall` corresponds to a real recursive call that reached the end
of the current prefix without contradiction.

### Frontier completeness

Every recursive call that could still succeed after appending input is either:

- already solved, or
- represented in `frontier`

### Forest persistence

All SPPF nodes produced strictly before the old prefix boundary remain valid for
append-only growth.

### Depth monotonicity

No resumed call may observe a different depth discipline than the original
search. Depth is part of the continuation semantics.

This matters because depth is what allows the parser to expose far-reaching
partial trees in left-recursive settings.

## File-by-file plan

The rewrite should stay local to the partial parser.

### `src/logic/partial/parse.rs`

Keep this as the public parser module, but simplify its role.

It should contain:

- `ParseError`
- `PartialParseOutcome`
- `Parser`
- `ParserStats`
- `PrefixState`
- public entry points only

It should not contain all continuation mechanics inline.

Public API after rewrite:

- `Parser::new(grammar)`
- `Parser::parse(input)`
- `Parser::partial(input)`
- `Parser::prefix(input) -> Result<PrefixState, ParseError>`
- `Parser::advance(prev: &PrefixState, new_input: &str) -> Result<PrefixState, ParseError>`
- `Parser::clear_cache()`
- `Parser::last_stats()`

Implementation note:

- `partial(input)` becomes `prefix(input)` followed by projection to final roots
- `advance` is append-only at first; non-append updates fall back to `prefix`

### `src/logic/partial/parse_state.rs`

This file should hold the persistent state and invariants.

Contents:

- `PrefixState`
- `ParseKey`
- `SuspendedCall`
- `CallShape`
- `SuffixState`
- helper constructors and normalization

Reason:

- these are the semantic objects of incremental parsing
- keeping them separate prevents `parse.rs` from collapsing into a 1500-line mix
  of public API and internal proof objects

If this file grows too small, merge it back. But start here because the state is
the heart of the design.

### `src/logic/partial/parse_resume.rs`

This file should contain only the continuation/resume logic.

Contents:

- `resume_nonterminal(...)`
- `resume_symbols(...)`
- `resume_terminal(...)`
- frontier extraction helpers

Role:

- operationalize "old recursive descent, but resumable"
- keep the core recursive functions structurally similar to the existing ones

This is the most important architectural choice: do not invent a second parser.
Instead split each current recursive function into two forms:

- fresh call
- resumed call

The resumed form takes a `SuspendedCall` and a segment delta.

### `src/logic/partial/parse_memo.rs`

This file should hold only memo-table logic.

Contents:

- `MemoTable`
- `MemoEntry`
- insertion / lookup / persistence rules
- prefix-advance reuse rules

The current memo is too output-oriented. It stores solved results, but not the
information needed to resume. The new memo entries should be:

```text
MemoEntry =
  { solved_results : Vec<ParsedNt>
  , suspended      : Vec<SuspendedCall>
  }
```

This is the key simplification: do not add a separate large "chart" type yet.
Let the chart-like structure emerge from persistent memo entries and suspended
calls.

This keeps the algorithm faithful to the current code.

## Exact algorithmic rewrite

### Step 1: reify memo entries

Current memo entries hold only `Vec<ParsedNt>`.

Change them to hold:

- completed results
- resumable boundary continuations discovered while computing those results

This lets each recursive call return both:

- what it already proved
- what could still be proved if more input arrives

Formally:

```text
parse_call : Subproblem -> (CompletedResults, SuspendedCalls)
```

This is the single most important change.

### Step 2: make recursive functions continuation-producing

The existing functions should be preserved structurally:

- `parse_nonterminal`
- `parse_production`
- `parse_symbols`
- `parse_symbol`
- `parse_regex`

But each now returns a richer object, conceptually:

```text
ParseStep<T> =
  { complete : Vec<T>
  , open     : Vec<SuspendedCall>
  }
```

This is still recursive descent; it just returns open continuations too.

### Step 3: terminal suspension

When `parse_regex` reaches end-of-prefix, do not merely emit a partial terminal.
Also emit a suspended terminal continuation with:

- remaining regex derivative
- binding
- absolute position
- owning call chain

This is the canonical frontier seed.

### Step 4: symbol-sequence suspension

When `parse_symbols` has matched a prefix of the RHS and the next symbol cannot
be decided only because input ended, emit a `SuspendedCall` containing:

- remaining RHS symbols
- already matched children
- current absolute position
- current recursion level

This preserves the existing recursive structure while preventing recomputation of
the matched prefix.

### Step 5: nonterminal suspension

When `parse_nonterminal` reaches the boundary via one of its productions, it
should aggregate suspended calls from its children and memoize them alongside its
solved results.

This is what makes a future append reuse the old search tree.

### Step 6: define advance as continuation replay

For append-only `new_input = old_input ++ delta`:

1. reuse old `solved`
2. reuse old `forest`
3. collect `frontier`
4. retokenize full input for correctness, but only resume from the old boundary
5. replay `SuspendedCall`s against the new suffix
6. cache any newly solved states back into `solved`
7. compute new frontier

This gives chart-like behavior without abandoning recursive descent.

## Relation to chart parsing

The resulting system will behave like a chart parser in that:

- solved subproblems persist
- open boundary states persist
- growth only explores reachable continuations

But operationally it remains recursive descent, because:

- the control flow is still defined by recursive call structure
- proof construction is still along productions and symbol sequences
- depth control remains exactly where it currently lives

This is the right compromise.

## Depth discipline

Depth must remain part of subproblem identity.

Do not "optimize away" depth from memo keys or frontier states.

Reason:

- depth is the operational approximation parameter for left-recursive or far-
  reaching partial exploration
- two calls with the same nonterminal and position but different effective depth
  budgets are not equivalent for partial parsing

So the subproblem identity should become something like:

```text
ParseKey =
  { input_fingerprint_or_boundary : ...
  , nt_name                       : String
  , binding                       : Option<String>
  , abs_pos                       : usize
  , level_budget                  : usize
  }
```

Or equivalently store `level` and derive remaining budget from parser config.

The important thing is semantic equivalence, not key aesthetics.

## Correctness argument

The proof should be by conservative extension of the existing parser.

### Theorem 1: fresh parse equivalence

The new `prefix(input)` with empty prior state returns exactly the same complete
and partial results as the current parser at the same depth bound.

Proof sketch:

- the recursive rules are unchanged
- the only extension is recording suspended continuations in addition to solved
  results
- projection to complete/partial forest matches old behavior

### Theorem 2: append conservativity

For append-only growth, `advance(prefix(x), x ++ y)` is observationally equal to
`prefix(x ++ y)` at the same depth bound.

Proof sketch:

- all reusable interior calls were already explored in `prefix(x)`
- every boundary-open recursive call is represented in `frontier`
- resuming all frontier calls explores exactly the missing search space
- no solved interior result is invalidated by append-only growth

### Theorem 3: depth preservation

`advance` never exposes parse results that would be forbidden by the original
depth discipline.

Proof sketch:

- suspended calls carry their recursive level semantics forward
- resumed calls use the same depth checks as fresh calls

## Complexity target

The intended improvement is not just constant factors.

Let `p1, p2, ..., pn` be growing prefixes.

Current cost is close to:

```text
sum_i cost(parse(pi))
```

Target cost is:

```text
cost(parse(p1)) + sum_i cost(resume(frontier(pi-1), delta_i))
```

This should be asymptotically better when:

- the frontier is small relative to the full proven interior
- the grammar has substantial stable prefix structure

The plan does not promise magic for highly branching frontiers, but it should
remove full-prefix rediscovery from the cost model.

## Experimental plan

Update `cargo run exp` to compare four regimes:

1. fresh full parse for every prefix
2. same-input warm reparse
3. append-only `advance`
4. `Synthesizer::feed` backed by `advance`

Track at least:

- solved states reused
- suspended calls resumed
- new memo states created
- SPPF nodes reused
- SPPF nodes added
- cumulative time over prefixes
- frontier width over prefixes
- number of resumed terminal derivatives

These metrics fit the actual algorithm better than generic cache hits.

## Migration sequence

1. Add `parse_state.rs`, `parse_resume.rs`, `parse_memo.rs`.
2. Move current memo structs from `parse.rs` into `parse_memo.rs`.
3. Change memo entries to hold completed + suspended information.
4. Thread `ParseStep`-style results through recursive functions.
5. Add `PrefixState` and `Parser::prefix`.
6. Add `Parser::advance` for append-only growth.
7. Keep `partial` and `parse` as projections on top of `prefix`.
8. Wire `Synthesizer` to keep a `PrefixState` and prefer `advance` on monotone
   input growth.
9. Measure cumulative prefix complexity.
10. Only after validation, simplify old exact-input caches that became redundant.

## What must not happen

- do not replace recursive descent with a chart engine
- do not remove depth from the semantics
- do not make `SPPF` the only operational state
- do not introduce hidden invalidation rules
- do not make the incremental path "best effort" and unprovable

## Final criterion

The rewrite is correct only if the new parser can be described as:

"the existing recursive-descent parser, extended so that each memoized call
returns both solved results and a resumable boundary continuation, with append-
only parsing defined as replaying those continuations under the same depth
discipline."

If that sentence stops being true, the design has drifted.
