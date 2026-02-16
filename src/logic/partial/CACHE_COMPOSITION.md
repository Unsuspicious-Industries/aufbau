# Partial Parser Cache: Composition Properties

This document states the minimal properties we need to prove (or enforce by construction)
to make a compositional memo cache correct.

The goal: cache complete subtrees for a nonterminal `N` over a delimited sub-input, and
reuse them as building blocks inside larger parses without changing the resulting forest.

## 1. Canonical Input Model

The parser operates on a tokenized sequence, not raw bytes.

- Let `S` be a *canonical* segment sequence.
- Define `canon(input: &str) -> S` such that if two strings are equivalent for parsing,
  they map to the same `S`.

Properties needed:

1) Canonicalization Determinism
   - For any `input`, `canon(input)` is deterministic.

2) Concatenation Compatibility
   - For any strings `a`, `b`, there exists a segment-sequence operation `++` such that:
     `canon(a + b) = canon(a) ++ canon(b)`.
   - If this does not hold for raw strings (whitespace, special tokens), define a
     normalized segment domain where it does.

3) Substring/Span Extraction Soundness
   - Every subtree we cache must correspond to a contiguous span of `S`.
   - The cache key must be derived from that span (not from the full remaining input).

## 2. Parse Judgement With Consumption

Define a judgement that makes consumption explicit:

- `parse(N, S, i) = { (t, k) }` meaning:
  - `t` is a (complete) `NonTerminal` tree rooted at `N`
  - `t` consumes `k` segments
  - `t` is built from the slice `S[i .. i+k]`

We need:

4) Locality (No Lookahead Beyond Span)
   - If `(t, k) ∈ parse(N, S, i)`, then validity of `(t, k)` depends only on `S[i .. i+k]`.
   - In particular, parsing `N` must not depend on the suffix `S[i+k ..]`.
   - Any feature that violates this (e.g., "must be end-of-input") must be represented
     explicitly in the grammar/tokenizer and thus in `S`.

5) Completeness Criterion
   - Only cache results where `t` is complete for `N` and `k > 0` (or allow `k = 0` only
     if the grammar production is epsilon and this is handled carefully).

## 3. Symbol-Sequence Composition ("Legos")

For a production RHS `X1 X2 ... Xm`, define parsing as composition of component parses.

Let `parse_seq([X1..Xm], S, i) = { ([n1..nm], K) }` where `K = sum(consumed(nj))`.

We need a composition theorem:

6) Sequential Composition Soundness
   - Parsing `X1..Xm` is equivalent to:
     - choose `(t1, k1) ∈ parse(X1, S, i)`
     - then choose `(t2, k2) ∈ parse(X2, S, i+k1)`
     - ...
     - total `K = k1 + ... + km`
   - The combined tree is formed by concatenating children in order.

7) Associativity of Composition
   - For any split point `j`:
     `parse_seq([X1..Xm], S, i)` equals composing:
     - `parse_seq([X1..Xj], S, i)` with `parse_seq([X{j+1}..Xm], S, i+K_left)`.
   - This is the formal reason caching a subtree for `X1..Xj` can replace recomputation.

8) Nonterminal Replacement Lemma
   - If `(t, k) ∈ parse(N, S, i)`, then in any larger derivation where `N` is parsed at
     `(S, i)`, we may replace the computed instance with cached `(t, k)` without changing:
     - accepted/rejected status
     - the set of resulting complete trees
     - consumption counts

## 4. Cache Key Correctness

Given the above, a correct cache entry must be keyed by:

- Nonterminal name `N`
- A delimited sub-input `span = S[i .. i+k]` OR an equivalent canonical representation
  that uniquely identifies that span
- The recursion/depth parameters that affect the result set

Properties:

9) Key Uniqueness
   - If two cache keys are equal, the parse result sets must be identical.

10) Prefix Matching Safety (If Used)
   - If you match by prefix (span-prefix), you must prove that the cached result set is
     exactly the parse result set for that shorter span.
   - Reusing prefix results for a longer span is only valid when the consumer requests
     parse results for the *shorter* span.
   - If the consumer requests parse results for the *longer* span, prefix reuse must be
     coupled with a proof that no additional parses exist (rare), or with a mechanism to
     compute the delta (otherwise it changes the forest).

## 5. Depth / Termination Parameters

The implementation uses depth limits to terminate on ambiguous/left-recursive grammars.
Depth is part of the semantics.

11) Depth-Parametric Semantics
   - Let `parse_d` be parsing under a depth budget `d`.
   - The cache must never reuse results computed with a different `d` unless you prove
     equality of result sets.
   - Safe default: only reuse cache entries with the exact same depth budget.

12) Depth-Limit Contamination
   - If a parse path hit a depth limit, its intermediate results are not guaranteed to be
     complete. Such results must not be cached.

## 6. What We Must Implement To Match The Proof

To make the above actually true in code, we need:

- A canonical segment representation `S` suitable for span keys.
- Cache keys based on delimited spans (the subtree input), not "remaining input".
- Cache entries store `(tree, consumed_len)` pairs for `N` over that span.
- Cache reuse replaces computation only when the requested parse domain matches the key.

Non-goals (until proven):

- Using prefix matches to answer queries on longer inputs without computing the delta.
- Caching partial/incomplete subtrees.
