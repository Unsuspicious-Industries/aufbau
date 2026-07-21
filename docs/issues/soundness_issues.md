# Soundness Issues

This file tracks deep structural and soundness issues discovered during development. 

## Issue 1: `extend()` failing on candidates from `gather()`

**Date:** 2026-04-07
**Component:** Search / Fusion Parser

### Description
The `extend()` function is meant to take a known-good prefix, append a valid candidate token string, and return the new parser state. By definition of the system's soundness, if `view.completions()` and `gather()` suggest a candidate string, appending that string **must** result in a valid parse state. 

However, during BFS search testing on a simple right-recursive grammar (`A ::= 'a' A | 'b'`), `extend()` failed with:
`TypedPrefixError { input_len: 9, depth: DepthMeta { searched_depth: 7, hit_depth_limit: false, depth_failures: 0 }, message: "no typed branches survived" }`

### Why this is a structural problem:
1. **Completability Soundness Violation**: The candidate generator (`gather`) is completely out of sync with the parser (`TypedParser`). `gather` claims the token is a valid continuation, but the parser rejects it.
2. **False error message**: The grammar is untyped, yet the error message says "no typed branches survived" and `hit_depth_limit` is false. This indicates a masking of the true parse failure or a bug in how `TypedParser` reports culling.
3. **State divergence**: If `extend()` falls back to `fresh.parse(&next_input)` instead of properly using `advance()`, it risks losing incremental state or hitting depth limits differently than the incremental `advance()` would. 

### Current Workarounds (Needs removal/fix):
- In `bfs.rs`, we are manually resetting the parser depth budget via `parser.with_max_depth(parse_depth_cap)` before returning the initial state. This masks the issue by giving the parser more room, but doesn't fix why `extend` fundamentally rejects valid grammatical continuations.
- In both `bfs.rs` and `mod.rs`, `extend` returning `Err` is caught and `continue` is called (ignoring the candidate). This hides the soundness violation. A sound system should ideally be able to `unwrap()` or `expect()` the result of `extend()` if the candidate came from `gather()`.

### Next Steps for Resolution:
1. Investigate *why* `TypedParser::advance` and `TypedParser::parse` reject the string `'a a a a b'` when the prefix was `'a a a a'`. 
2. Fix the error reporting in `TypedPrefixError` to accurately reflect syntactic vs typing failures.
3. Once fixed, `extend()` should probably panic or log a severe warning if it fails on a candidate generated internally, as this proves a divergence between the grammar graph and the parser execution.