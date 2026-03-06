#[D] Validation

The **validation** system verifies that Aufbau's engines produce correct results. It is the empirical counterpart to the formal properties defined in [Completability](./completability.md): where the spec defines *what* must hold, the validation system checks *that* it holds on concrete grammars and inputs.

Source: [`src/validation/`](~/src/validation/)

## Architecture

The validation system has three layers:

```
Layer 3: Test Suites (per-grammar: arithmetic, stlc, fun, imp, ...)
    │
    ▼
Layer 2: Test Harness (typed completion test runner, timeout, profiling)
    │
    ▼
Layer 1: Core Algorithms (completability.rs → search → scoring)
```

**Layer 1** is the [completion pipeline](../completion.md) and the [completability checker](./completability.md), i.e., the same algorithms used in production. The validation system does not use separate "test" algorithms; it validates the real pipeline.

**Layer 2** is a test harness that wraps Layer 1 with:
- Per-test configuration (depth budget, initial context, expected pass/fail)
- Timeout enforcement (default: 300 seconds per test)
- Prefix soundness checking (verifying *all* prefixes, not just the final input)
- Profiling metadata (timing, states explored, visited states)

**Layer 3** is a collection of test suites, each targeting a specific grammar from `examples/*.auf`.

## What is Validated

### Prefix Soundness

The primary property validated is [prefix soundness](./completability.md): for a given input $s$ and grammar $G$, *every prefix* of $s$ must be typed-completable. This is checked by `sound_complete`, which:

1. Enumerates all non-whitespace prefixes of $s$
2. For each proper prefix: parses it, computes typed completions, and calls `prefix_ok`
3. For the full input: runs the complete search to find a valid completion
4. Reports the first failing prefix (if any)

A completion system that can find completions for `let x : int in x` but *not* for `let x : int in` is broken: the system would accept a token that leads to a dead end. Prefix soundness catches this class of bugs by testing every intermediate state.

### Expected Failures

Tests can be marked `xfail` (expected to fail completion). This is used for inputs that are *intentionally* outside the grammar's typed language. An `xfail` test passes if completion fails, and fails if completion unexpectedly succeeds.

### Timeout Enforcement

Each test runs in a dedicated thread with a configurable timeout. This catches performance regressions and infinite loops in the search: if the search cannot find a completion within the time budget, the test fails with a timeout diagnostic.

## Test Structure

Each grammar has a dedicated test module:

| Grammar | Description | Key Properties Tested |
|---------|-------------|----------------------|
| `arithmetic` | Integer arithmetic with `+`, `*`, parentheses | Basic completability, operator precedence |
| `stlc` | Simply-typed lambda calculus | Variable binding, function application, type inference |
| `fun` | Let-bindings, if-then-else, functions | Context extension, nested scopes, keyword separators |
| `imp` | Imperative language (assignments, while loops) | Statement sequencing, mutable state |

## Parseability Checking

A separate, faster validation layer checks **parseability**: whether all prefixes of an input can be parsed at all (without completion search). This runs in $O(n^2)$ time (one parse per prefix) and catches parser bugs independently of the type system.

## Complexity Analysis

The validation system includes an empirical complexity measurement module that determines the parse-time exponent $k$ in $O(n^k)$ via log-log linear regression over increasing input sizes. This is used to detect performance regressions and to empirically verify that the parser operates within expected complexity bounds.
