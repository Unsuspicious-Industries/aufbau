#[D] Prefix Parsing

This chapter describes the parser as it is implemented in the [parser module](~/src/logic/parse/)

It is a mutable agenda machine over explicit Rust objects. Instead of building an AST and then running a typing pass, we fuse both processes into one system, allowing us to optimize grealty and reduce forrest explosion.

## Core Objects

>D Runtime Path
The parser runtime uses a path object
$$
\pi = [(i_0, a_0), \dots, (i_n, a_n)]
$$
where each pair means:

- $i_k$: child index taken at that step
- $a_k$: alternative chosen by the parent at that step

This is an implementation-level grammar path. It matches the `i@k` discipline from [Binding Resolution](../concepts/binding.md), not the child-index-only tree paths from [Partial Trees and Forests](../concepts/trees.md).
<

>D Item
An `Item` is the live parser state:
$$
I = (prod, dot, start, pos, sigma_{in}, sigma, node\_path, children, child\_sigmas)
$$
with Rust fields:

- `prod: ProdId`
- `dot: usize`
- `start: usize`
- `pos: usize`
- `sigma_in: TypingState`
- `sigma: TypingState`
- `node_path: Option<Box<Path>>`
- `children: Vec<ChildRef>`
- `child_sigmas: Vec<TypingState>`

Interpretation:

- `prod`, `dot`, `start`, `pos` are the syntactic dotted-production position
- `sigma_in` is the typing state at entry of the current node
- `sigma` is the current typing state after the already accepted children of this same node
- `node_path` is the runtime path of the current node
- `children` are the accepted children collected so far
- `child_sigmas` are the child exit states collected so far
<

>D Completion
A `Completion` is:
$$
C = (nt, start, end, node)
$$
where `node` is the completed arena node used to resume waiting parents.
<

>D Waiter
A `Waiter` is:
$$
W = (item, descended, path)
$$
where:

- `item` is the suspended parent item
- `descended` is the child-entry typing state returned by `descend`
- `path` is the runtime path of the child currently being waited on
<

>D Task
The agenda stores two task forms:

- `Process(Item)`
- `Complete(Completion)`
<

>D Parser Tables
The parser tables are:

- `agenda: VecDeque<Task>`
- `seen_process: HashSet<(ProdId, usize, usize, usize)>`
- `seen_complete: HashSet<(NtId, usize, usize)>`
- `results: HashMap<(NtId, usize), Vec<usize>>`
- `completed_nodes: HashMap<(NtId, usize, usize), Vec<NodeId>>`
- `waiters: HashMap<(NtId, usize), Vec<Waiter>>`
- `frontier: Vec<Item>`

Interpretation:

- `seen_process` deduplicates syntactic items by `(prod, dot, start, pos)`
- `seen_complete` deduplicates completion wakeups by `(nt, start, end)`
- `results[(nt, start)]` stores reachable segment end positions
- `completed_nodes[(nt, start, end)]` stores all nodes realized at that span
- `waiters[(nt, pos)]` stores parents blocked on `nt` starting at `pos`
- `frontier` stores items blocked only by end-of-input
<

---

## Path Operations

The parser has two local path helpers.

### `extend_path(parent, child, alt)`

This copies `parent` and appends `(child, alt)`.

So if the current node path is `π`, then crossing child `i` of alternative `a` creates the child path:

$$
\pi' = \pi \cdot (i, a)
$$

### `descend_binding_path(current_path, binding_path)`

This removes the prefix `current_path` from `binding_path` if possible.

It returns:

- `None` if `current_path` is not a prefix of `binding_path`
- `Some(rest)` if `binding_path = current_path ++ rest`

Important actual behavior:

- if `binding_path == current_path`, then `rest = []`
- this exact-node case matters for `prune`

---

## Dedup Semantics

### Process Dedup

`enqueue_process(item)` computes key:

$$
(item.prod, item.dot, item.start, item.pos)
$$

and inserts the task only if that key has not been seen before.

So the parser deliberately ignores `sigma`, `node_path`, `children`, and `child_sigmas` when deduplicating process work.

This is a real implementation choice, not a generic parsing fact.

### Completion Dedup

`enqueue_complete` does not deduplicate by itself.

Instead, `complete(completion)`:

1. always records the node in `completed_nodes`
2. consults `seen_complete` to decide whether waiter wakeup should happen

So:

- node recording is append-only
- completion propagation is once per span key

---

## The Role of `TypingState`

The parser does not treat runtime state as opaque in the specification sense. It carries a concrete `TypingState` through all steps.

At parser level, the important fields are:

- `ctx`
- `expected`
- `inferred`
- `path`
- `bindings`

The parser itself mostly delegates meaning to the typing runtime, but it does rely on two concrete facts:

- `sigma.path` is the current runtime path
- `sigma.bindings` is the data used by `prune`

So the parser and typing runtime are coupled at child boundaries and at seeding time.

---

## Pruning

Before seeding nonterminal `nt`, the parser runs `prune(sigma, nt)`.

The implementation computes:

1. all bindings `b` with `b.nt == nt`
2. for each such binding, `descend_binding_path(sigma.path, b.path)`
3. collect the returned descended suffixes into `paths`

Then:

- if `paths` is empty, return all productions of `nt`
- otherwise, read the first step of each descended path and extract its `alt`
- keep only those alternatives
- if `paths` is non-empty but every descended path is empty, panic

So `prune` is currently partial.

Its hidden precondition is:

> whenever relevant bindings exist for `nt`, at least one of them must descend to a non-empty suffix relative to `sigma.path`.

This is exactly why the current repros fail: bindings that refer to the current node itself descend to `[]`, yielding no alternative witness.

---

## Seeding

`seed(nt, pos, sigma)` performs:

1. `for prod in prune(sigma, nt)`
2. enqueue a fresh `Item` with:
   - `dot = 0`
   - `start = pos`
   - `pos = pos`
   - `sigma_in = sigma.clone()`
   - `sigma = sigma.clone()`
   - `node_path = sigma.path.clone()`
   - empty `children`
   - empty `child_sigmas`

So a seeded item starts at a fresh syntactic position but inherits the caller's current runtime path and typing state.

There is no separate implementation function corresponding to an abstract runtime `enter` relation. Child entry is realized later, when a specific child boundary is crossed and `prepare_child + descend` runs.

---

## `process(item)`

`process` is the main transition function.

It first loads the production `grammar.prod(item.prod)`. Then it splits into three cases.

### Case 1: Finished Item

If:

$$
item.dot = |rhs(item.prod)|
$$

then `process` calls `finish(item)`.

If `finish` returns a node, `process` enqueues a `Completion` for that node.

### Case 2: Terminal Next Symbol

If `rhs[item.dot]` is a terminal, `process` delegates to `consume(item, regex, symbol)`.

### Case 3: Nonterminal Next Symbol

If `rhs[item.dot]` is a nonterminal `nt_child`, `process` does all of the following in order:

1. compute `binding = symbol.binding()`
2. call `typing.prepare_child(item.prod, item.dot, binding, &item.sigma, &item.child_sigmas)`
3. compute child path `path = extend_path(item.node_path, item.dot, item.prod.1)`
4. call `typing.descend(&prep, &path, nt_child, binding)`
5. store `Waiter { item, descended, path }` in `waiters[(nt_child, item.pos)]`
6. resume immediately against already-known results in `results[(nt_child, item.pos)]`
7. seed fresh parsing work with `seed(nt_child, item.pos, &descended)`

That is the real control-flow heart of left recursion and memoized nonterminal calls.

---

## `consume(item, regex, symbol)`

`consume` implements the terminal step.

It does:

1. if `item.pos == segs.len()`, push `item` to `frontier` and stop
2. call `typing.prepare_child`
3. compute child path with `extend_path(item.node_path, item.dot, item.prod.1)`
4. call `typing.descend(&prep, &path, item.prod.0, binding)`
5. match current segment with `regex.prefix_match(segment.as_str())`
6. reject on `NoMatch` or `Prefix`
7. on `Complete` or `Extensible`, call `typing.finish_terminal_child`
8. build the next `Item` with:
   - `dot + 1`
   - `pos + 1`
   - updated `sigma`
   - appended terminal child
   - appended child sigma

Two implementation details matter here:

- `Prefix` is rejected because parsing is segment-based, not character-incremental within a segment
- `descend` happens before the regex result is known, so child-entry state exists transiently even for branches that will later be dropped

---

## `complete(completion)`

`complete` performs completion propagation.

Given `completion = (nt, start, end, node)` it:

1. appends `node` to `completed_nodes[(nt, start, end)]`
2. checks whether `(nt, start, end)` is first-seen in `seen_complete`
3. if first-seen:
   - insert `end` into `results[(nt, start)]` if missing
   - clone `waiters[(nt, start)]`
   - for each waiter:
     1. load the completed arena node
     2. call `typing.finish_node_child(&waiter.descended, waiter.path, &node, &node.bindings, segs)`
     3. resume the parent item with `dot + 1`, `pos = end`, appended child node, and appended child sigma
     4. enqueue the resumed parent item

So completion has two distinct effects:

- persistent fact storage in `completed_nodes` and `results`
- one-time operational wakeup through `seen_complete`

---

## `finish(item)` and `finalize(...)`

`finish(item)` computes whether the current partially built node should be `Complete` or `Partial` based on its children, then calls `finalize(...)` with a single branch.

`finalize(...)` then iterates candidate branches and calls:

$$
\mathrm{typing.finish\_production}((nt, alt), sigma_{in}, child\_sigmas, status)
$$

A branch contributes an arena node only if:

- `finish_production` succeeds, and
- `out.inferred` is `Some(ty)`

The resulting `ArenaNode` stores:

- `nt`
- `span` in byte offsets
- `status`
- `ty`
- `env_in = sigma_in.ctx`
- `env_out = out.ctx`
- `bindings = out.bindings`

So the parser's arena contains only typed nodes. Partial nodes are allowed, but untyped nodes are not.

---

## Frontier and Partial Parsing

`frontier` stores items that reached end-of-input while still expecting a terminal.

After the main agenda loop drains, `parse` enters a second phase:

1. iterate over current frontier items
2. try `finish(item)` on each one
3. enqueue resulting completions
4. drain the agenda again
5. repeat until no new progress occurs

So partial parsing is not a separate parser. It is the same completion machinery applied after terminal progress has been blocked by EOF.

---

## `parse(input, ctx)`

The full top-level flow is:

1. tokenize `input` into segments
2. reset `arena` and `tables`
3. compute `start_nt`
4. create `sigma0 = TypingState::new(Some(ctx), None)`
5. seed all start productions allowed by `prune(&sigma0, start_nt)`
6. drain `agenda`
7. run frontier lifting to saturation
8. read roots from `completed_nodes[(start_nt, 0, segs.len())]`
9. if no root exists, return `Err("no parse found")`
10. otherwise materialize `FusionAST`

---

## Actual Correspondence With Concepts

The spec-source match should be read as follows.

- the concepts chapter's tree paths are not the parser runtime path object
- the binding chapter's grammar-path alphabet `i@k` is the right conceptual match for parser `Path`
- the parser object corresponding to a live derivation is `Item`, not an abstract dotted rule alone
- suspended recursion is represented concretely by `Waiter`
- completed nonterminal facts are represented concretely by `Completion`, `results`, and `completed_nodes`
- pruning is not an abstract black-box relation; it is a concrete computation over `TypingState`, `binding.nt`, and descended runtime paths

That is the implementation the rest of the system actually depends on.

For reusable proof obligations, see [Parser Correctness Lemmas](./correctness.md).
