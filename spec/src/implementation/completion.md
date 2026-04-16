#[D] Completion and Search

The completion engine runs an informed best-first search from a partial parser frontier, attempting to find a syntactically valid and well-typed sequence of tokens that satisfy the grammar up to some acceptance condition.

## The Search State

The core search state is defined recursively as parsing paths.

>D Search State
A search state is a tuple $S = (I, PS, c)$ where:
- $I \in \Sigma^*$ is the prefix string formed by the concatenation of all segments.
- $PS$ is the parser state resulting from advancing the prefix string.
- $c$ is the typing context environment $\Gamma$ inherited from the initial problem.
<

## Acceptance Condition

Since the underlying incremental parser $PS$ accumulates a hypergraph of partial and complete nodes, the completion search requires a strict criterion to declare a path "complete."

>D Complete State Acceptance
A search state $(I, PS, c)$ is accepted as a valid completion if and only if reparsing $I$ from scratch with the start symbol produces an AST containing a root node that covers the entire input $I$ and has $\text{status} = \text{Complete}$.
<

This is enforced by the `verified_success` reparsing gate in the search loop. A branch merely having $NodeStatus::Complete$ at the frontier is not sufficient if there are lingering unmatched right-hand sides in higher-level rules.

## Synthesizer Contract

The completion engine uses the parser's expected frontier to synthesize possible next tokens. The synthesizer interface operates under a strict two-stage contract.

>D Synthesizer Hint Set
$\text{tokens\_with}(PS) \to \{ R_1, \dots, R_k \}$ returns a finite set of regex constraints representing the lexical shape of valid next segments at the frontier of $PS$. This is an over-approximation (hint set).
<

>D Synthesizer Feed Gate
$\text{feed}(R) \to \{ s_1, \dots, s_n \}$ generates concrete literal strings $s_i \in \mathcal{L}(R)$ that satisfy the regex $R$. 

The search engine constructs the next state by appending $s_i$ to $I$ and computing $\text{Advance}(PS, s_i)$. The transition is sound because the parser's incremental typing rules act as the final gatekeeper, discarding any string $s_i$ that is grammatically or semantically invalid.
<

## State Normalization and Deduplication

To prevent combinatorial explosion, especially when different syntactic derivations produce the same visible string prefix, the search space must be quotiented.

>D Visited State Equivalence
Two search states $S_1 = (I_1, PS_1, c_1)$ and $S_2 = (I_2, PS_2, c_2)$ are considered equivalent if $I_1 = I_2$.
<

The search algorithm normalizes the state by removing trailing whitespace before adding it to the `visited` set. If a normalized input string $I$ has been seen before on *any* path, the current branch is pruned. This sacrifices proof of shortest-path optimality in exchange for practical termination, as structural ambiguity inside $PS$ is collapsed into lexical prefixes.
