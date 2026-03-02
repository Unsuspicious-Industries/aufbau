#[D] Completion Set

After a partial parse, the **completion set** identifies the set of regex patterns that could legally appear as the next token — the frontier of what the grammar will accept.

Source: [`src/logic/partial/completion.rs`](~/src/logic/partial/completion.rs)

## CompletionSet

>D CompletionSet
A `CompletionSet` is a deduplicated set of `Regex` values:

$$\mathcal{C} \subseteq \{\text{Regex}\}$$

Each element describes a class of strings that, if appended to the current input, would produce a syntactically valid continuation. The set is computed from the partial forest, not from the grammar directly.
<

`CompletionSet::new` deduplicates using a `HashSet`. `cleanup` additionally removes nullable regexes (those matching the empty string), since an empty token is not a meaningful completion.

## Computing Completions

`PartialAST::completions(grammar)` iterates over the roots of the partial forest, calls `NonTerminal::collect_valid_tokens` on each, and collects the union into a `CompletionSet`.

### Collecting Tokens from a Nonterminal

`NonTerminal::collect_valid_tokens` traverses the partial tree's frontier. It distinguishes two cases:

**Complete nonterminal.** The tree is already fully parsed. The only valid "next tokens" come from extending the last child's terminal if it carries an extension derivative — a token that matched completely but could accept more characters (e.g., identifier `x` could extend to `xy`).

**Partial nonterminal.** The tree has an incomplete frontier. The last child determines the tokens:

- If the last child is a `Terminal::Partial` with a remainder derivative $r$: that remainder $r$ is the token needed to complete the match. If the partial terminal's value is empty (we haven't typed anything for this position yet), extensions from the previous child are also included.
- If the last child is an incomplete nonterminal: recurse into it.
- If the last child is a complete terminal (possibly extensible) or complete nonterminal: include any extension from the last child, then compute the FIRST set of the next symbol in the production RHS.

<!-- DIAGRAM: partial tree with frontier annotated, showing which branch of collect_valid_tokens fires at each node type -->

## FIRST Set

>D FIRST Set
`first_set(symbol, grammar)` computes the set of regexes that can begin a match of `symbol`:

- For a terminal $r$: $\{r\}$.
- For a nonterminal $A$: $\bigcup_{p \in A} \text{FIRST}(\text{head}(p))$, where $\text{head}(p)$ is the first symbol of production $p$.

Cycles (left-recursive nonterminals) are handled by a `visited` set — if $A$ is already in `visited`, recursion on $A$ returns $\emptyset$ immediately.
<

`first_set` is used when the frontier of a partial tree reaches the boundary between an already-parsed child and the next expected symbol. The completion engine needs to know which tokens can begin that next symbol.

>N FIRST is not FOLLOW
The completion set does not compute FOLLOW sets. A partial tree always carries explicit information about which production is active and how many symbols have been filled, so the next expected symbol is always known concretely from `production.rhs[children.len()]`. There is no need for grammar-global FOLLOW computation.
<

## Interaction with Typing

`CompletionSet` is a syntactic object — it knows nothing about types. The [synthesizer](../completion/synthesizer.md) layered on top of it adds type filtering: for each token in the completion set, it attempts to extend the input and type-check the result, discarding tokens that produce no well-typed tree.

The viz server's `/analyze` endpoint exposes both `completions` (type-filtered) and `all_completions` (raw syntactic set) separately, so callers can see what type filtering discarded.

## `CompletionSet::matches`

`matches(text)` checks whether a given string is consistent with any token in the set — useful for validation. It applies `prefix_match` from the regex engine and accepts `Extensible`, `Complete`, or `Prefix` outcomes.
