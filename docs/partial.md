# Partial parsing system

The partial parsing system is a generalized parser that tries all *valid* and *possibly valid* ways to parse a given input with a given grammar. 

Is takes a grammar input and code input, and produces what we call a **Partial AST** which is a *forest* of possible ASTs, that can contains either fully valid and parsed children or *completeable* states. It also has typing information, gathered by binding the grammar specified typing rules to the parsed nodes. 

### Fails and mismatches

The partial parser needs to ensure what we define as **correctness by completion** meaning that all of the nodes constituting a partial tree could become a valid complete AST node by adding a finite number of token. This is expressible through the Partial Production structure and the Partial symbol. A **Partial Node** is define as a node that corresponds to some non-zero amount of tokens in the input, but isnt complete by **lack of tokens**. It is defined by a produciton that is not fully matched, with a cursor indicating how many symbols have been matched. In some cases, we might have to stop mid-token. This is why we use the **Partial Symbol** which is a symbol that can match a prefix of a token.

Each production slot tracks whether it is **extensible** once satisfied. A slot is marked extensible when its underlying symbol can accept further occurrences without forcing the cursor to advance (for instance the synthetic `X*` introduced while desugaring repetitions). Extensibility flows through the parser via the `ParseResult::Match { extensible }` flag and is preserved on `Slot::Filled`. Completion uses this metadata to continue proposing tokens for repetitions even after the parent alternative is otherwise complete.

Mismatches occur when the parser encounters a token that doesnt correpsond to the symbols in a production. Mismatches should trhow errors, and mismatched trees should not be considered valid or completeable, and npt appear in the **Partial AST**. Errors should propagate up, an * AST branch* that is partial on a msimatch is an invalid branch, and should be cut until the closest upper valid point (with valid partial parralels)

## Completion System

The completion system computes the set of valid next tokens that can continue a partial parse. It operates on the Partial AST structure and returns a `CompletionSet` containing a list of `ValidToken` values.


- **`CompletionSet`**: A collection of valid next tokens, deduplicated through hashing. The type intentionally stays small: callers can iterate with `iter()` or ask whether a literal, regex pattern, or raw sample matches via `matches(&str)`. `matches` first checks derivatives against the sample text; when the caller provides a literal regex pattern (e.g. `"[a-z]+"`), we parse it back into the derivative engine and compare structurally so higher-level code can assert the presence of specific regex tokens.

### Algorithm

The completion algorithm works as follows:

1. **Extensible symbols**: For any alternative, gather FIRST sets for every slot marked extensible. This includes completed repetitions (`X*`, `X+`) that can accept additional matches without moving the cursor. Even fully complete alternatives remain productive when extensible slots exist.

2. **Incomplete alternatives**: Walk the production from the start up to the cursor.
   - Before the cursor, add FIRST sets for any earlier slot that is extensible and therefore able to accept more matches.
   - Still before the cursor, include nullable symbols that have already been satisfied; they can be skipped, so their lookahead contributes completions.
   - At the cursor, if we hold a partial symbol, forward its own completions (either by descending into a partial non-terminal subtree or by using the derivative of a terminal regex).
   - If there is no partial node at the cursor, fall back to FIRST-set traversal from the cursor onward, respecting nullable symbols to continue the lookahead.

3. **Type filtering**: Filter partial continuation and complete trees by well-typedness. Only consider continuations that can produce nodes matching the expected type at that position.

4. **FIRST set computation**: For any symbol, compute all tokens that can start that symbol:
   - Literal → the literal itself
   - Regex → The brzowski derivative (pattern if not started else computed using custom engine)
   - Expression (nonterminal) → FIRST sets of all its productions
   - Single/Group with repetition → FIRST set of inner symbol(s)

5. **Type constrains**: Type constraints are some metadata created when filtering by types, by evaluating type tules that may betattached to `Filled` or `Partial` parsed productions. They indicate what kind of constraint the next token mist satisty. Obviously they need more integration on the sampling side.

