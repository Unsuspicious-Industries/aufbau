# Partial parsing system

The partial parsing system is a generalized parser that tries all *valid* and *possibly valid* ways to parse a given input with a given grammar. 

Is takes a grammar input and code input, and produces what we call a **Partial AST** which is a *forest* of possible ASTs, that can contains either fully valid and parsed children or *completeable* states. It also has typing information, gathered by binding the grammar specified typing rules to the parsed nodes. 

### Fails and mismatches

The partial parser needs to ensure what we define as **correctness by completion** meaning that all of the nodes constituting a partial tree could become a valid complete AST node by adding a finite number of token. This is expressible through the Partial Production structure and the Partial symbol. A **Partial Node** is define as a node that corresponds to some non-zero amount of tokens in the input, but isnt complete by **lack of tokens**. It is defined by a produciton that is not fully matched, with a cursor indicating how many symbols have been matched. In some cases, we might have to stop mid-token. This is why we use the **Partial Symbol** which is a symbol that can match a prefix of a token.

Mismatches occur when the parser encounters a token that doesnt correpsond to the symbols in a production. Mismatches should trhow errors, and mismatched trees should not be considered valid or completeable, and npt appear in the **Partial AST**. Errors should propagate up, an * AST branch* that is partial on a msimatch is an invalid branch, and should be cut until the closest upper valid point (with valid partial parralels)

## Completion System

The completion system computes the set of valid next tokens that can continue a partial parse. It operates on the Partial AST structure and returns a `CompletionSet` containing a list of `ValidToken` values.


- **`CompletionSet`**: A collection of valid next tokens, deduplicated and sorted. Provides helper methods like `contains_literal()` and `contains_regex()`. These helpers rely on the derivative engine's `Regex::contains_regex(&self, other)` check, which runs a derivative-based emptiness search rather than string sampling so we can safely compare complex classes like `[a-z]+`.

### Algorithm

The completion algorithm works as follows:

1. **Complete alternatives**: If an alternative is complete (all symbols satisfied), check if the last symbol has tail repetition (`+` or `*`). If so, add the FIRST set of that symbol to completions.

2. **Incomplete alternatives**: 
   - If there's a partial symbol in progress at the cursor, suggest completions for that symbol
   - Otherwise, compute the FIRST set of the next symbol at the cursor position

3. **Type filtering**: Filter partial continuation and complete trees by well-typedness. Only consider continuations that can produce nodes matching the expected type at that position.

4. **FIRST set computation**: For any symbol, compute all tokens that can start that symbol:
   - Literal → the literal itself
   - Regex → The brzowski derivative (pattern if not started else computed using custom engine)
   - Expression (nonterminal) → FIRST sets of all its productions
   - Single/Group with repetition → FIRST set of inner symbol(s)

5. **Type constrains**: Type constraints are some metadata created when filtering by types, by evaluating type tules that may betattached to `Filled` or `Partial` parsed productions. They indicate what kind of constraint the next token mist satisty. Obviously they need more integration on the sampling side.

