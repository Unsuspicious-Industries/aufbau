## Partial parser: design and behavior

This document explains how the partial parser in `p7` works: the data model it builds, how it walks the grammar, what “partial” means, how repetition is handled, how type guidance prunes branches, and how to consume its API for completion and full parsing.

### Top-level pipeline

- **Tokenizer to segments**: The parser first tokenizes input using `Tokenizer::tokenize_with_spans`, then converts token char-spans into byte spans and `Segment` values: `(text, start, end)`.
- **Start symbol**: It reads the grammar start nonterminal via `Grammar::start_nonterminal()`.
- **Nonterminal parse**: It parses from segment position 0 with `PartialParser::partial_nt(segments, start, 0, None)`.
- **Root aggregation**: It aggregates alternative productions into a single `PartialAST` whose root is `PartialASTNode::NonTerminal(Vec<PartialNonTerminal>)`.
- **Optionally finalize**: `PartialAST::into_complete()` converts to a complete `ASTNode` when the parse is strictly complete and consumes the entire input.

### Core data structures

- `Segment { text, start, end }` tracks a token’s text and byte-span into the original input. `Segment::span()` returns a `SourceSpan`.
- `ParseOutcome` communicates control flow when parsing any symbol or sequence:
  - `Full { nodes, next_pos, symbols_parsed }`: Successfully parsed; `nodes` are AST fragments; `next_pos` is the segment cursor after consumption.
  - `Partial { full_nodes, partial_nodes, next_pos, symbols_parsed }`: Right-completable state (input ended or prefix matched). `full_nodes` are the fully built left side so far; `partial_nodes` are in-progress fragments.
  - `Error { next_pos, symbols_parsed }`: Mismatch that cannot be completed just by adding more tokens to the right at the current position.
- Partial AST model (optimized for visualization and completion):
  - `PartialAST { root, input }` wraps the root and the entire input string for completeness checks and extracting consumed slices.
  - `PartialASTNode` is either `Terminal(PartialTerminal)` or `NonTerminal(Vec<PartialNonTerminal>)` (parallel alternatives of one nonterminal).
  - `PartialNonTerminal` keeps: chosen `production` as a `PartialProduction` (with progress cursor), accumulated `children`, original nonterminal `value`, node `span`, optional `binding`, and optional `bound_typing_rule`.
  - `PartialProduction` mirrors a `Production` plus two integers: `fully_parsed_symbols` and a clamped `partially_parsed_symbols ∈ {0,1}`. These are used to compute a virtual cursor and expose an “in-progress” symbol.

### Parser entry points

- `PartialParser::new(grammar)`: initializes the parser and a `Tokenizer` with the grammar’s special tokens and default whitespace.
- `PartialParser::set_type_guidance(nonterminal, rules, expected_type)`: optional pruning that keeps only production branches compatible with the expected `Type` under any of the given `TypingRule`s. Guidance is applied per nonterminal name during parsing.
- `PartialParser::partial(input) -> Result<PartialAST, String>`: tokenizes, parses the start nonterminal, and merges all parallel branches into the `PartialAST` root. Returns an error only for unrecoverable start failures.
- `PartialParser::parse(input) -> Result<ASTNode, String>`: calls `partial` then enforces full-consumption and strict completeness, converting to the complete AST.

### How symbols are parsed

Parsing is segment-based and strictly left-to-right; the parser never scans ahead within a segment.

- `Symbol::Litteral(s)`
  - Matches exactly the next `Segment.text`.
  - If it mismatches but we are at the final segment and `segment.text` is a non-empty prefix of `s`, returns `Partial` (can be completed by typing more).
  - Otherwise returns `Error` at the current position.

- `Symbol::Regex(re)`
  - Requires a full segment match (from 0 to segment.length).
  - At the last segment, if the segment could be extended to match (approximate prefix test using a synthetic extension), returns `Partial`; else `Error`.

- `Symbol::Expression(nonterminal)`
  - Recursively calls `partial_nt` for that nonterminal at the current position.
  - Propagates `Full`/`Partial`/`Error` transparently.

- `Symbol::Single { value, binding, repetition }`
  - Parses `value` as a single symbol, optionally binding it to a name.
  - If `repetition` is present, delegates to repetition-aware sequence parsing (see below).

- `Symbol::Group { symbols, repetition }`
  - Parses an inline sequence of `symbols` as a group. With `repetition`, also delegates to repetition parsing.

### Sequences and repetition

There are two central helpers for sequences:

- `parse_symbols_once(segments, symbols, seg_pos, binding)` parses a list of symbols exactly once in order and returns a `ParseOutcome` for that one pass.
  - It accumulates `nodes` and advances `current_pos` on successful sub-steps.
  - When a sub-step returns `Partial`, the sequence prefers and keeps the fully parsed left side (`full_nodes`) and resumes from `next_pos` so later symbols still have a chance to make the entire sequence `Full`.
  - If a sub-step returns `Error`, the sequence returns `Error` immediately, carrying any `symbols_parsed` count accumulated.

- `parse_symbols_repetition(segments, symbols, seg_pos, binding, rep_kind)` handles `ZeroOrMore`, `OneOrMore`, and `ZeroOrOne` for either a single symbol or a group.
  - `ZeroOrMore`: loops `parse_symbols_once` until it no longer makes progress. If an inner `Partial` occurs, returns `Partial` with two branches encoded as `full_nodes` (skip) and `partial_nodes` (in-progress attempt).
  - `OneOrMore`: requires the first iteration to succeed; otherwise produces `Partial` indicating zero matches so far. Subsequent repetitions are handled by delegating to `ZeroOrMore`.
  - `ZeroOrOne`: tries once; failure yields `Full` with zero nodes at the same position; `Partial` propagates upward with the in-progress nodes.

In all cases, `next_pos` denotes the segment position reached by the successful portion of the parse. Repetition ensures termination by breaking on no-progress iterations.

### Parsing nonterminals and parallel branches

`partial_nt(segments, nt, seg_pos, binding)`:

- Retrieves all productions for `nt`.
- For each production, parses the entire RHS as a single sequence via `parse_symbols_once`.
- Records a `PartialNonTerminal` with:
  - `PartialProduction::from_progress(production, fully_symbols_parsed, partially_symbols_parsed)`
  - `children` produced so far
  - `span` from the first consumed segment start to last consumed segment end (when any progress was made)
- Tracks the furthest `next_pos` among fully parsed alternatives.
- Applies optional type-guidance pruning to both complete and partial branches.
- Returns:
  - `Full` with all complete alternatives (plus any partial alternatives appended) if at least one production is complete for this `nt`.
  - Else `Partial` with partial alternatives if any progress occurred.
  - Else `Error` with no progress at `seg_pos`.

At the parser root (`partial`), complete alternatives are merged into a single `PartialASTNode::NonTerminal` that lists all parallel branches.

### Strict completeness and conversion to complete AST

Completeness uses a stronger notion than “RHS fully traversed”: it also requires meaningful content and literal presence.

- `PartialProduction::is_finished()` is true when `fully_parsed_symbols == rhs.len()` and `partially_parsed_symbols == 0`.
- `PartialProduction::complete(children_count)` additionally requires `children_count > 0` to avoid empty nonterminals.
- `PartialNonTerminal::is_strict_complete()` checks:
  - Production finished, and
  - All literal `Symbol::Litteral` tokens appearing in RHS are present somewhere in the subtree, and
  - For every nonterminal child, at least one alternative in that child is strictly complete.
- `PartialAST::is_complete()` requires that at least one root branch is strictly complete and that the consumed byte-end equals `input.len()`.
- `PartialAST::into_complete()` enforces both conditions and then chooses the first strictly complete branch whose children all convert successfully into `CompleteAST` nodes.

### Next-token expectations for completion

`PartialAST::next_expectations()` walks the root branches to compute a compact view of what can come next:

- If a branch is strictly complete, it exposes repetition on the production’s right edge and then recurses into the rightmost child to surface right-edge repetition across wrappers.
- Otherwise, it looks at the next symbol at the current production cursor.
- Collected expectations include:
  - `literals: Vec<String>`: deduplicated set of next literal tokens that could appear next
  - `wants_identifier: bool`: whether an identifier-like token (regex/expression) is expected

This function is designed for editor UX (autocompletion, hints) and respects repetition semantics such as `(...)*` on the right edge.

### Type-guided pruning (optional)

If you configure type guidance for a nonterminal with `set_type_guidance(nonterminal, rules, expected_type)`, then after parsing an `nt`, both complete and partial branches are filtered using `select_branches_by_type` to keep only those consistent with at least one rule concluding `expected_type`. This is used to:

- Reduce ambiguity during parsing by removing semantically incompatible branches early.
- Enable better editor feedback by eliminating branches that cannot type-check under the provided rules.

Guidance is applied only where configured; other nonterminals remain unaffected.

### Error vs partial: key distinctions

- Use `Partial` when the current input is a prefix of a valid continuation at the current position. Typical cases:
  - Input ends in the middle of a literal or identifier.
  - A repetition attempt is in progress and could succeed if extended.
- Use `Error` when a symbol mismatch cannot be recovered by appending further tokens at this point.
- At the start nonterminal:
  - A `Full` parse that does not consume all segments results in an overall error in `partial` (enforces full-input consumption); otherwise the leftover would be indistinguishable from a different parse rooted elsewhere.

### Public API summary

- Build a parser and parse partially:
```rust
let mut parser = PartialParser::new(grammar.clone());
let past = parser.partial(input_str)?; // PartialAST
let exp = past.next_expectations();    // NextExpectations
```

- Enforce completeness and get a final AST:
```rust
let ast = parser.parse(input_str)?; // ASTNode
```

- Use optional type guidance on a specific nonterminal:
```rust
parser.set_type_guidance("Expr", vec![rule1, rule2], expected_type);
```

### Invariants and guarantees

- Segment positions are monotonically non-decreasing. `next_pos` never moves backward in a `Full` outcome.
- Repetition handlers always terminate; they break on no-progress iterations.
- `PartialProduction::cursor_value() == fully_parsed_symbols + partially_parsed_symbols`, with the partial count clamped to 0 or 1.
- `PartialAST::end()` equals the maximal end of any root branch span; `consumed_input()` slices `input[0..end]` safely by bytes.

### Extending or modifying behavior

- Add new repetition kinds by extending `RepetitionKind` and wiring the case in `parse_symbols_repetition`.
- To alter “prefix” detection for regex, improve the conservative check in `Symbol::Regex` handling.
- To tighten or relax strict completeness, adjust `PartialNonTerminal::is_strict_complete()` rules.
- For different whitespace or tokenization, construct `PartialParser` with a `Tokenizer` configured per grammar needs.

### Practical tips

- Treat `PartialAST` as a rich, visualizable forest: each nonterminal node can have multiple alternative branches. Use it to drive IDE features.
- Prefer `partial()` for editor-time operations, and `parse()` only when you require a final AST with full consumption.
- If you see too many ambiguous branches, consider using `set_type_guidance` to prune them early.


