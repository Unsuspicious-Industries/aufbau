#[D] Tokenizer

The tokenizer converts a raw input string into a sequence of **segments** — the atomic units that the parser operates on. It is constructed once from a grammar's special tokens and runs in a single linear pass over the input bytes.

Source: [`src/logic/grammar/tokenizer.rs`](~/src/logic/grammar/tokenizer.rs)

## Segment

>D Segment
A **segment** is a contiguous slice of the input string, carrying:
- `bytes`: the UTF-8 content
- `start`, `end`: byte offsets into the original input
- `index`: position in the token stream
- `is_partial_special`: true when the segment is a strict prefix of some special token but is not itself a complete special token
<

The `is_partial_special` flag has a specific consequence for the parser: any span whose last segment is a partial special is excluded from the span cache. The token's identity is not yet determined — it may grow into a complete special with more input, or may be classified as a word token.

## Tokenizer Construction

`Tokenizer::new(special_tokens, delimiters)` sorts the special token list by length descending, then alphabetically as a tiebreak, then deduplicates. Longest-match priority is structural: the first special that matches at a given position wins, and since longer specials come first, a two-character `->` takes priority over its one-character prefix `-`.

Delimiters (default: space, newline, tab) are consumed and discarded — they produce no segment.

## Tokenization Algorithm

The tokenizer scans left to right, applying three cases at each position in order:

**Case 1 — complete special match.** If any special token matches exactly at the current position, emit it as a segment and advance by its length. Because the list is sorted longest-first, this is always the longest possible special match.

**Case 2 — partial special at end of input.** If the entire remaining suffix is a strict prefix of some special token (but does not match any completely), emit it as a single segment with `is_partial_special = true` and stop. This case only fires when we are at the end of input, since otherwise there would be more characters to determine whether the match completes.

**Case 3 — word accumulation.** Consume characters one by one into a word token, stopping when:
- a delimiter is reached, or
- a complete special match starts at the current character, or
- a partial-special prefix starts at the current character **and** there is a character-class transition from the previous character (word↔non-word boundary).

The character-class condition in case 3 is the critical subtlety.

>N the word-boundary rule
Without the character-class condition, `Int` would split as `["In", "t"(partial)]` when `in` is a keyword — because `t` is a prefix of `then`. The rule says: only break accumulation for a prefix-special when the character class changes. `I→n` is word→word so accumulation continues; `o→-` is word→non-word so `foo-` correctly splits as `["foo", "-"(partial)]`.
<

<!-- DIAGRAM: timeline showing the three cases firing on a concrete input like "λx:Int->" with annotations for each segment produced -->

## Partial Special Detection

`prefix_special(text)` returns true if `text` is a strict prefix of any special token — i.e., there exists a special of length strictly greater than `text.len()` that starts with `text`. This is used in both case 2 (whole remaining suffix) and case 3 (mid-accumulation boundary check).

## Example

For the fun grammar with specials `["->", "λ", ":", ".", "=", "let", "in", "if", "then", "else", "true", "false", "(", ")"]` and delimiters `[' ', '\n', '\t']`:

Input `"λx:Int"` produces four segments: `λ` (special), `x` (word), `:` (special), `Int` (word, none partial). The trailing `t` of `Int` is a prefix of `then` and `true`, but since `t` and `n` are both word characters, the word-boundary rule suppresses the split.

Input `"λx:Int-"` produces five segments, the last being `-` with `is_partial_special = true`, because `-` is a strict prefix of `->` and there is a word→non-word boundary between `t` and `-`.

Input `"le"` at end of input produces one segment, `le`, with `is_partial_special = true` — it is a strict prefix of `let`.

>I tokenize
{"label":"tokenize: \u03bbx:Int->","input":"","steps":[{"token":"\u03bb","tokens":["\u03bb"],"display":"scan \u03bb\ncase 1: special match\nsegments: [\u03bb]"},{"token":"x","tokens":["x"],"display":"scan x\ncase 3: word accumulation\nsegments: [\u03bb, x]"},{"token":":","tokens":[":"],"display":"scan :\ncase 1: special match\nsegments: [\u03bb, x, :, ]"},{"token":"Int","tokens":["Int"],"display":"scan I, n, t\ncase 3: word (t is prefix of then but word\u2192word)\nsegments: [\u03bb, x, :, Int]"},{"token":"->","tokens":["->"],"display":"scan -, >\ncase 1: special match (-> longest)\nsegments: [\u03bb, x, :, Int, ->]"}]}
<
