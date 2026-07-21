#[D] Tokenizer

The tokenizer converts a raw input string into a sequence of **segments**, the atomic units that the parser operates on. It is constructed once from a grammar's special tokens and runs in a single linear pass over the input bytes.

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

1. If any special token matches exactly at the current position, emit it as a segment and advance by its length. Because the list is sorted longest-first, this is always the longest possible special match.

2. If the entire remaining suffix is a strict prefix of some special token (but does not match any completely), emit it as a single segment with `is_partial_special = true` and stop. This case only fires when we are at the end of input, since otherwise there would be more characters to determine whether the match completes.

3. Consume characters one by one into a word token, stopping when:
    - a delimiter is reached
    - a complete special match starts at the current character
    - a partial-special prefix starts at the current character **and** there is a character-class transition from the previous character (word↔non-word boundary).

The character-class condition in case 3 is the critical subtlety.

>E Tokenization example

For the fun grammar with specials `["->", "λ", ":", ".", "=", "let", "in", "if", "then", "else", "true", "false", "(", ")"]` and delimiters `[' ', '\n', '\t']`:

Input `"λx:Int"` produces four segments: 
- `λ` (special)
- `x` (word)
- `:` (special)
- `Int` (word, none partial). 

The trailing `t` of `Int` is a prefix of `then` and `true`, but since `t` and `n` are both word characters, the word-boundary rule suppresses the split.

Input `"λx:Int-"` produces five segments, the last being `-` with `is_partial_special = true`, because `-` is a strict prefix of `->` and there is a word↔non-word boundary between `t` and `-`.

Input `"le"` at end of input produces one segment, `le`, with `is_partial_special = true` (strict prefix of `let`).

<