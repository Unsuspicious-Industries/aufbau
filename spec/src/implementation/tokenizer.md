#[D] Tokenization

Tokenization partitions input into **segments**—the atomic units for parsing. It runs in a single left-to-right pass over the input.

**Source:** [`tokenizer.rs`](~/src/logic/grammar/tokenizer.rs)

## Segments

>D Segment
A **segment** $s = (w, [i, j), k)$ is a contiguous slice of input where:
- $w \in \Sigma^*$ is the content (UTF-8 bytes)
- $[i, j) \subseteq \mathbb{N}$ is the byte span
- $k \in \mathbb{N}$ is the position in the segment stream
<

>E Segment example
Input `"let x = 42"` produces segments:
$$\langle (\text{"let"}, [0, 3), 0), (\text{"x"}, [4, 5), 1), (\text{"="}, [6, 7), 2), (\text{"42"}, [8, 10), 3) \rangle$$
<

## Special Tokens and Delimiters

A grammar specifies two sets:

- **Special tokens** : keywords and operators (ex: `"let"`, `"->"`, `"("`)
$$
S = \\{s_1, \ldots, s_n\\} \subseteq \Sigma^*
$$
- **Delimiters**: whitespace characters (default: space, newline, tab)
$$
D \subseteq \Sigma
$$

Delimiters are consumed but produce no segments. 
>R Match ordering
Special tokens have **longest-match priority**: the tokenizer prefers longer specials (e.g., `"->"` over `"-"`).
<

## Tokenization Algorithm

Tokenization is a function $\mathrm{seg}: \Sigma^* \to (\mathrm{Segment})^*$ defined by a left-to-right scan. At each position $i$ in input $w$, apply the first matching case:

 * If $w[i] \in D$, skip to $i+1$ (no segment emitted).
 * If $w[i:i+m] \in S$ for some $m$, emit segment $(w[i:i+m], [i, i+m), k)$ and advance to $i+m$. Use the **longest** such $m$.
 * If $i + m = |w|$ and $w[i:|w|]$ is a strict prefix of some $s \in S$ (i.e., $w[i:|w|] \notin S$ but $\exists s \in S: w[i:|w|]$ prefixes $s$), emit $(w[i:|w|], [i, |w|), k)$ and terminate.
 * Else Consume characters into a word, stopping when either :
    - $w[j] \in D$ (delimiter)
    - $w[j:j+m] \in S$ for some $m$ (complete special starts)
    - $w[j:|w|]$ is a prefix of some $s \in S$ and there is a character-class boundary between $w[j-1]$ and $w[j]$

Emit $(w[i:j], [i, j), k)$ and advance to $j$.

The character-class boundary in case 4 prevents splitting words: `"then"` stays intact even when `"then" \in S`.

## Examples

>E Basic tokenization
**Grammar:** $S = \\{\text{"->", "λ", ":", "let", "then", "(", ")"}\\}$, $D = \\{\text{' ', '\\n', '\\t'}\\}$

**Input:** `"λx:Int"`

**Segments:**
1. $(\text{"λ"}, [0, 2), 0)$ — special token
2. $(\text{"x"}, [2, 3), 1)$ — word
3. $(\text{":"}, [3, 4), 2)$ — special token
4. $(\text{"Int"}, [4, 7), 3)$ — word

Note: `"Int"` is not split at `"t"` despite `"then" \in S$, because `"t"` and `"n"` are both word characters (no character-class boundary).
<

>E Partial special at end-of-input
**Grammar:** $S = \\{\text{"->"}\\}$

**Input:** `"λx:Int-"` (end-of-input)

**Segments:**
1. $(\text{"λ"}, [0, 2), 0)$
2. $(\text{"x"}, [2, 3), 1)$
3. $(\text{":"}, [3, 4), 2)$
4. $(\text{"Int"}, [4, 7), 3)$
5. $(\text{"-"}, [7, 8), 4)$ — **partial special** (prefix of `"->"`)

The segment `"-"` is marked as a partial special because it is a strict prefix of `"->" \in S` and occurs at end-of-input (case 3).
<

>E Prefix of keyword
**Grammar:** $S = \\{\text{"let"}\\}$

**Input:** `"le"` (end-of-input)

**Segments:**
1. $(\text{"le"}, [0, 2), 0)$ — **partial special** (prefix of `"let"`)

If more input arrives (e.g., `"let"`), re-tokenization will classify it as a complete special. If different input arrives (e.g., `"lex"`), it becomes a word.
<

## Longest-Match Priority

When multiple special tokens share a prefix, the tokenizer chooses the longest match.

>L Longest-Match Invariant
If $w[i:i+m_1], w[i:i+m_2] \in S$ with $m_1 < m_2$, the tokenizer emits $(w[i:i+m_2], [i, i+m_2), k)$, never $(w[i:i+m_1], [i, i+m_1), k)$.

This is enforced by sorting $S$ in descending order of length.
<

>E Longest-match example
**Grammar:** $S = \\{\text{"-", "->"}\\}$

**Input:** `"->"`

**Segments:**
1. $(\text{"->"},  [0, 2), 0)$ — matched as `"->"`, not `"-"` + `">"`

The tokenizer prefers the longer match `"->"` over the shorter prefix `"-"`.
<

## Partial Specials and Incremental Parsing

A segment emitted via case 3 is called a **partial special**. Its identity is undetermined: when more input arrives, it may complete to a full special or be reclassified as a word.

During incremental parsing (see [Typed Parsing](./parsing.md)), partial specials at frontiers trigger re-tokenization when new input arrives. This ensures the parser never commits to a token identity prematurely.

## Character Classes

The algorithm distinguishes two character classes:

- **Word characters:** alphanumeric and underscore (`a-z`, `A-Z`, `0-9`, `_`)
- **Non-word characters:** all others

Case 4 stops at a character-class boundary when a special-prefix match begins. This prevents over-splitting.

>E Character-class boundary
**Grammar:** $S = \\{\text{"then"}\\}$

**Input:** `"then"`

**Segments:**
1. $(\text{"then"}, [0, 4), 0)$ — matched as complete special (case 2)

**Input:** `"thenx"`

**Segments:**
1. $(\text{"thenx"}, [0, 5), 0)$ — word token (case 4)

The tokenizer does not split `"thenx"` into `"then"` + `"x"` because when scanning the word, it does not stop at character-class boundaries within the word.

**Input:** `"the"` (end-of-input)

**Segments:**
1. $(\text{"the"}, [0, 3), 0)$ — partial special (prefix of `"then"`, case 3)
<
