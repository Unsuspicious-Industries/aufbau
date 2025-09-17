# AST Serialization Format (Bound Rule Aware)

This document describes the Lisp-style S-expression format used to serialize and deserialize the abstract syntax tree (AST) in the Beam system. Only lightweight metadata (node kind, value, optional binding, optional rule name) is serialized; bound typing rule internals are NOT stored.

## File Structure

1. Version header
2. Optional rules header (names only)
3. Blank line
4. S-expression body

### 1. Version Header

    ;!ast 1

Must appear first.

### 2. Rules Header (Optional)

Lists rule names (alphabetical) that appear as `(rule name)` metadata inside nodes:

    ;!rules: Add, App, Lambda

Only the names are persisted; the parser/binder must re-resolve them to `BoundTypingRule`s if needed after load.

### 3. Blank Line

Separates headers from body.

### 4. S-Expression Body

Two node kinds:

- `(T ...)` Terminal
- `(N ...)` Nonterminal

Whitespace/newlines are for readability.

#### 4.1 Terminal

```
(T "value" (b binding)?)
```

- `value`: string literal (C-style escapes supported)
- Optional `(b name)` attaches a semantic binding identifier (used by binding resolver)

**Example:**

```
(T "123" (b num))
```

#### 4.2 Nonterminal

```
(N Name (rule RuleName)? (b binding)? child*)
```

- `Name`: production LHS symbol
- Optional `(rule RuleName)`: original typing rule name (no bound data). Present only if the production carried a rule annotation when parsed.
- Optional `(b binding)`: semantic binding name for this subtree
- Children: zero or more nested node S-exprs

**Examples:**

```lisp
(N Expr (rule Add)
  (N Expr (rule Num) (T "1"))
  (T "+")
  (N Expr (rule Num) (T "2"))
)
(N Empty)
```

## Binding & Serialization Boundary

During parsing, if a production has a rule, a `BoundTypingRule` is constructed and attached in-memory (`nonterminal.bound_typing_rule`). Serialization does NOT dump that structure—only `(rule name)` is written plus the global header list. After deserialization, nodes have `bound_typing_rule: None`. To re-enable checking you must re-run binding with the original `Grammar` (future helper planned).

## API Summary

**Serialize:**

- `ASTNode::serialize()` → single-line S-expression
- `ASTNode::pretty()` → multi-line indented form
- `ASTNode::save(path)` writes headers + S-expression, auto-injecting `;!rules:` if any nodes carry `(rule ...)` metadata.

**Deserialize:**

- `ASTNode::load(path, &grammar)` parses headers, ignores unknown comment lines, reconstructs structure.
- Rule names in `(rule ...)` are not resolved automatically; binding happens only in the live parsing path currently.

## Parsing Steps (Deserializer)

1. Strip comment / header lines until first non-comment for body.
2. Tokenize: parentheses, atoms, string literals (escapes handled: `\\`, `\"`, `\n`, `\t`).
3. Build `ASTNode` tree. `(rule X)` captured as metadata but no rule resolution performed.

## Determinism & Stability

- Field order inside each node list is fixed: tag, name/value, metadata pairs, children.
- Unknown metadata keys inside a node list become child nodes (forward-compatible extension point).

## Practical Notes

- Bound rule attachment is ephemeral; do not rely on serialized files to preserve bound state.
- The `;!rules:` header is advisory (for debugging / fast scanning) and not required for parsing correctness.

---

Concise: The AST file stores structure + rule names only. Bound rule resolution is an in-memory concern handled by the parser, not the serializer.
