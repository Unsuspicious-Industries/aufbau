# AST Serialization Format

This document describes the Lisp-style S-expression format used to serialize and deserialize the abstract syntax tree (AST) in the Beam parser/typing system.

## File Structure

A serialized AST file consists of:

1. A version header.
2. An optional rules header listing all typing rules encountered in the tree.
3. A blank line separating headers from the body.
4. An S-expression body representing the AST.

### 1. Version Header

    ;!ast 1

The version header must appear at the top of the file and currently must be exactly `;!ast 1`.

### 2. Rules Header (Optional)

If the tree uses any typing rules, they are listed in alphabetical order:

    ;!rules: ruleA, ruleB, ruleC

This line must start with `;!rules:` followed by a comma-separated list of rule names.

### 3. Blank Line

A single blank line separates the headers from the S-expression body.

### 4. S-Expression Body

The body is a nested S-expression describing the AST. There are two node types:

- **Terminal** nodes, tagged with `T`.
- **Nonterminal** nodes, tagged with `N`.

Whitespace and newlines are only for readability; the parser ignores them outside of string literals.

#### 4.1 Terminal Node

    (T "value" [ (b binding) ])

- `T`: Atom indicating a terminal node.
- `"value"`: A string literal containing the terminal text, with standard C-style escapes (`\\`, `\"`, `\n`, etc.).
- Optional `(b binding)`: An atom binding name for subsequent referencing.

**Example:**

    (T "123" (b num))

#### 4.2 Nonterminal Node

    (N name [ (rule ruleName) ] [ (b binding) ] [ children... ])

- `N`: Atom indicating a nonterminal node.
- `name`: Atom naming the nonterminal symbol.
- Optional `(rule ruleName)`: Indicates the typing rule applied to this node.
- Optional `(b binding)`: An atom binding name for this subtree.
- `children...`: Zero or more nested S-expressions, each representing a child node.

If there are children, you may format them over multiple lines and indent for readability:

```lisp
(N Expr (rule Add)
  (N Expr (rule Num) (T "1"))
  (T "+")
  (N Expr (rule Num) (T "2"))
)
```

If there are no children, the nonterminal closes immediately:

```lisp
(N Empty)
```

## Parsing and Serialization

- To **serialize** an in-memory AST, use `ASTNode::serialize()` followed by `ASTNode::save(path)`.
- To **parse** a file, use `ASTNode::load(path, &grammar)` or `ASTNode::parse(&content, &grammar)`.

The parser will:

1. Strip off any leading comment lines (`;...`) until the first non-comment.
2. Tokenize the remaining text into parentheses, atoms, and string literals.
3. Reconstruct the AST, resolving rule names against the provided `Grammar`.
