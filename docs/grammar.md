# P7 Grammar Specification

**Version 1.0**

This document defines the grammar file format for the P7 constraint system. A grammar file specifies both the syntax (production rules) and semantics (typing rules) of a target language.

---

## 1. File Structure

A grammar file consists of **blocks** separated by blank lines. Each block is either:
- A **production block** (syntax rules)
- A **typing rule block** (semantic rules)

```
// Comments start with double slashes
ProductionBlock

TypingRuleBlock

ProductionBlock
```

**Start Symbol**: The last declared non-terminal becomes the start symbol.

---

## 2. Production Rules

### 2.1 Basic Syntax

```
NonTerminal ::= Symbol₁ Symbol₂ ... Symbolₙ
```

A production defines how a non-terminal expands into a sequence of symbols.

### 2.2 Non-Terminal Names

Non-terminals are identifiers starting with an uppercase letter:
```
Expression
AtomicType
FunctionDecl
```

### 2.3 Rule Annotation

Associate a typing rule with a production using parentheses:
```
NonTerminal(ruleName) ::= ...
```

The `ruleName` must match a declared typing rule.

### 2.4 Symbols

| Symbol Type | Syntax | Example |
|-------------|--------|---------|
| Non-terminal | `Name` | `Expression`, `Type` |
| Literal (single quotes) | `'text'` | `'λ'`, `'let'`, `'->'` |
| Literal (double quotes) | `"text"` | `"if"`, `"then"` |
| Regex pattern | `/pattern/` | `/[a-z]+/`, `/[0-9]+/` |
| Empty (epsilon) | `ε` | `ε` |

**Note**: ASCII placeholders like `epsilon` or `EPS` are **not** accepted. Use `ε` (U+03B5).

### 2.5 Bindings

Attach a binding to any symbol using square brackets:
```
NonTerminal ::= Symbol[bindingName]
```

Bindings create anchors for the typing system to reference AST nodes.

```
Lambda(abs) ::= 'λ' Identifier[x] ':' Type[τ] '.' Expression[e]
```

### 2.6 Alternatives

Multiple alternatives are separated by `|`:
```
Type ::= BaseType | FunctionType | '(' Type ')'
```

Alternatives may span multiple lines if continuation lines start with `|`:
```
Expression ::= Variable
             | Lambda
             | Application
             | '(' Expression ')'
```

### 2.7 Empty Productions

Use `ε` alone to define a nullable production:
```
OptionalArgs ::= ArgList | ε
```

Epsilon cannot:
- Carry a binding
- Mix with other symbols in the same alternative

---

## 3. Typing Rules

### 3.1 Structure

```
premise₁, premise₂, ...
----------------------- (ruleName)
conclusion
```

### 3.2 Premise Forms

| Form | Syntax | Semantics |
|------|--------|-----------|
| Judgment | `Γ ⊢ e : τ` | Term `e` has type `τ` in context `Γ` |
| Membership | `x ∈ Γ` | Variable `x` is bound in `Γ` |
| Extension | `Γ[x:τ] ⊢ e : σ` | Check `e : σ` with `x:τ` added to `Γ` |

**Premise extensions are local**: `Γ[x:τ]` in a premise only affects that premise's scope.
**Context Obstruction**: Only one occurence of each variable is allowed in context. If you try to add `x:τ` when `x` is already in `Γ`, it results in an error.
```
Γ[x:τ₁] ⊢ e : τ₂      // x:τ₁ visible only when checking e
----------------------- (abs)
τ₁ → τ₂
```

### 3.3 Conclusion Forms

| Form | Syntax | Semantics |
|------|--------|-----------|
| Bare type | `τ` | Rule produces type `τ` |
| Lookup | `Γ(x)` | Return type of `x` from context |
| Transform | `Γ → Γ'[x:τ] ⊢ σ` | Extend parent context with `x:τ` |

**Conclusion extensions propagate upward**: The arrow `→` in conclusions modifies the context visible to *parent* nodes.

```
----------------------- (let)
Γ → Γ[x:τ] ⊢ τ         // x:τ added to context for subsequent code
```

### 3.4 Context Scoping Summary

| Location | Syntax | Scope |
|----------|--------|-------|
| Premise | `Γ[x:τ] ⊢ ...` | Local to that premise only |
| Conclusion | `Γ → Γ[x:τ] ⊢ ...` | Propagates to parent/siblings |

### 3.5 Axioms

Rules with empty premises:
```
----------------------- (literal)
'int'
```

---

## 4. Type Expressions

### 4.1 Base Types

| Type | Syntax | Description |
|------|--------|-------------|
| Atom | `τ`, `Int`, `Bool` | Type variable or named type |
| Raw | `'int'`, `'void'` | Literal/concrete type |
| Universe | `⊤` | Top type (all values) |
| Empty | `∅` | Bottom type (no values) |

### 4.2 Composite Types

| Type | Syntax | Description |
|------|--------|-------------|
| Function | `τ₁ → τ₂` or `τ₁ -> τ₂` | Function type (right-associative) |
| Negation | `¬τ` or `!τ` | Complement type |
| Context Call | `Γ(x)` | Type lookup |

### 4.3 Inference Variables

Variables prefixed with `?` are inference variables for pattern matching:
```
?A, ?B, ?Result
```

These unify during type checking and enable rules like:
```
Γ ⊢ f : ?A → ?B, Γ ⊢ x : ?A
----------------------------- (app)
?B
```

### 4.4 Operator Precedence

From highest to lowest:
1. Negation (`¬`, `!`) — prefix
2. Arrow (`→`, `->`) — right-associative

Use parentheses to override: `(τ₁ → τ₂) → τ₃`

---

## 5. Unicode Support

### 5.1 Recommended Symbols

| Symbol | Unicode | Description |
|--------|---------|-------------|
| `→` | U+2192 | Arrow (function type) |
| `λ` | U+03BB | Lambda |
| `τ` | U+03C4 | Tau (type variable) |
| `Γ` | U+0393 | Gamma (context) |
| `⊢` | U+22A2 | Turnstile |
| `∈` | U+2208 | Element of |
| `¬` | U+00AC | Negation |
| `⊤` | U+22A4 | Top/universe |
| `∅` | U+2205 | Empty set |
| `ε` | U+03B5 | Epsilon |

### 5.2 Subscripts

Subscript digits for distinguishing variables:
```
τ₁ τ₂ τ₃ τ₄ τ₅ τ₆ τ₇ τ₈ τ₉ τ₀
```

---

## 6. Complete Example: Simply Typed Lambda Calculus

```
// Lexical elements
Identifier ::= /[a-z][a-zA-Z0-9]*/

// Expressions
Variable(var) ::= Identifier[x]
Abstraction(abs) ::= 'λ' Identifier[x] ':' Type[τ] '.' Expression[e]

AtomicExpression ::= Variable | '(' Expression ')'
Application(app) ::= AtomicExpression[e₁] AtomicExpression[e₂]

Expression ::= AtomicExpression | Abstraction | Application

// Types
BaseType ::= 'Int' | 'Bool'
AtomicType ::= BaseType | '(' Type ')'
FunctionType ::= AtomicType[τ₁] '→' Type[τ₂]
Type ::= AtomicType | FunctionType

// Variable lookup
x ∈ Γ
----------- (var)
Γ(x)

// Lambda abstraction
Γ[x:τ] ⊢ e : τ₂
----------------------- (abs)
τ → τ₂

// Function application
Γ ⊢ e₁ : ?A → ?B, Γ ⊢ e₂ : ?A
-------------------------------- (app)
?B
```

---

## 7. Restrictions

### 7.1 Not Supported

- **Repetition operators** (`*`, `+`, `?`) — use recursive productions instead
- **Inline grouping** — use separate non-terminals
- **EBNF extensions** — pure BNF only

### 7.2 Recursive Patterns

Instead of `A*`, use:
```
AList ::= ε | A AList
```

Instead of `A+`, use:
```
AList ::= A | A AList
```

Instead of `A?`, use:
```
OptA ::= A | ε
```

---

## 8. Grammar Constraints

1. Every `ruleName` in a production annotation must have a corresponding typing rule
2. Every binding referenced in a typing rule must exist in the annotated production (or reachable via non-terminals)
3. The grammar must be unambiguous for deterministic binding resolution
4. Epsilon alternatives cannot carry bindings or mix with other symbols

---

## 9. File Extension

Grammar files use the `.spec` extension by convention.

---

## Appendix: BNF of the Grammar Format

```
GrammarFile   ::= Block (BlankLine Block)*
Block         ::= ProductionBlock | TypingBlock
BlankLine     ::= '\n' '\n'+

ProductionBlock ::= Production (ContinuationLine)*
Production      ::= NonTerminal '::=' RHS
ContinuationLine ::= '|' RHS
NonTerminal     ::= Identifier ('(' RuleName ')')?
RuleName        ::= Identifier
RHS             ::= Alternative ('|' Alternative)*
Alternative     ::= Symbol+ | 'ε'
Symbol          ::= (Terminal | NonTerminalRef | Regex) Binding?
Terminal        ::= SingleQuoted | DoubleQuoted
SingleQuoted    ::= "'" [^']+ "'"
DoubleQuoted    ::= '"' [^"]+ '"'
Regex           ::= '/' [^/]+ '/'
NonTerminalRef  ::= Identifier
Binding         ::= '[' Identifier ']'

TypingBlock   ::= Premises? Separator Conclusion
Premises      ::= Premise (',' Premise)*
Premise       ::= Setting? (Judgment)?
Setting       ::= Context Extension*
Context       ::= Identifier
Extension     ::= '[' Identifier ':' TypeExpr ']'
Judgment      ::= Ascription | Membership
Ascription    ::= Term ':' TypeExpr
Membership    ::= Identifier '∈' Context
Separator     ::= '-'+ '(' RuleName ')'
Conclusion    ::= TypeExpr | ContextLookup | ContextTransform
ContextLookup ::= Context '(' Identifier ')'
ContextTransform ::= Context '->' Setting '⊢' TypeExpr
TypeExpr      ::= ... (see Section 4)
```
