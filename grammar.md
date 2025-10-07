# Unified Grammar & Typing Specification (Bound Rule Model)

This document specifies the unified grammar + typing rule format and explains how the bound rule pipeline works in the current implementation.

## High-Level Flow

spec grammar + typing rules  → (load) → `Grammar` (productions + `TypingRule`s)
→ input source text → (parser) → AST (`NonTerminal.bound_typing_rule` attached where annotated)
→ type checker consumes only `BoundTypingRule` (no meta lookup)

Key invariant: By the time checking starts, every rule the checker sees is ground: all schematic term/type variables resolved to concrete AST nodes and `BoundType` structures.

## Components

1. Grammar productions: syntax + optional `(rule RuleName)` annotation.
2. Typing rules: inference rules with premises and a conclusion.
3. Binding phase (performed inside the parser): converts `TypingRule` → `BoundTypingRule` for each matched production instance.
4. Type checker: interprets bound premises and conclusion; premises are pure (no ambient mutation); only conclusions may thread context via a transform.

## Bound Rule Semantics (Recap)

- No symbolic variables at check time.
- Premise expansion handles repeated bound variables (Kleene sections) automatically.
- Membership: concrete variable node must have extractable terminal text.
- Ascription: node must infer to a type compatible with the expected bound type (after resolving any context lookups inside the type structure).
- Conclusion may:
  - Yield a type (identity context if no transform)
  - Perform a lookup Γ(x)
  - Thread context: `Γ_in → Γ_out ⊢ τ` where inline extensions produce a functional delta (no destructive removal).

## Grammar Syntax

Production form:
```
NonTerminal(ruleName)? ::= RHS
```
- `ruleName` must match a typing rule name declared in the spec.
- RHS uses terminals, nonterminals, and optional semantic bindings `[binding]` (these become node.binding and drive the resolver).

### Symbol Types
- Literals: enclosed in single quotes `'keyword'`
- Regex patterns: enclosed in slashes `/pattern/`
- Nonterminals: bare identifiers
- Groups: enclosed in parentheses `(...)`
- Bindings: suffix notation `Symbol[binding]`

The binder collects all sibling nodes sharing a binding symbol to expand premises.

## Type System

The grammar system supports a rich type language with various type expressions and constructs. This section details all supported type expressions and their syntax.

### Type Expression Categories

#### 1. Basic Types

**Atom Types**: Simple type identifiers
```
int, string, bool, τ, σ, MyType
```
- Can contain alphanumeric characters, underscores, and Unicode type symbols
- Examples: `int`, `string`, `τ₁`, `MyCustomType`

**Raw/Concrete Types**: Quoted literal types
```
'int', 'string', 'void'
```
- Used for concrete language types that don't need variable resolution
- Syntax: single quotes around the type name

**Universe Type**: The type of all types
```
⊤
```
- Represents the universal type (top type)
- Keyword: `⊤`

**Empty Type**: The uninhabited type
```
∅
```
- Represents the empty type (bottom type)
- Keyword: `∅`

#### 2. Composite Types

**Function Types**: Arrow types for functions
```
τ₁ → τ₂
int → string
(int → string) → bool
```
- Right-associative: `A → B → C` ≡ `A → (B → C)`
- Alternative syntax: `->` (ASCII arrow)
- Examples: `int → bool`, `string -> int -> bool`

**Tuple Types**: Meta-types for tuples
```
(τ...)
(int...)
(MyType...)
```
- Used for representing tuple/product types at the meta level
- Syntax: `(<identifier>...)` where identifier is the tuple element type

**Pointer Types**: For C-like pointer types
```
*τ
*int
**char
```
- Prefix syntax with `*`
- Can be nested: `**int` (pointer to pointer to int)

**Array Types**: For array/list types
```
τ[N]
int[10]
string[]
MyType[size]
```
- Syntax: `base_type[size_expression]`
- Empty brackets `[]` for dynamic arrays
- Size can be any string expression

#### 3. Logical Types

**Union Types**: Either-or types
```
τ₁ ∨ τ₂
int ∨ string
A | B
```
- Represents values that can be either type
- Alternative operators: `∨`, `v`, `|`
- Left-associative

**Intersection Types**: Both-and types
```
τ₁ ∧ τ₂
Readable ∧ Writable
A & B
```
- Represents values that are both types
- Alternative operators: `∧`, `^`, `&`
- Left-associative

**Negation Types**: Complement types
```
¬τ
¬int
!string
```
- Represents "anything that is not τ"
- Alternative operators: `¬`, `!`
- Prefix syntax

#### 4. Context Types

**Context Calls**: Type lookups from typing context
```
Γ(x)
Delta(variable)
Θ(myVar)
```
- Syntax: `ContextName(variable)`
- Used to look up the type of a variable in a typing context
- Context names can use Greek letters and subscripts

### Type Expression Parsing Rules

#### Operator Precedence (highest to lowest)
1. **Negation** (`¬`, `!`) - prefix
2. **Pointer** (`*`) - prefix  
3. **Array** (`[...]`) - postfix
4. **Intersection** (`∧`, `^`, `&`) - left-associative
5. **Union** (`∨`, `v`, `|`) - left-associative
6. **Arrow** (`→`, `->`) - right-associative

#### Parentheses
- Regular parentheses `()` can be used to override precedence
- Tuple syntax `(...)` is reserved for meta-types
- Context calls `Context(var)` use parentheses for variable lookup

#### Unicode Support
The type system supports Unicode characters commonly used in type theory:
- Greek letters: `τ`, `σ`, `Γ`, `Δ`, `Θ`, etc.
- Subscripts: `τ₁`, `τ₂`, `₃₄₅₆₇₈₉₀`
- Type theory symbols: `→`, `∧`, `∨`, `¬`, `⊢`, `∈`

### Examples of Complex Type Expressions

```
// Function types
int → string → bool
(int → string) → (string → bool) → (int → bool)

// Union and intersection
Readable ∧ Writable ∨ Default
¬(int ∨ string) ∧ Serializable

// Pointers and arrays
*int[10]           // Pointer to array of 10 ints
(*int)[10]         // Array of 10 int pointers
**char[]           // Pointer to pointer to dynamic char array

// Context lookups in complex types
Γ(f) → Γ(x) → Γ(result)
(Γ(input) ∨ Default) → Γ(output)

// Mixed complex types
¬(*int ∨ *string) ∧ (Serializable → Γ(T))
```

## Typing Rule Concrete Format

Strict textual shape in the spec (before binding):
```
p1, p2, ..., pN
------------------- (rule_name)
conclusion
```
Premises comma-separated. Empty premise list = axiom form.

### Premise Forms
1. **Typing Judgment**: `Γ[x:τ][y:σ] ⊢ term : τ'`
   - Context `Γ` with optional extensions `[var:type]`
   - Turnstile `⊢` separates context from judgment
   - Term and its ascribed type separated by `:`

2. **Membership**: `x ∈ Γ`
   - Variable `x` is a member of context `Γ`
   - Used to check if a variable is bound in the context

3. **Bare Setting**: `Γ[x:τ]` (context only, rarely used)
   - Just establishes context extensions without judgment
   - Used for context manipulation

### Conclusion Forms
1. **Bare Type**: `τ` 
   - Simple type result (desugars to identity context transform)
   - Examples: `int`, `string → bool`, `Γ(x) → Γ(y)`

2. **Context Lookup**: `Γ(x)`
   - Look up the type of variable `x` in context `Γ`
   - Returns the type that `x` was bound to

3. **Context-Transforming Type**: `[Γ_in][→ Γ_out] ⊢ τ`
   - Explicit context transformation with type result
   - Arrow sugar variants:
     - `Γ_in ⊢ τ` ≡ `Γ_in → Γ_in ⊢ τ` (no context change)
     - `→ Γ_out ⊢ τ` ≡ `Γ → Γ_out ⊢ τ` (inherit input context)
     - `τ` ≡ `Γ → Γ ⊢ τ` (identity transform)

Implementation stores these as structured `Conclusion { context: {input, output}, kind }`.

## Binding Details

`DefaultBindingResolver`:
- Maps each schematic term variable to the concrete `NonTerminal` whose `binding` matches.
- Resolves type expressions, substituting any schematic type atoms bound through the same mechanism (currently simple: atoms treated as raw names unless matched via bindings in premises/settings).
- Expands a single schematic premise over repeated bindings (`collect_nt_bindings_same_level`). Zero matches → premise dropped.
- Assembles `BoundTypingRule { premises, conclusion }` and performs a shallow well-formedness check.

Errors during binding (missing binding, unresolvable type variable) abort the parse of that production instance.

## Type Checking Algorithm (Bound)

For a nonterminal node:
1. If it has a `BoundTypingRule`, call `apply_bound_rule`.
2. Else if exactly one nonterminal child, recursively check that child (bubbling through syntactic wrappers).
3. Else error (no rule and ambiguous children).

`apply_bound_rule`:
- For each premise: build a child context (apply setting extensions), evaluate judgment. On success, merge any new bindings (delta) back into ambient context (current implementation commits variable types learned inside premise; can be tightened later if needed).
- Evaluate conclusion:
  - Type: apply input context (currently used for expression evaluation placeholder), then commit output extensions to ambient Γ.
  - Lookup: extract terminal text from resolved node, look up type in Γ.

Returned value is the bound type of the node (or None for terminals).

## Serialization Boundary

Serialized AST only keeps `(rule RuleName)` markers; bound internals are not persisted. Deserialization yields nodes without `bound_typing_rule`. Re-binding requires re-running resolver with the same `Grammar` rules (future helper planned).

Headers:
```
;!ast 1
;!rules: ruleA, ruleB

(N ...)
```
Rules listed alphabetically if any appear.

## Complete Grammar Examples

### Example 1: Simple Typed Lambda Calculus

Grammar with type expressions:
```text
// Variables and types
Variable(var) ::= Identifier[x]
Type ::= 'int' | 'bool' | Type[τ₁] '->' Type[τ₂]

// Expressions  
Lambda(lambda) ::= 'λ' Variable[x] ':' Type[τ₁] '.' Term[e]
Application(app) ::= Term[f] Term[a]
Term ::= Variable | Lambda | Application

// Typing rules using type expressions
x ∈ Γ
-------- (var)
Γ(x)

Γ[x:τ₁] ⊢ e : τ₂
-------------------- (lambda)
τ₁ → τ₂

Γ ⊢ f : τ₁ → τ₂, Γ ⊢ a : τ₁
----------------------------- (app)
τ₂
```

### Example 2: C-like Language with Pointers and Arrays

Grammar demonstrating pointer and array types:
```text
// Types with pointers and arrays
PrimitiveType ::= 'int' | 'char' | 'float'
PointerType ::= '*' Type[base]
ArrayType ::= Type[base] '[' Number? ']'
Type ::= PrimitiveType | PointerType | ArrayType

// Expressions
Variable(var) ::= Identifier[x]
Dereference(deref) ::= '*' Expr[e]
AddressOf(addr) ::= '&' Expr[e]
ArrayAccess(access) ::= Expr[arr] '[' Expr[idx] ']'

// Typing rules with pointer/array types
x ∈ Γ
-------- (var)
Γ(x)

Γ ⊢ e : *τ
---------- (deref)
τ

Γ ⊢ e : τ
---------- (addr)
*τ

Γ ⊢ arr : τ[], Γ ⊢ idx : 'int'
------------------------------- (access)
τ
```

### Example 3: Functional Language with Union Types

Grammar using union and intersection types:
```text
// Advanced type system
BaseType ::= 'int' | 'string' | 'bool'
UnionType ::= Type[τ₁] '|' Type[τ₂]
IntersectionType ::= Type[τ₁] '&' Type[τ₂]
NegationType ::= '!' Type[τ]
Type ::= BaseType | UnionType | IntersectionType | NegationType

// Pattern matching with union types
Match(match) ::= 'match' Expr[e] '{' Case+ '}'
Case ::= Pattern '->' Expr

// Typing rules with logical types
Γ ⊢ e : τ₁ ∨ τ₂
----------------- (union_elim_left)
τ₁

Γ ⊢ e : τ₁
----------- (union_intro_left)
τ₁ ∨ τ₂

Γ ⊢ e : τ₁, Γ ⊢ e : τ₂
----------------------- (intersection_intro)
τ₁ ∧ τ₂

Γ ⊢ e : ¬τ, Γ ⊢ e : τ
--------------------- (negation_elim)
∅
```

### Example 4: Context-Aware Type System

Grammar showing context manipulation:
```text
// Variable declarations and scoping
VarDecl(decl) ::= 'let' Variable[x] ':' Type[τ] '=' Expr[e]
Block(block) ::= '{' Stmt* Expr[result] '}'
LetIn(letin) ::= 'let' Variable[x] '=' Expr[value] 'in' Expr[body]

// Context-transforming rules
Γ ⊢ e : τ
-------------- (decl)
Γ → Γ[x:τ] ⊢ τ

Γ ⊢ value : τ, Γ[x:τ] ⊢ body : σ
---------------------------------- (letin)
σ

Γ₁ ⊢ s₁ : τ₁, Γ₁[x₁:τ₁] ⊢ s₂ : τ₂, ..., Γₙ ⊢ result : σ
--------------------------------------------------------- (block)
Γ → Γₙ ⊢ σ
```

### Example 5: Generic Types and Type Variables

Grammar with parameterized types:
```text
// Type variables and generic types
TypeVar ::= /[a-z][a-zA-Z0-9']*/
TypeConstructor ::= /[A-Z][a-zA-Z0-9]*/
GenericType ::= TypeConstructor '<' TypeList '>'
TypeList ::= Type | TypeList ',' Type

// List and option types
ListType ::= Type 'list'
OptionType ::= Type 'option'
Type ::= TypeVar | TypeConstructor | GenericType | ListType | OptionType

// Polymorphic function types
ForallType ::= '∀' TypeVar '.' Type
Type ::= ... | ForallType

// Generic typing rules
Γ ⊢ e : List<τ>, Γ ⊢ f : τ → σ
------------------------------- (map)
List<σ>

Γ ⊢ e : Option<τ>
----------------- (some_elim)
τ ∨ ∅

∀α. Γ ⊢ e : τ[α]
----------------- (forall_intro)
∀α. τ
```

### Type Expression Usage Patterns

#### In Grammar Productions
```text
// Type annotations in productions
FunctionDef(func) ::= Type[ret] Identifier[name] '(' ParamList ')' Block[body]
TypedParam ::= Type[τ] Identifier[x]
Cast(cast) ::= '(' Type[target] ')' Expr[e]
```

#### In Typing Rule Premises
```text
// Complex premise patterns
Γ ⊢ obj : Record<{field: τ}>, Γ ⊢ field : 'string'
-------------------------------------------------- (field_access)
τ

Γ ⊢ f : ∀α. α → α, Γ ⊢ e : τ
----------------------------- (polymorphic_app)
τ
```

#### In Typing Rule Conclusions
```text
// Different conclusion forms
Γ ⊢ e : τ₁, Γ ⊢ e : τ₂
----------------------- (intersection)
τ₁ ∧ τ₂

x ∈ Γ
----- (lookup)
Γ(x)

Γ ⊢ value : τ
-------------- (extend_context)
Γ → Γ[x:τ] ⊢ τ
```

## Type Expression Validation and Best Practices

### Character Set and Validation

The type system validates expressions using the following character set:
```
abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_λτ→₁₂₃₄₅₆₇₈₉₀ ∧∨()!¬*[] where,.;''
```

Additional characters are dynamically added based on configured operators.

### Common Mistakes and Solutions

#### 1. Incorrect Operator Precedence
```text
// WRONG: Ambiguous without parentheses
A | B & C -> D

// CORRECT: Use parentheses to clarify intent
(A | B) & (C -> D)   // Intersection of union and function type
A | (B & C) -> D     // Union with function from intersection
```

#### 2. Invalid Context Names
```text
// WRONG: Invalid characters in context name
My_Context(x)

// CORRECT: Use Greek letters or simple identifiers
Γ(x)
Context(x)
```

#### 3. Malformed Array Types
```text
// WRONG: Missing brackets or invalid size
int[
*int[10

// CORRECT: Proper bracket matching
int[]
int[10]
*int[10]
```

#### 4. Incorrect Arrow Association
```text
// WRONG: Misunderstanding right-associativity
(A -> B) -> C   // This is A -> (B -> C), not (A -> B) -> C

// CORRECT: Explicit parentheses when needed
(A -> B) -> C   // Function from (A->B) to C
A -> B -> C     // Function from A to (B->C)
```

### Type Expression Design Guidelines

#### 1. Use Meaningful Type Names
```text
// GOOD: Descriptive type names
UserRecord, HttpResponse, DatabaseConnection

// AVOID: Generic or unclear names
T, X, Thing
```

#### 2. Consistent Naming Conventions
```text
// GOOD: Consistent Greek letters for type variables
τ, σ, ρ for types
Γ, Δ, Θ for contexts

// GOOD: Consistent subscripts
τ₁, τ₂, τ₃ for related types
```

#### 3. Logical Type Composition
```text
// GOOD: Meaningful logical combinations
Readable ∧ Writable     // Something that can be both read and written
Number ∨ String         // Either a number or string
¬Null                   // Anything that is not null

// AVOID: Nonsensical combinations
int ∧ string            // Nothing can be both int and string
¬⊤                      // Negation of universe is empty
```

#### 4. Context Management
```text
// GOOD: Clear context transformations
Γ[x:τ] ⊢ body : σ       // Add binding to context
Γ → Γ[x:τ] ⊢ τ          // Explicit context extension

// GOOD: Meaningful context names
LocalScope, GlobalEnv, TypeEnv
```

### Error Messages and Debugging

Common type expression parsing errors:

1. **"Invalid type expression"**: Usually indicates invalid characters or malformed syntax
2. **"Type expression cannot be empty"**: Empty string passed to type parser
3. **"Invalid ascription"**: Missing colon in `term : type` format
4. **"Invalid setting"**: Malformed context extension like `Γ[x:τ]`

### Performance Considerations

- **Complex nested types**: Deep nesting can impact parsing performance
- **Long union/intersection chains**: Consider factoring into separate type definitions
- **Unicode characters**: Fully supported but may have slight parsing overhead

### Integration with Grammar Productions

Type expressions integrate seamlessly with grammar productions:

```text
// Type-annotated grammar productions
FunctionDecl(func) ::= Type[returnType] Identifier[name] '(' ParamList ')' Block[body]
VariableDecl(var) ::= Type[varType] Identifier[name] ('=' Expr[init])?
TypeCast(cast) ::= '(' Type[targetType] ')' Expr[expr]

// Corresponding typing rules use the same type expressions
Γ ⊢ body : returnType, Γ ⊢ params : paramTypes
------------------------------------------------ (func)
paramTypes → returnType

Γ ⊢ init : varType
------------------ (var)
Γ → Γ[name:varType] ⊢ varType

Γ ⊢ expr : sourceType, sourceType <: targetType
----------------------------------------------- (cast)
targetType
```

## Current Limitations / Future Work
- Single context symbol Γ only (no Δ, Θ yet)
- No polymorphism / quantifiers
- No context joins/branching (if/else) or deletions
- Re-binding after deserialize not automated
- Type compatibility currently simple (structural compatibility + equality); no subtyping lattice

Planned enhancements: multi-context support, polymorphic/generalized types, context merging, richer repetition semantics, caching bound rule templates.

---
Concise: Grammar + typing rules produce ground `BoundTypingRule`s at parse time; the checker executes pure evaluation over these ground rules with optional functional context threading.

