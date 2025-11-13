# Unified Grammar & Typing Specification (Bound Rule Model)

This document specifies the unified grammar + typing rule format and explains how the bound rule pipeline works in the current implementation.

## High-Level Flow

spec grammar + typing rules  → (load) → `Grammar` (productions + `TypingRule`s)
→ input source text → (parser) → AST (`NonTerminal.bound_typing_rule` attached where annotated)
→ type checker consumes only `BoundTypingRule` (no meta lookup)

Key invariant: By the time checking starts, every rule the checker sees is ground: all schematic term/type variables resolved to concrete AST nodes and `BoundType` structures.

### Syntactic sugar

While repetition are needed for same level multiple binding type parsing (see [[challenges.md]]), groups and other forms can be considered *syntactic sugar* that desugar into core forms before binding. 

Here, groups are converted to phantom nonterminals that just wrap their contents, and other constructs can be similarly desugared.

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

### Repetition Operators
- `Symbol*` zero or more
- `Symbol+` one or more
- `Symbol?` zero or one
Bindings attach before the operator: `Expr[e]*`.

The binder collects all sibling nodes sharing a binding symbol to expand premises.

## Type System

The grammar system supports an extensible type language with various type expressions and constructs. This section details all supported type expressions and their syntax.

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

3. **Context-Transformation**: `[Γ_in][→ Γ_out] ⊢ τ`
   - We can modify the *output* context, based on declarations.
   - Right now the name of the output context doesnt chnage anything, its for looks, 
     what matters is the extensions applies to it. 
   - Input context can also be extended:
       - `Γ_in → Γ_out[y:σ] ⊢ τ'`
   - Arrow sugar variants:
     - `Γ_in ⊢ τ` ≡ `Γ_in → Γ_in ⊢ τ` (no context change)
     - `→ Γ_out ⊢ τ` ≡ `Γ → Γ_out ⊢ τ` (inherit input context)
     - `τ` ≡ `Γ → Γ ⊢ τ` (identity transform)

#### Some examples
This let rule modify the context to add the variable *binded* to `x`, with the type of the thing being binded to `τ`.
```
-------------------------------- (let)
Γ -> Γ[x:τ] ⊢ τ
```

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



### Type Expression Usage Patterns

#### In Grammar Productions
```text
// Type annotations in productions
FunctionDef(func) ::= Type[ret] Identifier[name] '(' ParamList ')' Block[body]
TypedParam ::= Type[τ] Identifier[x]
Cast(cast) ::= '(' Type[target] ')' Expr[e]
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


Additional characters are dynamically added based on configured operators.

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

// Type compatibility handles subtyping with ':'
Γ ⊢ expr : sourceType, sourceType : targetType
----------------------------------------------- (cast)
targetType
```

## Current Limitations / Future Work
- Single context symbol Γ only (no Δ, Θ yet)
- No polymorphism / quantifiers
- No context joins/branching (if/else) or deletions
- Re-binding after deserialize not automated
- No pointer/reference/array type checking logic yet
- Type compatibility currently simple (structural compatibility + equality); no subtyping lattice

Planned enhancements: multi-context support, polymorphic/generalized types, context merging, richer repetition semantics, caching bound rule templates.

---
Concise: Grammar + typing rules produce ground `BoundTypingRule`s at parse time; the checker executes pure evaluation over these ground rules with optional functional context threading.

