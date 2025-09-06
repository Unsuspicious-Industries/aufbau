# Unified Grammar Specification

A unified grammar format that combines syntax definitions with typing rules using standard mathematical inference notation with a deterministic, unambiguous format. This specification includes a generalizable type checker that supports context membership, type ascription, and extensible judgment systems.

## Core Architecture

### Type Checker Design

The type checker implements a rule-based inference system with the following key components:

1. Typing Context (Γ): Maps variables to their types, supports extensions and lookups
2. Judgment System: Extensible framework for different logical operations on types
3. Premise Evaluation: Validates conditions before applying typing rules
4. Conclusion Inference: Determines result types from rule conclusions

### Bound Rule Evaluation (new)

- All meta-variable resolution from grammar/rules to concrete AST nodes and concrete types happens in the binding phase (bind module).
- The type checker consumes BoundTypingRule only; it does not perform meta-variable lookups at check time.
- Premises and conclusions reference concrete AST nodes and concrete Type values.
- This eliminates runtime binding lookups and simplifies evaluation.
- Strictness: the binder MUST supply only concrete types (no unresolved Type::Atom placeholders) and variable nodes MUST expose an extractable terminal name.

Implications:
- NonTerminal nodes that carry a typing rule must embed a BoundTypingRule where all variables are resolved.
- The checker evaluates premises directly on these nodes/types and uses the current context Γ only for lookups of terminal variable names.

## Syntax

### Production Rules
```
NonTerminal(typing_rule) ::= syntax_rhs
```

### Typing Rules
```
premises
--------- (typing_rule)
conclusion
```

Where:
- NonTerminal(typing_rule): Grammar rule name with typing rule reference in parentheses
- syntax_rhs: Traditional EBNF right-hand side with semantic bindings
- premises: Comma-separated conditions that must hold (above the line)
- -----: Horizontal inference bar
- (typing_rule): Typing rule name (matches the (typing_rule) annotation)
- conclusion: The typing judgment this rule establishes (below the line)

### Comments
``` 
// This is a comment
```

### Semantic Bindings
Non-terminals in the RHS bind semantic variables that capture their meaning. The bind module resolves these to concrete nodes/types and stores a BoundTypingRule on the NonTerminal.

### Repetition Operators

The grammar supports repetition operators for symbols in production rules:

- `Symbol*`: Zero or more occurrences (Kleene star)
- `Symbol+`: One or more occurrences (Kleene plus)
- `Symbol?`: Zero or one occurrence (optional)

#### Syntax Examples

```
// Zero or more statements in a block
Block ::= '{' Stmt* '}'

// One or more parameters
FunctionDef ::= 'def' Identifier '(' Param+ ')'

// Optional else clause
IfStmt ::= 'if' '(' Expr ')' Stmt Else?
Else ::= 'else' Stmt
```

#### Repetition with Bindings

Repetition operators can be combined with semantic bindings:

```
// Zero or more statements bound to variable 's'
Block ::= '{' Stmt[s]* '}'

// One or more parameters bound to variable 'p'
FunctionDef ::= 'def' Identifier[name] '(' Param[p]+ ')'
```

#### Parsing Behavior

- `*` (Zero or More): Parser attempts to match the symbol repeatedly until it fails, then continues. No error if zero matches are found.
- `+` (One or More): Same as `*` but requires at least one successful match. Error if zero matches are found.
- `?` (Zero or One): Parser attempts to match the symbol once. If it fails, continues without error. If it succeeds, does not attempt additional matches.

All repetition parsing uses backtracking to handle ambiguity and ensure correct parsing of surrounding symbols.

## Start Nonterminal (parsing)

- Convention: the start symbol is the last declared production LHS in the spec.
- Rationale: specs usually declare smaller building blocks first and the full program/term last.
- Loader behavior: when parsing the spec, the loader records the order of the first appearance of each production LHS. After loading, it sets Grammar.start to the last one seen.
- Override: a future extension may allow an explicit directive to set the start nonterminal, but by default the last LHS is used. The parser requires Grammar.start and does not guess.

## Typing rules

### Strict Rule Structure
Every typing rule MUST follow this exact format:
```
<premise1>, <premise2>, ..., <premiseN>
----------------------------------------- (<rule_name>)
<conclusion>
```

Validation Rules:
- Premises MUST be comma-separated (no other separators allowed)
- Rule name MUST match a production rule annotation exactly
- Conclusion MUST be a valid type expression or typing judgment
- All semantic variables MUST exist in the corresponding grammar production and be resolvable by the bind phase

### Premise Format

Premises establish the conditions that must hold for a typing rule to apply. Each premise consists of:

1. Setting (optional): Context information and operations
2. Judgment: A logical assertion about types or variables

#### Premise Types

1) Type Ascription Premises: [Γ[extensions]] ⊢ e : τ

Format: Γ[x:τ₁][y:τ₂] ⊢ e : σ

Components:
- Context: Γ with optional extensions [var:type]
- Expression: Resolved AST node (from bind phase)
- Type: Concrete Type value (variables resolved by bind phase when possible)

Evaluation:
- Temporarily extends the typing context with the given extensions; each extension is provided as BoundTypeAscription with a concrete AST node for the variable and a concrete Type.
- Recursively type-checks the provided node.
- Verifies the inferred type matches the expected type.
- Strictness: No fallback to context lookup; if no type can be inferred for the node in an ascription, the rule fails.

2) Context Membership Premises: x ∈ Γ

Format: x ∈ Γ

Components:
- Variable node: Concrete AST node whose terminal value is the variable name
- Context: Currently Γ

Evaluation:
- Checks if the variable name extracted from the node exists in Γ

#### Context Operations

Context Extensions: Γ[var:type]

In the bound system, extensions are provided as a list of BoundTypeAscription entries where var is a concrete AST node and type is a concrete Type.

Scoping:
- Extensions are temporary and only apply during premise evaluation
- Original context is restored after premise check
- Extensions shadow existing bindings with the same name

### Context Format

Valid Context Examples:
- Γ
- Γ[x:τ]
- Γ[x:τ₁][y:τ₂]
- Γ[f:τ->σ][x:τ]

### Conclusion Format

Conclusions specify the type or judgment that results from applying a typing rule.

1) Direct Type Expression
- Format: concrete Type, e.g., Int, τ₁ → τ₂ after bind resolution.
- In the bound system the checker returns the provided Type as-is (no further resolution).

2) Context Lookup
- Format: Γ(var_node)
- Usage: Retrieves the type of a variable from Γ using the terminal name extracted from var_node.

## Type Checker Algorithm (updated)

Core Checking Process:
1. Rule Resolution:
   - If node has a BoundTypingRule: apply bound-rule inference
   - Else if the node has one nonterminal child: bubble through
   - Else: error
2. Premise Validation (bound):
   - For each bound premise, optionally extend Γ with BoundTypeAscription entries (node → name extracted, type), then check the bound judgment directly
3. Conclusion Inference (bound):
   - If Type: return it
   - If ContextLookup: extract name from node and look up in Γ

Removal of runtime binding lookups:
- The checker no longer uses meta-variable resolution during checking. All such resolution must be performed by bind.
- Strictness: No fallbacks are performed by the checker; all required information must be supplied by the binder.

## Assigning types directly in context

In some rules you can avoid pattern variables by assigning a type directly into Γ via settings, then relying on ContextLookup or Membership. For example, a rule can add Γ[x: Int] via settings and use Γ(x) as conclusion.

## Example: Simply Typed Lambda Calculus (updated)

Typing Rules (bound-friendly):

// Variable rule: look up type in context
x ∈ Γ
------------- (var)
Γ(x)

// Lambda rule: function type formation
Γ[x:τ₁] ⊢ e : τ₂
----------------------- (lambda)
τ₁ → τ₂

// Application rule: function application
Γ ⊢ f : τ₁ → τ₂, Γ ⊢ e : τ₁
------------------------------- (app)
τ₂

The bind phase resolves x, e, f, τ₁, τ₂ to concrete nodes/types in the BoundTypingRule attached to the AST. The checker then evaluates the rule without additional binding lookups.

## Limitations and Future Work

- Single context Γ for now
- No polymorphism or subtyping yet
- Bind phase is responsible for correctness of resolution (well-formedness checks recommended)

