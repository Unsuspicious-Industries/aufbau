// Identifier (supports Unicode)
Identifier ::= /[A-Za-z_][A-Za-z0-9]*/

// Variables with var typing rule
Variable(var) ::= Identifier[x]

TypeName ::= /[A-Za-z0-9_τ₁₂₃₄₅₆₇₈₉₀]+/

// Base types (parentheses are literals, hence quoted)
BaseType ::= TypeName | '(' Type ')'

// Function types (right-associative)
AtomicType ::= BaseType | '(' Type ')'
FunctionType ::= AtomicType '->' Type
Type ::= AtomicType | FunctionType

// Lambda abstraction (dot is a literal)
Lambda(lambda) ::= 'λ' Identifier[a] ':' Type[τ] '.' Expression[e]

// Atomic expressions
AtomicExpression ::= Variable | '(' Expression ')' | Lambda

// Applications (left-associative via iteration)
Application(app) ::= AtomicExpression[l] Expression[r]

// Expressions (start symbol)
Expression ::= AtomicExpression |  Application

// Typing Rules
x ∈ Γ
----------- (var)
Γ(x)

Γ[a:τ] ⊢ e : ?B
--------------------------- (lambda)
τ → ?B

Γ ⊢ r : ?A → ?B, Γ ⊢ l : ?A
-------------------------------- (app)
?B
