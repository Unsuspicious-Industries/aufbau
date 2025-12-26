// Identifier (supports Unicode)
Identifier ::= /[A-Za-z_][A-Za-z0-9_τ₁₂₃₄₅₆₇₈₉₀]*/

// Variables with var typing rule
Variable(var) ::= Identifier[x]

// Type names (supports Unicode type variables like τ₁, τ₂)
TypeName ::= Identifier

// Base types (parentheses are literals, hence quoted)
BaseType ::= TypeName | '(' Type ')'

// Function types (right-associative)
Type ::= BaseType[τ₁] '->' Type[τ₂] | BaseType[τ]

// Lambda abstraction (dot is a literal)
Lambda(lambda) ::= 'λ' Variable[x] ':' Type[τ] '.' Term[e]

BaseTerm ::= Term | Variable

// Applications (left-associative via iteration)
Application(app) ::= BaseTerm[f] BaseTerm[e]

// Terms
Term ::=  Application | Lambda | '(' Term ')'


// Typing Rules
x ∈ Γ
----------- (var)
Γ(x)

Γ[x:τ] ⊢ e : ?B
--------------------------- (lambda)
τ → ?B

Γ ⊢ f : ?A → ?B, Γ ⊢ e : ?A
-------------------------------- (app)
?B
