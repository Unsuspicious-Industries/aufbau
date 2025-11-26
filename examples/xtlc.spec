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

// variable declaration
Let(let) ::= '{' Identifier[x] ':' Type[τ] '}'


// Base terms (cannot be applications; parentheses are literal tokens)
BaseTerm ::= Variable | Lambda | '(' Term ')' 

// Applications (left-associative via iteration)
Application(app) ::= BaseTerm[f] BaseTerm[e]


// Terms
Term ::=  Application[e] | BaseTerm[e] 

Expr ::= Term | Let

Program ::= Expr ProgramTail
ProgramTail ::= ε | Expr ProgramTail

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

-------------------------------- (let)
Γ -> Γ[x:τ] ⊢ τ