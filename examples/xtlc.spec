// this is a tentative sec for an extended lambda calculus with
// product types (pairs)

Identifier ::= /[A-Za-z_][A-Za-z0-9]*/
// Type syntax
TypeName ::= Identifier
BaseType ::= TypeName | '(' Type ')'
Type ::= BaseType '->' Type | BaseType
// Term syntax
Variable(var) ::= Identifier[x]
Lambda(lambda) ::= 'λ' Variable[x] ':' Type[τ] '.' Term[e]
Let(let) ::= '{' Identifier[x] ':' Type[τ]  '}'
BaseTerm ::= Variable | Lambda |  '(' Term ')'

// Application is left-associative: f x y = (f x) y
// Using right-recursive grammar that builds left-associative structure:
// - AppChain matches "BaseTerm BaseTerm*" 
// - Each Application node is created for adjacent pairs
// - The AppTail wraps the result to build nested structure
Application(app) ::= BaseTerm[f] AppTail[e]
AppTail ::= BaseTerm AppTail | BaseTerm

Term ::= Application[e] | BaseTerm[e]
Expr ::= Term | Let
Program ::= Expr ProgramTail
ProgramTail ::= ε | Expr ProgramTail

// typing rules

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
Γ → Γ[x:τ] ⊢ ∅

