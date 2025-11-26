// Simple Typed Lambda Calculus (STLC)
// Mirrors the specification described in docs/challenges.md

// Grammar for expressions
Identifier ::= /[a-z][a-zA-Z0-9]*/
Variable(dec) ::= Identifier[x]
Abstraction(abs) ::= 'λ' Identifier[x] ':' Type[τ] '.' Expression[e]

AtomicExpression ::= Variable | '(' Expression ')'
Application(app) ::= AtomicExpression[e1] AtomicExpression[e2]

BaseType ::= 'Int' | 'Bool'
AtomicType ::= BaseType | '(' Type ')'
FunctionType ::= AtomicType[τ1] '→' Type[τ2]
Type ::= AtomicType | FunctionType

Expression ::= AtomicExpression | Abstraction | Application

// Typing rules (dec, abs, app)
x ∈ Γ
----------- (dec)
Γ(x)

Γ[x:τ1] ⊢ e : τ2
----------------------- (abs)
τ1 → τ2

Γ ⊢ e1 : ?A → ?B, Γ ⊢ e2 : ?A
-------------------------------- (app)
?B
