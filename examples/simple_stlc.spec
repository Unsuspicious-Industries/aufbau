# Simple Typed Lambda Calculus (STLC)
# A minimal functional language with lambda abstractions, applications, and variables

# Grammar for expressions
Identifier ::= /[a-z][a-zA-Z0-9]*/
Variable(var) ::= Identifier[x]
Abstraction(abs) ::= 'λ' Identifier[x] ':' Type[τ] '.' Expression[e]
Application(app) ::= Expression[e1] Expression[e2]
Expression ::= Variable | Abstraction | Application | '(' Expression ')'

# Grammar for types
BaseType ::= 'Int' | 'Bool'
FunctionType(fun_type) ::= Type[τ1] '→' Type[τ2]
Type ::= BaseType | FunctionType | '(' Type ')'

# Typing rules for STLC

# Variable lookup rule
x ∈ Γ
----------- (var)
Γ(x)

# Lambda abstraction rule  
Γ[x:τ1] ⊢ e : τ2
----------------------- (abs)
Γ ⊢ λx:τ1.e : τ1 → τ2

# Function application rule
Γ ⊢ e1 : τ1 → τ2,   Γ ⊢ e2 : τ1
-------------------------------- (app)
Γ ⊢ e1 e2 : τ2

# Function type construction rule
Γ ⊢ τ1 : Type,   Γ ⊢ τ2 : Type
------------------------------- (fun_type)
Γ ⊢ τ1 → τ2 : Type
