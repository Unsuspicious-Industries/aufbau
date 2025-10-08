//Simple Typed Lambda Calculus (STLC)
//A minimal functional language with lambda abstractions, applications, and variables

# Grammar for expressions
Identifier ::= /[a-z][a-zA-Z0-9]*/
Variable(dec) ::= Identifier[x]
Abstraction(abs) ::= 'λ' Identifier[x] ':' Type[τ] '.' Expression[e]
Application(app) ::= Expression[e1] Expression[e2]


//Grammar for types
BaseType ::= 'Int' | 'Bool'
FunctionType ::= Type[τ1] '→' Type[τ2]
Type ::= BaseType | FunctionType | '(' Type ')'

Expression ::= Variable | Abstraction | Application | '(' Expression ')'

//Variable lookup rule
x ∈ Γ
----------- (dec)
Γ(x)

//Lambda abstraction rule  
Γ[x:τ1] ⊢ e : τ2
----------------------- (abs)
τ1 → τ2

//Function application rule
Γ ⊢ e1 : τ1 → τ2,   Γ ⊢ e2 : τ1
-------------------------------- (app)
τ2

