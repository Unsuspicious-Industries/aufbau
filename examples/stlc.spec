//Simple Typed Lambda Calculus (STLC)
//A minimal functional language with lambda abstractions, applications, and variables

// Grammar for expressions
Identifier ::= /[a-z][a-zA-Z0-9]*/
Variable(dec) ::= Identifier[x]
Abstraction(abs) ::= 'λ' Identifier[x] ':' Type[τ] '.' Expression[e]

// Atomic expressions (no left recursion)
AtomicExpression ::= Variable | '(' Expression ')'

// Application uses right-associativity to avoid left recursion
Application(app) ::= AtomicExpression[e1] AtomicExpression[e2]+

//Grammar for types  
BaseType ::= 'Int' | 'Bool'
AtomicType ::= BaseType | '(' Type ')'

// Function types are right-associative to avoid left recursion
FunctionType ::= AtomicType[τ1] '→' Type[τ2]

Type ::= AtomicType | FunctionType

// Move Expression to be last so it becomes the start symbol
Expression ::= AtomicExpression | Abstraction | Application

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

