// Extended Typed Lambda Calculus 

pub const XTLC_SPEC: &str = r#"

// Identifier (supports Unicode)
Identifier ::= /[\p{L}][\p{L}\p{N}_τ₁₂₃₄₅₆₇₈₉₀]*/

// Variables with var typing rule
Variable(var) ::= Identifier[x]

// Type names (supports Unicode type variables like τ₁, τ₂)
TypeName ::= Identifier

// Base types
BaseType ::= TypeName | '(' Type ')'

// Function types (right-associative)
Type ::= BaseType[τ₁] '->' Type[τ₂] | BaseType[τ]

// Typed parameter
TypedParam ::= Variable[x] ':' Type[τ]

// Lambda abstraction
Lambda(lambda) ::= 'λ' TypedParam '.' Term[e]


// Base terms (cannot be applications)
BaseTerm ::= Variable | Lambda | '(' Term ')'

// Applications (left-associative via iteration)
Application(app) ::= BaseTerm[f] BaseTerm[e]

// Terms
Term ::= Application[e] | BaseTerm[e]

// Typing Rules
x ∈ Γ
----------- (var)
Γ(x)

Γ[x:τ₁] ⊢ e : τ₂
--------------------------- (lambda)
τ₁ → τ₂

Γ ⊢ f : τ₁ → τ₂, Γ ⊢ e : τ₁
-------------------------------- (app)
τ₂
"#;