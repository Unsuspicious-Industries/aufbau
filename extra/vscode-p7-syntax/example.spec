// P7 Grammar and Typing Rules Example

// Basic identifier support with Unicode
Identifier ::= /[\p{L}][\p{L}\p{N}_τ₁₂₃₄₅₆₇₈₉₀]*/

// Variables with typing rule annotation
Variable(var) ::= Identifier[x]

// Type system with various type expressions
BaseType ::= 'int' | 'bool' | 'string'
PointerType ::= '*' Type[base]
ArrayType ::= Type[base] '[' Number? ']'
FunctionType ::= Type[τ₁] '->' Type[τ₂]
UnionType ::= Type[τ₁] '|' Type[τ₂]
IntersectionType ::= Type[τ₁] '&' Type[τ₂]
Type ::= BaseType | PointerType | ArrayType | FunctionType | UnionType | IntersectionType

// Lambda calculus expressions
Lambda(lambda) ::= 'λ' Variable[x] ':' Type[τ₁] '.' Term[e]
Application(app) ::= Term[f] Term[a]
Term ::= Variable | Lambda | Application | '(' Term ')'

// Complex type expressions examples
ComplexType ::= '(' Γ(input) ∨ Default ')' → Γ(output)
AdvancedType ::= ¬(*int ∨ *string) ∧ (Serializable → Γ(T))

// Typing rules with Unicode symbols

// Variable lookup rule
x ∈ Γ
----------- (var)
Γ(x)

// Lambda abstraction rule
Γ[x:τ₁] ⊢ e : τ₂
--------------------------- (lambda)
τ₁ → τ₂

// Function application rule
Γ ⊢ f : τ₁ → τ₂, Γ ⊢ e : τ₁
-------------------------------- (app)
τ₂

// Context-transforming rule
Γ ⊢ value : τ, Γ[x:τ] ⊢ body : σ
---------------------------------- (let)
Γ → Γ[x:τ] ⊢ σ

// Pointer dereference
Γ ⊢ e : *τ
---------- (deref)
τ

// Array access
Γ ⊢ arr : τ[], Γ ⊢ idx : 'int'
------------------------------- (access)
τ

// Union type introduction
Γ ⊢ e : τ₁
----------- (union_intro_left)
τ₁ ∨ τ₂

// Intersection type introduction
Γ ⊢ e : τ₁, Γ ⊢ e : τ₂
----------------------- (intersection_intro)
τ₁ ∧ τ₂

// Complex rule with multiple premises
Γ ⊢ obj : Record<{field: τ}>, Γ ⊢ field : 'string', field ∈ obj
---------------------------------------------------------------- (field_access)
τ

// Axiom rule (no premises)
-------------------------------- (unit)
⊤
