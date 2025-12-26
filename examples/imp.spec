Identifier ::= /[a-z]+/
TypeName ::= /[A-Z][a-z]*/
Integer(int) ::= /[0-9]+/[n]

Variable(var) ::= Identifier[x]
Value ::= Integer | Variable
Assignment(assign) ::= Identifier[x] ':' TypeName[τ] '=' Value[v] ';'

Operator ::= '+' | '-' | '*' | '/'
Operation(op) ::= Value[lhs] Operator[op] Value[rhs] ';'

Expression(expr) ::= Assignment | Operation
Program ::= Expression Program | Expression


// Typing rules
x ∈ Γ
----------- (var)
Γ(x)

Γ ⊢ v : τ
-------------------------------- (assign)
Γ → Γ[x:τ] ⊢ ∅

Γ ⊢ lhs : ?A, Γ ⊢ rhs : ?B, ?A = ?B
--------------------------------------(op)
?A

-------------------------------- (int)
'Int'
