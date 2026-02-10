// ===================== Core Tokens =====================
Identifier ::= /[a-z][a-z0-9_]*/
TypeName ::= /[A-Z][a-z]*/

// ===================== Types (with unions) =====================
BaseType ::= TypeName | '(' Type ')'
UnionType ::= BaseType '|' Type
Type ::= BaseType | UnionType

// ===================== Literals =====================
Integer(int) ::= /[0-9]+/[n]
Boolean(bool) ::= 'true' | 'false'
Number(int_num) ::= Integer
Number(neg_int) ::= '-' Integer[n]

// ===================== Expressions =====================
Variable(var) ::= Identifier[x]
Atom ::= Number | Boolean | Variable | '(' Value ')'

ArithOp ::= '+' | '-' | '*' | '/'
CmpOp ::= '==' | '!=' | '<' | '<=' | '>' | '>='

Value(value_atom) ::= Atom[v]
Value(value_bin) ::= Atom[lhs] ArithOp[op] Value[rhs]
Condition(cond) ::= Value[lhs] CmpOp[op] Value[rhs]

// ===================== Statements =====================
Assignment(assign) ::= Identifier[x] ':' Type[τ] '=' Value[v] ';'
Operation(op) ::= Value[lhs] ArithOp[op] Value[rhs] ';'
If(if) ::= 'if' Condition[c] '{' Program[t] '}' 'else' '{' Program[e] '}'
While(while) ::= 'while' Condition[c] '{' Program[body] '}'

Statement(stmt) ::= Assignment[stmt] | Operation[stmt] | If[stmt] | While[stmt]
Program(prog_cons) ::= Statement[a] Program[b]
Program(prog_one) ::= Statement[a]
Program(prog_empty) ::= ε

// ===================== Typing Rules =====================
x ∈ Γ
----------- (var)
Γ(x)

----------- (int)
'Int'

----------- (bool)
'Bool'

----------- (int_num)
'Int'

----------- (neg_int)
'Int'

Γ ⊢ v : ?T
---------- (value_atom)
?T

Γ ⊢ lhs : 'Int', Γ ⊢ rhs : 'Int'
-------------------------------- (value_bin)
'Int'

Γ ⊢ v : ?A, ?A ⊆ τ
------------------------- (assign)
Γ → Γ[x:τ] ⊢ ∅

Γ ⊢ lhs : 'Int', Γ ⊢ rhs : 'Int'
------------------------------- (op)
'Int'

Γ ⊢ lhs : ?A, Γ ⊢ rhs : ?A
-------------------------- (cond)
'Bool'

Γ ⊢ c : 'Bool', Γt ⊢ t : ?T, Γe ⊢ e : ?T
-------------------------------------- (if)
?T

Γ ⊢ c : 'Bool', Γbody ⊢ body : ?B
----------------------------- (while)
∅

Γ ⊢ a : ?A, Γ ⊢ b : ?B
---------------------- (prog_cons)
?B

Γ ⊢ stmt : ?S
------------- (stmt)
?S

Γ ⊢ a : ?A
---------- (prog_one)
?A

----------- (prog_empty)
∅
