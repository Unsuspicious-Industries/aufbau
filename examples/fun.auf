// Fun — a functional language with modern syntax but STLC semantics
// Type system has same power as STLC: no polymorphism, just monomorphic functions

// ===================== Identifiers =====================
Identifier ::= /[a-z][a-z0-9]*/
TypeName ::= /[A-Z][a-z0-9]*/

// ===================== Type Annotations =====================
BaseType ::= TypeName | '(' Type ')'
FunctionType ::= BaseType '->' Type
Type ::= BaseType | FunctionType

// ===================== Literals =====================
Integer(int) ::= /[0-9]+/
Boolean(bool) ::= 'true' | 'false'
Float(float) ::= /[0-9]+\.[0-9]+/


// ===================== Operators =====================

// Integer operators
IntOp ::= '+' | '-' | '*' | '/'

// Float operators (OCaml-style)
FloatOp ::= '+.' | '-.' | '*.' | '/.'


// ===================== Expressions =====================

// Variable reference
Variable(var) ::= Identifier[x]

// Lambda: (x: Int) => x + 1
Lambda(lambda) ::= '(' Identifier[param] ':' Type[τ] ')' '=>' Expression[body]

// Let binding
Let(let) ::= 'let' Identifier[name] ':' Type[τ] '=' Expression[value] ';' Expression[body]

// Atomic expressions
AtomicExpression ::= Variable
                   | Integer
                   | Float
                   | Boolean
                   | Lambda
                   | '(' Expression ')'

// Binary integer operation
IntBinary(bin_int) ::= Expression[left] IntOp[op] Expression[right]

// Binary float operation
FloatBinary(bin_float) ::= Expression[left] FloatOp[op] Expression[right]

// Application
Application(app) ::= AtomicExpression[func] '(' Expression[arg] ')'

// Top-level expression
Expression ::= IntBinary
             | FloatBinary
             | Application
             | Let
             | AtomicExpression


// ===================== Typing Rules =====================

// Variable lookup
x ∈ Γ
----------- (var)
Γ(x)

// Integer literal
----------- (int)
'Int'

// Float literal
----------- (float)
'Float'

// Boolean literal
----------- (bool)
'Bool'

// Lambda
Γ[param:τ] ⊢ body : ?R
----------------------- (lambda)
τ -> ?R

// Let
Γ ⊢ value : τ, Γ[name:τ] ⊢ body : ?R
------------------------------------- (let)
?R

// Integer binary operators
Γ ⊢ left : 'Int', Γ ⊢ right : 'Int'
---------------------------------- (bin_int)
'Int'

// Float binary operators
Γ ⊢ left : 'Float', Γ ⊢ right : 'Float'
-------------------------------------- (bin_float)
'Float'

// Application
Γ ⊢ func : ?A -> ?R, Γ ⊢ arg : ?A
---------------------------------- (app)
?R
