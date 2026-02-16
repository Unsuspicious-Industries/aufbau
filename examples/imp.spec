// Imp — an imperative language with modern syntax
// Statements for control flow, expressions for computation

// ===================== Identifiers =====================
Identifier ::= /[a-z][a-z0-9_]*/
TypeName ::= 'Int' | 'Bool' 

// ===================== Types =====================
BaseType ::= TypeName | '(' Type ')'
UnionType ::= BaseType '|' Type
Type ::= BaseType | UnionType

// ===================== Literals =====================
Integer(int_lit) ::= /[0-9]+/
Boolean(bool_lit) ::= 'true' | 'false'

// ===================== Expressions =====================
// Expressions compute values, no side effects

Variable(var) ::= Identifier[x]

// Atomic expressions
AtomicExpr ::= Variable | Integer | Boolean | '(' Expression ')'

// Binary operations - no left recursion!
// We build up precedence by having expression reference the next level
ArithOp(arith_op) ::= AtomicExpr[lhs] ArithOperator[op] AtomicExpr[rhs]

ArithOperator ::= '+' | '-' | '*' | '/' 

CompOp(comp_op) ::= AtomicExpr[lhs] CompOperator[op] AtomicExpr[rhs]
CompOperator ::= '==' | '!=' | '<' | '<=' | '>' | '>='

// Top level expression
Expression ::= ArithOp | CompOp | AtomicExpr

// ===================== Statements =====================
// Statements perform actions, modify state

// Variable declaration: let x: Int = 5;
Declaration(decl) ::= 'let' Identifier[name] ':' Type[τ] '=' Expression[value] ';'

// Assignment (to existing variable): x = 10;
Assignment(assign) ::= Identifier[name] '=' Expression[value] ';'

// If statement with optional else
IfStmt(if) ::= 'if' '(' Expression[cond] ')' Block[then_block]
IfElseStmt(if_else) ::= 'if' '(' Expression[cond] ')' Block[then_block] 'else' Block[else_block]

// While loop
WhileStmt(while) ::= 'while' '(' Expression[cond] ')' Block[body]

// Block: { statements }
Block(block) ::= '{' Statements[stmts] '}'

// Statement list
Statement ::= Declaration | Assignment | IfStmt | IfElseStmt | WhileStmt

Statements(stmts) ::= Statement[head] Statements[tail] | ε

// Top-level program is a block
Program(program) ::= Block[main]

// ===================== Typing Rules =====================

// ---------- Expressions (produce values) ----------

// Variable lookup
x ∈ Γ
----------- (var)
Γ(x)

// Integer literal
----------- (int_lit)
'Int'

// Boolean literal
----------- (bool_lit)
'Bool'

// Binary operation
Γ ⊢ lhs : 'Int', Γ ⊢ rhs : 'Int'
------------------------------------------------- (arith_op)
'Int'

Γ ⊢ lhs : 'Int', Γ ⊢ rhs : 'Int'
------------------------------------------------- (comp_op)
'Bool'


// ---------- Statements (produce void type ∅) ----------

// Declaration: extend context, check value type
Γ ⊢ value : τ
----------------------- (decl)
Γ → Γ[name:τ] ⊢ ∅

// Assignment: variable must exist and types must match
name ∈ Γ, Γ ⊢ value : Γ(name)
------------------------------ (assign)
∅

// If statement: condition is Bool, body produces ?T (no ctx propagation from block)
Γ ⊢ cond : 'Bool', [Γ] ⊢ then_block : ?T
--------------------------------------- (if)
?T

// If-else: both branches produce same type
Γ ⊢ cond : 'Bool', [Γ] ⊢ then_block : ?T, [Γ] ⊢ else_block : ?T
------------------------------------------------------------ (if_else)
?T

// While: condition is Bool, body produces ?T
Γ ⊢ cond : 'Bool', [Γ] ⊢ body : ?T
-------------------------------- (while)
?T

// check without asserting type
Γ ▷ head, Γ ▷ tail 
-------------- (stmts)
∅

// Block: statements produce ?T (no ctx propagation)
[Γ] ▷ stmts 
-------------- (block)
∅


// Program
Γ ▷ main 
-------------- (program)
∅
