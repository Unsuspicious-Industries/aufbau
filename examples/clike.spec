// Identifiers and literals
Identifier ::= /[a-zA-Z_][a-zA-Z0-9_]*/
Number(int-lit) ::= /\d+/
String ::= /"[^"]*"/

// Variables
Variable(var) ::= Identifier[x]

// Primitive types
PrimitiveType ::= 'int' | 'float' | 'char' | 'bool' | 'void'

// All types
Type ::= PrimitiveType | '(' Type ')'

// Expressions
Literal ::= Number | String | 'true' | 'false' | 'NULL'

// Binary operations (restructured to avoid left recursion)
ArOp ::= '+' | '-' | '*' | '/' | '%'
BoolOp ::= '==' | '!=' | '<' | '>' | '<=' | '>='

// Primary expressions (atomic expressions)
Primary ::= Literal | Variable | '(' Expr ')'

// Function calls
Args ::= Expr | Expr ',' Args
Call(call) ::= Identifier[name] '(' ')' | Identifier[name] '(' Args ')'

// Right-recursive expressions to avoid left recursion
ArOpExpr(ar-op-expr) ::= Primary[left] ArOp[op] Expr[right]
BoolOpExpr(bool-op-expr) ::= Primary[left] BoolOp[op] Expr[right]

// All expressions (no left recursion)
Expr ::= ArOpExpr 
    | BoolOpExpr 
    | Call
    | Primary 

// Statements
VarDeclInit(vardecl) ::= Type[type] Variable[var] '=' Expr[init] ';'
VarDeclNoInit(vardecl_noinit) ::= Type[type] Variable[var] ';'
VarDecl ::= VarDeclInit | VarDeclNoInit

VarInitForInit(vardecl) ::= Type[type] Variable[var] '=' Expr[init]
VarInitForNoInit(vardecl_noinit) ::= Type[type] Variable[var]
VarInitFor ::= VarInitForInit | VarInitForNoInit

Assignment(assign) ::= Expr[target] '=' Expr[value]
AssignmentStmt(assignstmt) ::= Assignment[a] ';'
ElseOpt ::= 'else' BlockStmt[else] | ε
IfStmt(if) ::= 'if' '(' Expr[cond] ')' BlockStmt[then] ElseOpt
WhileStmt(while) ::= 'while' '(' Expr[cond] ')' Stmt[body]
// For-loop header with specific init/update forms and separators
ForInit ::= VarInitFor | Assignment
ForUpdate ::= Assignment
ForStmt(for) ::= 'for' '(' ForInit[init] ';' Expr[cond] ';' ForUpdate[update] ')' Stmt[body]

ReturnStmt(return) ::= 'return' Expr[ret_val] ';'


StmtSeq ::= Stmt[s] StmtSeq | Stmt[s]
StmtSeqOpt ::= ε | StmtSeq
BlockStmt(block) ::= '{' StmtSeqOpt '}'

ExprStmt(exprstmt) ::= Expr[e] ';'

Stmt ::= VarDecl | AssignmentStmt | IfStmt | WhileStmt | ForStmt | BlockStmt | ReturnStmt[ret]

FunctionDef(funcdef) ::= Type[ret_ty] Identifier[name] '(' ParamDeclListOpt ')' '{' StmtSeqOpt '}'
ParamDecl ::= Type[in_tys] Identifier
ParamDeclTail ::= ',' ParamDecl ParamDeclTail | ε
ParamDeclList ::= ParamDecl ParamDeclTail
ParamDeclListOpt ::= ε | ParamDeclList

// Program (sequence of items)
Item ::= FunctionDef | VarDecl
Program ::= Item | Item Program


// Type rule for Int literals - concrete int type
-------------- (int-lit)
'int'


// var stuff
x ∈ Γ
----------- (var)
Γ(x)

// use context call to find variable types
Γ ⊢ right: τ, Γ ⊢ left: τ
----------- (bool-op-expr)
'bool'

// should be cool
Γ ⊢ right: τ, Γ ⊢ left: τ
----------- (ar-op-expr)
τ


// var decl with initializer 
Γ ⊢ init : type
------------------- (vardecl)
Γ -> Γ[var:type] ⊢ 'void'

// var decl without initializer commits to Γ
------------------- (vardecl_noinit)  
Γ -> Γ[var:type] ⊢ 'void'

// if/while/for/assign typing
Γ ⊢ cond : 'bool', Γ ⊢ then : 'void'
---------------- (if)
'void'

Γ ⊢ cond : 'bool', Γ ⊢ body : 'void'
---------------- (while)
'void'

Γ ⊢ init : 'void', Γ ⊢ cond : 'int', Γ ⊢ update : 'void', Γ ⊢ body : 'void'
---------------- (for)
'void'

Γ ⊢ target : τ, Γ ⊢ value : τ
---------------- (assign)
'void'

// blocks, return and expr statements are void-typed statements
Γ ⊢ s : 'void'
---------------- (block)
'void'

-------------- (return)
'void'

Γ ⊢ e : τ
---------------- (exprstmt)
'void'

Γ ⊢ a : 'void'
---------------- (assignstmt)
'void'

// function: check return type matches
// Note: s is the statement, ret is the return statement node
// We use simpler typing for now
----------------------- (funcdef)
(in_tys...) -> ret_ty

