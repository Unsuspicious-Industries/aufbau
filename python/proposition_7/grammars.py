"""Built-in grammars with typing rules (context-dependant generation)."""

from typing import Dict, List

GRAMMARS: Dict[str, str] = {
    
    # Simply Typed Lambda Calculus
    "xtlc": """
    Identifier ::= /[A-Za-z_][A-Za-z0-9_τ₁₂₃₄₅₆₇₈₉₀]*/
    Variable(var) ::= Identifier[x]
    TypeName ::= Identifier
    BaseType ::= TypeName | '(' Type ')'
    Type ::= BaseType[τ₁] '->' Type[τ₂] | BaseType[τ]

    Lambda(lambda) ::= 'λ' Variable[x] ':' Type[τ] '.' Term[e]
    Let(let) ::= '{' Identifier[x] ':' Type[τ] '}'

    BaseTerm ::= Variable | Lambda | '(' Term ')' 
    Application(app) ::= BaseTerm[f] BaseTerm[e]

    Term ::=  Application[e] | BaseTerm[e] 
    Expr ::= Term | Let

    Program ::= Expr ProgramTail
    ProgramTail ::= ε | Expr ProgramTail

    x ∈ Γ
    ----------- (var)
    Γ(x)

    Γ[x:τ] ⊢ e : ?B
    --------------------------- (lambda)
    τ → ?B

    Γ ⊢ f : ?A → ?B, Γ ⊢ e : ?A
    -------------------------------- (app)
    ?B

    -------------------------------- (let)
    Γ -> Γ[x:τ] ⊢ τ
    """,
    
    # C-like with type checking
    "clike": """
        Identifier ::= /[a-zA-Z_][a-zA-Z0-9_]*/
        Number(int_lit) ::= /[0-9]+/
        String ::= '"' /[^"]*/ '"'
        
        Variable(var) ::= Identifier[x]
        
        PrimitiveType ::= 'int' | 'float' | 'char' | 'bool' | 'void'
        Type ::= PrimitiveType | '(' Type ')'
        
        Literal ::= Number | String | 'true' | 'false'
        ArOp ::= '+' | '-' | '*' | '/'
        BoolOp ::= '==' | '!=' | '<' | '>'
        
        Primary ::= Literal | Variable | '(' Expr ')'
        ArOpExpr(ar_op) ::= Primary[left] ArOp[op] Expr[right]
        BoolOpExpr(bool_op) ::= Primary[left] BoolOp[op] Expr[right]
        Expr ::= ArOpExpr | BoolOpExpr | Primary
        
        VarDecl(vardecl) ::= Type[type] Variable[var] '=' Expr[init] ';'
        Assignment(assign) ::= Variable[target] '=' Expr[value] ';'
        IfStmt(if_stmt) ::= 'if' '(' Expr[cond] ')' '{' Stmt '}' 
        WhileStmt(while_stmt) ::= 'while' '(' Expr[cond] ')' '{' Stmt '}'
        ReturnStmt(return_stmt) ::= 'return' Expr[ret] ';'
        
        Stmt ::= VarDecl | Assignment | IfStmt | WhileStmt | ReturnStmt
        start ::= Stmt
        
        -------------- (int_lit)
        'int'
        
        x ∈ Γ
        ----------- (var)
        Γ(x)
        
        Γ ⊢ left : ?T, Γ ⊢ right : ?T
        ----------- (ar_op)
        ?T
        
        Γ ⊢ left : ?T, Γ ⊢ right : ?T
        ----------- (bool_op)
        'bool'
        
        Γ ⊢ init : type
        ------------------- (vardecl)
        Γ -> Γ[var:type] ⊢ 'void'
        
        Γ ⊢ target : ?T, Γ ⊢ value : ?T
        ---------------- (assign)
        'void'
        
        Γ ⊢ cond : 'bool'
        ---------------- (if_stmt)
        'void'
        
        Γ ⊢ cond : 'bool'
        ---------------- (while_stmt)
        'void'
        
        Γ ⊢ ret : ?T
        -------------- (return_stmt)
        'void'
    """,
    
    # Typed arithmatic
    "typed_arithmetic": """
        IntLit(int_lit) ::= /[0-9]+/
        FloatLit(float_lit) ::= /[0-9]+\\.[0-9]+/
        
        Variable(var) ::= /[a-z][a-z0-9]*/[x]
        
        ArithOp ::= '+' | '-' | '*' | '/'
        CompOp ::= '<' | '>' | '==' | '!='
        
        Atom ::= IntLit | FloatLit | Variable | '(' Expr ')'
        ArithExpr(arith) ::= Atom[left] ArithOp[op] Atom[right]
        CompExpr(comp) ::= Atom[left] CompOp[op] Atom[right]
        
        Expr ::= ArithExpr | CompExpr | Atom
        
        LetExpr(let_expr) ::= 'let' Variable[x] ':' Type[τ] '=' Expr[e] 'in' Expr[body]
        
        Type ::= 'Int' | 'Float' | 'Bool'
        
        Program ::= Expr | LetExpr
        start ::= Program
        
        -------------- (int_lit)
        'Int'
        
        -------------- (float_lit)
        'Float'
        
        x ∈ Γ
        ----------- (var)
        Γ(x)
        
        Γ ⊢ left : ?T, Γ ⊢ right : ?T
        ----------- (arith)
        ?T
        
        Γ ⊢ left : ?T, Γ ⊢ right : ?T
        ----------- (comp)
        'Bool'
        
        Γ ⊢ e : τ, Γ[x:τ] ⊢ body : ?R
        --------------------------- (let_expr)
        ?R
    """,
}


def list_grammars() -> List[str]:
    return list(GRAMMARS.keys())


def get_grammar(name: str) -> str:
    if name not in GRAMMARS:
        available = ", ".join(GRAMMARS.keys())
        raise ValueError(f"Unknown grammar '{name}'. Available: {available}")
    return GRAMMARS[name]
