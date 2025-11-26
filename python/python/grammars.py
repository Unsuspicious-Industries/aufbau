"""
Built-in grammar specifications with typing rules.

These are context-dependent grammars - they include typing rules that
constrain generation beyond pure syntax. This is what distinguishes
Proposition 7 from CFG-only approaches like Outlines.

Usage:
    import p7_constrained as p7
    
    gen = p7.Generator("gpt2", grammar=p7.GRAMMARS["stlc"])
"""

from typing import Dict, List

# Only grammars with typing rules - context-dependent generation
GRAMMARS: Dict[str, str] = {
    
    # Simply Typed Lambda Calculus (ASCII-friendly for LLMs)
    "stlc": """
        // Grammar for expressions
        Identifier ::= /[a-z][a-zA-Z0-9]*/
        Variable(dec) ::= Identifier[x]
        Abstraction(abs) ::= 'fn' Identifier[x] ':' Type[τ] '=>' Expression[e]
        
        AtomicExpression ::= Variable | '(' Expression ')'
        Application(app) ::= AtomicExpression[e1] AtomicExpression[e2]
        
        BaseType ::= 'Int' | 'Bool'
        AtomicType ::= BaseType | '(' Type ')'
        FunctionType ::= AtomicType[τ1] '->' Type[τ2]
        Type ::= AtomicType | FunctionType
        
        Expression ::= AtomicExpression | Abstraction | Application
        start ::= Expression
        
        // Typing rules
        x ∈ Γ
        ----------- (dec)
        Γ(x)
        
        Γ[x:τ1] ⊢ e : τ2
        ----------------------- (abs)
        τ1 → τ2
        
        Γ ⊢ e1 : ?A → ?B, Γ ⊢ e2 : ?A
        -------------------------------- (app)
        ?B
    """,
    
    # Extended Typed Lambda Calculus (with let bindings, ASCII)
    "xtlc": """
        // Identifier
        Identifier ::= /[a-z][a-zA-Z0-9]*/
        
        // Variables
        Variable(var) ::= Identifier[x]
        
        // Types
        TypeName ::= 'Int' | 'Bool' | 'String'
        BaseType ::= TypeName | '(' Type ')'
        Type ::= BaseType[τ1] '->' Type[τ2] | BaseType[τ]
        
        // Lambda: fn x : T => body
        Lambda(lambda) ::= 'fn' Variable[x] ':' Type[τ] '=>' Term[e]
        
        // Let binding: let x : T
        Let(let) ::= 'let' Identifier[x] ':' Type[τ] ';'
        
        // Base terms
        BaseTerm ::= Variable | Lambda | '(' Term ')' 
        
        // Application
        Application(app) ::= BaseTerm[f] BaseTerm[e]
        
        Term ::= Application[e] | BaseTerm[e] 
        Expr ::= Term | Let
        start ::= Expr
        
        // Typing Rules
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
    
    # C-like language with type checking
    "clike": """
        // Identifiers and literals
        Identifier ::= /[a-zA-Z_][a-zA-Z0-9_]*/
        Number(int_lit) ::= /[0-9]+/
        String ::= '"' /[^"]*/ '"'
        
        // Variables
        Variable(var) ::= Identifier[x]
        
        // Primitive types
        PrimitiveType ::= 'int' | 'float' | 'char' | 'bool' | 'void'
        Type ::= PrimitiveType | '(' Type ')'
        
        // Expressions
        Literal ::= Number | String | 'true' | 'false'
        ArOp ::= '+' | '-' | '*' | '/'
        BoolOp ::= '==' | '!=' | '<' | '>'
        
        Primary ::= Literal | Variable | '(' Expr ')'
        ArOpExpr(ar_op) ::= Primary[left] ArOp[op] Expr[right]
        BoolOpExpr(bool_op) ::= Primary[left] BoolOp[op] Expr[right]
        Expr ::= ArOpExpr | BoolOpExpr | Primary
        
        // Statements
        VarDecl(vardecl) ::= Type[type] Variable[var] '=' Expr[init] ';'
        Assignment(assign) ::= Variable[target] '=' Expr[value] ';'
        IfStmt(if_stmt) ::= 'if' '(' Expr[cond] ')' '{' Stmt '}' 
        WhileStmt(while_stmt) ::= 'while' '(' Expr[cond] ')' '{' Stmt '}'
        ReturnStmt(return_stmt) ::= 'return' Expr[ret] ';'
        
        Stmt ::= VarDecl | Assignment | IfStmt | WhileStmt | ReturnStmt
        start ::= Stmt
        
        // Type rules - context dependent!
        
        // Int literals have type int
        -------------- (int_lit)
        'int'
        
        // Variable lookup
        x ∈ Γ
        ----------- (var)
        Γ(x)
        
        // Arithmetic ops preserve type
        Γ ⊢ left : ?T, Γ ⊢ right : ?T
        ----------- (ar_op)
        ?T
        
        // Comparisons return bool
        Γ ⊢ left : ?T, Γ ⊢ right : ?T
        ----------- (bool_op)
        'bool'
        
        // Var decl extends context
        Γ ⊢ init : type
        ------------------- (vardecl)
        Γ -> Γ[var:type] ⊢ 'void'
        
        // Assignment type checks
        Γ ⊢ target : ?T, Γ ⊢ value : ?T
        ---------------- (assign)
        'void'
        
        // Control flow
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
    
    # Typed arithmetic with explicit number types
    "typed_arithmetic": """
        // Numbers with types
        IntLit(int_lit) ::= /[0-9]+/
        FloatLit(float_lit) ::= /[0-9]+\\.[0-9]+/
        
        Variable(var) ::= /[a-z][a-z0-9]*/[x]
        
        // Operators
        ArithOp ::= '+' | '-' | '*' | '/'
        CompOp ::= '<' | '>' | '==' | '!='
        
        // Expressions
        Atom ::= IntLit | FloatLit | Variable | '(' Expr ')'
        ArithExpr(arith) ::= Atom[left] ArithOp[op] Atom[right]
        CompExpr(comp) ::= Atom[left] CompOp[op] Atom[right]
        
        Expr ::= ArithExpr | CompExpr | Atom
        
        // Let binding for context
        LetExpr(let_expr) ::= 'let' Variable[x] ':' Type[τ] '=' Expr[e] 'in' Expr[body]
        
        Type ::= 'Int' | 'Float' | 'Bool'
        
        Program ::= Expr | LetExpr
        start ::= Program
        
        // Typing rules
        -------------- (int_lit)
        'Int'
        
        -------------- (float_lit)
        'Float'
        
        x ∈ Γ
        ----------- (var)
        Γ(x)
        
        // Arithmetic preserves numeric type
        Γ ⊢ left : ?T, Γ ⊢ right : ?T
        ----------- (arith)
        ?T
        
        // Comparison returns Bool
        Γ ⊢ left : ?T, Γ ⊢ right : ?T
        ----------- (comp)
        'Bool'
        
        // Let extends context
        Γ ⊢ e : τ, Γ[x:τ] ⊢ body : ?R
        --------------------------- (let_expr)
        ?R
    """,
}


def list_grammars() -> List[str]:
    """List available built-in grammars (all with typing rules)."""
    return list(GRAMMARS.keys())


def get_grammar(name: str) -> str:
    """Get a built-in grammar by name."""
    if name not in GRAMMARS:
        available = ", ".join(GRAMMARS.keys())
        raise ValueError(f"Unknown grammar '{name}'. Available: {available}")
    return GRAMMARS[name]
