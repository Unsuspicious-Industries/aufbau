#[cfg(test)]

use crate::logic::{
    grammar::Grammar, 
    parser::Parser, 
    check::TypeChecker, 
};
use crate::{debug_info, set_debug_level, set_debug_input, DebugLevel};

pub const C_LIKE_SPEC: &str = r#"
// Identifiers and literals
Identifier ::= /[a-zA-Z_][a-zA-Z0-9_]*/
Number(int-lit) ::= /\d+/
String ::= /"[^"]*"/

// Variables
Variable(var) ::= Identifier[x]

// Primitive types
PrimitiveType ::= 'int' | 'float' | 'char' | 'bool' | 'void'

// All types
Type ::= PrimitiveType | PointerType | ArrayType | StructType | FunctionType | '(' Type ')'

// Expressions
Literal ::= Number | String | 'true' | 'false' | 'NULL'

// Binary operations (restructured to avoid left recursion)
ArOp ::= '+' | '-' | '*' | '/' | '%'
BoolOp ::= '==' | '!=' | '<' | '>' | '<=' | '>='

// Primary expressions (atomic expressions)
Primary ::= Literal | Variable | '(' Expr ')'

// Right-recursive expressions to avoid left recursion
ArOpExpr(ar-op-expr) ::= Primary[left] ArOp[op] Expr[right]
BoolOpExpr(bool-op-expr) ::= Primary[left] BoolOp[op] Expr[right]

// All expressions (no left recursion)
Expr ::= ArOpExpr 
    | BoolOpExpr 
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
// Introduce Else nonterminal to avoid grouped optional
Else ::= 'else' Stmt[else]
IfStmt(if) ::= 'if' '(' Expr[cond] ')' Stmt[then] Else?
WhileStmt(while) ::= 'while' '(' Expr[cond] ')' Stmt[body]
// For-loop header with specific init/update forms and separators
ForInit ::= VarInitFor | Assignment
ForUpdate ::= Assignment
ForStmt(for) ::= 'for' '(' ForInit[init] ';' Expr[cond] ';' ForUpdate[update] ')' Stmt[body]

ReturnStmt(return) ::= 'return' Expr[ret_val] ';'

// Blocks allow both statements and return statements
BlockItem ::= Stmt | ReturnStmt
BlockStmt(block) ::= '{' BlockItem[s]* '}'

ExprStmt(exprstmt) ::= Expr[e] ';'

Stmt ::= VarDecl | AssignmentStmt | IfStmt | WhileStmt | ForStmt | BlockStmt | ExprStmt


FunctionDef(funcdef) ::= Type[ret_ty] Identifier[name] '(' ParamDecl* ')' '{' Stmt[s]* ReturnStmt '}'
ParamDecl ::= Type[in_tys] Identifier ','?

// Program (sequence of items)
Item ::= FunctionDef | Stmt | Expr
Program ::= Item+


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

// function: type all statements (sequenced) then the return expression
Γ ⊢ s : 'void', Γ ⊢ ret_val: ret_ty
----------------------- (funcdef)
(in_tys...) -> ret_ty




// More complex rules would be needed for pointers, structs, etc.
"#;


#[test]
fn test_pass() {

    // Enable debug output for this test
    set_debug_level(DebugLevel::Debug);

    let grammar = Grammar::load(C_LIKE_SPEC).expect("Failed to load C-like grammar");
    debug_info!("test", "Loaded grammar with {} rules", grammar.typing_rules.len());
    let mut parser = Parser::new(grammar.clone());
    debug_info!("test", "Initialized parser");
    // Debug: Print all loaded productions
    println!("=== LOADED PRODUCTIONS ===");
    for (nt, prods) in &grammar.productions {
        println!("Nonterminal: {}", nt);
        for (i, prod) in prods.iter().enumerate() {
            println!("  Production {}: {:?}", i, prod);
        }
    }
    println!("=== END PRODUCTIONS ===");
    

    let exprs = [
        "int main() {return 10;}",
        "int main() {int x = 5; return x;}",
        "int main() {int x = 5; int y = x + 2; return y;}",
        // Ensure function body ends with a ReturnStmt to satisfy the grammar
        "int main() {if (1) {return 10;} else {return 20;} return 0;}",
        "int main() {int x = 0; while (x < 10) {x = x + 1;} return x;}",
        "int main() {for (int i = 0; i < 10; i = i + 1) {} return 0;}",
        "int add(int a, int b) {return a + b;} int main() {return add(3, 4);}",
        // Test variable declarations
        "int x = 5;",
        "float y;",
        "int main() {int a; int b = 10; return b;}",
    ];

    for expr in exprs {
        set_debug_input(Some(expr.to_string()));

        let mut tc = TypeChecker::new();
        debug_info!("test", "Initialized type checker");

        let ast = parser.parse(expr).unwrap();
        debug_info!("test", "AST: {}", ast.pretty());
        let rt = tc.check(&ast).unwrap();

        // if type is some print, else not
        if let Some(ty) = rt {
            println!("return type: {:?}", ty);
        } else {
            println!("no return type");
        }
        println!("---");
    }
}

#[test]
fn test_fail() {
    // Enable debug output for this test
    set_debug_level(DebugLevel::Debug);

    let grammar = Grammar::load(C_LIKE_SPEC).expect("Failed to load C-like grammar");
    debug_info!("test", "Loaded grammar with {} rules", grammar.typing_rules.len());
    let mut parser = Parser::new(grammar.clone());
    debug_info!("test", "Initialized parser");

    let exprs = [
        "int main() {return \"r\";}",
        "int main() {int x = 5; return y;}",
        // Test variable declaration type errors
        "int x = \"string\";",  // type mismatch
        "float z = true;",     // type mismatch
    ];

    for expr in exprs {
        set_debug_input(Some(expr.to_string()));

        let mut tc = TypeChecker::new();
        debug_info!("test", "Initialized type checker");

        let ast = parser.parse(expr).unwrap();
        debug_info!("test", "AST: {}", ast.pretty());
        let err = tc.check(&ast).unwrap_err();

        println!("Type error: {}", err);
        println!("---");
    }
}