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

// Pointer types (UNIMPLEMENTED - should fail type checking)
PointerType ::= Type[base] '*'
ArrayType ::= Type[base] '[' Number? ']'

// Struct types (UNIMPLEMENTED - should fail)
StructType ::= 'struct' Identifier
FieldDecl ::= Type Identifier
StructDef ::= 'struct' Identifier '{' FieldDecl* '}'

// Function types
FunctionType ::= Type[ret] '(' ParamList? ')'
ParamList ::= Type (',' Type)*

// All types
Type ::= PrimitiveType | PointerType | ArrayType | StructType | FunctionType | '(' Type ')'

// Expressions
Literal ::= Number | String | 'true' | 'false' | 'NULL'

// Pointer operations (UNIMPLEMENTED)
AddressOf(addressof) ::= '&' Expr[e]
Dereference(deref) ::= '*' Expr[e]
ArrayAccess(arrayaccess) ::= Expr[arr] '[' Expr[idx] ']'
FieldAccess(fieldaccess) ::= Expr[obj] '.' Identifier[field]

// Function calls
FunctionCall(call) ::= Expr[func] '(' ArgList? ')'
ArgList ::= Expr | ArgList ',' Expr

// Binary operations
BinaryOp(binop) ::= Expr[left] Op[op] Expr[right]
Op ::= '+' | '-' | '*' | '/' | '==' | '!=' | '<' | '>' | '&&' | '||'

// All expressions
Expr ::= Variable | Literal | AddressOf | Dereference | ArrayAccess | FieldAccess | FunctionCall | BinaryOp | '(' Expr ')'

// Statements
VarDecl(vardecl) ::= Type[type] Variable[var] ('=' Expr[init])?
Assignment(assign) ::= Expr[target] '=' Expr[value]
IfStmt(if) ::= 'if' '(' Expr[cond] ')' Stmt[then] ('else' Stmt[else])?
WhileStmt(while) ::= 'while' '(' Expr[cond] ')' Stmt[body]
ForStmt(for) ::= 'for' '(' Stmt[init] Expr[cond] ';' Stmt[update] ')' Stmt[body]
BlockStmt ::= '{' Stmt* '}'
ExprStmt ::= Expr

Stmt ::= VarDecl | Assignment | IfStmt | WhileStmt | ForStmt | BlockStmt | ExprStmt

// Function definitions (UNIMPLEMENTED - should fail complex cases)
ReturnStmt(return) ::= 'return' Expr[ret_val] ';'
FunctionDef(funcdef) ::= Type[ret_ty] Identifier[name] '(' ParamDecl* ')' '{' Stmt* ReturnStmt '}'
ParamDecl ::= Type[in_tys] Identifier ','?

// Program
Program ::= FunctionDef | Stmt | Expr


// Type rule for Int literals - concrete int type
-------------- (int-lit)
'int'

// Basic typing rules (many advanced features not implemented)
x ∈ Γ
----------- (var)
Γ(x)

Γ ⊢ ret_val: ret_ty
----------------------- (funcdef)
(in_tys...) -> ret_ty


// More complex rules would be needed for pointers, structs, etc.
"#;

#[test]
fn test_simple() {
    let expr = "int main() {return 't';}";
    // Enable debug output for this test
    set_debug_level(DebugLevel::Debug);
    set_debug_input(Some(expr.to_string()));

    let grammar = Grammar::load(C_LIKE_SPEC).expect("Failed to load C-like grammar");
    
    // Debug: Print all loaded productions
    println!("=== LOADED PRODUCTIONS ===");
    for (nt, prods) in &grammar.productions {
        println!("Nonterminal: {}", nt);
        for (i, prod) in prods.iter().enumerate() {
            println!("  Production {}: {:?}", i, prod);
        }
    }
    println!("=== END PRODUCTIONS ===");
    
    debug_info!("test", "Loaded grammar with {} rules", grammar.typing_rules.len());
    let mut parser = Parser::new(grammar);
    debug_info!("test", "Initialized parser");
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


}