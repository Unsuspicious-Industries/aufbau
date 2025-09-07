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
Number ::= /\d+/
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
ReturnStmt(return) ::= 'return' Expr?[ret_val] ';'
BlockStmt ::= '{' Stmt* '}'
ExprStmt ::= Expr

Stmt ::= VarDecl | Assignment | IfStmt | WhileStmt | ForStmt | ReturnStmt | BlockStmt | ExprStmt

// Function definitions (UNIMPLEMENTED - should fail complex cases)
FunctionDef(funcdef) ::= Type[ret_ty] Identifier[name] '(' ParamDeclList? ')' BlockStmt[body]
ParamDecl ::= Type Identifier
ParamDeclList ::= ParamDecl | ParamDeclList ',' ParamDecl

// Program
Program ::= FunctionDef | Stmt | Expr

// Basic typing rules (many advanced features not implemented)
x ∈ Γ
----------- (var)
Γ(x)

Γ ⊢ ret_val: ret_ty
----------------------- (funcdef)


Γ ⊢ left : float, Γ ⊢ right : float


// More complex rules would be needed for pointers, structs, etc.
"#;

#[test]
fn test_simple() {
    let expr = "int main() {}";
    // Enable debug output for this test
    set_debug_level(DebugLevel::Debug);
    set_debug_input(Some(expr.to_string()));

    let grammar = Grammar::load(C_LIKE_SPEC).expect("Failed to load C-like grammar");
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
        println!("return type: {}", ty);
    } else {
        println!("no return type");
    }


}