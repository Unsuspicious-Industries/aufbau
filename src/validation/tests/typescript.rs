//! TypeScript-like Language Tests
//!
//! Tests typed completion for a TypeScript-inspired language with:
//! - Type annotations on variables and functions
//! - Arrow functions with type inference
//! - Generic types
//! - Union types
//! - Object types with properties
//! - Interface declarations

use super::*;

/// TypeScript-like grammar with rich type system
const TYPESCRIPT_GRAMMAR: &str = r#"
// Identifiers and literals
Identifier ::= /[a-zA-Z_][a-zA-Z0-9_]*/
Number(num-lit) ::= /\d+/
String ::= /"[^"]*"/ | /'[^']*'/
Boolean ::= 'true' | 'false'

// Variables with var typing rule
Variable(var) ::= Identifier[x]

// Primitive types
PrimitiveType ::= 'number' | 'string' | 'boolean' | 'void' | 'null' | 'undefined' | 'any'

// Type expressions
TypeName ::= Identifier
GenericArgs ::= '<' Type '>' | '<' Type ',' TypeList '>'
TypeList ::= Type | Type ',' TypeList
GenericType ::= TypeName GenericArgs
ArrayType ::= AtomicType '[' ']'
ParenType ::= '(' Type ')'
AtomicType ::= PrimitiveType | TypeName | GenericType | ArrayType | ParenType

// Function type: (a: T, b: U) => R
ParamType ::= Identifier ':' Type
ParamTypeList ::= ParamType | ParamType ',' ParamTypeList
ParamTypeListOpt ::= ε | ParamTypeList
FunctionType ::= '(' ParamTypeListOpt ')' '=>' Type

// Union and intersection types
UnionType ::= AtomicType '|' Type
IntersectionType ::= AtomicType '&' Type

// Object type: { name: string, age: number }
PropDef ::= Identifier ':' Type
PropDefList ::= PropDef | PropDef ',' PropDefList
PropDefListOpt ::= ε | PropDefList
ObjectType ::= '{' PropDefListOpt '}'

// All types
Type ::= FunctionType | UnionType | IntersectionType | ObjectType | AtomicType

// Expressions
Literal ::= Number | String | Boolean | 'null' | 'undefined'
Primary ::= Literal | Variable | '(' Expr ')'

// Arrow function: (x: T) => expr  or  x => expr
ArrowParam ::= Identifier ':' Type | Identifier
ArrowParamList ::= ArrowParam | ArrowParam ',' ArrowParamList
ArrowParamListOpt ::= ε | ArrowParamList
ArrowFunction(arrow) ::= '(' ArrowParamListOpt ')' '=>' Expr
SimpleArrow(simple-arrow) ::= Identifier[x] '=>' Expr[body]

// Function call
ArgList ::= Expr | Expr ',' ArgList
ArgListOpt ::= ε | ArgList
Call(call) ::= Primary[func] '(' ArgListOpt ')'

// Binary operations
BinOp ::= '+' | '-' | '*' | '/' | '===' | '!==' | '<' | '>' | '&&' | '||'
BinaryExpr(binop) ::= Primary[left] BinOp[op] Expr[right]

// Property access
PropAccess(prop) ::= Primary[obj] '.' Identifier[prop]

// All expressions
Expr ::= ArrowFunction | SimpleArrow | BinaryExpr | Call | PropAccess | Primary

// Statements
VarDecl(vardecl) ::= 'let' Identifier[x] ':' Type[τ] '=' Expr[init] ';'
VarDeclInfer(vardecl-infer) ::= 'let' Identifier[x] '=' Expr[init] ';'
ConstDecl(constdecl) ::= 'const' Identifier[x] ':' Type[τ] '=' Expr[init] ';'
ConstDeclInfer(constdecl-infer) ::= 'const' Identifier[x] '=' Expr[init] ';'

ReturnStmt(return) ::= 'return' Expr[ret] ';'
ExprStmt(exprstmt) ::= Expr[e] ';'

StmtList ::= Stmt | Stmt StmtList
StmtListOpt ::= ε | StmtList
Block(block) ::= '{' StmtListOpt '}'

IfStmt(if) ::= 'if' '(' Expr[cond] ')' Block[then] ElseOpt
ElseOpt ::= 'else' Block[else] | ε

Stmt ::= VarDecl | VarDeclInfer | ConstDecl | ConstDeclInfer | ReturnStmt | IfStmt | Block | ExprStmt

// Function declaration
FuncParam ::= Identifier ':' Type
FuncParamList ::= FuncParam | FuncParam ',' FuncParamList
FuncParamListOpt ::= ε | FuncParamList
FunctionDecl(funcdef) ::= 'function' Identifier[name] '(' FuncParamListOpt ')' ':' Type[ret] Block[body]

// Interface declaration
InterfaceMember ::= Identifier ':' Type ';'
InterfaceMemberList ::= InterfaceMember | InterfaceMember InterfaceMemberList
InterfaceMemberListOpt ::= ε | InterfaceMemberList
InterfaceDecl(interface) ::= 'interface' Identifier[name] '{' InterfaceMemberListOpt '}'

// Type alias
TypeAlias(typealias) ::= 'type' Identifier[name] '=' Type ';'

// Top-level declarations
Decl ::= FunctionDecl | InterfaceDecl | TypeAlias | VarDecl | VarDeclInfer | ConstDecl | ConstDeclInfer
Program ::= Decl | Decl Program

// =====================
// TYPING RULES
// =====================

// Number literals have type 'number'
-------------- (num-lit)
'number'

// Variable lookup
x ∈ Γ
----------- (var)
Γ(x)

// Binary operations
Γ ⊢ left : τ, Γ ⊢ right : τ
----------- (binop)
τ

// Simple arrow function: x => body introduces x with Universe type
Γ[x:'any'] ⊢ body : ?R
------------------------- (simple-arrow)
'any' → ?R

// Variable declaration with type annotation
Γ ⊢ init : τ
------------------- (vardecl)
Γ -> Γ[x:τ] ⊢ 'void'

// Variable declaration with inference
Γ ⊢ init : ?T
------------------- (vardecl-infer)
Γ -> Γ[x:?T] ⊢ 'void'

// Const declaration with type annotation
Γ ⊢ init : τ
------------------- (constdecl)
Γ -> Γ[x:τ] ⊢ 'void'

// Const declaration with inference
Γ ⊢ init : ?T
------------------- (constdecl-infer)
Γ -> Γ[x:?T] ⊢ 'void'

// Block: sequence of statements
------------------- (block)
'void'

// Return statement
Γ ⊢ ret : τ
------------------- (return)
τ

// Expression statement
Γ ⊢ e : τ
------------------- (exprstmt)
'void'

// If statement
Γ ⊢ cond : 'boolean', Γ ⊢ then : 'void'
------------------- (if)
'void'

// Function declaration
------------------- (funcdef)
(τ...) -> ret

// Interface declaration (introduces type name)
------------------- (interface)
'void'

// Type alias
------------------- (typealias)
'void'

// Property access
Γ ⊢ obj : ?O
------------------- (prop)
'any'

// Function call
Γ ⊢ func : ?F
------------------- (call)
'any'
"#;

fn typescript_grammar() -> Grammar {
    load_inline_grammar(TYPESCRIPT_GRAMMAR)
}

// ============================================================================
// Batch Test Cases
// ============================================================================

#[test]
fn check_completable() {
    let cases = vec![
        // Empty/start
        TypedCompletionTestCase::new("empty", "", false).with_depth(5),
        
        // Let declarations
        TypedCompletionTestCase::new("let start", "let", false).with_depth(8),
        TypedCompletionTestCase::new("let name", "let x", false).with_depth(8),
        TypedCompletionTestCase::new("let colon", "let x :", false).with_depth(8),
        TypedCompletionTestCase::new("let type", "let x : number", false).with_depth(8),
        TypedCompletionTestCase::new("let equals", "let x : number =", false).with_depth(8),
        TypedCompletionTestCase::new("let value", "let x : number = 5", false).with_depth(3),
        TypedCompletionTestCase::new("let complete", "let x : number = 5;", false).with_depth(1),
        TypedCompletionTestCase::new("const", "const y : string = \"hi\";", false).with_depth(1),
        
        // Functions
        TypedCompletionTestCase::new("function start", "function", false).with_depth(10),
        TypedCompletionTestCase::new("function name", "function foo", false).with_depth(10),
        TypedCompletionTestCase::new("function parens", "function foo()", false).with_depth(10),
        TypedCompletionTestCase::new("function return type", "function foo() : void", false).with_depth(10),
        TypedCompletionTestCase::new("function complete", "function foo() : void {}", false).with_depth(1),
        
        // Arrow functions
        TypedCompletionTestCase::new("simple arrow", "let f = x => x;", false).with_depth(3),
        TypedCompletionTestCase::new("typed arrow", "let f = (x: number) => x;", false).with_depth(3),
        
        // Types
        TypedCompletionTestCase::new("union type", "let x : number | string = 5;", false).with_depth(3),
        TypedCompletionTestCase::new("array type", "let arr : number[] = null;", false).with_depth(3),
        TypedCompletionTestCase::new("generic type", "let arr : Array<string> = null;", false).with_depth(3),
        
        // Interface
        TypedCompletionTestCase::new("interface start", "interface", false).with_depth(10),
        TypedCompletionTestCase::new("interface name", "interface Foo", false).with_depth(10),
        TypedCompletionTestCase::new("interface empty", "interface Foo {}", false).with_depth(1),
        
        // Type alias
        TypedCompletionTestCase::new("type alias", "type Num = number;", false).with_depth(1),
    ];

    let grammar = typescript_grammar();
    let res = run_test_batch(&grammar, &cases);
    assert!(res.passed == cases.len(), "{} out of {} tests passed", res.passed, cases.len());
    println!("Average duration: {:?}", res.avg_duration);
}

#[test]
fn check_fail() {
    let cases = vec![
        // Syntax errors
        TypedCompletionTestCase::new("equals first", "= 5", true),
        TypedCompletionTestCase::new("close brace first", "}", true),
        TypedCompletionTestCase::new("semicolon first", ";", true),
        TypedCompletionTestCase::new("arrow first", "=>", true),
        TypedCompletionTestCase::new("double colon", "let x :: number = 5;", true),
    ];

    let grammar = typescript_grammar();
    let res = run_test_batch(&grammar, &cases);
    assert!(res.passed == cases.len(), "{} out of {} tests passed", res.passed, cases.len());
    println!("Average duration: {:?}", res.avg_duration);
}
