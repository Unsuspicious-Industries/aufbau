use crate::logic::grammar::Grammar;
use crate::logic::parser::Parser;
use crate::logic::check::TypeChecker;

#[test]
fn test_lib() {
    use crate::logic::parser;


    let c_grammar = r#"
// Step-by-step C grammar expansion

// Lexical terminals
Identifier ::= /[\p{L}_][\p{L}\p{N}_]*/
Number ::= /\d+/

// Types
Type ::= 'int' | 'void'

// Variables  
Variable(var) ::= Identifier[x]

// Literals (no typing rule for now to avoid empty premise issues)
Number ::= /\d+/

// Basic expressions
Primary ::= Variable | Number
ArithOp(arith) ::= Primary[e1] /[+\-]/ Primary[e2]
Assignment(assign) ::= Variable[v] '=' Primary[e]
Expr ::= Assignment | ArithOp | Primary

// Statements
VarDecl(vardecl) ::= Type Variable[x] '=' Expr[e] ';'
VarDeclNoInit ::= Type Variable[x] ';'
ExprStmt ::= Expr ';'
ReturnStmt(returnstmt) ::= 'return' Expr[e] ';'
Block ::= '{' Stmt* '}'
Stmt ::= VarDecl | VarDeclNoInit | ExprStmt | ReturnStmt | Block

// Function definition
FunctionDef(funcdef) ::= Type Identifier[name] '(' ')' Block[body]

// Program
Program ::= FunctionDef

// Typing rules

// Variable lookup
x ∈ Γ
----------- (var)
Γ(x)

// Arithmetic operations
Γ ⊢ e1 : int, Γ ⊢ e2 : int
--------------------------------- (arith)
int

// Assignment
Γ ⊢ v : int, Γ ⊢ e : int
------------------------------- (assign)
int
    "#;

    let grammar = Grammar::load(c_grammar).expect("Failed to load C-like grammar");
    println!("Start nonterminal: {:?}", grammar.start_nonterminal());

    println!("✅ C-like grammar loaded successfully!");
    println!("📋 Productions: {:?}", grammar.productions.keys().collect::<Vec<_>>());
    println!("🔧 Typing rules: {:?}", grammar.typing_rules.keys().collect::<Vec<_>>());


    // Simpler test program that matches our grammar
    let program = r#"int main() { int x = 10; }"#;

    let mut parser = Parser::new(grammar.clone());
    
    let mut tc = TypeChecker::with_input(Some(program.to_string()));

    println!("\n🔄 Attempting to parse program...");

    let ast = parser.parse(program);

    match ast {
        Ok(parsed_ast) => {
            println!("✅ Parsing successful!");
            println!("AST: {}", parsed_ast.pretty());
            
            println!("\n🔧 Type checking attempted anyway...");
            let ty = tc.check(&parsed_ast);
            match ty {
                Ok(result_type) => println!("🎯 Type checking succeeded: {:?}", result_type),
                Err(e) => println!("🎯 Type checking failed (expected): {}", e),
            }
        }
        Err(e) => {
            println!("❌ Parsing failed: {:?}", e);
        }
    }


}