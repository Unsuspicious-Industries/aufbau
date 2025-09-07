use crate::logic::check::TypeChecker;
use crate::logic::{grammar::Grammar, parser::Parser};
use crate::{debug_info, set_debug_level, set_debug_input, DebugLevel};

pub const PYTHON_LIKE_SPEC: &str = r#"
// Start with expressions
Expr ::= BinaryExpr | UnaryExpr | Primary

// Lexical elements
Identifier ::= /[a-zA-Z_][a-zA-Z0-9_]*/
Number ::= /\d+(\.\d+)?/
String ::= /'[^']*'/ | /"[^"]*"/

// Variables
Variable(var) ::= Identifier[x]

// Literals
NumberLit ::= Number
StringLit ::= String
BoolLit ::= 'True' | 'False' | 'None'
Literal ::= NumberLit | StringLit | BoolLit

// Simple expressions
Primary ::= Variable | Literal | '(' Expr ')'
UnaryExpr ::= Primary | 'not' UnaryExpr | '-' UnaryExpr | '+' UnaryExpr
BinaryExpr(binop) ::= Primary[left] BinOp[op] Primary[right]
BinOp ::= '+' | '-' | '*' | '/' | 'and' | 'or' | '==' | '!=' | '<' | '>'

// Simple statements
ExprStmt ::= Expr
AssignStmt(assign) ::= Variable[target] '=' Expr[value]
ReturnStmt(return) ::= 'return' Expr[value]

Stmt ::= ExprStmt | AssignStmt | ReturnStmt

// Program
Program ::= Expr | Stmt

// Typing rules (simplified dynamic typing)
x ∈ Γ
----------- (var)
Any

Γ ⊢ left : Any, Γ ⊢ right : Any
-------------------------------- (binop)
Any

Γ ⊢ target : Any, Γ ⊢ value : Any
---------------------------------- (assign)
Any
"#;

fn load_python_grammar() -> Grammar {
    Grammar::load(PYTHON_LIKE_SPEC).expect("Failed to load Python-like grammar")
}

#[test]
fn test_python_basic() {
    // Enable debug output for this test
    set_debug_level(DebugLevel::Debug);

    let grammar = Grammar::load(PYTHON_LIKE_SPEC).expect("Failed to load C-like grammar");
    debug_info!("test", "Loaded grammar with {} rules", grammar.typing_rules.len());
    let mut parser = Parser::new(grammar.clone());
    debug_info!("test", "Initialized parser");

    let exprs = [
        "def main(): return 'r'",
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