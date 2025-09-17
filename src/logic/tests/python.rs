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

// Simple statements kept minimal
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

#[test]
fn test_python_basic() {
    set_debug_level(DebugLevel::None);
    let grammar = Grammar::load(PYTHON_LIKE_SPEC).expect("Failed to load Python-like grammar");
    let mut parser = Parser::new(grammar.clone());

    let exprs = [
        "return 'r'",
        "x = 'r'",
        "'a' == 'b'",
    ];

    for expr in exprs {
        set_debug_input(Some(expr.to_string()));
        let mut tc = TypeChecker::new();
        let ast = parser.parse(expr).unwrap();
        let _ = tc.check(&ast);
        // touch parser & grammar to silence unused warnings
        debug_info!("test", "nt_count={} first_nt={:?}", grammar.productions.len(), grammar.production_order.first());
        println!("parsed: {}", expr);
    }
}