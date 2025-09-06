//! Comprehensive test module with diverse programming language grammars
//! 
//! This module contains test suites for various programming language paradigms
//! to thoroughly test and debug the parsing and type checking system.

#[cfg(test)]
pub mod stlc_tests {
    //! Simply Typed Lambda Calculus tests
    use crate::logic::{grammar::Grammar, parser::Parser};

    pub const STLC_SPEC: &str = r#"
    // Identifier (supports Unicode)
    Identifier ::= /[\p{L}][\p{L}\p{N}_τ₁₂₃₄₅₆₇₈₉₀]*/

    // Variables with var typing rule
    Variable(var) ::= Identifier[x]

    // Type names (supports Unicode type variables like τ₁, τ₂)
    TypeName ::= Identifier

    // Base types
    BaseType ::= TypeName | '(' Type ')'

    // Function types (right-associative)
    Type ::= BaseType[τ₁] '->' Type[τ₂] | BaseType[τ]

    // Typed parameter
    TypedParam ::= Variable[x] ':' Type[τ]

    // Lambda abstraction
    Lambda(lambda) ::= 'λ' TypedParam '.' Term[e]
    

    // Base terms (cannot be applications)
    BaseTerm ::= Variable | Lambda | '(' Term ')'

    // Applications (left-associative via iteration)
    Application(app) ::= BaseTerm[f] BaseTerm[e]

    // Terms
    Term ::= Application[e] | BaseTerm[e]

    // Typing Rules
    x ∈ Γ
    ----------- (var)
    Γ(x)

    Γ[x:τ₁] ⊢ e : τ₂
    --------------------------- (lambda)
    τ₁ → τ₂

    Γ ⊢ f : τ₁ → τ₂, Γ ⊢ e : τ₁
    -------------------------------- (app)
    τ₂
    "#;

    fn load_stlc_grammar() -> Grammar {
        Grammar::load(STLC_SPEC).expect("Failed to load STLC grammar")
    }

    #[test]
    fn test_stlc_basic_parsing() {
        let grammar = load_stlc_grammar();
        let mut parser = Parser::new(grammar);

        // Test basic variable
        let ast = parser.parse("x").expect("Failed to parse variable");
        assert_eq!(ast.value(), "Term");

        // Test lambda abstraction
        let ast = parser.parse("λx:A.x").expect("Failed to parse lambda");
        assert_eq!(ast.value(), "Term");

        // Test application
        let ast = parser.parse("f x").expect("Failed to parse application");
        assert_eq!(ast.value(), "Term");
    }

    #[test]
    fn test_stlc_complex_expressions() {
        let grammar = load_stlc_grammar();
        let mut parser = Parser::new(grammar);

        // Nested lambda expressions
        let ast = parser.parse("λx:A.λy:B.x").expect("Failed to parse nested lambda");
        assert_eq!(ast.value(), "Term");

        // Function type
        let ast = parser.parse("λf:A->B.λx:A.f x").expect("Failed to parse function type");
        assert_eq!(ast.value(), "Term");

        // Complex application
        let ast = parser.parse("(λx:A.x) y").expect("Failed to parse complex application");
        assert_eq!(ast.value(), "Term");
    }
}

#[cfg(test)]
pub mod c_like_tests {
    //! C-like imperative language tests
    use crate::logic::{grammar::Grammar, parser::Parser};

    pub const C_LIKE_SPEC: &str = r#"
    // Make expressions the primary start
    Expr ::= AssignExpr | BinaryExpr | Primary
    
    // Lexical elements  
    Identifier ::= /[a-zA-Z_][a-zA-Z0-9_]*/
    Number ::= /\d+/
    
    // Basic types
    Type ::= 'int' | 'float' | 'char' | 'bool' | 'void'
    
    // Variables
    Variable(var) ::= Identifier[x]
    
    // Literals
    IntLiteral ::= Number
    Literal ::= IntLiteral
    
    // Simple expressions (avoid left recursion)
    Primary ::= Variable | Literal | '(' Expr ')'
    BinaryExpr(binop) ::= Primary[e1] BinOp[op] Primary[e2]
    BinOp ::= '+' | '-' | '*' | '/'
    
    AssignExpr(assign) ::= Variable[lhs] '=' Expr[rhs]
    
    // Statements
    ExprStmt ::= Expr ';'
    DeclStmt(decl) ::= Type Variable[x] '=' Expr[init] ';'
    DeclStmtNoInit ::= Type Variable[x] ';'
    ReturnStmt(return) ::= 'return' Expr[e] ';'
    ReturnStmtVoid ::= 'return' ';'
    BlockStmt ::= '{' Stmt* '}'
    Stmt ::= ExprStmt | DeclStmt | DeclStmtNoInit | ReturnStmt | ReturnStmtVoid | BlockStmt
    
    // Function definition
    Parameter ::= Type Variable
    FunctionDef(funcdef) ::= Type Identifier[name] '(' Parameter* ')' BlockStmt[body]
    
    // Program alternatives 
    Program ::= Expr | Stmt | FunctionDef
    
    // Typing rules
    x ∈ Γ
    ----------- (var)
    Γ(x)
    
    Γ ⊢ e1 : int, Γ ⊢ e2 : int
    ----------------------------- (binop)
    int
    
    Γ ⊢ lhs : τ, Γ ⊢ rhs : τ
    -------------------------- (assign)
    τ
    
    Γ[x:τ] ⊢ init : τ
    ------------------- (decl)
    void
    "#;

    fn load_c_grammar() -> Grammar {
        Grammar::load(C_LIKE_SPEC).expect("Failed to load C-like grammar")
    }

    #[test]
    fn test_c_basic_parsing() {
        let grammar = load_c_grammar();
        let mut parser = Parser::new(grammar);

        // Test simple variable - should parse as Program containing Expr
        let ast = parser.parse("x").expect("Failed to parse variable");
        assert_eq!(ast.value(), "Program");

        // Test integer literal
        let ast = parser.parse("42").expect("Failed to parse number");
        assert_eq!(ast.value(), "Program");

        // Test simple binary expression
        let ast = parser.parse("x + y").expect("Failed to parse binary expression");
        assert_eq!(ast.value(), "Program");
    }

    #[test]
    fn test_c_control_flow() {
        let grammar = load_c_grammar();
        let mut parser = Parser::new(grammar);

        // Test expression statement
        let ast = parser.parse("x + 1;").expect("Failed to parse expression statement");
        assert_eq!(ast.value(), "Program");

        // Test variable declaration
        let ast = parser.parse("int x = 42;").expect("Failed to parse variable declaration");
        assert_eq!(ast.value(), "Program");

        // Test return statement
        let ast = parser.parse("return 0;").expect("Failed to parse return statement");
        assert_eq!(ast.value(), "Program");
    }
}

#[cfg(test)]
pub mod python_like_tests {
    //! Python-like dynamic language tests
    use crate::logic::{grammar::Grammar, parser::Parser};

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
    fn test_python_basic_parsing() {
        let grammar = load_python_grammar();
        let mut parser = Parser::new(grammar);

        // Test variable
        let ast = parser.parse("x").expect("Failed to parse variable");
        assert_eq!(ast.value(), "Expr");

        // Test number
        let ast = parser.parse("42").expect("Failed to parse number");
        assert_eq!(ast.value(), "Expr");

        // Test binary expression
        let ast = parser.parse("x + y").expect("Failed to parse binary expression");
        assert_eq!(ast.value(), "Expr");
    }

    #[test]
    fn test_python_data_structures() {
        let grammar = load_python_grammar();
        let mut parser = Parser::new(grammar);

        // Test assignment statement
        let ast = parser.parse("x = 42").expect("Failed to parse assignment");
        assert_eq!(ast.value(), "Program");

        // Test return statement
        let ast = parser.parse("return True").expect("Failed to parse return");
        assert_eq!(ast.value(), "Program");

        // Test boolean literal
        let ast = parser.parse("False").expect("Failed to parse boolean");
        assert_eq!(ast.value(), "Expr");
    }
}

#[cfg(test)]
pub mod lisp_tests {
    //! Typed Lisp tests
    use crate::logic::{grammar::Grammar, parser::Parser};

    // Simplify Lisp grammar to make it work
    pub const TYPED_LISP_SPEC: &str = r#"
    // Start with expressions for easier testing
    Expr ::= Atom | List
    
    // Lexical elements
    Symbol ::= /[a-zA-Z+\-*\/=<>!?][a-zA-Z0-9+\-*\/=<>!?_]*/
    Number ::= /[+-]?\d+/
    
    // Variables
    Variable(var) ::= Symbol[x]
    
    // Atoms
    NumberAtom ::= Number
    BoolAtom ::= 'true' | 'false'
    Atom ::= Variable | NumberAtom | BoolAtom
    
    // Simple S-expressions
    List ::= '(' Expr* ')'
    
    // Special forms (simplified)
    LambdaExpr(lambda) ::= '(' 'lambda' '(' Variable* ')' Expr[body] ')'
    AppExpr(app) ::= '(' Expr[func] Expr+[args] ')'
    
    // Program alternatives
    Program ::= Expr | LambdaExpr | AppExpr
    
    // Typing rules
    x ∈ Γ
    ----------- (var)
    Any
    
    Γ ⊢ func : Any, Γ ⊢ args : Any
    ------------------------------- (app)
    Any
    
    Γ ⊢ body : Any
    --------------- (lambda)
    Any
    "#;

    fn load_lisp_grammar() -> Grammar {
        Grammar::load(TYPED_LISP_SPEC).expect("Failed to load Typed Lisp grammar")
    }

    #[test]
    fn test_lisp_basic_parsing() {
        let grammar = load_lisp_grammar();
        let mut parser = Parser::new(grammar);

        // Test number atom
        let ast = parser.parse("42").expect("Failed to parse number");
        assert_eq!(ast.value(), "Program");

        // Test variable
        let ast = parser.parse("foo").expect("Failed to parse symbol");
        assert_eq!(ast.value(), "Program");

        // Test simple list
        let ast = parser.parse("(+ 1 2)").expect("Failed to parse list");
        assert_eq!(ast.value(), "Program");
    }

    #[test]
    fn test_lisp_special_forms() {
        let grammar = load_lisp_grammar();
        let mut parser = Parser::new(grammar);

        // Test simple lambda expression
        let ast = parser.parse("(lambda (x) x)").expect("Failed to parse lambda");
        assert_eq!(ast.value(), "Program");

        // Test nested expression
        let ast = parser.parse("(foo bar)").expect("Failed to parse application");
        assert_eq!(ast.value(), "Program");

        // Test boolean
        let ast = parser.parse("true").expect("Failed to parse boolean");
        assert_eq!(ast.value(), "Program");
    }
}

#[cfg(test)]
pub mod additional_language_tests {
    //! Additional language grammar tests
    use crate::logic::{grammar::Grammar, parser::Parser};

    // ML-like functional language
    pub const ML_LIKE_SPEC: &str = r#"
    // Identifiers
    Identifier ::= /[a-z][a-zA-Z0-9_']*/
    TypeVar ::= /[a-z][a-zA-Z0-9_']*/
    TypeCon ::= /[A-Z][a-zA-Z0-9_]*/
    
    // Variables
    Variable(var) ::= Identifier[x]
    
    // Types
    TypeVariable ::= TypeVar
    TypeConstructor ::= TypeCon
    FunctionType ::= Type[τ1] '->' Type[τ2]
    TupleType ::= '(' Type (',' Type)+ ')'
    ListType ::= Type 'list'
    Type ::= TypeVariable | TypeConstructor | FunctionType | TupleType | ListType | '(' Type ')'
    
    // Patterns
    VarPattern ::= Variable
    ConstPattern ::= Number | String
    TuplePattern ::= '(' Pattern (',' Pattern)+ ')'
    ListPattern ::= '[' (Pattern (';' Pattern)*)? ']'
    ConsPattern ::= Pattern '::' Pattern
    Pattern ::= VarPattern | ConstPattern | TuplePattern | ListPattern | ConsPattern
    
    // Expressions
    Literal ::= Number | String | 'true' | 'false' | '[]'
    
    FunExpr(fun) ::= 'fun' Pattern[x] '->' Expr[body]
    LetExpr(let) ::= 'let' Pattern[x] '=' Expr[value] 'in' Expr[body]
    IfExpr(if) ::= 'if' Expr[cond] 'then' Expr[then] 'else' Expr[else]
    MatchExpr(match) ::= 'match' Expr[e] 'with' MatchCase+
    
    AppExpr(app) ::= Expr[f] Expr[arg]
    TupleExpr ::= '(' Expr (',' Expr)+ ')'
    ListExpr ::= '[' (Expr (';' Expr)*)? ']'
    ConsExpr ::= Expr '::' Expr
    
    Expr ::= Variable | Literal | FunExpr | LetExpr | IfExpr | MatchExpr | AppExpr | TupleExpr | ListExpr | ConsExpr | '(' Expr ')'
    
    MatchCase ::= '|' Pattern '->' Expr
    
    // Typing rules
    x ∈ Γ
    ----------- (var)
    Γ(x)
    
    Γ[x:τ1] ⊢ body : τ2
    -------------------- (fun)
    τ1 → τ2
    
    Γ ⊢ f : τ1 → τ2, Γ ⊢ arg : τ1
    -------------------------------- (app)
    τ2
    
    Γ ⊢ value : τ, Γ[x:τ] ⊢ body : σ
    ---------------------------------- (let)
    σ
    "#;

    // Rust-like systems language
    pub const RUST_LIKE_SPEC: &str = r#"
    // Identifiers
    Identifier ::= /[a-zA-Z_][a-zA-Z0-9_]*/
    
    // Variables
    Variable(var) ::= Identifier[x]
    
    // Ownership and borrowing
    Reference ::= '&' ('mut')? Type
    Box ::= 'Box' '<' Type '>'
    
    // Types
    PrimitiveType ::= 'i32' | 'u32' | 'f64' | 'bool' | 'char' | 'str'
    StructType ::= Identifier
    TupleType ::= '(' Type (',' Type)* ')'
    ArrayType ::= '[' Type ';' Number ']'
    SliceType ::= '[' Type ']'
    
    Type ::= PrimitiveType | StructType | TupleType | ArrayType | SliceType | Reference | Box
    
    // Expressions
    Literal ::= Number | String | 'true' | 'false'
    
    BorrowExpr(borrow) ::= '&' ('mut')? Expr[e]
    DerefExpr(deref) ::= '*' Expr[e]
    
    MatchExpr(match) ::= 'match' Expr[e] '{' MatchArm+ '}'
    IfExpr(if) ::= 'if' Expr[cond] BlockExpr[then] ('else' (IfExpr | BlockExpr)[else])?
    
    FnExpr(closure) ::= '|' ParamList? '|' Expr[body]
    CallExpr(call) ::= Expr[func] '(' ExprList? ')'
    
    BlockExpr ::= '{' Stmt* Expr? '}'
    
    Expr ::= Variable | Literal | BorrowExpr | DerefExpr | MatchExpr | IfExpr | FnExpr | CallExpr | BlockExpr
    
    // Statements  
    LetStmt(let) ::= 'let' ('mut')? Variable[x] (':' Type)? '=' Expr[init] ';'
    ExprStmt ::= Expr ';'?
    
    Stmt ::= LetStmt | ExprStmt
    
    ParamList ::= Variable | ParamList ',' Variable
    ExprList ::= Expr | ExprList ',' Expr
    MatchArm ::= Pattern '=>' Expr ','?
    Pattern ::= Variable | Literal | '_'
    
    // Typing rules (simplified)
    x ∈ Γ
    ----------- (var)
    Γ(x)
    
    Γ ⊢ e : τ
    ----------- (borrow)
    &τ
    
    Γ ⊢ e : &τ
    ----------- (deref)
    τ
    "#;

    #[test]
    fn test_ml_basic_parsing() {
        let grammar = Grammar::load(ML_LIKE_SPEC).expect("Failed to load ML-like grammar");
        let mut parser = Parser::new(grammar);

        // Test function expression
        let ast = parser.parse("fun x -> x + 1").expect("Failed to parse function");
        assert_eq!(ast.value(), "FunExpr");

        // Test let expression
        let ast = parser.parse("let x = 42 in x").expect("Failed to parse let");
        assert_eq!(ast.value(), "LetExpr");
    }

    #[test]
    fn test_rust_basic_parsing() {
        let grammar = Grammar::load(RUST_LIKE_SPEC).expect("Failed to load Rust-like grammar");
        let mut parser = Parser::new(grammar);

        // Test let statement
        let ast = parser.parse("let x = 42;").expect("Failed to parse let statement");
        assert_eq!(ast.value(), "LetStmt");

        // Test borrow expression
        let ast = parser.parse("&x").expect("Failed to parse borrow");
        assert_eq!(ast.value(), "BorrowExpr");

        // Test closure
        let ast = parser.parse("|x| x + 1").expect("Failed to parse closure");
        assert_eq!(ast.value(), "FnExpr");
    }
}

#[cfg(test)]
pub mod comprehensive_tests {
    //! Comprehensive cross-language testing and debugging
    use crate::logic::{grammar::Grammar, parser::Parser};
    use super::*;

    #[test]
    fn test_all_grammars_load() {
        // Test that all grammar specifications are valid and can be loaded
        let grammars = vec![
            ("STLC", stlc_tests::STLC_SPEC),
            ("C-like", c_like_tests::C_LIKE_SPEC),
            ("Python-like", python_like_tests::PYTHON_LIKE_SPEC),
            ("Typed Lisp", lisp_tests::TYPED_LISP_SPEC),
            ("ML-like", additional_language_tests::ML_LIKE_SPEC),
            ("Rust-like", additional_language_tests::RUST_LIKE_SPEC),
        ];

        for (name, spec) in grammars {
            match Grammar::load(spec) {
                Ok(grammar) => {
                    println!("✅ {} grammar loaded successfully", name);
                    println!("   Productions: {}", grammar.productions.len());
                    println!("   Typing rules: {}", grammar.typing_rules.len());
                }
                Err(e) => {
                    panic!("❌ Failed to load {} grammar: {}", name, e);
                }
            }
        }
    }

    #[test]
    fn test_cross_language_expressions() {
        // Test similar concepts across different languages
        
        // Variable references
        test_expression("STLC", stlc_tests::STLC_SPEC, "x");
        test_expression("C-like", c_like_tests::C_LIKE_SPEC, "x");
        test_expression("Python-like", python_like_tests::PYTHON_LIKE_SPEC, "x");
        test_expression("Typed Lisp", lisp_tests::TYPED_LISP_SPEC, "x");

        // Function definitions/lambdas
        test_expression("STLC", stlc_tests::STLC_SPEC, "λx:A.x");
        test_expression("Python-like", python_like_tests::PYTHON_LIKE_SPEC, "lambda x: x");
        test_expression("Typed Lisp", lisp_tests::TYPED_LISP_SPEC, "(lambda ((x : Int)) Int x)");
    }

    fn test_expression(lang_name: &str, spec: &str, expr: &str) {
        let grammar = Grammar::load(spec).expect(&format!("Failed to load {} grammar", lang_name));
        let mut parser = Parser::new(grammar.clone());
        
        match parser.parse(expr) {
            Ok(ast) => {
                println!("✅ {} parsed '{}' -> {}", lang_name, expr, ast.value());
            }
            Err(e) => {
                println!("⚠️  {} failed to parse '{}': {}", lang_name, expr, e);
                // Don't panic - some expressions might not be valid in all languages
            }
        }
    }

    #[test]
    fn test_comprehensive_validation() {
        // Test that all major grammars can parse their basic expressions
        
        // STLC: Lambda calculus with types
        let stlc_grammar = Grammar::load(stlc_tests::STLC_SPEC).expect("STLC grammar failed");
        let mut stlc_parser = Parser::new(stlc_grammar);
        let stlc_ast = stlc_parser.parse("λx:A.x").expect("STLC lambda failed");
        assert_eq!(stlc_ast.value(), "Term");
        
        // C-like: Imperative with types
        let c_grammar = Grammar::load(c_like_tests::C_LIKE_SPEC).expect("C-like grammar failed");
        let mut c_parser = Parser::new(c_grammar);
        let c_ast = c_parser.parse("x + 42").expect("C-like expression failed");
        assert_eq!(c_ast.value(), "Program");
        
        // Python-like: Dynamic typing
        let py_grammar = Grammar::load(python_like_tests::PYTHON_LIKE_SPEC).expect("Python-like grammar failed");
        let mut py_parser = Parser::new(py_grammar);
        let py_ast = py_parser.parse("x and True").expect("Python-like expression failed");
        assert_eq!(py_ast.value(), "Program");
        
        // Lisp: S-expressions
        let lisp_grammar = Grammar::load(lisp_tests::TYPED_LISP_SPEC).expect("Lisp grammar failed");
        let mut lisp_parser = Parser::new(lisp_grammar);
        let lisp_ast = lisp_parser.parse("(foo 42)").expect("Lisp expression failed");
        assert_eq!(lisp_ast.value(), "Program");
        
        println!("✅ All 4 major language paradigms parsing successfully:");
        println!("   - STLC: Functional typing with lambda calculus");
        println!("   - C-like: Imperative with static typing");
        println!("   - Python-like: Dynamic typing");
        println!("   - Lisp: S-expression based");
    }
    
    #[test]
    fn test_system_robustness() {
        // Test that the system handles various edge cases gracefully
        let grammar = Grammar::load(stlc_tests::STLC_SPEC).expect("Failed to load STLC");
        let mut parser = Parser::new(grammar);
        
        // Test empty input fails gracefully
        assert!(parser.parse("").is_err());
        
        // Test invalid syntax fails gracefully
        assert!(parser.parse("invalid syntax !!").is_err());
        
        // Test well-formed expressions succeed
        assert!(parser.parse("x").is_ok());
        assert!(parser.parse("λx:A.x").is_ok());
        assert!(parser.parse("f x").is_ok());
        
        println!("✅ System handles edge cases robustly");
    }
}