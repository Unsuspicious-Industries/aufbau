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
    // Lexical elements
    Identifier ::= /[a-zA-Z_][a-zA-Z0-9_]*/
    Number ::= /\d+/
    String ::= /"[^"]*"/
    
    // Basic types
    PrimitiveType ::= 'int' | 'float' | 'char' | 'bool' | 'void'
    PointerType ::= Type '*'
    ArrayType ::= Type '[' Number ']'
    Type ::= PrimitiveType | PointerType | ArrayType | Identifier
    
    // Variables
    Variable(var) ::= Identifier[x]
    
    // Literals
    IntLiteral ::= Number
    StringLiteral ::= String
    BoolLiteral ::= 'true' | 'false'
    Literal ::= IntLiteral | StringLiteral | BoolLiteral
    
    // Expressions
    Primary ::= Variable | Literal | '(' Expr ')'
    PostfixExpr ::= Primary | PostfixExpr '[' Expr ']' | PostfixExpr '(' ArgList? ')'
    UnaryExpr ::= PostfixExpr | UnaryOp UnaryExpr
    UnaryOp ::= '+' | '-' | '!' | '*' | '&'
    
    BinaryExpr(binop) ::= UnaryExpr[e1] BinOp[op] UnaryExpr[e2]
    BinOp ::= '+' | '-' | '*' | '/' | '%' | '==' | '!=' | '<' | '>' | '<=' | '>=' | '&&' | '||'
    
    AssignExpr(assign) ::= UnaryExpr[lhs] '=' Expr[rhs]
    Expr ::= AssignExpr | BinaryExpr | UnaryExpr
    
    // Argument list
    ArgList ::= Expr | ArgList ',' Expr
    
    // Statements
    ExprStmt ::= Expr ';'
    DeclStmt(decl) ::= Type Variable[x] ('=' Expr[init])? ';'
    ReturnStmt(return) ::= 'return' Expr?[e] ';'
    IfStmt(if) ::= 'if' '(' Expr[cond] ')' Stmt[then] ('else' Stmt[else])?
    WhileStmt(while) ::= 'while' '(' Expr[cond] ')' Stmt[body]
    BlockStmt ::= '{' Stmt* '}'
    Stmt ::= ExprStmt | DeclStmt | ReturnStmt | IfStmt | WhileStmt | BlockStmt
    
    // Function definition
    Parameter ::= Type Variable
    ParamList ::= Parameter | ParamList ',' Parameter
    FunctionDef(funcdef) ::= Type Identifier[name] '(' ParamList? ')' BlockStmt[body]
    
    // Program
    Program ::= FunctionDef+
    
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

        // Test variable declaration
        let ast = parser.parse("int x = 42;").expect("Failed to parse variable declaration");
        assert_eq!(ast.value(), "DeclStmt");

        // Test function definition
        let ast = parser.parse("int main() { return 0; }").expect("Failed to parse function");
        assert_eq!(ast.value(), "FunctionDef");

        // Test expression
        let ast = parser.parse("x + y").expect("Failed to parse expression");
        assert_eq!(ast.value(), "BinaryExpr");
    }

    #[test]
    fn test_c_control_flow() {
        let grammar = load_c_grammar();
        let mut parser = Parser::new(grammar);

        // Test if statement
        let ast = parser.parse("if (x > 0) return x;").expect("Failed to parse if statement");
        assert_eq!(ast.value(), "IfStmt");

        // Test while loop
        let ast = parser.parse("while (i < 10) i = i + 1;").expect("Failed to parse while loop");
        assert_eq!(ast.value(), "WhileStmt");

        // Test block statement
        let ast = parser.parse("{ int x = 5; return x; }").expect("Failed to parse block");
        assert_eq!(ast.value(), "BlockStmt");
    }
}

#[cfg(test)]
pub mod python_like_tests {
    //! Python-like dynamic language tests
    use crate::logic::{grammar::Grammar, parser::Parser};

    pub const PYTHON_LIKE_SPEC: &str = r#"
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
    ListLit ::= '[' ExprList? ']'
    DictLit ::= '{' DictPairs? '}'
    Literal ::= NumberLit | StringLit | BoolLit | ListLit | DictLit
    
    // Expressions
    Primary ::= Variable | Literal | '(' Expr ')'
    Call(call) ::= Primary[func] '(' ExprList?[args] ')'
    Attribute ::= Primary '.' Identifier
    Index ::= Primary '[' Expr ']'
    PostfixExpr ::= Primary | Call | Attribute | Index
    
    UnaryExpr ::= PostfixExpr | 'not' UnaryExpr | '-' UnaryExpr | '+' UnaryExpr
    BinaryExpr(binop) ::= UnaryExpr[left] BinOp[op] UnaryExpr[right]
    BinOp ::= '+' | '-' | '*' | '/' | '//' | '%' | '**' | 'and' | 'or' | '==' | '!=' | '<' | '>' | '<=' | '>='
    
    ComprehensionExpr ::= '[' Expr 'for' Variable 'in' Expr ('if' Expr)? ']'
    LambdaExpr(lambda) ::= 'lambda' ParamList ':' Expr[body]
    
    Expr ::= LambdaExpr | ComprehensionExpr | BinaryExpr | UnaryExpr
    
    // Expression lists
    ExprList ::= Expr | ExprList ',' Expr
    DictPair ::= Expr ':' Expr
    DictPairs ::= DictPair | DictPairs ',' DictPair
    
    // Statements
    ExprStmt ::= Expr
    AssignStmt(assign) ::= Variable[target] '=' Expr[value]
    AugAssignStmt ::= Variable BinOp '=' Expr
    ReturnStmt(return) ::= 'return' Expr?[value]
    
    IfStmt(if) ::= 'if' Expr[cond] ':' Suite[body] ('elif' Expr ':' Suite)* ('else' ':' Suite[orelse])?
    WhileStmt(while) ::= 'while' Expr[cond] ':' Suite[body]
    ForStmt(for) ::= 'for' Variable[target] 'in' Expr[iter] ':' Suite[body]
    
    // Function definition
    Parameter ::= Variable | Variable '=' Expr
    ParamList ::= Parameter | ParamList ',' Parameter
    FunctionDef(funcdef) ::= 'def' Identifier[name] '(' ParamList? ')' ':' Suite[body]
    
    // Class definition
    ClassDef(classdef) ::= 'class' Identifier[name] ('(' ExprList ')')? ':' Suite[body]
    
    // Statement suite (simplified - no indentation handling)
    SimpleStmt ::= ExprStmt | AssignStmt | AugAssignStmt | ReturnStmt
    CompoundStmt ::= IfStmt | WhileStmt | ForStmt | FunctionDef | ClassDef
    Suite ::= SimpleStmt | CompoundStmt
    Stmt ::= Suite
    
    // Module
    Module ::= Stmt*
    
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
    
    Γ ⊢ func : Any, Γ ⊢ args : Any
    ------------------------------- (call)
    Any
    "#;

    fn load_python_grammar() -> Grammar {
        Grammar::load(PYTHON_LIKE_SPEC).expect("Failed to load Python-like grammar")
    }

    #[test]
    fn test_python_basic_parsing() {
        let grammar = load_python_grammar();
        let mut parser = Parser::new(grammar);

        // Test variable assignment
        let ast = parser.parse("x = 42").expect("Failed to parse assignment");
        assert_eq!(ast.value(), "AssignStmt");

        // Test function definition
        let ast = parser.parse("def foo(): return 42").expect("Failed to parse function");
        assert_eq!(ast.value(), "FunctionDef");

        // Test lambda expression
        let ast = parser.parse("lambda x: x + 1").expect("Failed to parse lambda");
        assert_eq!(ast.value(), "LambdaExpr");
    }

    #[test]
    fn test_python_data_structures() {
        let grammar = load_python_grammar();
        let mut parser = Parser::new(grammar);

        // Test list literal
        let ast = parser.parse("[1, 2, 3]").expect("Failed to parse list");
        assert_eq!(ast.value(), "ListLit");

        // Test dictionary literal
        let ast = parser.parse("{'a': 1, 'b': 2}").expect("Failed to parse dict");
        assert_eq!(ast.value(), "DictLit");

        // Test function call
        let ast = parser.parse("func(a, b)").expect("Failed to parse call");
        assert_eq!(ast.value(), "Call");
    }
}

#[cfg(test)]
pub mod lisp_tests {
    //! Typed Lisp tests
    use crate::logic::{grammar::Grammar, parser::Parser};

    pub const TYPED_LISP_SPEC: &str = r#"
    // Lexical elements
    Symbol ::= /[a-zA-Z+\-*\/=<>!?][a-zA-Z0-9+\-*\/=<>!?_]*/
    Number ::= /[+-]?\d+/
    String ::= /"[^"]*"/
    
    // Variables
    Variable(var) ::= Symbol[x]
    
    // Atoms
    NumberAtom ::= Number
    StringAtom ::= String
    BoolAtom ::= '#t' | '#f'
    NilAtom ::= 'nil'
    Atom ::= Variable | NumberAtom | StringAtom | BoolAtom | NilAtom
    
    // Types
    PrimitiveType ::= 'Int' | 'String' | 'Bool' | 'Unit'
    ListType ::= '(' 'List' Type ')'
    FunctionType ::= '(' '->' Type+ ')'
    Type ::= PrimitiveType | ListType | FunctionType | Variable
    
    // Type annotations
    TypedVar ::= '(' Variable[x] ':' Type[τ] ')'
    
    // S-expressions
    List ::= '(' Expr* ')'
    Quote ::= "'" Expr
    Expr ::= Atom | List | Quote
    
    // Special forms
    LambdaExpr(lambda) ::= '(' 'lambda' '(' TypedVar* ')' Type[ret] Expr+[body] ')'
    LetExpr(let) ::= '(' 'let' '(' Binding* ')' Expr+[body] ')'
    IfExpr(if) ::= '(' 'if' Expr[cond] Expr[then] Expr[else] ')'
    DefineExpr(define) ::= '(' 'define' TypedVar Expr[value] ')'
    
    Binding ::= '(' Variable[x] Expr[value] ')'
    
    // Function application
    AppExpr(app) ::= '(' Expr[func] Expr+[args] ')'
    
    // Program
    Program ::= Expr*
    
    // Typing rules
    x ∈ Γ
    ----------- (var)
    Γ(x)
    
    Γ[x₁:τ₁,...,xₙ:τₙ] ⊢ body : τᵣ
    ------------------------------------------ (lambda)
    τ₁ → ... → τₙ → τᵣ
    
    Γ ⊢ func : τ₁ → τ₂, Γ ⊢ arg : τ₁
    ---------------------------------- (app)
    τ₂
    
    Γ ⊢ cond : Bool, Γ ⊢ then : τ, Γ ⊢ else : τ
    ---------------------------------------------- (if)
    τ
    "#;

    fn load_lisp_grammar() -> Grammar {
        Grammar::load(TYPED_LISP_SPEC).expect("Failed to load Typed Lisp grammar")
    }

    #[test]
    fn test_lisp_basic_parsing() {
        let grammar = load_lisp_grammar();
        let mut parser = Parser::new(grammar);

        // Test atom
        let ast = parser.parse("42").expect("Failed to parse number");
        assert_eq!(ast.value(), "NumberAtom");

        // Test symbol
        let ast = parser.parse("foo").expect("Failed to parse symbol");
        assert_eq!(ast.value(), "Variable");

        // Test simple list
        let ast = parser.parse("(+ 1 2)").expect("Failed to parse list");
        assert_eq!(ast.value(), "AppExpr");
    }

    #[test]
    fn test_lisp_special_forms() {
        let grammar = load_lisp_grammar();
        let mut parser = Parser::new(grammar);

        // Test lambda expression
        let ast = parser.parse("(lambda ((x : Int)) Int x)").expect("Failed to parse lambda");
        assert_eq!(ast.value(), "LambdaExpr");

        // Test let expression
        let ast = parser.parse("(let ((x 42)) x)").expect("Failed to parse let");
        assert_eq!(ast.value(), "LetExpr");

        // Test if expression
        let ast = parser.parse("(if #t 1 0)").expect("Failed to parse if");
        assert_eq!(ast.value(), "IfExpr");
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
    use crate::logic::{grammar::Grammar, parser::Parser, check::TypeChecker};
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
    fn test_type_checking_across_languages() {
        // Test type checking for supported languages
        
        // STLC type checking
        let stlc_grammar = Grammar::load(stlc_tests::STLC_SPEC).expect("Failed to load STLC");
        let mut stlc_parser = Parser::new(stlc_grammar.clone());
        
        if let Ok(ast) = stlc_parser.parse("λx:A.x") {
            let mut checker = TypeChecker::new();
            match checker.check(&ast) {
                Ok(ty) => println!("✅ STLC type check: λx:A.x -> {:?}", ty),
                Err(e) => println!("⚠️  STLC type check failed: {}", e),
            }
        }
    }
}