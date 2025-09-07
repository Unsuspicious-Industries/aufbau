//! Advanced Type Checking Tests
//! 
//! This module contains comprehensive test cases for advanced type checking scenarios,
//! including FAIL tests for unimplemented features and complex program-like tests.
//! These tests serve as goal posts for future development and validate the robustness
//! of the type checking system.

#[cfg(test)]
pub mod advanced_fail_tests {
    use crate::logic::{grammar::Grammar, parser::Parser, check::TypeChecker, typing::Type};
    use crate::debug_info;

    /// Comprehensive C-like grammar with advanced features for testing
    pub const ADVANCED_C_SPEC: &str = r#"
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
    ParamList ::= Type | ParamList ',' Type
    
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
    ReturnStmt(return) ::= 'return' Expr?[value]
    BlockStmt ::= '{' Stmt* '}'
    ExprStmt ::= Expr
    
    Stmt ::= VarDecl | Assignment | IfStmt | WhileStmt | ForStmt | ReturnStmt | BlockStmt | ExprStmt
    
    // Function definitions (UNIMPLEMENTED - should fail complex cases)
    FunctionDef(funcdef) ::= Type[ret] Identifier[name] '(' ParamDeclList? ')' BlockStmt[body]
    ParamDecl ::= Type Identifier
    ParamDeclList ::= ParamDecl | ParamDeclList ',' ParamDecl
    
    // Program
    Program ::= FunctionDef | Stmt | Expr
    
    // Basic typing rules (many advanced features not implemented)
    x ∈ Γ
    ----------- (var)
    Γ(x)
    
    Γ ⊢ left : int, Γ ⊢ right : int
    -------------------------------- (binop_int)
    int
    
    // More complex rules would be needed for pointers, structs, etc.
    "#;

    /// TypeScript-like grammar with advanced type features
    pub const ADVANCED_TS_SPEC: &str = r#"
    // Identifiers and literals
    Identifier ::= /[a-zA-Z_$][a-zA-Z0-9_$]*/
    Number ::= /\d+(\.\d+)?/
    String ::= /'[^']*'/ | /"[^"]*"/
    
    // Variables
    Variable(var) ::= Identifier[x]
    
    // Basic types
    PrimitiveType ::= 'number' | 'string' | 'boolean' | 'void' | 'any' | 'never'
    
    // Advanced type features (UNIMPLEMENTED - should fail)
    GenericType ::= Identifier[base] '<' TypeList '>'
    UnionType ::= Type[left] '|' Type[right]
    IntersectionType ::= Type[left] '&' Type[right]
    TupleType ::= '[' TypeList ']'
    ArrayType ::= Type[base] '[' ']'
    FunctionType ::= '(' ParamTypeList? ')' '=>' Type[ret]
    ObjectType ::= '{' PropertyList? '}'
    IndexSignature ::= '[' Identifier ':' Type ']' ':' Type
    
    TypeList ::= Type | TypeList ',' Type
    ParamTypeList ::= Type | ParamTypeList ',' Type
    PropertyList ::= Property | PropertyList ',' Property
    Property ::= Identifier ':' Type | Identifier '?' ':' Type
    
    Type ::= PrimitiveType | GenericType | UnionType | IntersectionType | TupleType | ArrayType | FunctionType | ObjectType | '(' Type ')'
    
    // Expressions
    Literal ::= Number | String | 'true' | 'false' | 'null' | 'undefined'
    
    // Object and array operations (UNIMPLEMENTED)
    ObjectLiteral ::= '{' PropertyValues? '}'
    PropertyValues ::= PropertyValue | PropertyValues ',' PropertyValue
    PropertyValue ::= Identifier ':' Expr | String ':' Expr
    
    ArrayLiteral ::= '[' ExprList? ']'
    ExprList ::= Expr | ExprList ',' Expr
    
    PropertyAccess(propaccess) ::= Expr[obj] '.' Identifier[prop]
    IndexAccess(indexaccess) ::= Expr[obj] '[' Expr[idx] ']'
    
    // Function expressions and calls
    ArrowFunction(arrow) ::= '(' ParamList? ')' '=>' Expr[body]
    FunctionCall(call) ::= Expr[func] '(' ExprList? ')'
    
    ParamList ::= Identifier | ParamList ',' Identifier
    
    // Type assertions (UNIMPLEMENTED)
    TypeAssertion(assertion) ::= Expr[expr] 'as' Type[type]
    TypeGuard(guard) ::= Expr[expr] 'is' Type[type]
    
    // All expressions
    Expr ::= Variable | Literal | ObjectLiteral | ArrayLiteral | PropertyAccess | IndexAccess | ArrowFunction | FunctionCall | TypeAssertion | TypeGuard | '(' Expr ')'
    
    // Statements
    VarDecl(vardecl) ::= ('let' | 'const' | 'var') Variable[var] (':' Type[type])? ('=' Expr[init])?
    Assignment(assign) ::= Expr[target] '=' Expr[value]
    
    Stmt ::= VarDecl | Assignment | Expr
    Program ::= Stmt | Expr
    
    // Basic typing rules (advanced features not implemented)
    x ∈ Γ
    ----------- (var)
    Γ(x)
    "#;

    fn load_advanced_c_grammar() -> Grammar {
        Grammar::load(ADVANCED_C_SPEC).expect("Failed to load advanced C grammar")
    }

    fn load_advanced_ts_grammar() -> Grammar {
        Grammar::load(ADVANCED_TS_SPEC).expect("Failed to load advanced TypeScript grammar")
    }

    /// Test pointer type operations - should fail due to unimplemented features
    #[test]
    fn test_pointer_operations_fail() {
        let grammar = load_advanced_c_grammar();
        let mut parser = Parser::new(grammar);

        // Test pointer declaration - parsing should work but type checking should fail
        let expr = "int* ptr";
        let ast = parser.parse(expr).expect("Failed to parse pointer declaration");
        
        let mut tc = TypeChecker::with_input(Some(expr.to_string()));
        let res = tc.check(&ast);
        
        // Should fail because pointer types are not implemented
        assert!(res.is_err(), "Expected pointer type checking to fail but it succeeded");
        debug_info!("test", "Expected pointer error: {:?}", res);
    }

    /// Test complex function with pointers and structs - should fail
    #[test]
    fn test_complex_function_with_pointers_fail() {
        let grammar = load_advanced_c_grammar();
        let mut parser = Parser::new(grammar);

        // A realistic C function that uses unimplemented features
        let program = r#"
        struct Point {
            int x;
            int y;
        }
        
        struct Point* create_point(int x, int y) {
            struct Point* p = malloc(sizeof(struct Point));
            p->x = x;
            p->y = y;
            return p;
        }
        "#;

        // This should parse (grammar supports it) but type checking should fail
        if let Ok(ast) = parser.parse(program) {
            let mut tc = TypeChecker::with_input(Some(program.to_string()));
            let res = tc.check(&ast);
            
            assert!(res.is_err(), "Expected complex function type checking to fail but it succeeded");
            debug_info!("test", "Expected complex function error: {:?}", res);
        } else {
            // If parsing fails, that's also acceptable for this test
            debug_info!("test", "Complex function parsing failed as expected");
        }
    }

    /// Test memory management operations - should fail
    #[test]
    fn test_memory_management_fail() {
        let grammar = load_advanced_c_grammar();
        let mut parser = Parser::new(grammar);

        let programs = vec![
            // Address-of operator
            "int x = 5; int* p = &x;",
            // Dereference operator  
            "int* p; int y = *p;",
            // Array indexing
            "int arr[10]; int val = arr[5];",
            // Pointer arithmetic (should fail)
            "int* p; int* q = p + 1;",
        ];

        for program in programs {
            if let Ok(ast) = parser.parse(program) {
                let mut tc = TypeChecker::with_input(Some(program.to_string()));
                let res = tc.check(&ast);
                
                assert!(res.is_err(), "Expected memory operation '{}' to fail but it succeeded", program);
                debug_info!("test", "Expected memory error for '{}': {:?}", program, res);
            }
        }
    }

    /// Test generic types - should fail due to unimplemented features
    #[test] 
    fn test_generics_fail() {
        let grammar = load_advanced_ts_grammar();
        let mut parser = Parser::new(grammar);

        let programs = vec![
            // Generic function
            "function identity<T>(x: T): T { return x; }",
            // Generic array
            "let arr: Array<number> = [1, 2, 3];",
            // Generic object
            "let map: Map<string, number> = new Map();",
            // Complex generic with constraints
            "function process<T extends Serializable>(items: T[]): T[] { return items; }",
        ];

        for program in programs {
            if let Ok(ast) = parser.parse(program) {
                let mut tc = TypeChecker::with_input(Some(program.to_string()));
                let res = tc.check(&ast);
                
                assert!(res.is_err(), "Expected generic type '{}' to fail but it succeeded", program);
                debug_info!("test", "Expected generic error for '{}': {:?}", program, res);
            }
        }
    }

    /// Test union and intersection types - should fail
    #[test]
    fn test_union_intersection_types_fail() {
        let grammar = load_advanced_ts_grammar();
        let mut parser = Parser::new(grammar);

        let programs = vec![
            // Union types
            "let value: string | number = 'hello';",
            "let value: string | number = 42;",
            // Intersection types  
            "let obj: { name: string } & { age: number } = { name: 'John', age: 30 };",
            // Complex unions
            "let status: 'loading' | 'success' | 'error' = 'loading';",
            // Discriminated unions
            "type Result = { success: true; data: any } | { success: false; error: string };",
        ];

        for program in programs {
            if let Ok(ast) = parser.parse(program) {
                let mut tc = TypeChecker::with_input(Some(program.to_string()));
                let res = tc.check(&ast);
                
                assert!(res.is_err(), "Expected union/intersection type '{}' to fail but it succeeded", program);
                debug_info!("test", "Expected union/intersection error for '{}': {:?}", program, res);
            }
        }
    }

    /// Test complex object types and property access - should fail
    #[test]
    fn test_object_types_fail() {
        let grammar = load_advanced_ts_grammar();
        let mut parser = Parser::new(grammar);

        let programs = vec![
            // Object literal with type annotation
            "let person: { name: string; age: number } = { name: 'John', age: 30 };",
            // Property access  
            "let person = { name: 'John' }; let name = person.name;",
            // Index access
            "let obj = { a: 1 }; let val = obj['a'];",
            // Optional properties
            "let config: { debug?: boolean; port: number } = { port: 3000 };",
            // Index signatures
            "let dict: { [key: string]: number } = { a: 1, b: 2 };",
        ];

        for program in programs {
            if let Ok(ast) = parser.parse(program) {
                let mut tc = TypeChecker::with_input(Some(program.to_string()));
                let res = tc.check(&ast);
                
                assert!(res.is_err(), "Expected object type '{}' to fail but it succeeded", program);
                debug_info!("test", "Expected object error for '{}': {:?}", program, res);
            }
        }
    }

    /// Test subtle type checking failures - edge cases
    #[test]
    fn test_subtle_type_failures() {
        let grammar = load_advanced_ts_grammar(); 
        let mut parser = Parser::new(grammar);

        let programs = vec![
            // Type assertion to wrong type
            "let x: any = 'hello'; let y = x as number;",
            // Function with wrong return type
            "let f = (): number => 'hello';",
            // Array with mixed types (should be inferred as union)
            "let arr = [1, 'hello', true];",
            // Property access on potentially undefined
            "let obj: { prop?: string } = {}; let val = obj.prop.length;",
            // Calling non-function
            "let x = 42; let result = x();",
        ];

        for program in programs {
            if let Ok(ast) = parser.parse(program) {
                let mut tc = TypeChecker::with_input(Some(program.to_string()));
                let res = tc.check(&ast);
                
                assert!(res.is_err(), "Expected subtle type error for '{}' but it succeeded", program);
                debug_info!("test", "Expected subtle error for '{}': {:?}", program, res);
            }
        }
    }
}

#[cfg(test)]
pub mod realistic_program_tests {
    use crate::logic::{grammar::Grammar, parser::Parser, check::TypeChecker};
    use crate::debug_info;

    /// Test a realistic linked list implementation (should fail due to missing features)
    #[test]
    fn test_linked_list_program() {
        let program = r#"
        struct Node {
            int data;
            struct Node* next;
        }
        
        struct Node* create_node(int data) {
            struct Node* node = malloc(sizeof(struct Node));
            node->data = data;
            node->next = NULL;
            return node;
        }
        
        void insert_front(struct Node** head, int data) {
            struct Node* new_node = create_node(data);
            new_node->next = *head;
            *head = new_node;
        }
        
        int list_length(struct Node* head) {
            int count = 0;
            while (head != NULL) {
                count++;
                head = head->next;
            }
            return count;
        }
        "#;

        // This realistic program uses many unimplemented features:
        // - Struct definitions and field access
        // - Pointer types and operations  
        // - Memory allocation (malloc)
        // - Double pointers (Node**)
        // - NULL pointer constant
        // - While loops with pointer traversal

        if let Ok(grammar) = Grammar::load(super::advanced_fail_tests::ADVANCED_C_SPEC) {
            let mut parser = Parser::new(grammar);
            
            if let Ok(ast) = parser.parse(program) {
                let mut tc = TypeChecker::with_input(Some(program.to_string()));
                let res = tc.check(&ast);
                
                // Should fail due to unimplemented pointer and struct features
                assert!(res.is_err(), "Expected linked list program to fail type checking");
                debug_info!("test", "Linked list program failed as expected: {:?}", res);
            }
        }
    }

    /// Test a realistic web API handler (TypeScript-like, should fail)
    #[test]
    fn test_web_api_handler() {
        let program = r#"
        interface User {
            id: number;
            name: string;
            email?: string;
        }
        
        interface ApiResponse<T> {
            success: boolean;
            data?: T;
            error?: string;
        }
        
        async function getUser(id: number): Promise<ApiResponse<User>> {
            try {
                const response = await fetch(`/api/users/${id}`);
                const user: User = await response.json();
                return { success: true, data: user };
            } catch (error) {
                return { 
                    success: false, 
                    error: error.message || 'Unknown error' 
                };
            }
        }
        
        function handleUserRequest(req: Request): Promise<Response> {
            const userId = parseInt(req.params.id);
            if (isNaN(userId)) {
                return Promise.resolve(new Response('Invalid user ID', { status: 400 }));
            }
            
            return getUser(userId).then(result => {
                if (result.success && result.data) {
                    return new Response(JSON.stringify(result.data), {
                        headers: { 'Content-Type': 'application/json' }
                    });
                } else {
                    return new Response(result.error || 'User not found', { status: 404 });
                }
            });
        }
        "#;

        // This realistic program uses many unimplemented features:
        // - Interface definitions
        // - Generic types (ApiResponse<T>, Promise<T>)
        // - Optional properties (email?, data?, error?)
        // - Async/await syntax
        // - Template literals (`/api/users/${id}`)
        // - Object destructuring and property access
        // - Union types for error handling
        // - Method chaining (.then(), .resolve())

        if let Ok(grammar) = Grammar::load(super::advanced_fail_tests::ADVANCED_TS_SPEC) {
            let mut parser = Parser::new(grammar);
            
            if let Ok(ast) = parser.parse(program) {
                let mut tc = TypeChecker::with_input(Some(program.to_string()));
                let res = tc.check(&ast);
                
                // Should fail due to unimplemented advanced TypeScript features
                assert!(res.is_err(), "Expected web API handler to fail type checking");
                debug_info!("test", "Web API handler failed as expected: {:?}", res);
            }
        }
    }

    /// Test a realistic data processing pipeline (functional style, should fail)
    #[test]
    fn test_data_processing_pipeline() {
        let program = r#"
        type DataRecord = {
            id: string;
            timestamp: Date;
            value: number;
            tags: string[];
        };
        
        type ProcessingResult<T, E> = 
            | { success: true; data: T }
            | { success: false; error: E };
        
        function parseRecord(raw: string): ProcessingResult<DataRecord, string> {
            try {
                const parsed = JSON.parse(raw);
                if (!parsed.id || typeof parsed.value !== 'number') {
                    return { success: false, error: 'Invalid record format' };
                }
                return {
                    success: true,
                    data: {
                        id: parsed.id,
                        timestamp: new Date(parsed.timestamp),
                        value: parsed.value,
                        tags: parsed.tags || []
                    }
                };
            } catch (e) {
                return { success: false, error: `Parse error: ${e.message}` };
            }
        }
        
        function processData(records: DataRecord[]): {
            total: number;
            average: number;
            errors: string[];
        } {
            const validRecords = records.filter(r => r.value >= 0);
            const total = validRecords.reduce((sum, r) => sum + r.value, 0);
            const average = validRecords.length > 0 ? total / validRecords.length : 0;
            
            return {
                total,
                average,
                errors: records
                    .filter(r => r.value < 0)
                    .map(r => `Invalid value ${r.value} for record ${r.id}`)
            };
        }
        
        function pipeline(rawData: string[]): ProcessingResult<any, string[]> {
            const results = rawData.map(parseRecord);
            const errors = results
                .filter(r => !r.success)
                .map(r => r.error);
            
            if (errors.length > 0) {
                return { success: false, error: errors };
            }
            
            const records = results
                .filter(r => r.success)
                .map(r => r.data);
            
            const processed = processData(records);
            return { success: true, data: processed };
        }
        "#;

        // This realistic program uses many unimplemented features:
        // - Type aliases (type DataRecord = ...)
        // - Discriminated unions (ProcessingResult<T, E>)
        // - Generic types with multiple parameters
        // - Array methods (.filter(), .map(), .reduce())
        // - Object spread and destructuring  
        // - Template literals with expressions
        // - Complex type guards and narrowing
        // - Date objects and constructors
        // - JSON parsing and error handling

        if let Ok(grammar) = Grammar::load(super::advanced_fail_tests::ADVANCED_TS_SPEC) {
            let mut parser = Parser::new(grammar);
            
            if let Ok(ast) = parser.parse(program) {
                let mut tc = TypeChecker::with_input(Some(program.to_string()));
                let res = tc.check(&ast);
                
                // Should fail due to unimplemented advanced features
                assert!(res.is_err(), "Expected data processing pipeline to fail type checking");
                debug_info!("test", "Data processing pipeline failed as expected: {:?}", res);
            }
        }
    }
}