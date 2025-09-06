// Demo of repetition operators working
use beam::logic::grammar::Grammar;
use beam::logic::parser::Parser;

fn main() {
    // Grammar that demonstrates all repetition operators
    let grammar_text = r#"
    // Function definitions with optional return type and variable parameters
    Param ::= Identifier[name] ':' Type[type]
    ReturnType ::= '->' Type[type]
    Block ::= '{' Stmt* '}'
    Stmt ::= VarDecl | Assignment | Expression ';'
    VarDecl ::= 'let' Identifier[name] '=' Expression[value] ';'
    Assignment ::= Identifier[name] '=' Expression[value] ';'
    Expression ::= Identifier | Number
    Type ::= 'int' | 'string'
    Identifier ::= /[a-zA-Z][a-zA-Z0-9]*/
    Number ::= /[0-9]+/
    FunctionDef ::= 'fn' Identifier[name] '(' Param* ')' ReturnType? Block[body]
    "#;

    println!("Loading grammar with repetition operators...");
    
    match Grammar::load(grammar_text) {
        Ok(grammar) => {
            println!("✅ Grammar loaded successfully!");
            
            // Check that repetition operators were parsed correctly
            let function_def = grammar.productions.get("FunctionDef").unwrap();
            println!("FunctionDef production:");
            for symbol in &function_def[0].rhs {
                if let Some(rep) = &symbol.repetition {
                    println!("  - Symbol '{}' has repetition: {:?}", symbol.value, rep);
                } else {
                    println!("  - Symbol '{}'", symbol.value);
                }
            }
            
            let mut parser = Parser::new(grammar);
            
            // Test parsing function with no parameters
            println!("\nTesting: fn main() {{ }}");
            match parser.parse("fn main ( ) { }") {
                Ok(_) => println!("✅ Successfully parsed function with no parameters!"),
                Err(e) => println!("❌ Failed to parse: {}", e),
            }
            
            // Reset parser for next test
            let grammar = Grammar::load(grammar_text).unwrap();
            let mut parser = Parser::new(grammar);
            
            // Test parsing function with parameters and body
            println!("\nTesting: fn add(x: int, y: int) -> int {{ let result = x; }}");
            match parser.parse("fn add ( x : int , y : int ) -> int { let result = x ; }") {
                Ok(_) => println!("✅ Successfully parsed function with parameters and body!"),
                Err(e) => println!("❌ Failed to parse: {}", e),
            }
        }
        Err(e) => {
            println!("❌ Failed to load grammar: {}", e);
        }
    }
}