use beam::logic::typing::Type;

fn main() {
    println!("Testing type parsing...");
    
    // Test the problematic case
    let test_cases = vec![
        "'int'",
        "int", 
        "'void'",
        "void",
        "'string'",
        "string"
    ];
    
    for case in test_cases {
        match Type::parse(case) {
            Ok(ty) => println!("✅ '{}' parsed as: {:?}", case, ty),
            Err(e) => println!("❌ '{}' failed: {}", case, e),
        }
    }
}
