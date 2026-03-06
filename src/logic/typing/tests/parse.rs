use crate::logic::typing::Type;

#[test]
fn test_parse_raw() {
    println!("A -> {:?}", Type::parse("A"));
    println!("A raw -> {:?}", Type::parse_raw("A"));
}
