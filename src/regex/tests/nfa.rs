use crate::regex::nfa::NFA;
use crate::regex::Regex;

#[test]
fn test_build_from_regex() {
    let regex = Regex::from_str("(a|b)bb").unwrap();
    let automata = NFA::from(regex);
    println!("{}", automata);
}
