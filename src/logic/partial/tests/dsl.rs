use crate::logic::grammar::Grammar;
use crate::logic::partial::{MetaParser, Node};

fn count_named_children(root: &crate::logic::partial::NonTerminal, name: &str) -> usize {
    root.children
        .iter()
        .filter(|child| matches!(child, Node::NonTerminal(nt) if nt.name == name))
        .count()
}

#[test]
fn c_like_stmt_star_stays_flat() {
    let spec = r#"
    Identifier ::= /[a-z]+/
    Number ::= /[0-9]+/
    Assign ::= 'let' Identifier '=' Number ';'
    ExprStmt ::= Identifier ';'
    Stmt ::= Assign | ExprStmt
    Block ::= '{' Stmt* '}'
    start ::= Block
    "#;
    let grammar = Grammar::load(spec).unwrap();
    let mut parser = MetaParser::new(grammar);

    let ast = parser.parse("{ let x = 1; let y = 2; x; y; }").unwrap();
    let root = ast.complete().expect("complete block");

    assert_eq!(root.name, "start");
    let block = match &root.children[0] {
        Node::NonTerminal(nt) => nt,
        other => panic!("expected block node, got {:?}", other),
    };
    assert_eq!(block.name, "Block");
    assert_eq!(count_named_children(block, "Stmt"), 4);
}

#[test]
fn markdownish_item_plus_collects_items_without_nested_list_spine() {
    let spec = r#"
    Word ::= /[a-z]+/
    Item ::= '-' Word
    List ::= Item+
    start ::= List
    "#;
    let grammar = Grammar::load(spec).unwrap();
    let mut parser = MetaParser::new(grammar);

    let ast = parser.parse("- red - blue - green").unwrap();
    let root = ast.complete().expect("complete list");
    let list = match &root.children[0] {
        Node::NonTerminal(nt) => nt,
        other => panic!("expected list node, got {:?}", other),
    };

    assert_eq!(list.name, "List");
    assert_eq!(count_named_children(list, "Item"), 3);
}

#[test]
fn sexpr_like_args_star_accepts_empty_and_many_args() {
    let spec = r#"
    Atom ::= /[a-z]+/
    Call ::= '(' Atom Atom* ')'
    start ::= Call
    "#;
    let grammar = Grammar::load(spec).unwrap();
    let mut parser = MetaParser::new(grammar.clone());

    assert!(parser.parse("(f)").is_ok());

    let mut parser = MetaParser::new(grammar);
    let ast = parser.parse("(f x y z)").unwrap();
    let root = ast.complete().expect("complete call");
    let call = match &root.children[0] {
        Node::NonTerminal(nt) => nt,
        other => panic!("expected call node, got {:?}", other),
    };

    assert_eq!(count_named_children(call, "Atom"), 4);
}

#[test]
fn flat_repetition_program_has_smaller_forest_than_recursive_spine() {
    let recursive_spec = r#"
    Identifier ::= /[a-z]+/
    Number ::= /[0-9]+/
    Stmt ::= 'let' Identifier '=' Number ';'
    Stmts ::= Stmt Stmts | ε
    Block ::= '{' Stmts '}'
    start ::= Block
    "#;

    let flat_spec = r#"
    Identifier ::= /[a-z]+/
    Number ::= /[0-9]+/
    Stmt ::= 'let' Identifier '=' Number ';'
    Block ::= '{' Stmt* '}'
    start ::= Block
    "#;

    let input = "{ let x = 1; let y = 2; let z = 3; let w = 4; }";

    let mut recursive = MetaParser::new(Grammar::load(recursive_spec).unwrap());
    let recursive_ast = recursive.parse(input).unwrap();

    let mut flat = MetaParser::new(Grammar::load(flat_spec).unwrap());
    let flat_ast = flat.parse(input).unwrap();

    assert!(flat_ast.node_count() <= recursive_ast.node_count());
    assert!(flat_ast.total_alternatives() <= recursive_ast.total_alternatives());
}
