//! Testing utilities for P7
//!
//! This module provides common testing utilities to reduce code duplication
//! across test modules. It includes grammar loading, parsing assertions,
//! type checking assertions, and tree comparison utilities.
//!
//! # Equality Checking Strategy
//!
//! ## Full Equality
//!
//! Since serializations are injective (one-to-one), we use hashes of serializations
//! for efficient equality checking. This eliminates redundant tree traversal code.
//!
//! ## Structural Equality
//!
//! When we only want to check for equality of structure,
//! and not take into account extra info like continuations/derivatives,
//! we use the `serialize_structure` method to get a structure-only serialization
//! and compare hashes of those.

use crate::logic::grammar::Grammar;
use crate::logic::partial::parse::Parser;
use crate::logic::partial::structure::{Node, NonTerminal, Terminal};
use std::path::Path;

// ============================================================================
// Grammar Loading Utilities
// ============================================================================

/// Load a grammar from the examples directory
///
/// # Example
/// ```ignore
/// let g = load_example_grammar("stlc");
/// ```
pub fn load_example_grammar(name: &str) -> Grammar {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let path = Path::new(manifest_dir)
        .join("examples")
        .join(format!("{}.auf", name));
    let content = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("Failed to read {}: {}", path.display(), e));
    Grammar::load(&content).unwrap_or_else(|e| panic!("Failed to load grammar '{}': {}", name, e))
}

/// Load grammar from inline specification
///
/// # Example
/// ```ignore
/// let g = load_inline_grammar(r#"
///     expr ::= "x" | "y"
///     start ::= expr
/// "#);
/// ```
pub fn load_inline_grammar(spec: &str) -> Grammar {
    Grammar::load(spec).expect("Failed to load inline grammar")
}

/// Common test grammars as lazy statics
pub mod grammars {
    use super::*;
    use std::sync::OnceLock;

    static STLC: OnceLock<Grammar> = OnceLock::new();
    static CLIKE: OnceLock<Grammar> = OnceLock::new();
    static IMP: OnceLock<Grammar> = OnceLock::new();
    static FUN: OnceLock<Grammar> = OnceLock::new();

    pub fn stlc() -> &'static Grammar {
        STLC.get_or_init(|| load_example_grammar("stlc"))
    }

    pub fn clike() -> &'static Grammar {
        CLIKE.get_or_init(|| load_example_grammar("clike"))
    }

    pub fn imp() -> &'static Grammar {
        IMP.get_or_init(|| load_example_grammar("imp"))
    }

    pub fn fun() -> &'static Grammar {
        FUN.get_or_init(|| load_example_grammar("fun"))
    }
}

// ============================================================================
// Parsing Assertions
// ============================================================================

/// Assert that input parses successfully and produces a complete AST
pub fn assert_parses(grammar: &Grammar, input: &str) {
    let mut p = Parser::new(grammar.clone());
    let ast = p
        .parse(input)
        .unwrap_or_else(|e| panic!("Failed to parse '{}': {:?}", input, e));
    assert!(
        ast.is_complete(),
        "AST should be complete for input: {}",
        input
    );
}

/// Assert that input parses successfully and produces a complete AST with the given root name
pub fn assert_parses_as(grammar: &Grammar, input: &str, expected_root: &str) {
    let mut p = Parser::new(grammar.clone());
    let ast = p
        .parse(input)
        .unwrap_or_else(|e| panic!("Failed to parse '{}': {:?}", input, e));
    assert!(
        ast.is_complete(),
        "AST should be complete for input: {}",
        input
    );
    assert!(!ast.roots.is_empty(), "AST should have at least one root");
    assert_eq!(
        ast.roots[0].name, expected_root,
        "Expected root '{}', got '{}'",
        expected_root, ast.roots[0].name
    );
}

/// Assert that input fails to parse
pub fn assert_parse_fails(grammar: &Grammar, input: &str) {
    let mut p = Parser::new(grammar.clone());
    let result = p.parse(input);
    assert!(
        result.is_err(),
        "Expected parsing to fail for input: {}",
        input
    );
}

/// Assert that parsing produces the expected serialized AST structure
///
/// This deserializes the expected string and compares the trees structurally
/// rather than comparing serialized strings.
pub fn assert_parse_matches(grammar: &Grammar, input: &str, expected_serialized: &str) {
    let mut p = Parser::new(grammar.clone());

    let ast = p
        .parse(input)
        .unwrap_or_else(|e| panic!("Failed to parse '{}': {:?}", input, e));

    assert!(
        ast.is_complete(),
        "AST should be complete for input: {}",
        input
    );
    assert!(!ast.roots.is_empty(), "AST should have at least one root");

    // Deserialize the expected tree
    let expected = NonTerminal::deserialize(expected_serialized, grammar)
        .unwrap_or_else(|e| panic!("Failed to deserialize expected tree: {}", e));

    // Compare trees
    let actual = &ast.roots[0];
    assert_trees_equal(actual, &expected, input);
}

/// Assert that parsing produces a complete tree matching the expected serialization
pub fn assert_complete_matches(grammar: &Grammar, input: &str, expected_serialized: &str) {
    assert_parse_matches(grammar, input, expected_serialized);
}

/// Assert that parsing produces a partial tree that contains the expected structure
///
///  This checks that the parse succeeds and produces at least
/// one tree matching the expected structure, but doesn't require the parse to be complete.
pub fn assert_partial_matches(grammar: &Grammar, input: &str, expected_serialized: &str) {
    let mut p = Parser::new(grammar.clone());

    let ast = p
        .parse(input)
        .unwrap_or_else(|e| panic!("Failed to parse '{}': {:?}", input, e));

    assert!(!ast.roots.is_empty(), "AST should have at least one root");

    // Deserialize the expected tree
    let expected = NonTerminal::deserialize(expected_serialized, grammar)
        .unwrap_or_else(|e| panic!("Failed to deserialize expected tree: {}", e));

    // Find a matching tree in the roots
    let found = ast.roots.iter().any(|root| trees_equal(root, &expected));

    assert!(
        found,
        "No tree matching expected structure found in parse forest for input: '{}'",
        input
    );
}

// ============================================================================
// Tree Equality Checking (Hash-based)
// ============================================================================

/// Compute hash of a serialized structure for equality comparison.
/// Since serializations are injective, equal hashes imply equal structures.
fn compute_hash(serialized: &str) -> u64 {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    let mut hasher = DefaultHasher::new();
    serialized.hash(&mut hasher);
    hasher.finish()
}

/// Check if two trees are equal by comparing serialization hashes.
/// Serializations are injective, so this is a reliable equality check.
fn trees_equal(actual: &NonTerminal, expected: &NonTerminal) -> bool {
    compute_hash(&actual.serialize()) == compute_hash(&expected.serialize())
}

/// Check if two nodes are equal by comparing serialization hashes
fn nodes_equal(actual: &Node, expected: &Node) -> bool {
    compute_hash(&actual.serialize()) == compute_hash(&expected.serialize())
}

/// Check if two terminals are equal by comparing serialization hashes
fn terminals_equal(actual: &Terminal, expected: &Terminal) -> bool {
    compute_hash(&actual.serialize()) == compute_hash(&expected.serialize())
}

/// Assert two NonTerminal trees are equal using hash-based comparison
pub fn assert_trees_equal(actual: &NonTerminal, expected: &NonTerminal, context: &str) {
    if !trees_equal(actual, expected) {
        panic!(
            "Tree mismatch at '{}'\nExpected:\n{}\n\nActual:\n{}\n",
            context,
            expected.serialize(),
            actual.serialize()
        );
    }
}

/// Assert two Node trees are equal using hash-based comparison
#[allow(dead_code)]
fn assert_nodes_equal(actual: &Node, expected: &Node, context: &str) {
    if !nodes_equal(actual, expected) {
        panic!(
            "Node mismatch at '{}'\nExpected:\n{}\n\nActual:\n{}\n",
            context,
            expected.serialize(),
            actual.serialize()
        );
    }
}

/// Assert two Terminals are equal using hash-based comparison
#[allow(dead_code)]
fn assert_terminals_equal(actual: &Terminal, expected: &Terminal, context: &str) {
    if !terminals_equal(actual, expected) {
        panic!(
            "Terminal mismatch at '{}'\nExpected:\n{}\n\nActual:\n{}\n",
            context,
            expected.serialize(),
            actual.serialize()
        );
    }
}

// ============================================================================
// Structural Matching (ignoring derivatives)
// ============================================================================

/// Check if two NonTerminals structurally match by comparing structure-only serializations.
/// This ignores extensions/remainders but checks everything else.
/// Uses the serialize_structure method from the serialization module.
pub fn trees_match(actual: &NonTerminal, expected: &NonTerminal) -> bool {
    let actual_structure = actual.serialize_structure();
    let expected_structure = expected.serialize_structure();
    compute_hash(&actual_structure) == compute_hash(&expected_structure)
}

/// Check if two nodes structurally match
fn nodes_match(actual: &Node, expected: &Node) -> bool {
    match (actual, expected) {
        (Node::NonTerminal(a), Node::NonTerminal(e)) => trees_match(a, e),
        (
            Node::Terminal(Terminal::Complete {
                value: av,
                binding: ab,
                ..
            }),
            Node::Terminal(Terminal::Complete {
                value: ev,
                binding: eb,
                ..
            }),
        ) => av == ev && ab == eb,
        (
            Node::Terminal(Terminal::Partial {
                value: av,
                binding: ab,
                ..
            }),
            Node::Terminal(Terminal::Partial {
                value: ev,
                binding: eb,
                ..
            }),
        ) => av == ev && ab == eb,
        _ => false,
    }
}

/// Assert that two trees structurally match (ignoring extensions/remainders)
pub fn assert_trees_match(actual: &NonTerminal, expected: &NonTerminal, context: &str) {
    if !trees_match(actual, expected) {
        panic!(
            "Tree structure mismatch at '{}'\nExpected structure:\n{}\n\nActual structure:\n{}\n",
            context,
            expected.serialize_structure(),
            actual.serialize_structure()
        );
    }
}

/// Assert that two nodes structurally match
#[allow(dead_code)]
fn assert_nodes_match(actual: &Node, expected: &Node, context: &str) {
    if !nodes_match(actual, expected) {
        panic!(
            "Node structure mismatch at '{}'\nExpected:\n{}\n\nActual:\n{}\n",
            context,
            expected.serialize(),
            actual.serialize()
        );
    }
}

/// Assert that parsing produces a tree that structurally matches (ignoring derivatives)
pub fn assert_parse_structurally_matches(
    grammar: &Grammar,
    input: &str,
    expected_serialized: &str,
) {
    let mut parser = Parser::new(grammar.clone());
    let ast = parser
        .parse(input)
        .unwrap_or_else(|e| panic!("Failed to parse '{}': {:?}", input, e));

    assert!(
        ast.is_complete(),
        "AST should be complete for input: {}",
        input
    );
    assert!(!ast.roots.is_empty(), "AST should have at least one root");

    // Deserialize the expected tree
    let expected = NonTerminal::deserialize(expected_serialized, grammar)
        .unwrap_or_else(|e| panic!("Failed to deserialize expected tree: {}", e));

    // Compare trees structurally (ignoring extensions/remainders)
    let actual = &ast.roots[0];
    assert_trees_match(actual, &expected, input);
}

/// Assert that parsing produces a partial tree with at least one root that structurally matches
pub fn assert_partial_structurally_matches(
    grammar: &Grammar,
    input: &str,
    expected_serialized: &str,
) {
    let mut parser = Parser::new(grammar.clone());
    let ast = parser
        .parse(input)
        .unwrap_or_else(|e| panic!("Failed to parse '{}': {:?}", input, e));

    assert!(!ast.roots.is_empty(), "AST should have at least one root");

    // Deserialize the expected tree
    let expected = NonTerminal::deserialize(expected_serialized, grammar)
        .unwrap_or_else(|e| panic!("Failed to deserialize expected tree: {}", e));

    // Check if any root structurally matches
    let found_match = ast.roots.iter().any(|root| trees_match(root, &expected));

    assert!(
        found_match,
        "No root in AST matches expected tree for input '{}'\nExpected:\n{}\nActual roots:\n{}",
        input,
        expected.serialize(),
        ast.roots
            .iter()
            .enumerate()
            .map(|(i, r)| format!("Root {}:\n{}", i, r.serialize()))
            .collect::<Vec<_>>()
            .join("\n")
    );
}

// ============================================================================
// Tree Inspection Utilities
// ============================================================================

/// Count the total number of nodes in a tree
pub fn count_nodes(root: &NonTerminal) -> usize {
    1 + root
        .children
        .iter()
        .map(|child| match child {
            Node::NonTerminal(nt) => count_nodes(nt),
            Node::Terminal(_) => 1,
        })
        .sum::<usize>()
}

/// Get all terminal values in the tree (in order)
pub fn collect_terminal_values(root: &NonTerminal) -> Vec<String> {
    let mut values = Vec::new();
    collect_terminals_rec(root, &mut values);
    values
}

fn collect_terminals_rec(nt: &NonTerminal, values: &mut Vec<String>) {
    for child in &nt.children {
        match child {
            Node::NonTerminal(nt) => collect_terminals_rec(nt, values),
            Node::Terminal(Terminal::Complete { value, .. }) => values.push(value.clone()),
            Node::Terminal(Terminal::Partial { value, .. }) => values.push(value.clone()),
        }
    }
}

/// Get all bindings in the tree
pub fn collect_bindings(root: &NonTerminal) -> Vec<(String, String)> {
    let mut bindings = Vec::new();
    collect_bindings_rec(root, &mut bindings);
    bindings
}

fn collect_bindings_rec(nt: &NonTerminal, bindings: &mut Vec<(String, String)>) {
    if let Some(ref binding) = nt.binding {
        bindings.push((nt.name.clone(), binding.clone()));
    }

    for child in &nt.children {
        match child {
            Node::NonTerminal(nt) => collect_bindings_rec(nt, bindings),
            Node::Terminal(Terminal::Complete {
                binding: Some(b), ..
            })
            | Node::Terminal(Terminal::Partial {
                binding: Some(b), ..
            }) => {
                bindings.push(("Terminal".to_string(), b.clone()));
            }
            _ => {}
        }
    }
}

// ============================================================================
// Convenience Macros
// ============================================================================

/// Parse input with a grammar and panic with a nice error message on failure
#[macro_export]
macro_rules! parse {
    ($grammar:expr, $input:expr) => {{
        let mut p = $crate::logic::partial::parse::Parser::new(($grammar).clone());
        p.parse($input)
            .unwrap_or_else(|e| panic!("Failed to parse '{}': {:?}", $input, e))
    }};
}

/// Assert that two values are equal with a custom message
#[macro_export]
macro_rules! assert_eq_msg {
    ($left:expr, $right:expr, $msg:expr) => {
        assert_eq!($left, $right, "{}", $msg)
    };
}
