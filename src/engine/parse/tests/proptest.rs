//! Property-based tests for the prefix parser engine.
//!
//! These tests fuzz grammars and inputs to exercise structural invariants
//! of the Earley-style parser: monotonicity, deduplication, arena consistency,
//! status propagation, and obligation correctness.

use super::utils::*;
use crate::engine::grammar::SPG;
use crate::engine::parse::arena::ChildRef;
use crate::engine::parse::Task;
use proptest::prelude::*;
use proptest::proptest;

// ── Grammar and input generation ──────────────────────────────────────────────

/// Curated set of CFG specs without typing rules.
const GRAMMAR_SHAPES: &[&str] = &[
    "A ::= 'a' 'b' | 'a' A 'b'\nstart ::= A",
    "A ::= 'x' A | 'y'\nstart ::= A",
    "A ::= 'a' | 'b' | 'c'\nstart ::= A",
    "A ::= 'a' B | B\nB ::= 'b' | ε\nstart ::= A",
    "A ::= '(' A ')' | 'x'\nstart ::= A",
    "A ::= 'x' B | 'y'\nB ::= A 'z' | ε\nstart ::= A",
    "A ::= 'a' A | ε\nstart ::= A",
    "A ::= 'a' | A '+' A\nstart ::= A",
    "E ::= 'n' | E '+' E | '(' E ')'\nstart ::= E",
    "S ::= 'a' S 'b' S | ε\nstart ::= S",
    "A ::= 'z' | A '*' A | '-' A\nstart ::= A",
    "X ::= 'a' X 'a' | 'b'\nstart ::= X",
];

const TOKEN_ALPHABET: &[&str] = &["a", "b", "c", "x", "y", "z", "+", "*", "(", ")", "-", "n"];

/// Pick a grammar from the curated list and generate a random token sequence.
fn grammar_and_input() -> impl Strategy<Value = (String, String)> {
    let spec_idx = 0usize..GRAMMAR_SHAPES.len();
    let token_indices = proptest::collection::vec(0usize..TOKEN_ALPHABET.len(), 1..=5);

    (spec_idx, token_indices).prop_map(move |(spec_i, indices)| {
        let spec = GRAMMAR_SHAPES[spec_i].to_string();
        let input_len = indices.len();
        let input: String = indices
            .into_iter()
            .take(input_len)
            .map(|i| TOKEN_ALPHABET[i].to_string())
            .collect::<Vec<_>>()
            .join(" ");
        (spec, input)
    })
}

// ── Helpers ───────────────────────────────────────────────────────────────────

fn try_parse(grammar: &SPG, input: &str) -> Result<bool, ()> {
    let mut parser = stub_parser(grammar.clone());
    match parser.parse(input, 0) {
        Ok(ast) => Ok(ast.is_complete()),
        Err(_) => Err(()),
    }
}

fn token_prefixes(grammar: &mut SPG, input: &str) -> Vec<String> {
    match grammar.tokenize(input) {
        Ok(segs) => {
            let mut prefixes = vec![String::new()];
            for seg in &segs {
                let mut p = if let Some(last) = prefixes.last() {
                    last.clone()
                } else {
                    String::new()
                };
                if !p.is_empty() {
                    p.push(' ');
                }
                p.push_str(seg.as_str());
                if !prefixes.contains(&p) {
                    prefixes.push(p);
                }
            }
            prefixes
        }
        Err(_) => {
            let chars: Vec<char> = input.chars().collect();
            (0..=chars.len())
                .map(|i| chars[..i].iter().collect())
                .collect()
        }
    }
}

// ════════════════════════════════════════════════════════════════════════════════
// Property 1: Prefix monotonicity
// ════════════════════════════════════════════════════════════════════════════════
//
// If a full input parses (no error returned), every segment-boundary prefix
// of that input must also parse without error. The parser never "un-accepts"
// a prefix it already admitted.

proptest! {
    #![proptest_config(ProptestConfig::with_cases(200))]

    #[test]
    fn prefix_monotone((spec, input) in grammar_and_input()) {
        let mut grammar = SPG::load(&spec).unwrap();
        let full = try_parse(&grammar, &input);
        let prefixes = token_prefixes(&mut grammar, &input);

        for prefix in &prefixes {
            let pfx = try_parse(&grammar, prefix);
            if full.is_ok() && pfx.is_err() {
                panic!(
                    "monotonicity violated:\n  grammar: {spec}\n  full '{input}' accepted\n  prefix '{prefix}' rejected"
                );
            }
        }
    }
}

// ════════════════════════════════════════════════════════════════════════════════
// Property 2: Arena node span consistency
// ════════════════════════════════════════════════════════════════════════════════
//
// Every arena node has start ≤ end.

proptest! {
    #![proptest_config(ProptestConfig::with_cases(200))]

    #[test]
    fn arena_spans_are_consistent((spec, input) in grammar_and_input()) {
        let grammar = SPG::load(&spec).unwrap();
        let mut parser = stub_parser(grammar);
        let Ok(ast) = parser.parse(&input, 0) else { return Ok(()); };
        let arena = ast.arena();

        for id in 0..arena.node_count() {
            let Some(node) = arena.node(id) else { continue; };
            prop_assert!(
                node.span.start <= node.span.end,
                "node {id} has inverted span: start={}, end={}",
                node.span.start,
                node.span.end
            );
        }
    }
}

// ════════════════════════════════════════════════════════════════════════════════
// Property 3: Parse determinism
// ════════════════════════════════════════════════════════════════════════════════
//
// Parsing the same grammar+input twice yields the same number of roots.

proptest! {
    #![proptest_config(ProptestConfig::with_cases(100))]

    #[test]
    fn parse_is_deterministic((spec, input) in grammar_and_input()) {
        let grammar = SPG::load(&spec).unwrap();

        let mut p1 = stub_parser(grammar.clone());
        let mut p2 = stub_parser(grammar.clone());

        let r1 = p1.parse(&input, 0);
        let r2 = p2.parse(&input, 0);

        match (r1, r2) {
            (Ok(a1), Ok(a2)) => {
                prop_assert_eq!(
                    a1.root_ids().len(),
                    a2.root_ids().len(),
                    "parse non-deterministic: different root counts"
                );
            }
            (Err(_), Err(_)) => {},
            (a, b) => {
                panic!(
                    "parse non-deterministic: one succeeded ({}) one failed ({})",
                    a.is_ok(), b.is_ok()
                );
            }
        }
    }
}

// ════════════════════════════════════════════════════════════════════════════════
// Property 4: Agenda deduplication via seen_process
// ════════════════════════════════════════════════════════════════════════════════
//
// The parser uses `Tables::seen_process` to deduplicate process items.
// Verify that every process-item key in the agenda has a corresponding
// entry in `seen_process`, and that no duplicate entries leak out.

proptest! {
    #![proptest_config(ProptestConfig::with_cases(100))]

    #[test]
    fn seen_process_covers_agenda((spec, input) in grammar_and_input()) {
        let grammar = SPG::load(&spec).unwrap();
        let mut parser = stub_parser(grammar);

        let start_nt = {
            let g = parser.grammar();
            g.start()
                .and_then(|s| g.nt_index(s))
                .unwrap_or(0)
        };
        parser.set_input(&input).unwrap();
        parser.seed_for_test(start_nt, 0, 0);

        let mut limit = 0;
        let max_steps = 800;

        while let Some(task) = parser.tables.agenda.pop_front() {
            limit += 1;
            if limit > max_steps { break; }

            // Every Process item in the agenda must be tracked in seen_process.
            if let Task::Process(ref item) = task {
                let key = (item.prod, item.dot, item.start, item.pos, item.ctx);
                prop_assert!(
                    parser.tables.seen_process.contains(&key),
                    "agenda item not in seen_process: prod={:?} dot={} start={} pos={} ctx={}",
                    item.prod, item.dot, item.start, item.pos, item.ctx
                );
            }

            match task {
                Task::Process(item) => {
                    let _ = parser.process_for_test(item);
                }
                Task::Complete(c) => {
                    let _ = parser.complete_for_test(c);
                }
            }
        }
    }
}

// ════════════════════════════════════════════════════════════════════════════════
// Property 5: Complete roots → all children complete
// ════════════════════════════════════════════════════════════════════════════════
//
// If a root node is marked complete, recursively all its descendants in the
// selected packed alternative must also be complete.

proptest! {
    #![proptest_config(ProptestConfig::with_cases(100))]

    #[test]
    fn complete_roots_have_complete_descendants((spec, input) in grammar_and_input()) {
        let grammar = SPG::load(&spec).unwrap();
        let mut parser = stub_parser(grammar);
        let Ok(ast) = parser.parse(&input, 0) else { return Ok(()); };
        let arena = ast.arena();

        for &root_id in ast.root_ids() {
            if let Some(node) = arena.node(root_id) {
                if node.is_complete() {
                    let mut stack = vec![root_id];
                    while let Some(id) = stack.pop() {
                        if let Some(alts) = arena.alts_for(id) {
                            if let Some(alt) = alts.first() {
                                for child in &alt.children {
                                    if let ChildRef::Node(cid) = child {
                                        if let Some(cnode) = arena.node(*cid) {
                                            prop_assert!(
                                                cnode.status.complete(),
                                                "child {cid} of complete node {id} has status {:?}",
                                                cnode.status
                                            );
                                        }
                                        stack.push(*cid);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

// ════════════════════════════════════════════════════════════════════════════════
// Property 6: Every node in the arena has at least one alternative
// ════════════════════════════════════════════════════════════════════════════════

proptest! {
    #![proptest_config(ProptestConfig::with_cases(200))]

    #[test]
    fn every_arena_node_has_alternatives((spec, input) in grammar_and_input()) {
        let grammar = SPG::load(&spec).unwrap();
        let mut parser = stub_parser(grammar);
        let Ok(ast) = parser.parse(&input, 0) else { return Ok(()); };
        let arena = ast.arena();

        for id in 0..arena.node_count() {
            let Some(node) = arena.node(id) else { continue; };
            // Every node should have at least one packed alternative
            let alts = arena.alts_for(id);
            prop_assert!(
                alts.is_some(),
                "node {id} (nt={}, span=[{},{}]) has no alternatives",
                node.nt, node.span.start, node.span.end
            );
        }
    }
}
