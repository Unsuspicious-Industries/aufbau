use crate::logic::{Parser, grammar::Grammar};
use once_cell::sync::Lazy;
use std::collections::{HashMap, hash_map::DefaultHasher};
use std::hash::{Hash, Hasher};
use std::sync::Mutex;

static TEST_DATA_CACHE: Lazy<Mutex<HashMap<u64, Grammar>>> =
    Lazy::new(|| Mutex::new(HashMap::new()));

pub fn _load_test_data(spec: &str) -> (Grammar, Parser) {
    // compute a hash key for the spec
    let mut hasher = DefaultHasher::new();
    spec.hash(&mut hasher);
    let key = hasher.finish();
    let mut cache = TEST_DATA_CACHE.lock().unwrap();
    let grammar = cache
        .entry(key)
        .or_insert_with(|| Grammar::load(spec).unwrap())
        .clone();
    let parser = Parser::new(grammar.clone());
    (grammar, parser)
}
